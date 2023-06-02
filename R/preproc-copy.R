## Copyright (C) Brodie Gaslam
##
## This file is part of "r2c - Fast Iterated Statistic Computation in R"
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 or 3 of the License.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## Go to <https://www.r-project.org/Licenses> for copies of the licenses.

## Functions to support preprocessing of calls so data is bound to memory
## allocated in a manner compatible with branching code.  Primarily this is for
## data e.g. set in branches, and used after the branches.  Post branch code can
## address a single location for a particular variable irrespective of what
## branch is taken.  This creates the constraint that such variables must be the
## same size irrespective of branch.  Implementation means ensuring such data is
## assigned to an r2c generated allocation that can be relocated, which is done
## by adding instruction to copy data into such allocations.

# Frequently used indices (probably should just make data flat...).

CAND <- c('copy', 'cand')
ACT <- c('copy', 'act')
# Local binding
B.LOC <- c('bind', 'loc')
# Local and computed
B.LOC.CMP <- c('bind', 'loc.compute')
# Global and computing
B.ALL <- c('bind', 'all')

# Copy Branch Data
#
# `r2c` operates by analyzing an expression and pre-determining memory required
# for each sub-computation in the expression.  Temporary vectors are then
# allocated to accomdate each sub-computation.  Different sub-computations may
# use the same temporary allocation if their results are not required to exist
# concurrently.
#
# Sub-computations take as inputs pointers to the temporary memory that
# child sub-computations wrote their results to, as well as a pointer to a
# temporary memory location to write its result to.  E.g in:
#
#     x + (y * z)
#
# Conceptually `r2c` could generates code as follows, where x, y, z, tmp1, tmp2,
# and result are all pointers to doubles:
#
#     # Run once at allocation time (assuming all vectors same size)
#     alloc(tmp1, sizeof(<iteration-vec>))
#     alloc(tmp2, sizeof(<iteration-vec>))
#     alloc(result, sizeof(<full-result-vec>))
#
#     # Each iteration
#     mult(y, z, tmp1)                      # y * z -> tmp1
#     add(x, tmp1, tmp2)                    # x + tmp1 -> tmp2
#     # copy tmp2 to correct spot in result vector
#     copy(result + iteration_offset, tmp2)
#
# But instead, for efficiency, `r2c` aliases `tmp2` to the result vector.  This
# saves the copy each iteration:
#
#     # Run once at allocation time
#     alloc(tmp1, sizeof(y * z))
#     alloc(result, sizeof(<precomputed-full-result-size>))
#
#     # Each Iteration
#     tmp2 = result + iteration_offset
#     mult(y, z, tmp1)
#     add(x, tmp1, tmp2)   # this writes directly to result
#
# In order for the aliasing to work it must be the case that e.g. `tmp2` can be
# treated like any other temporary allocation.  It must be writeable, and it
# has to be available to hold other intermediate calculations as any temporary
# allocation would be.  Normally this is a straightforward in a depth-first
# compute / reduce pattern, but it is complicated by:
#
# * External memory references.
# * Non-computing expressions.
# * Branches, and bindings in branches (e.g. `if(a) x <- mean(y)`).
#
# @section External Memory References:
#
# External memory references are symbols that resolve to memory allocated
# outside of `r2c`, either those bound to the iteration varying data, or to the
# enclosing environment chain.  We have a problem when the result of an `r2c`
# expression is such a value, e.g. one that simply returns such
# an external reference:
#
#    x  # return whatever is bound to `x`
#
# In this case `x` would be external memory we cannot modify that needs to be
# explicitly copied into the result, as opposed to implicitly copied by a
# computing expression.  To allow this type of return value we inject copy calls
# into the expression, such that:
#
#    x
#
# Becomes:
#
#    vcopy(x)
#
# The user-visible semantics are unchanged, but now the R expression is
# compatible with our result aliasing strategy.
#
# @section Passive vs Computing:
#
# In the prior example, `x` is a non-computing expression (a.k.a passive)
# because the return value of the expression is the same memory as the input.
# Other example are:
#
#     x <- y
#     x <- {y; z}
#
# In contrast:
#
#     mean(x)
#
# Is a computing (non-passive) expression because it's result will be in
# new `r2c` allocated memory.  A bit more complicated:
#
#     x <- {z <- mean(y); u}
#
# This is considered passive because the return value of `{` is `u`.  So it's
# possible for a passive expression to contain a computed expression so long as
# the computed expression is not returned.  Passiveness is weak, so if a
# sub-call returns a computed expression then the entire expression becomes
# computed, e.g. the following is computed:
#
#     x <- y <- mean(z)
#
# @section Branches
#
# To implement branches we require that every symbol written to in one branch
# that is used subsequent to the branch be bound to the same memory in both
# branches.  This allows subsequent references to that symbol to work
# irrespective of branch taken at run time.  This is accomplished at allocation
# time by first processing the call as if the branches are just sequential, but
# then remapping the allocations across the branches bound to the same symbol to
# a shared allocation.  Subsequent accesses will then read from the shared
# allocation.  For the remapping to be possible the allocations bound to each
# symbol must have been written to in the branch (i.e. be branch local).
# If this does not happen naturally it can be forced by injecting a `vcopy`.
#
# The same remapping happens with the branch return value.  It is possible for
# an assignment to also be a return value, e.g. `if(a) x <- y`.
#
# Consider:
#
#     if(a) mean(x)
#     else mean(y)
#
# In this case, the return value of both branches of the `if/else` are branch
# local because they are generated by an in-branch computation.  We can thus
# take the two allocations and remap them to a single new allocation.  It is an
# allocation-time error for the two allocations to be unequal size.
#
# A more interesting case:
#
#     if(a) mean(x)
#     else y
#
# `y` is not branch local, so we cannot just remap it freely without potentially
# intefering with branch-external calculations.  In some cases we _could_, but
# discerning the cases where it is safe to do so requires substantial additional
# complexity.  To resolve the above the expression would be processed into:
#
#     if(a) mean(x)
#     else vcopy(y)
#
# A more complicated example highlights the risk of not `vcopy`ing:
#
#    x <- mean(y)    # original `x` alloc, it is r2c local
#    if(a) {
#      x <- x + 1    # new `x` alloc, frees original `x` alloc
#      z <- mean(x)  # `z` could be written to the original `x` alloc
#    } else {
#      z <- x
#    }
#    x + z
#
# We cannot use the original `x` allocation for `z` even though it would work
# for the `else` branch.  In the special case where in both branches we have
# e.g. `z <- x`  and `x` isn't touched we could avoid the `vcopy`, but we don't
# worry about that special case.
#
# Another interesting complexity where we bind the return value of a branch, and
# also generate bindings in the return value:
#
#    y <- if(a) {      # if return value bound to `y`
#      x <- mean(y)
#      sum(y)
#    } else {
#      x <- mean(z)    # this whole expression needs `vcopy`ing
#    }
#    x + y
#
# Where even though `x` is created independently in each branch, we still need
# to `vcopy` the second one because we cannot have the `x` memory bound both
# `x`, and to the return value of the `if`.
#
# @section Loops:
#
# Loops are treated like branches, where the branches are not running the loop
# at all, and running it one or more times.
#
# @section Vcopy vs Rec:
#
# The implementation of the call processing will inject `vcopy` calls where
# necessary, and will also inject `rec` calls.  The `rec` call is a marker that
# tells the allocator the expression in question needs to be reconciled across
# branches (i.e. the pair of expressions need to write to the same memory).
# This is needed because some expressions that need to be reconciled don't need
# to be copied.  So e.g. our earlier example:
#
#     if(a) mean(x)
#     else y
#
# The processed call will look like:
#
#     if(a) rec(mean(x))
#     else rec(vcopy(y))
#
# This tells the allocator that `mean(x)` and `vcopy(y)` both need to write to
# the same location.  `mean(x)` does not require a `vcopy` because it computes
# and thus writes to r2c allocated memory.  When there are multiple
# reconciliations required in a single `if/else` the allocator will pair up
# allocations from context.
#
# @seealso copy_branchdat_rec for details on what gets copied and why.
# @param x call to process

copy_branchdat <- function(x) {
  if(is.symbol(x)) {
    # Special case: a single symbol.  For recursive logic to work cleanly every
    # symbol needs to be part of a call
    x <- en_vcopy(x)
  } else {
    # Compute locations requring vcopy
    promoted <- unique(copy_branchdat_rec(x)[[ACT]])

    # Modify the symbols / sub-calls that need to be vcopy'ed
    x <- inject_rec_and_copy(x, promoted)
  }
  x
}
# Compute Locations of Required `rec` and `vcopy`s
#
# Because we don't now know whether we need to `vcopy` an unbound symbol or a
# symbol bound in an if/else branch until after the point where the `vcopy`
# needs to happen, we record the coordinates of all such symbols in the call
# tree as "candidates" for `vcopy`.
#
# Once we can determine that they should be `vcopy`ed, e.g. because the symbol
# was used after the if/else, or it was returned as the final result, we promote
# the candidates to an "actual" `vcopy` list.  These are components in the
# `data` parameter and return value.  When the full recursive traversal of the
# input call is complete, we can use the returned "actual" list and modify calls
# to add `vcopy` (this is done by `inject_rec_and_copy`).
#
# Symbols bound in branches are only required to reference memory allocated in
# the branch if they are used after the corresponding if/else. Other
# branch-bound symbols are branch-local and thus don't require `vcopy`. Because
# of this distinction we track local bindings and global bindings separately in
# the `data` object.  We also track whether local bindings were computed locally
# or not, as sometimes locally computed values can avoid a copy.
#
# In general, we try to avoid unnecessary `vcopy`s, but to avoid
# over-complicating the code there is no guarantee that there are no redundant
# `vcopy`s.
#
# @seealso `copy_branchdat` for context, `inject_rec_and_copy` for how we
#   actually modify the call, `generate_candidate` for additional details on
#   what requires reconciliation / copy..
# @param index integer vector with the coordinates of `x` in the outermost
#   expression (see `callptr`).
# @param assign.to character vector of symbol names that are part of current
#   binding call chain (e.g. in `a <- b <- {d <- a+b; c}` once the recursion
#   gets to `c` it will be `c("a", "b")` because both `a` and `b` will be
#   assigned to what `c` points to..
# @param last whether the current call being processed was the last sub-call of
#   the parent call.  This is only ultimately relevant for passive calls like
#   assignment and braces which could pass on an external allocation as the
#   result of an `r2c` expression. Normal calls like `mean(x)` compute and thus
#   their result will be an r2c allocation.
# @param branch.res whether the parent call is a branch call, the current call
#   is part of the return value, AND the result of the branch is potentially
#   used (dead code notwithstanding) so we need to vcopy the return value.
# @param in.branch whether the parent call (or effective parent call in the case
#   the parent call is passive) is a branch.
# @param in.compute whether there is some parent call that computes on the
#   current call (e.g. `mean(x)` where `x` is the current call).  Used to
#   determine whether the result of a branch is further used.
# @param data list with named elements:
#   * "bind": a list containing branch-local bindings, branch-local computed
#     bindings, and global bindings.
#   * "copy": a list with elements "cand" and "act", each respectively a list of
#     `callptr` generated objects used to track calls that need to be reconciled
#     or vcopied (potentially for "cand", definitively for "act").
#   * "passive": whether `x` is passive (i.e. non-computing); this is part of
#     what the caller uses to determine whether a reconciliation needs to be
#     vcopied.
#   * "assigned.to": character vector of characters that were bound by the
#     processed call or its children, used to set triggers in some cases.
#   * "leaf.name": if the leaf of a call is a symbol, the name of the symbol.
# @return an updated `data` object.

copy_branchdat_rec <- function(
  x, index=integer(), assign.to=character(),
  in.compute=FALSE, in.branch=FALSE, branch.res=FALSE, last=TRUE,
  data=list(
    bind=list(loc=character(), all=character(), loc.compute=character()),
    copy=list(cand=list(), act=list()),
    passive=TRUE, assigned.to=character(), leaf.name=""
  )
) {
  sym.name <- get_lang_name(x)
  call.assign <- first.assign <- leaf <- FALSE

  if (is.symbol(x)) {
    sym.local.cmp <- sym.name %in% data[[B.LOC.CMP]]
    sym.local <- sym.name %in% data[[B.LOC]]
    sym.global <- sym.name %in% data[[B.ALL]]
    passive <- !sym.local.cmp
    leaf <- TRUE
    data[['leaf.name']] <- sym.name

    # For symbols matching candidate(s): promote candidate if allowed.
    cand <- data[[CAND]]
    cand.prom.i <- which(names(cand) == sym.name & !sym.local)

    # Effect promotions, and clear promoted candidates from candidate list.
    data[[ACT]] <- c(data[[ACT]], cand[cand.prom.i])
    data[[CAND]] <- clear_candidates(data[[CAND]], cand.prom.i)

    if(!in.branch && length(assign.to) && sym.local.cmp) {
      # Outside of branches, aliasing a locally computed symbol makes the others
      # also locally computed.
      data[[B.LOC.CMP]] <- union(data[[B.LOC.CMP]], assign.to)
    }
  } else if(is.call(x)) {
    # Recursion, except special handling for if/else and for assignments
    call.assign <- sym.name %in% ASSIGN.SYM
    # `passive` is whether this single call is passive, `data[['passive']]`
    # is whether all its sub-calls also return passively (and is only knowable
    # after we've recursed through the expression).  For this function purposes
    # we treat `r2c_if`  and similar as computing, b/c we are guaranteed the
    # return value will be computed if used, even though strictly it does not
    # compute itself.
    passive <- sym.name %in% PASSIVE.BRANCH.SYM
    leaf <- !passive # for candidacy purposes, computing calls are leaves

    if(sym.name %in% BRANCH.EXEC.SYM) {
      if(sym.name != 'r2c_if')
        stop("Internal Error: add support for loop branch")
      # New if/else context resets all local bindings
      data.next <- data
      data.next[[B.LOC]] <- data.next[[B.LOC.CMP]] <- character()

      # When branch result used, subsequent code needs to know it is branch.res.
      branch.res.next <- last || length(assign.to) || in.compute

      # Recurse through each branch independently since they are "simultaneous"
      # with respect to call order (either could be last too).
      data.T <- copy_branchdat_rec(
        x[[c(2L,2L)]], index=c(index, c(2L,2L)), data=data.next, last=last,
        in.branch=TRUE, branch.res=branch.res.next
      )
      data.F <- copy_branchdat_rec(
        x[[c(3L,2L)]], index=c(index, c(3L,2L)), data=data.next, last=last,
        in.branch=TRUE, branch.res=branch.res.next
      )
      # Recombine branch data and the pre-branch data
      data <- merge_copy_dat(data, data.T, data.F, index)

      # Guaranteed branch result is non-passive (if it is used)
      data[['passive']] <- FALSE
    } else {
      passive.now <- data[['passive']] # pre-recursion passive status
      rec.skip <- 1L
      if(call.assign) {
        tar.sym <- get_target_symbol(x, sym.name)
        assign.to <- union(assign.to, tar.sym)
        rec.skip <- 1:2
      }
      # Recurse on language subcomponents
      for(i in seq_along(x)[-rec.skip]) {
        if(is.language(x[[i]])) {
          # assign.to is forwarded by passive calls
          next.last <- i == length(x) && passive
          assign.to.next <- if(!next.last) character() else assign.to
          data[['passive']] <- passive.now
          data <- copy_branchdat_rec(
            x[[i]], index=c(index, i),
            assign.to=assign.to.next, last=last && next.last,
            branch.res=branch.res && next.last,
            in.compute=in.compute || !passive,
            in.branch=in.branch,
            data=data
          )
      } }
      data[['passive']] <- passive && data[['passive']]

      if(call.assign) {
        # Any prior candidates bound to this symbol are voided by the new
        # assignment.  We need to clear them, except:
        #
        # * We don't want to clear the candidate we just created.
        # * We need to allow candidates that will be cleared to be used during
        #   computation of the assignment (so we do it after recursion above).
        #
        #  Start by finding the first candidate earlier than this assignment
        indices <- lapply(data[[CAND]], "[[", 2L)
        indices.gt <- vapply(indices, index_greater, TRUE, index)
        # Clear those candidates
        data[[CAND]] <-
          data[[CAND]][names(data[[CAND]]) != tar.sym | indices.gt]

        # Update bindings
        if(!data[['passive']]) {
          data[[B.LOC]] <- union(data[[B.LOC]], tar.sym)
          data[[B.LOC.CMP]] <- union(data[[B.LOC.CMP]], tar.sym)
          data[[B.ALL]] <- union(data[[B.ALL]], tar.sym)
        } else {
          data[[B.LOC]] <- union(data[[B.LOC]], tar.sym)
          if(in.branch) {
            # non-computing local expressions make global bindings non-global
            data[[B.ALL]] <- setdiff(data[[B.ALL]], tar.sym)
          }
        }
      }
    }
  }
  data <- generate_candidate(
    x, data, index, branch.res=branch.res, in.branch=in.branch, last=last,
    passive=passive, call.assign=call.assign, assign.to=assign.to, leaf=leaf,
    sym.name=sym.name
  )
  data[['assigned.to']] <- assign.to
  data
}
# Generate Candidate (and Actual) Call Pointers
#
# Compute which expressions might need to  be copied and/or reconciled, and
# generate candidates (i.e. `callptr`s to the language location of the
# expression) if it is not yet know whether they are truly needed, or actuals if
# they are needed.
#
# Candidates are needed for assignments in branches and return values from
# branches **if** such values might be used (see `copy_branchdat_rec`).  A
# branch return value might be used if it is assigned to a symbol, or if it is
# also the return value of the entire `r2c` expression.
#
# Each candidate will be assigned a (set of) triggering symbol(s) that will,
# should they appear after the branch, cause a promotion of the candidate to an
# actual (requirement of `rec`/`vcopy`).  For example in:
#
#     if(a) x <- z      # z is a candidate for promotion
#     x                 # use of `x` triggers any `x` candidates
#
# `z` becomes a candidate with `x` as the triggering symbol.  Because we do use
# `x` after the branch the candidate is promoted and we would get:
#
#     if(a) x <- rec(vcopy(z))
#     x
#
# > For expositional convenience we will ignore the implicit `else` branch in
# > all examples.  Those would be modified (see `merge_copy_dat`).
#
# A variation:
#
#     if(a) x <- z
#
# Becomes:
#
#     if(a) rec(vcopy(x <- z))
#
# We know the branch result will be used (as the r2c return value) so the entire
# branch result is copied.  More interesting:
#
#     u <- if(a) x <- z
#     x + u
#
# The branch result is used, but so is the binding generated by the branch
# result, so we get:
#
#     u <- if(a) rec(vcopy(x <- rec(vcopy(z))))
#     x + u
#
# If we did not copy both we could create conflicts in cases with the same
# symbols bound to different values in the alternate branch (not shown here).
#
# An important concept is  "passive" vs. computed expressions (see "Passive
# vs. Computing" in `copy_branchdat`), which affect whether a candidate needs to
# be copied or just reconciled.
#
# In some cases we can know (or wish to assume) that a `rec`/`vcopy` will always
# be required irrespective of the code that follows.  In those cases we generate
# actual `callptr`s instead of candidates, e.g. if an `r2c` expression returns
# an external symbol:
#
#     if(a) rec(copy(x))  # this is branch result and r2c result
#     else -y
#
# We can tell if an sub-expression is part of the return value of an expression
# without having to process the whole expression, so we know thta `x` needs to
# be copied as soon as we encounter it.
#
# @param x current sub-expression being processed.
# @param data see `copy_branchdat_rec`.
# @param branch.res whether `x` could be the return value of a branch, **and**
#   the return value could be further used. This status carries through to
#   sub-expressions. See details.
# @param in.branch whether `x` is nested inside a branch.
# @param last whether `x` could be the return value of the entire `r2c`
#   expression.  It is possible but not required for `branch.res` and `last` to
#   both be true.  Like `branch.res` this status carries to sub-expressions.
# @param call.assign whether `x` is an assigning call.
# @param leaf `x` is a symbol, OR a computing **call** (because we never make
#   candidates from children of a computing call).
#
# @return data, updated (see copy_branchdat_rec).

generate_candidate <- function(
  x, data, index, branch.res, in.branch, last, passive, call.assign, assign.to,
  leaf, sym.name
) {
  # data[['passive']]: like `passive`, except if the outer calls are passive,
  # but the return value of the inner sub-calls are computed, the call is
  # considered non-passive.
  # add_actual sets data[['passive']], so save current value.
  passive.now <- data[['passive']]

  first.assign <- call.assign && length(assign.to) == 1L

  if(in.branch) {
    # Symbols bound in branches will require rec and/or vcopy of their payload
    # **if** they are used. vcopy not always needed, e.g:
    #
    #   if(a) x <- rec(vcopy(y)); x     # rec and vcopy
    #   if(a) x <- rec(mean(y)); x      # only rec
    #
    if(length(assign.to) && !first.assign && (call.assign || leaf)) {
      # Always reconcile
      triggers <- if(call.assign) assign.to[-length(assign.to)] else assign.to
      data <- add_candidate_callptr(
        data, index, triggers=triggers, rec=TRUE,
        copy=passive # but only vcopy passive
      )
      # Locally Computed symbols require copy if used after branch (note
      # triggers), e.g:
      #
      #     y <- if(a) {
      #       x <- mean(y)
      #       x
      #     }
      #     y + x
      #
      # Notice `triggers=sym.name`:
      if(!passive && is.symbol(x)) {
        data <- add_candidate_callptr(
          data, index, triggers=sym.name, rec=FALSE, copy=TRUE
        )
      }
    }
    # Branch result requires an additional outer vcopy, e.g:
    #
    #     if(a) rec(vcopy(x))          # or...
    #     if(a) rec(vcopy(x <- ...))
    #
    # `branch.res` only TRUE if the branch result _could_ be used either because
    # also `r2c` exp return value, or bound to symbol.
    if(branch.res) {
      if(leaf && !length(assign.to)) {
        # Always reconcile
        data <- add_actual_callptr(data, index, rec=TRUE, copy=passive)
        # See "Locally computed Symbols require copy" in prior branch
        if(!passive && is.symbol(x)) {
          data <- add_candidate_callptr(
            data, index, triggers=sym.name, rec=FALSE, copy=TRUE
          )
        }
      } else if (first.assign) {
        # Always reconcile
        data <- add_actual_callptr(data, index, rec=TRUE, copy=FALSE)
        # Might also need vcopy
        if(passive.now) {
          data <- add_actual_callptr(data, index, rec=FALSE, copy=TRUE)
        } else {
          # Computed require outer vcopy if symbols they are bound to are used.
          # data[['assigned.to']]: all nested assignments (e.g. x <- y <- z...)
          data <- add_candidate_callptr(
            data, index, triggers=data[['assigned.to']], rec=FALSE, copy=TRUE
          )
        }
      }
    }
  } else if (last) {
    # Final return not from branch: need to copy if not computed already.
    # Reconciliation not necessary since we're not in branch.
    # if(a) vcopy(x)   # need to copy
    # if(a) mean(x)    # don't need to copy
    computed.sym <- c(data[[B.ALL]], data[[B.LOC.CMP]])
    assign.passive <- !data[['leaf.name']] %in% computed.sym
    sym.passive <- is.symbol(x) && !sym.name %in% computed.sym
    if(
      (first.assign && passive.now && assign.passive) ||
      (sym.passive && !length(assign.to))
    )
      data <- add_actual_callptr(data, index, rec=FALSE)
  }
  data
}
# A "Pointer" to Spot in Call Tree to Modify
#
# Allows us to track potential candidates for `vcopy` promotion and/or `rec`
# (reconciliation) tagging.
#
# `callptr` is used both to create candidates and actual direct promotions, but
# this is not ideal.  In particular, once a candidate is promoted, or a sub-call
# is direct-promoted, the meaning of the `name` field is lost as the only thing
# that matters is the location of the sub-call that needs to be `vcopy`ed, not
# the symbol that caused the promotion (and in direct-promotion there is not a
# causal symbol anyway).
#
# @param name scalar character the symbol that triggers promotion of a
#   candidate, which when a symbol is up for promotion is typically not that
#   symbols name (e.g. in `x <- y; x` the trigger is `x`, but the symbol that
#   will be `vcopy`ed is `y`).
# @param index integer vector that can be used to pull up a sub-call within a
#   call tree e.g. with `x[[index]]` where `index` might be `c(2,3,1)`.  This
#   gives the location of the expression to `vcopy`/`rec`.  If the index ends in
#   0L then a `sym <- vcopy(sym)` (assuming `x` is the symbol `sym`) is added at
#   the beginning of the branch the index points to; this represents an external
#   symbol missing from one branch but used in the other.  For the special case
#   where index is 0 length, we record 0L as an indication that the entire call
#   needs to be `vcopy`/`rec`'ed.
# @param rec whether this is a value that will require reconciliation across
#   branches.
# @param copy whether this is a value that need to be `vcopy`ed (see
#   copy_branchdat`).

callptr <- function(name, index, rec=TRUE, copy=FALSE) {
  # If the entire expression needs to be wrapped, the index is zero length.  We
  # deal with that special case by setting an index of 0L
  if(!length(index)) index <- 0L
  list(name=name, index=index, rec=rec, copy=copy)
}
add_actual_callptr  <- function(data, index, rec=TRUE, copy=TRUE) {
  new.act <- callptr(NA_character_, index, copy=copy, rec=rec)
  data[[ACT]] <- c(data[[ACT]], list(new.act))
  data[['passive']] <- !copy
  data
}
add_candidate_callptr <- function(data, index, triggers, rec=TRUE, copy=TRUE) {
  new.cand <- gen_callptrs(triggers, index, copy=copy, rec=rec)
  data[[CAND]] <- c(data[[CAND]], new.cand)
  data
}
gen_callptrs <- function(names, index, rec, copy)
  sapply(names, callptr, copy=copy, rec=rec, index=index, simplify=FALSE)

# Remove Promoted Candidates
#
# Once candidates are promoted, they should be removed from candidate list.  In
# general, we remove candidates based on the position in the call tree they
# promote, so it is possible that one promotion will clear many candidates,
# including some bound to different trigger symbols.  The exception is for
# balancing candidates (those with indices ending in 0 that are not just 0, see
# `merge_copy_dat`), which are only removed if they also match on symbol.
#
# @param cand list of candidate pointers
# @param ii which of those in `cand` were promoted

clear_candidates <- function(cand, ii) {
  cand.unm <- lapply(cand, function(x) {x[[1L]] <- ""; x})
  to.clear <- logical(length(cand))
  # use hashed matching to match candidate pointers
  for(i in ii) {
    ci <- cand[[i]]
    if(ci[['index']][length(ci[['index']])]) {
      # Regular promotion don't care about names
      to.clear[cand.unm %in% cand.unm[i]] <- TRUE
    } else {
      # Balancing promotion (see `merge_copy_dat`) do care about names
      to.clear[cand %in% cand[i]] <- TRUE
    }
  }
  cand[!to.clear]
}
# Compare Two `callptr` Indices
#
# TRUE if the index a points to `a` position later in the tree than `b` in a
# depth-first traversal order.
#
# @param a a integer vector of the form of `index` from `callptr`.
# @param b same as a.
# @return TRUE if `a` is a sub-call appearing later in the call tree than `b`.

index_greater <- function(a, b) {
  if(length(a) > length(b)) length(b) <- length(a)
  else length(a) <- length(b)
  a[is.na(a)] <- 0L
  b[is.na(b)] <- 0L
  all(a >= b) & any(a > b)
}
# Merge Candidates between Branches
#
# `a` corresponds to the TRUE branch, and `b` to the FALSE branch.
# `old` is the state before the branch.  This function compares the three sets
# of candidates and actuals and reconciles them into one self consistent set.
# Both `a` and `b` contain lists at index CAND and ACT.  These are named lists
# with the names being those of symbols in the expression that trigger
# promotions, and the elements each produced by `callptr` (essentially a
# "pointer" to the location of a symbol that might need to be `vcopy`ed, see
# `callptr`).  Binding tracking information is also reconciled.
#
# The pointer lists are named for convenience, but we rely on each "pointer"
# element also containing the name.  This allows us to use `unique` and similar
# on the lists to recongize that elements with different names are different
# despite pointing to the same location (because each `vcopy` candidate might
# have multiple triggers, e.g. if `x` is a candidate in `y <- z <- x`, then
# either use of `y` or `z` could result in promotion).
#
# @param a list should be `data` from `copy_branchdat_rec` (see details).
# @param b like `a`.
# @param old like `a`.
# @param index integer vector the index into the call returning the expression
#   setting the current if/else context (see `callptr`).

merge_copy_dat <- function(old, a, b, index) {
  # Removed shared history from both branches.  This is for the case where a
  # pre-existing candidate is cleared in one branch, but not the other.  If we
  # didn't remove shared history, the old candidate would be seen as new.
  a.cand <- setdiff(a[[CAND]], old[[CAND]])
  b.cand <- setdiff(b[[CAND]], old[[CAND]])
  # Track shared history to add back later.
  old.cand <- intersect(a[[CAND]], b[[CAND]])

  # Candidates or computed bindings in one branch missing in the other need to
  # be balanced by injecting e.g.  `x <- vcopy(x)`
  a.names <- c(names(a.cand), a[[B.LOC.CMP]], a[[B.LOC]])
  b.names <- c(names(b.cand), b[[B.LOC.CMP]], b[[B.LOC]])
  a.miss <- setdiff(b.names, a.names)
  b.miss <- setdiff(a.names, b.names)
  # Inject at start (hence 0L; so branch return value unchanged).
  a.miss.list <- gen_callptrs(a.miss, c(index, 2L, 0L), copy=TRUE, rec=TRUE)
  b.miss.list <- gen_callptrs(b.miss, c(index, 3L, 0L), copy=TRUE, rec=TRUE)

  # Recombine all the pieces into the new set of candidates
  copy.cand <- unique(
    c(old.cand, a.cand, b.cand, a.miss.list, b.miss.list))

  # Merge all promotions. Promotions irrevocable, so union is sufficient.
  copy.act <- unique(c(a[[ACT]], b[[ACT]], old[[ACT]]))

  # regen names lost in unique/union
  names(copy.cand) <- vapply(copy.cand, "[[", "", 1L)
  names(copy.act) <- vapply(copy.act, "[[", "", 1L)
  old[['copy']] <- list(cand=copy.cand, act=copy.act)

  # Any symbols that were assigned to in the branches are no longer local
  # in the parent expression.  Because usage of the symbols will promote any
  # associated candidates we can assume all locals become global computed.
  branch.all.new <- setdiff(union(a[[B.ALL]], b[[B.ALL]]), old[[B.ALL]])
  branch.loc <- union(a[[B.LOC]], b[[B.LOC]])
  branch.loc.cmp <- union(a[[B.LOC.CMP]], b[[B.LOC.CMP]])
  old[[B.ALL]] <- unique(c(old[[B.ALL]], branch.loc, branch.loc.cmp))
  old[[B.LOC]] <- setdiff(old[[B.LOC]], union(branch.all.new, branch.loc))
  old[[B.LOC.CMP]] <-
    setdiff(old[[B.LOC.CMP]], union(branch.all.new, branch.loc.cmp))

  old
}
# Wrap a Symbol in vcopy / rec

VCOPY.FUN.NAME <- call("::", as.name("r2c"), as.name("vcopy"))
en_vcopy <- function(x) as.call(list(VCOPY.FUN.NAME, x=x))

REC.FUN.NAME <- call("::", as.name("r2c"), as.name("rec"))
en_rec <- function(x, clean=FALSE) {
  tmp <- as.call(list(REC.FUN.NAME, x=x))
  if(clean) names(tmp) <- NULL  # recompose_ifelse uses en_rec, hence this option
  tmp
}

# Inject vcopy Calls
#
# @param x call to modify.
# @promoted list of `callptr` objects pointing to sub-calls/symbols that need to
#   be wrapped in `vcopy` / `rec`.

inject_rec_and_copy <- function(x, promoted) {
  if(length(promoted)) {
    # Order the indices in reverse order of appearance
    indices <- lapply(promoted, "[[", "index")
    indices.eq <- lapply(indices, `length<-`, max(lengths(indices)))
    indices.mx <- do.call(cbind, indices.eq)
    indices.order <- do.call(
      order, c(split(indices.mx, row(indices.mx)), list(na.last=FALSE))
    )
    # Inject the vcopies in reverse order so that indices are not made invalid
    # by tree modifications ahead of them (which could happen if we have nested
    # vcopies such as `vcopy(y <- vcopy(x))` at end of branch).
    for(i in promoted[rev(indices.order)]) {
      if(identical(i[["index"]], 0L)) {
        # Special case, wrap entire expression in vcopy
        if(i[['copy']]) x <- en_vcopy(x)
      } else if(i[["index"]][length(i[["index"]])]) {
        # Trailing index not zero, so wrap a symbol/call in vcopy if needed
        if(i[['copy']]) x[[i[["index"]]]] <- en_vcopy(x[[i[["index"]]]])
      } else {
        # These always require a copy, even if the alternate branch doesn't.
        # Insert an e.g. `x <- vcopy(x)` when trailing index is zero
        par.idx <- i[["index"]][-length(i[["index"]])]
        call <- x[[c(par.idx, 2L)]]  # par.idx should point to if_true/false
        call.sym <- get_lang_name(call)
        if(!is.call(call) || call.sym != "{") call <- call("{", call)

        # generate e.g. `x <- vcopy(x)`
        sym.miss <- as.symbol(i[["name"]])
        sym.vcopy <- call("<-", sym.miss, en_vcopy(sym.miss))
        call.list <- as.list(call)
        call <- as.call(c(call.list[1L], list(sym.vcopy), call.list[-1L]))
        # in order to look match-called we need names on the call
        names(call)[seq(2L, length(call), 1L)] <- "..."
        x[[c(par.idx, 2L)]] <- call
        # update index to point to new `vcopy` for potential `rec` next
        i[["index"]] <- c(par.idx, 2L, 2L, 3L)
      }
      if(i[['rec']]) {
        if(identical(i[["index"]], 0L)) x <- en_rec(x)
        else x[[i[["index"]]]] <- en_rec(x[[i[["index"]]]])
      }
    }
  }
  x
}
