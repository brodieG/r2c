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

#' @include util.R

# Frequently used indices (probably should just make data flat...).

CAND <- c('copy', 'cand')
ACT <- c('copy', 'act')
# Local binding
B.LOC <- c('bind', 'loc')
# Local and computed
B.LOC.CMP <- c('bind', 'loc.compute')
# Global and computing
B.ALL <- c('bind', 'all')
# Global and computing, but to inject at front by e.g. subassign
B.ALL0 <- c('bind', 'all0')
# Track all bindings irrespective of type for free variables
B.NAMED <- c('bind', 'named')

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
# compatible with our result aliasing strategy.  We use `vcopy` instead of
# `copy` to avoid a name collision with `data.table::copy`.
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
# at all, and running it one or more times.  Loops have additional preprocessing
# requirements to handle use before set variables (see `copy_fordat`).
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
    sym.free <- as.character(x)
    x <- en_vcopy(x)
  } else {
    # Compute locations requring vcopy
    branch.dat <- copy_branchdat_rec(x, x0=x)
    sym.free <- branch.dat[['free']]

    # Modify the symbols / sub-calls that need to be vcopy'ed
    x <- inject_rec_and_copy(x, branch.dat)
  }
  sym.free[sym.free == '.R2C.DOTS'] <- '...'
  list(call=x, sym.free=sym.free)
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
# This also handles restrictions on `[<-` assignments:
#
# * Return value cannot be used.
# * RHS needs to be vcopy'ed if it is the same as LHS e.g (x[seq_along(x)] <- x).
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
# @param sub.assign.to scalar character of any symbol being sub assigned to
#   (e.g. in `x[a] <- y`, "x").  These are unchainable so there can only be one.
# @param last whether the current call being processed was the last sub-call of
#   the parent call.  This is only ultimately relevant for passive calls like
#   assignment and braces which could pass on an external allocation as the
#   result of an `r2c` expression. Normal calls like `mean(x)` compute and thus
#   their result will be an r2c allocation.
# @param branch.res whether the parent call is a branch call, the current call
#   is part of the return value, AND the result of the branch is potentially
#   used (dead code notwithstanding) so we need to vcopy the return value.
# @param in.branch whether the parent call (or effective parent call in the case
#   the parent call is passive) is a branch, designated by the call tree index
#   of the branch.  NULL if not in branch.
# @param in.compute whether there is some parent call that computes on the
#   current call (e.g. `mean(x)` where `x` is the current call).  Used to
#   determine whether the result of a branch is further used.
# @param data list with named elements:
#   * "bind": a list containing branch-local bindings, branch-local computed
#     bindings, global bindings (all, and all0 for those re-injected at
#     beginning of call by e.g. subassign), and every encountered binding
#     (named).
#   * "copy": a list with elements "cand", and "act". Each is a list
#     of `callptr` generated objects used to track calls that need to be
#     reconciled and/or vcopied (potentially for "cand", definitively).
#   * "passive": whether `x` is passive (i.e. non-computing); this is part of
#     what the caller uses to determine whether a reconciliation needs to be
#     vcopied.
#   * "assigned.to": character vector of characters that were bound by the
#     processed call or its children, used to set triggers in some cases.
#   * "leaf.name": if the leaf of a call is a symbol, the name of the symbol.
#   * "free": any used symbols that are not previously bound.
# @return an updated `data` object.

copy_branchdat_rec <- function(
  x, index=integer(), assign.to=character(), sub.assign.to="",
  in.compute=FALSE, in.branch=NULL, branch.res=FALSE, last=TRUE,
  prev.call="",
  data=list(
    bind=list(
      loc=character(), loc.compute=character(),
      all=character(), all0=character(),
      named=character()
    ),
    copy=list(cand=list(), act=list()),
    passive=TRUE, assigned.to=character(), leaf.name="", free=character()
  ),
  x0  # for debugging to see what the indices are pointing at
) {
  sym.name <- get_lang_name(x)
  call.assign <- call.modify <- first.assign <- leaf <- FALSE
  tar.sym <- ""

  if (!is.call(x)) {
    # Could be either a symbol, or alternatively a literal (e.g. `42`).
    # If a a literal, it will be checked for numeric-ness by the allocator.  It
    # should never be the case that external parameters become candidates

    sym.local.cmp <- sym.name %in% data[[B.LOC.CMP]]
    sym.global <- sym.name %in% c(data[[B.ALL]], data[[B.ALL0]])
    passive <- !sym.local.cmp
    leaf <- TRUE
    data[['leaf.name']] <- sym.name  # "" for literals

    if(!sym.name %in% data[[B.NAMED]] && nzchar(sym.name)) {
      data[['free']] <- union(data[['free']], sym.name)
    }
    # For symbols matching candidate(s): promote candidate if allowed.
    cand <- data[[CAND]]
    cand.match <- names(cand) == sym.name
    cand.match.source <- lapply(cand[cand.match], "[[", "br.index")
    cand.after.branch <- vapply(
      cand.match.source,
      function(a, b) {
        # Definitely after branch if not in branch but symbol is from branch
        if(is.null(a)) length(b) > 0
        # Otherwise check if current branch is later than symbol branch
        else {
          if(length(a) > length(b)) length(a) <- length(b)
          index_greater(a, b)
        }
      },
      TRUE, a=in.branch  # in.branch is current position
    )
    cand.prom.i <- seq_along(cand)[cand.match][cand.after.branch]
    cand.prom <- cand[cand.prom.i]

    # Effect promotions, and clear promoted candidates from candidate list.
    data[[ACT]] <- c(data[[ACT]], cand.prom)
    data[[CAND]] <- clear_candidates(data[[CAND]], cand.prom.i)

    # Triggered branch balance candidates may require updating free symbol list
    # For these the trigger is the same as the symbol (e.g. x <- vcopy(x))
    br.bal.free <- vapply(cand.prom, "[[", TRUE, "free")
    br.bal.free.sym <- vapply(cand.prom[br.bal.free], "[[", "", "name")
    data[['free']] <- union(data[['free']], br.bal.free.sym)

    if(is.null(in.branch) && length(assign.to) && sym.local.cmp) {
      # Outside of branches, aliasing a locally computed symbol makes the others
      # also locally computed.
      data[[B.LOC.CMP]] <- union(data[[B.LOC.CMP]], assign.to)
    }
  } else if(is.call(x)) {
    # Recursion, except special handling for if/else and for assignments
    call.assign <- sym.name %in% ASSIGN.SYM
    call.modify <- sym.name %in% MODIFY.SYM
    # `passive` is whether this single call is passive, `data[['passive']]`
    # is whether all its sub-calls also return passively (and is only knowable
    # after we've recursed through the expression).  For this function purposes
    # we treat `r2c_if`  and similar as computing, b/c we are guaranteed the
    # return value will be computed if used, even though strictly it does not
    # compute itself.
    passive <- sym.name %in% PASSIVE.BRANCH.SYM
    # for candidacy purposes, computing calls are leaves, as are the LCOPY funs
    # (really just LSET which is otherwise passive).  This prevents copy/recs
    # from penetrating non-passive and loop rec calls.
    leaf <- !passive || sym.name %in% L.COPY.FUNS

    if(call.modify) tar.sym <- get_target_symbol(x, sym.name)
    sub.assign.to <- if(sym.name == "subassign") {
      # If we ever change to allow this, we then need to prevent this happening
      # in a call parameter(i.e. `fun(x[a] <- y)`). This disallows that as well.
      if(
        branch.res && (last || length(assign.to)) ||
        last || length(assign.to) ||
        !prev.call %in% c("{", CTRL.SUB.SYM)
      )
        stop("Result of `[<-` may not be used directly.")

      tar.sym
    } else ""  # We want sub-assign symbol to spread to its children only

    if(sym.name %in% BRANCH.EXEC.SYM) {
      # Different struture for loops and if/else (see `transform_control`)
      if(sym.name == 'r2c_if') {
        idx.offset <- 0L
      } else {
        idx.offset <- 1L
        # recurse into the iterator (maybe not necessary?)
        data <- copy_branchdat_rec(
          x[[2L]], index=c(index, 2L),
          last=FALSE, branch.res=FALSE,
          in.compute=FALSE, in.branch=in.branch, prev.call=sym.name,
          data=data, x0=x0
        )
      }
      idx.T <- 2L + idx.offset
      idx.F <- 3L + idx.offset
      idx.T.all <- c(index, idx.T)
      idx.F.all <- c(index, idx.F)

      # New branch context resets all local bindings
      data.next <- data
      data.next[[B.LOC]] <- data.next[[B.LOC.CMP]] <- character()

      # When branch result used, subsequent code needs to know it is branch.res.
      branch.res.next <- branch.res || last || length(assign.to) || in.compute

      # Recurse through each branch independently since they are "simultaneous"
      # with respect to call order (either could be last too).  In loops, the
      # not-taken branch is the FALSE branch (see code-ifelse.R, code-loop.R).
      prev.T <- get_lang_name(x[[idx.T]])
      data.T <- copy_branchdat_rec(
        x[[c(idx.T, 2L)]], index=c(idx.T.all, 2L), data=data.next, last=last,
        in.branch=idx.T.all, branch.res=branch.res.next, prev.call=prev.T, x0=x0
      )
      prev.F <- get_lang_name(x[[idx.F]])
      data.F <- copy_branchdat_rec(
        x[[c(idx.F, 2L)]], index=c(idx.F.all, 2L), data=data.next, last=last,
        in.branch=idx.F.all, branch.res=branch.res.next, prev.call=prev.F, x0=x0
      )
      # Recombine branch data and the pre-branch data
      data <- merge_copy_dat(data, data.T, data.F, index, idx.offset)

      # Guaranteed branch result is non-passive (if it is used)
      data[['passive']] <- FALSE
    } else {
      passive.now <- data[['passive']] # pre-recursion passive status
      rec.skip <- 1L
      if(call.assign) {
        assign.to <- union(assign.to, tar.sym)
        # Potentially this could be 1:3 for `for_iter`?  Right now both `seq`
        # and `seq.1` get treated as payloads for the iteration variable, but it
        # shouldn't matter since never used after branch.
        rec.skip <- 1:2
      }
      par.ext.names <- names(VALID_FUNS[[c(sym.name, "extern")]])

      # Recurse on subcomponents (literals too)
      for(i in seq_along(x)[-rec.skip]) {
        # Skip external/constant params
        if(!names2(x)[i] %in% par.ext.names) {
          # assign.to is forwarded by non-leaf calls
          next.last <- i == length(x) && !leaf
          assign.to.next <- if(!next.last) character() else assign.to
          sub.assign.to.next <- if(i != length(x)) "" else sub.assign.to
          data[['passive']] <- passive.now # reset pre-rec passive status
          data <- copy_branchdat_rec(
            x[[i]], index=c(index, i),
            assign.to=assign.to.next, sub.assign.to=sub.assign.to.next,
            last=last && next.last,
            branch.res=branch.res && next.last,
            in.compute=in.compute || !passive,
            in.branch=in.branch, prev.call=sym.name,
            data=data, x0=x0
          )
        } else {
          # We still record free symbols to generate the interface for r2cq/l.
          # This is not perfect because the external expression may be creating
          # symbols it uses for itself, but that's a documented corner case.
          syms <- collect_call_symbols(x)
          new.free.syms <- syms[
            !syms %in% data[[B.NAMED]] & nzchar(syms) &
            !grepl(R2C.PRIV.RX, syms)  # Could these exist here?
          ]
          data[['free']] <- union(data[['free']], new.free.syms)
        }
      }
      data[['passive']] <- passive && data[['passive']]

      if(call.assign) {  # not call.modify
        # Any prior candidates bound to this symbol are voided by the new
        # assignment.  We need to clear them, except:
        #
        # * We don't want to clear the candidate we just created.
        # * We need to allow candidates that will be cleared to be used during
        #   computation of the assignment (so we do it after recursion above).
        #
        # Essentially any candidates that are part of the current assignment
        # chain are kept, but others are dropped.
        indices <- lapply(data[[CAND]], "[[", "index")
        indices.br <- lapply(data[[CAND]], "[[", "br.index")
        indices.gtoc <- vapply(indices, index_greater_or_child, TRUE, index)
        # We know candidates from a different (child) branch are not part of the
        # same assignment chain, so clear those.  Might be missing some other
        # condtion that also requires clearing children.
        indices.br.same <- vapply(indices.br, identical, TRUE, in.branch)
        # Clear those candidates
        data[[CAND]] <- data[[CAND]][
          names(data[[CAND]]) != tar.sym | indices.gtoc & indices.br.same
        ]
      }
    }
  } else stop("Internal Error: disallowed token type ", typeof(x))

  data <- generate_candidate(
    x, data, index, branch.res=branch.res, in.branch=in.branch, last=last,
    passive=passive, call.assign=call.assign, call.modify=call.modify,
    assign.to=assign.to, sub.assign.to=sub.assign.to, leaf=leaf,
    sym.name=sym.name, tar.sym=tar.sym
  )
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
#     if(a) rec(vcopy(x))  # this is branch result and r2c result
#     else rec(-y)
#
# We can tell if a sub-expression is part of the return value of an expression
# without having to process the whole expression, so we know that `x` needs to
# be copied as soon as we encounter it.
#
# @param x current sub-expression being processed.
# @param data see `copy_branchdat_rec`.
# @param branch.res whether `x` could be the return value of a branch, **and**
#   the return value could be further used. This status carries through to
#   sub-expressions. See details.
# @param in.branch whether `x` is nested inside a branch, see
#   `copy_branchdat_rec`.
# @param last whether `x` could be the return value of the entire `r2c`
#   expression.  It is possible but not required for `branch.res` and `last` to
#   both be true.  Like `branch.res` this status carries to sub-expressions.
# @param call.assign whether `x` is an assigning call.
# @param call.modify whether `x` is a modifying call (includes `call.assign`,
#   and additionally e.g. subassign).
# @param leaf `x` is a symbol, or a computing **call**.  For computing calls, we
#   don't need to `vcopy` them because they are a fresh value.
#
# @return data, updated (see copy_branchdat_rec).

generate_candidate <- function(
  x, data, index, branch.res, in.branch, last, passive,
  call.assign, assign.to, call.modify, tar.sym, leaf,
  sym.name, sub.assign.to
) {
  # data[['passive']]: like `passive`, except if the outer calls are passive,
  # but the return value of the inner sub-calls are computed, the call is
  # considered non-passive.
  # add_actual sets data[['passive']], so save current value.
  passive.now <- data[['passive']]
  computed.sym <- unique(c(data[[B.ALL]], data[[B.ALL0]], data[[B.LOC.CMP]]))

  first.assign <- call.assign && length(assign.to) == 1L

  if(sym.name %in% MODIFY.SYM) {
    tar.sym <- get_target_symbol(x, sym.name)
    call.modify <- TRUE
  } else tar.sym <- ""

  # Subassignment from same symbol (e.g. `x[s] <- x`, note `x` twice) must
  # be vcopy'ed to avoid read/write conflicts.  No reconciliation needed.
  if(
    nzchar(sub.assign.to) &&
    (
      leaf && sym.name == sub.assign.to ||
      call.assign && tar.sym == sub.assign.to
  ) ) {
    data <- add_actual_callptr(data, index, rec=FALSE, copy=TRUE)
  }
  # Subassignment to an external symbol requires self-copy to make it internal.
  # This will be branch-balanced in `merge_copy_dat`.
  if(sym.name == "subassign" && !tar.sym %in% computed.sym) {
    # Self-copy goes all the way to the beginning of code because that works,
    # and if we did it adjacent existing code that happened to be in a loop we
    # would be copying the vector each iteration (and over-writing any
    # modifications).
    s.cpy.idx <- -1L
    data <-
      add_actual_callptr(data, s.cpy.idx, name=tar.sym, rec=FALSE, copy=TRUE)
  }
  if(length(in.branch)) {
    # Symbols bound in branches will require rec and/or vcopy of their payload
    # **if** they are used (after the branch?). vcopy not always needed, e.g:
    #
    #   if(a) x <- rec(vcopy(y)); x     # rec and vcopy
    #   if(a) x <- rec(mean(y)); x      # only rec
    #
    if(length(assign.to) && !first.assign && (call.assign || leaf)) {
      # Always reconcile
      triggers <- if(call.assign) assign.to[-length(assign.to)] else assign.to
      data <- add_candidate_callptr(
        data, index, br.index=in.branch, triggers=tail(triggers, 1L), rec=TRUE,
        copy=passive # but only vcopy passive
      )
      # Locally Computed symbols require copy if used after branch e.g:
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
          data, index, br.index=in.branch, triggers=sym.name,
          rec=FALSE, copy=TRUE
        )
      }
    }
    # Branch result requires an additional outer vcopy, e.g:
    #
    #     if(a) rec(vcopy(x))          # or...
    #     if(a) rec(vcopy(x <- ...))
    #
    # `branch.res` only TRUE if the branch result _could_ be used either because
    # also `r2c` exp return value, or bound to symbol.  This may result in a
    # superfluous rec/vcopy when e.g. the bound symbol is not used.  Could be
    # addressed by dead code removal.
    if(branch.res) {
      if(leaf && !length(assign.to)) {
        # Always reconcile
        data <- add_actual_callptr(data, index, rec=TRUE, copy=passive)
        # See "Locally computed Symbols require copy" in prior branch
        if(!passive && is.symbol(x)) {
          data <- add_candidate_callptr(
            data, index, br.index=in.branch, triggers=sym.name,
            rec=FALSE, copy=TRUE
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
            data, index, br.index=in.branch, triggers=data[['assigned.to']],
            rec=FALSE, copy=TRUE
          )
        }
      }
    }
  } else if (last) {
    # Final return not from branch: need to copy if not computed already.
    # Reconciliation not necessary since we're not in branch.
    assign.passive <- !data[['leaf.name']] %in% computed.sym
    sym.passive <- is.symbol(x) && !sym.name %in% computed.sym
    if(
      (first.assign && passive.now && assign.passive) ||
      (sym.passive && !length(assign.to))
    )
      data <- add_actual_callptr(data, index, rec=FALSE)
  }
  # Update bindings to reflect new actual computed bindings we introduced or
  # previous computed ones we overwrote with non-computing.
  if(call.modify) {
    if(!data[['passive']]) {
      if(call.assign) {
        data[[B.LOC]] <- union(data[[B.LOC]], tar.sym)
        data[[B.LOC.CMP]] <- union(data[[B.LOC.CMP]], tar.sym)
        data[[B.ALL]] <- union(data[[B.ALL]], tar.sym)
      }
      if(call.modify) {
        data[[B.ALL0]] <- union(data[[B.ALL0]], tar.sym)
      }
    } else if(call.assign) {
      data[[B.LOC]] <- union(data[[B.LOC]], tar.sym)
      if(length(in.branch)) {
        # non-computing local expressions make global bindings non-global
        data[[B.ALL]] <- setdiff(data[[B.ALL]], tar.sym)
      }
    }
  }
  # Track anytime a symbol is assigned to for free symbols for formals
  if(call.assign) data[[B.NAMED]] <- union(data[[B.NAMED]], tar.sym)

  data[['assigned.to']] <- assign.to
  data
}
# A "Pointer" to Spot in Call Tree to Modify
#
# Allows us to track candidates or actual positions in the call tree for
# injection of `vcopy`, `rec`, and/or self-copies (e.g. `x <- vcopy(x)`).  See
# `generate_candidate` for context.  The point of injection is designated by the
# `index` member which represents a recursive indexing vector to be used with
# double brackets, e.g. `quote(x + y * z)[[c(3L, 2L)]]` retrieves `y`.
#
# Self copies are identified by a `-1L` at the trailing index value (e.g. `c(1L,
# 2L, 2L, -1L)`).  The `-1L` is a marker that has no other meaning in the
# context of index (and needs to be removed when using the index to access the
# call tree).
#
# `callptr` is used for candidates for injection, promoted candidates for
# injection (i.e. those that will cause injections), and actuals (injections
# that are known to be required from the get go).  The semantics of a `callptr`
# object are similar but not identical depending on which of the three
# types of `callptr` we are dealing with.  The semantics are embedded in the
# context the `callptr` objects are used in (not ideal, but so it is).
#
# @seealso generate_candidate, inject_rec_and_copy.
#
# @param name scalar character.  For candidates, the symbol that triggers
#   promotion of a candidate, which when a symbol is up for promotion is
#   typically not that symbols name (e.g. in `x <- y; x` the trigger is `x`, but
#   the symbol that will be `vcopy`ed is `y`).  For promoted candidates, as for
#   candidates.  For actuals that where never candidates, depends on context.
#   For self-copies, it will be the symbol that needs to be copied (e.g. "`x`"
#   in `x <- vcopy(x)`), and NA for other actuals.
# @param index integer vector that can be used to pull up a sub-call within a
#   call tree e.g. with `x[[index]]` where `index` might be `c(2,3,1)`.  This
#   gives the location of the expression to `vcopy`/`rec`.  If the index ends in
#   -1L then a `sym <- vcopy(sym)` is prepended to the expression pointed at
#   by `index`, possibly wrapping the expression in braces first if it isn't
#   already.
# @param br.index like `index`, except this points to the nearest enclosing
#   branch if any, or NULL if there is no parent branch.  This is to detect out
#   of branch uses when we trigger the candidates.
# @param rec whether this is a value that will require reconciliation across
#   branches.
# @param copy whether this is a value that need to be `vcopy`ed (see
#   copy_branchdat`).
# @param free if the candidate is triggered, it is possible that doing so when
#   we're dealing with `x <- vcopy(x)` that we introduce a free symbol.  Only
#   allowed if the training index is -1 which is the special case used to branch
#   balance..

callptr <- function(
  name, index, br.index, rec=TRUE, copy=FALSE, free=FALSE
) {
  if(free && index[length(index)] != -1)
    stop(
      "Internal Error: callptr with free symbols only allowed for branch-",
      "balance assignments."
    )
  list(name=name, index=index, br.index=br.index, rec=rec, copy=copy, free=free)
}
add_actual_callptr  <- function(
  data, index, rec=TRUE, copy=TRUE, name=NA_character_
) {
  new.act <- callptr(name, index, br.index=NULL, copy=copy, rec=rec)
  data[[ACT]] <- c(data[[ACT]], list(new.act))
  data[['passive']] <- !copy
  data
}
add_candidate_callptr <- function(
  data, index, br.index, triggers, rec=TRUE, copy=TRUE
) {
  new.cand <- gen_callptrs(triggers, index, br.index, copy=copy, rec=rec)
  data[[CAND]] <- c(data[[CAND]], new.cand)
  data
}
gen_callptrs <- function(names, index, br.index, rec, copy, free=FALSE)
  Map(
    callptr,
    names, copy=copy, rec=rec, index=list(index), br.index=list(br.index),
    free=free
  )

# Remove Promoted Candidates
#
# Once candidates are promoted, they should be removed from candidate list.  In
# general, we remove candidates based on the position in the call tree they
# promote, so it is possible that one promotion will clear many candidates,
# including some bound to different trigger symbols.  The exception is for
# balancing candidates (those with indices ending in -1, see `merge_copy_dat`),
# which are only removed if they also match on symbol.
#
# @param cand list of candidate pointers
# @param ii which of those in `cand` were promoted

clear_candidates <- function(cand, ii) {
  cand.unm <- lapply(cand, function(x) {x[[1L]] <- ""; x})
  to.clear <- logical(length(cand))
  # use hashed matching to match candidate pointers
  for(i in ii) {
    ci <- cand[[i]]
    if(ci[['index']][length(ci[['index']])] > 0L) {
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
# TRUE if the index `a` points to a position later in the tree than `b` in a
# depth-first traversal order.  If `a` is a parent call to `b` it is consider to
# occur later than `b` (because arguments are evaluated first - well we operate
# under the assumption they are for our purposes).
#
# @param a a integer vector of the form of `index` from `callptr`.
# @param b same as a.
# @return TRUE if `a` is a sub-call appearing later in the call tree than `b`.
#   See details.

index_greater <- function(a, b) {
  if(length(a) > length(b)) length(b) <- length(a)
  else length(a) <- length(b)
  # c(3,3) is greater than c(3,3,1) b/c it is evaluated after
  max.i <- max(c(a, b, 0), na.rm=TRUE)
  a[is.na(a)] <- max.i + 1L
  b[is.na(b)] <- max.i + 1L
  neq <- which(a != b)
  # First non-equal subindex determins which is greater
  any(length(neq)) && a[neq[1L]] > b[neq[1L]]
}
# TRUE if a is a callptr that is child to b.  See index_greater
index_child <- function(a, b) {
  if(length(a) <= length(b)) FALSE
  else all(b == a[seq_along(b)])
}
index_greater_or_child <- function(a, b)
  index_greater(a, b) || index_child(a, b)

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
# @param idx integer vector the index into the call returning the expression
#   setting the current if/else context (see `callptr`).
# @param idx.offset needed b/c loops have the TRUE/FALSE branches in different
#   positions.

merge_copy_dat <- function(old, a, b, idx, idx.offset) {
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
  # Inject at start (hence -1L; so branch return value unchanged).
  prev.bound <- c(old[[B.LOC.CMP]], old[[B.LOC]], old[[B.ALL]])
  a.miss.list <- gen_callptrs(
    a.miss, c(idx, 2L + idx.offset, 2L, -1L),
    br.index=c(idx, 2L + idx.offset),
    copy=TRUE, rec=TRUE, free=!a.miss %in% prev.bound
  )
  b.miss.list <- gen_callptrs(
    b.miss, c(idx, 3L + idx.offset, 2L, -1L),
    br.index=c(idx, 3L + idx.offset),
    copy=TRUE, rec=TRUE, free=!b.miss %in% prev.bound
  )
  # Combine all found free symbols.  Other free symbols may be introduced by
  # branch balance injections, but those ae handled by setting the `free` param
  # in `gen_callptrs` above.
  old[['free']] <- union(a[['free']], b[['free']])

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
  old[[B.LOC]] <- setdiff(old[[B.LOC]], union(branch.all.new, branch.loc))
  old[[B.LOC.CMP]] <-
    setdiff(old[[B.LOC.CMP]], union(branch.all.new, branch.loc.cmp))
  old[[B.ALL]] <- unique(c(old[[B.ALL]], branch.loc, branch.loc.cmp))
  old[[B.ALL0]] <- unique(c(old[[B.ALL0]], a[[B.ALL0]], b[[B.ALL0]]))

  old
}

# Marker functions for singalling to `alloc` for branches
VCOPY.FUN.NAME <- call("::", as.name("r2c"), as.name("vcopy"))
en_vcopy <- function(x) as.call(list(VCOPY.FUN.NAME, x=x))

REC.FUN.NAME <- call("::", as.name("r2c"), as.name("rec"))
en_rec <- function(x, clean=FALSE) {
  tmp <- as.call(list(REC.FUN.NAME, x=x))
  if(clean) names(tmp) <- NULL  # recompose_ifelse uses en_rec, hence this option
  tmp
}

# Marker functions for signalling to `alloc` for use before set in loops
LSET.FUN.NAME <- pkg_fun("lset")
LCOPY.FUN.NAME <- pkg_fun("lcopy")
en_lset <- function(x, rec.i) as.call(list(LSET.FUN.NAME, rec.i=rec.i, x=x))
en_lcopy <- function(x, rec.i) as.call(list(LCOPY.FUN.NAME, rec.i=rec.i, x=x))

# Inject Self-vcopy
#
# Injects a self vcopy (e.g. `x <- vcopy(x)`) just prior to the position pointed
# to by `head(ptr[['index']], -1L)`.
#
# @param x a call
# @param ptr a `callptr` object

inject_copy_in_brace_at <- function(x, ptr) {
  index <- ptr[['index']]
  if(index[length(index)] != -1L)
    stop("Internal Error: expected -1L trailing index.") # see callptr
  par.idx <- index[-length(index)]
  par.call <- if(!length(par.idx)) x else x[[par.idx]]

  if(ptr[['copy']]) {
    # Add braces to call if not present
    if(!is.call(par.call) || get_lang_name(par.call) != "{")
      par.call <- call("{", par.call)

    # generate e.g. `x <- vcopy(x)`
    sym.miss <- as.symbol(ptr[["name"]])
    sym.vcopy <- en_assign(sym.miss, en_vcopy(sym.miss))
    call.list <- as.list(par.call)
    new.call <- dot_names(
      as.call(c(call.list[1L], list(sym.vcopy), call.list[-1L]))
    )
    # Inject call back
    if(length(par.idx)) x[[par.idx]] <- new.call else x <- new.call

    # Remove possibly redundant nested braces (not sure if this is necessary
    # anymore now that we do so in `preprocess`.
    if(length(par.idx)) {
      gpar.idx <- par.idx[-length(par.idx)]
      gpar.call <- if(!length(gpar.idx)) x else x[[gpar.idx]]

      if(get_lang_name(gpar.call) == "{") {
        merge.point <- par.idx[length(par.idx)]
        if(merge.point < 2L) {
          # if gpar is a "{", then parent must be at least 2nd element
          stop("Internal Error: unexpected index structure.")
        }
        gpar.call <- merge_braces(gpar.call, merge.point)
        if(!length(gpar.idx)) x <- gpar.call else x[[gpar.idx]] <- gpar.call
      }
    }
  }
  if(ptr[['rec']]) {
    # This relies on a self copy having been injected first.  Check out how we
    # order `indices.mx` in `inject_rec_and_copy`.
    if(is.call(par.call) && get_lang_name(par.call) == "{") {
      # Brace was not collapsed
      x[[c(par.idx, 2L, 3L)]] <- en_rec(x[[c(par.idx, 2L, 3L)]])
    } else {
      # Brace was collapsed
      x[[c(par.idx, 3L)]] <- en_rec(x[[c(par.idx, 3L)]])
    }
  }
  x
}

# Inject vcopy/rec Calls
#
# Take candidate and actual injection coordinates and modify the call to
# add `vcopy`/`rec`/self-copies.
#
# @seealso generate_candidate, callptr
# @param x call to modify.
# @branch.dat data object from `copy_branchdat` used to identify the locations
#   where we need to inject `vcopy` / `rec`.

inject_rec_and_copy <- function(x, branch.dat) {
  promoted <- unique(branch.dat[[ACT]])

  if(length(promoted)) {
    # Order the indices in reverse order of appearance
    indices <- lapply(promoted, "[[", "index")
    # Extend shorter indices to length of longest
    indices.eq <- lapply(indices, `length<-`, max(c(lengths(indices), 1L)))
    indices.mx <- do.call(cbind, indices.eq)
    if(isTRUE(any(indices.mx == 0L)))
      stop("Internal Error: 0 callptr index vals disallowed.")
    # Fill extensions with 0 so that self-copies with -1 index values sort first
    indices.mx[is.na(indices.mx)] <- 0L
    # Break tie with reconcile value so that reconcile sorts first (once
    # reversed, the reconciles will be applied second; this is required for the
    # self copies which must be inserted first before we can `en_rec` them).
    rec <- vapply(promoted, "[[", TRUE, "rec")
    indices.mx <- rbind(indices.mx, !rec)
    indices.order <- do.call(order, split(indices.mx, row(indices.mx)))

    # Inject the vcopies in reverse order so that indices are not made invalid
    # by tree modifications ahead of them (which could happen if we have nested
    # vcopies such as `vcopy(y <- vcopy(x))` at end of branch).
    for(ii in rev(seq_along(promoted)[indices.order])) {
      i <- promoted[[ii]]
      if(!length(i[["index"]])) {
        # Empty index means full expression.  Ideally `x[[integer()]] <- ` would
        # work and then this would just fold into next branch.
        x <- en_vcopy(x)
      } else if(i[["index"]][length(i[["index"]])] > 0L) {
        # Trail index positive, so wrap a symbol/call in vcopy if needed
        if(i[['copy']]) x[[i[["index"]]]] <- en_vcopy(x[[i[["index"]]]])
      } else if(i[["index"]][length(i[["index"]])] < 0L) {
        # Trail index negative; these require addition of e.g. `x <- vcopy(x)`
        x <- inject_copy_in_brace_at(x, i)
        # Self copies do reconciliation in `inject_copy_in_brace_at`
        i[['rec']] <- FALSE
      } else stop("Internal Error: invalid injection index.")

      # Add reconcile if needed
      if(i[['rec']] && i[["index"]][length(i[["index"]])] > 0L) {
        if(!length(i[["index"]])) x <- en_rec(x)
        else x[[i[["index"]]]] <- en_rec(x[[i[["index"]]]])
      }
    }
  }
  x
}
# Given the location of a child brace call, splice it into the parent call.
#
# Duplicate with collapse_braces in optim.R.  Keeping it for now until we
# conclude we can remove it from inject_copy_in_brace_at.
#
# @param x call assumed to be a brace call.
# @param id index in x of a sub-call assumed to be a brace call.

merge_braces <- function(x, id) {
  x.list <- as.list(x)
  x.child <- as.list(x.list[[id]])[-1L]
  # Empty braces change semantics of call (`{5;{}}` is not equal to `{5}`),
  # sub-in numeric(0) as a proxy for NULL like we do with missing branches.
  x.list <- c(
    x.list[1L:(id - 1L)],
    if(!length(x.child)) list(numeric(0)) else x.child,
    if(id < length(x.list)) x.list[(id + 1L):length(x.list)]
  )
  # in order to look match-called we need names on the call
  x.call <- as.call(x.list)
  names(x.call) <- dot_names(x.call)
  x.call
}
# Prepare For Loop
#
# Loops need to handle variables that are used before they are set in the same
# loop because such variables will be updated after each use and iteration.
# E.g. in:
#
#   res <- 0
#   for(i in x) {
#     tmp <- res + i
#     res <- tmp
#   }
#
# The memory occupied by `res` is not the same as that occupied by `tmp`, which
# without corrective action would mean r2c calculations become incorrect after
# the first iteration because symbols are only resolved once at allocation time,
# not during run-time.  We handle this by adding a command to explicitly copy
# `tmp` to the original `res` slot at the end of the loop body.
#
# In order to effect the copy safely we:
#
# 1. Ensure that the original variable is un-aliased by adding a `vcopy` before
#    loop start.  If we did not do this step 4 would overwrite the aliased
#    variable too.
# 2. Add a custom alias to the variable copy so we can control the lifetime of
#    the associated memory (in form `.R2C.FOR.SYM.N`).
# 3. Mark where the variable is set.
# 4. Add a command (`lcopy`) to copy the last value of the variable to the
#   original variable (we recover the original location via the alias).
#
# `alloc` can then look for `lset` and `lcopy` pairs matched up by the unique
# "rec.id" values generated by this function to identify which slots to copy
# to/from.
#
# @param x call tree
# @param index integer index into call tree we're currently at
# @param symbols list with equal-length elements:
#   * level: integer nesting `for` depth.
#   * indices: 3 row list-matrix of `index` values, with rows first.use,
#     first.assign, last.assign, and for column names the symbol names (these
#     will contain duplicates, deduplicated by 'level'.
# @param prior.name name of parent call for purposes of detecting illegal nested
#   braces (not sure there is any gurantee these won't exist).  This is to avoid
#   issues like `a <- {b <- c; d}` in composing assignment chains.

copy_fordat <- function(
  x, index=integer(), symbols=init_sym_idx(), assign.to=character(), llvl=0L
) {
  name <- get_lang_name(x)
  is.loop <-
    name == "{" && length(x) == 3L &&
    length(x[[2L]]) > 1L && identical(x[[2:1]], QFOR.INIT) &&
    length(x[[3L]]) > 1L && identical(x[[c(3L,1L)]], QR2C.FOR)
  assign.to.prev <- assign.to

  if (is.symbol(x) && llvl) {
    # Record symbol uses; we'll determine which ones are use before set later
    symbols <- append_foruse_dat(
      symbols, names=name, index=index, llvl=llvl
    )
    assign.to <- character()
  } else if(is.call_w_args(x)) {
    # Use before set only matters inside `for_n`; other components of loop ok
    llvl <- llvl + (name == FOR.N)

    # Modify for_n branch since other one doesn't do anything.
    rec.skip <- 1L
    if(name %in% ASSIGN.SYM && llvl) {
      # We only record target symbol so if there is an assign chain they can
      # all be set to point to the same element.  ASSIGN.SYM includes `for_iter`
      # which will never form anything but a length 1 chain.
      assign.to <- union(assign.to, get_target_symbol(x, name))
      rec.skip <- if(name == FOR.ITER) 1:3 else 1:2
    } else assign.to <- character()
    # Skip external params
    par.ext.names <- names(VALID_FUNS[[c(name, "extern")]])
    rec.skip <- union(rec.skip, which(names2(x) %in% par.ext.names))

    # Recurse
    for(i in seq_along(x)[-rec.skip]) {
      sub.index <- c(index, i)
      tmp <- copy_fordat(
        x[[i]], index=sub.index, symbols=symbols, assign.to=assign.to, llvl=llvl
      )
      x[[i]] <- tmp[['call']]
      symbols <- tmp[['symbols']]
    }
  }
  # Reached payload of assignment so record each of them
  if(!length(assign.to) && length(assign.to.prev) && llvl) {
    symbols <- append_forassign_dat(
      symbols, names=assign.to.prev, index=index, llvl=llvl
    )
  }
  # Modify loop on exit to handle use before set.  We modify the call from the
  # end to make sure indices still work.  This shuffles the order of operations
  # from that of the steps described in the docs.
  if(is.loop) {
    # Collect all first use at this level (earlier loops of equal level that
    # we've exited from previously have first use symbols removed). We need
    # llvl + 1 b/c llvl is only incremented at for_n and we're at r2c_for
    at.lvl <- symbols[['level']] == (llvl + 1L)
    ind <- symbols[['indices']]
    target <- at.lvl & lengths(ind['first.use',])
    use.b4.set <- vapply(
      seq_len(ncol(ind)),
      function(i)
        # empty index means no use; special case b/c otherwise it is
        # interpreted as a parent to any other index
        length(ind[['first.use', i]]) && length(ind[['first.assign', i]]) &&
        index_greater(ind[['first.assign', i]], ind[['first.use', i]]),
      TRUE
    )
    ind.rec <- ind[, use.b4.set, drop=FALSE]
    if(anyDuplicated(colnames(ind.rec)))
      stop("Internal Error: duplicated loop reconcile symbols.")

    # We should be at the `{` of `{for_iter(); r2c_for()}`
    brace.ind <- c(3L, 3L, 2L)
    if(!is.brace_call(x[[c(brace.ind)]]))
      stop("Internal Error: expected braces nested in ", FOR.N)

    # We only worry about the for_n branch, for_0 should just be `numeric(0)`
    braces <- as.list(x[[brace.ind]])

    lcopy.id.0 <- symbols[['lcopy.id']]
    rec.syms <- list()
    for(i in seq_len(ncol(ind.rec))) {
      indi <- ind.rec[, i]
      lcopy.id <- symbols[['lcopy.id']] <- symbols[['lcopy.id']] + 1L
      # Generate the aliases (part of steps 2 and 4)
      rec.sym <- as.name(sprintf(".R2C.FOR.SYM.%d", symbols[['lcopy.id']]))
      rec.syms[[colnames(ind.rec)[i]]] <- rec.sym # we'll use these later
      ind.base <- c(index, brace.ind) # braces inside `for_n`
      ind.use <- indi[['first.use']]
      ind.set <- indi[['last.assign']]
      # Indices are absolute, but we need to make them relative to modify the
      # current sub-call.
      if(
        !identical(ind.use[seq_along(ind.base)], ind.base) ||
        !identical(ind.set[seq_along(ind.base)], ind.base)
      )
        stop("Internal Error: mismatched for and use|set indices.")
      ind.set <- ind.set[-seq_along(ind.base)]
      # Add lset/lcopy calls (steps 3-4)
      braces[[ind.set]] <- en_lset(braces[[ind.set]], lcopy.id)
      braces <- c(
          braces[seq_len(length(braces) - 1L)],
          # We reference rec.sym here so it is clear to static analysis the
          # symbol is in use until the very end of the loop (although right now
          # other code explicitly assumes any symbol used in loop is in use
          # until end of loop see `collect_loop_call_symbols`).
          en_lcopy(rec.sym, lcopy.id),
          braces[length(braces)]  # trailing numeric(0)
      )
    }
    x[[brace.ind]] <- dot_names(as.call(braces))

    # Add the symbol copies ahead of the loop (step 1).
    sym.copies <- lapply(
      names(rec.syms),
      function(x, syms) {
        xn <- as.name(x)
        en_assign(syms[[x]], en_assign(xn, en_vcopy(xn)))
      },
      rec.syms
    )
    x.list <- as.list(x)
    x <- dot_names(as.call(c(x.list[1L], sym.copies, x.list[-1L])))
  }
  list(call=x, symbols=symbols)
}
# See `symbols` in `copy_fordat`
init_sym_idx <- function(names = character()) {
  list(
    lcopy.id=0L,    # how many loop reconciliations we've made
    level=integer(),
    indices=matrix(
      replicate(3 * length(names), integer(0), simplify=FALSE),
      nrow=3, ncol=length(names),
      dimnames=list(c('first.use', 'first.assign', 'last.assign'), names)
    )
  )
}
init_and_fill_sym_idx <- function(symbols, names, llvl) {
  # Start by generating a dummy table for all the names we're assigning to
  new.idx <- init_sym_idx(names)[['indices']]

  # Fill in with existing data
  existing <-
    symbols[['level']] == llvl &
    colnames(symbols[['indices']]) %in% names
  exist.idx <- symbols[['indices']][, existing, drop=FALSE]
  new.idx[, colnames(exist.idx)] <- exist.idx
  new.idx
}
update_sym_dat <- function(symbols, new.idx, llvl) {
  # Replace existing data with new and retun
  existing <-
    symbols[['level']] == llvl &
    colnames(symbols[['indices']]) %in% colnames(new.idx)
  symbols[['level']] <-
    c(symbols[['level']][!existing], rep(llvl, ncol(new.idx)))
  symbols[['indices']] <-
    cbind(symbols[['indices']][, !existing, drop=FALSE], new.idx)
  symbols
}
# We have a primary key of name/level against which we need to be able to insert
# and update.  In R that means we need to be able remove and replace.

append_forassign_dat <- function(symbols, names, index, llvl) {
  # Start by generating a dummy table for all the names we're assigning to
  new.idx <- init_and_fill_sym_idx(symbols, names, llvl)

  # Update with new data; loops are decomposed so should never see a 0 index.
  new.idx['first.assign', lengths(new.idx['first.assign',]) == 0] <- list(index)
  new.idx['last.assign', ] <- list(index)
  update_sym_dat(symbols, new.idx, llvl)
}
append_foruse_dat <- function(symbols, names, index, llvl) {
  # Start by generating a dummy table for all the names we're assigning to
  new.idx <- init_and_fill_sym_idx(symbols, names, llvl)

  # Update with new data; loops are decomposed so should never see a 0 index.
  new.idx['first.use', lengths(new.idx['first.use',]) == 0] <- list(index)
  update_sym_dat(symbols, new.idx, llvl)
}

# Used to make sure "{" calls have named dotted args.

dot_names <- function(x) {
  stopifnot(is.call_w_args(x))
  names(x) <- c("", rep("...", length(x) - 1L))
  x
}
