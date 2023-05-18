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
# r2c allows assignments by binding a symbol to the memory associated with the
# RHS of the assignment.  In many cases, that RHS is a computation, e.g. as in
# `y <- mean(x)`, but in others it might just be a rebinding as in `x <- y`.
# This becomes an issue in two contexts:
#
# 1. Final return value: normally we just redirect the write of the final
#    expression to to the result vector, but for that to happen the final
#    expression must be one that computes and generates new data. If the final
#    result is just a symbol referencing iteration or external data, that needs
#    to be explicitly copied to the result vector as there is no possibility of
#    redirection of a prior computation result (since there is no computation).
# 2. Branches: to implement branches we require that every symbol written to in
#    one branch that is used subsequent to the branch be bound to the same
#    memory in both branches.  This allows subsequent references to that symbol
#    to work irrespective of branch taken at run time.  This is accomplished
#    at allocation time by remapping the different allocations in each branch to
#    a new shared allocation.  For this to work the allocation both these
#    allocations must occur in the if/else.  Subsequent accesses will then read
#    from the shared location.  If the allocations are not the same size that
#    will result in an allocation-time error.
#
# For 1 we need to make sure that a returned value is either a computation, or
# is explicitly copied by injection of `vcopy`, i.e `x <- y` becomes
# `x <- vcopy(y)`.
#
# The second requires that every symbol bound in a branch (either `if/else`, or
# body of a loop (NB: a loop body may be taken or not)) point to memory newly
# allocated in that branch, which may require `vcopy`ing symbols *re*bound in
# that branch.  While it might be tempting, It is unsafe to try to remap
# `r2c` allocated memory that was in use prior to the branches because that
# memory could be used in different conflicting ways across the branches:
#
#    x <- mean(y)    # original `x` alloc
#    if(a) {
#      x <- x + 1    # new `x` alloc, frees original `x` alloc
#      z <- mean(x)  # potentially uses the original `x` alloc
#    } else {
#      z <- x
#    }
#    x + z
#
# We cannot try to use the original `x` allocation for `z` even though it would
# work for the `else` branch.  In the special case where in both branches we
# have e.g. `z <- x`  and `x` isn't touched we could avoid the `vcopy`, but we
# don't worry about that special case.
#
# Finally, the e.g. if/else return value itself must be written to the same
# location irrespective of branch taken.  This creates a broader set of
# situations that require copying data, e.g.:
#
#    y <- if(a) {
#      x <- mean(y)
#      sum(y)
#    } else {
#      x <- mean(z)    # this whole expression needs `vcopy`ing
#    }
#    x + y
#
# Where even though `x` is created independently in each branch, we still need
# to `vcopy` it because we cannot have the `x` memory bound both `x`, and to the
# return value of the `if`.
#
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
# Compute Locations of Required `vcopy`s
#
# Because we don't now know whether we need to `vcopy` an unbound symbol or a
# symbol bound in an if/else branch until after the point where the `vcopy`
# needs to happen, we record the coordinates of all such symbols in the call
# tree as "candidates" for `vcopy`.
#
# Once we can determine whether they should be `vcopy`ed, e.g. because the
# symbol was used after the if/else, or it was returned as the final result, we
# promote the candidates to an "actual" `vcopy` list.  When the full recursive
# traversal of the input call is complete, we can use the returned "actual" list
# and modify calls to add `vcopy` (this is done outside of this function).
#
# In addition to tracking candidates and actual vcopy promotions, we also track
# internal bindings (i.e. symbols that have been assigned to in the call being
# processed).  We only consider bindings to be internal when they are known to
# reference r2c allocated memory.  This means we do not include bindings to
# `vcopy` candidates until such a time as the candidates are promoted.  So e.g.
# in:
#
#    x <- y
#
# If `y` is an external symbol, then `x` is not considered an internal binding
# until **after** `x` is used and triggers the promotion of `y` into an actual
# `vcopy`ed symbol.
#
# Symbols bound in branches are only required to reference memory allocated in
# the branch if they are used after the corresponding if/else  Other
# branch-bound symbols are branch-local and thus don't require `vcopy`  Because
# of this distinction we track local bindings and global bindings separately in
# the `data` object.
#
# @seealso `copy_branchdat` for context.
# @param index integer vector that can be used to pull up a sub-call within a
#   call tree e.g. with `x[[index]]` where `index` might be `c(2,3,1)`.
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
# @param in.compute whether there is some parent call that computes on the
#   current call (e.g. `mean(x)` where `x` is the current call).  Used to
#   determine whether the result of a branch is further used.
# @param in.branch whether the parent call (or effective parent call in the case
#   the parent call is passive) is a branch.
# @param data list of realized bindings "bind", candidate/actual sub-calls for
#   reconcilation / copy in "copy", and a flag denoting if the processed call
#   was fully passive. See details.

copy_branchdat_rec <- function(
  x, index=integer(), assign.to=character(),
  in.compute=FALSE, in.branch=FALSE, branch.res=FALSE, last=TRUE,
  data=list(
    bind=list(loc=character(), all=character(), loc.compute=character()),
    copy=list(cand=list(), act=list()),
    passive=TRUE, assigned.to=character()
  )
) {
  if (is.symbol(x)) {
    # In depth-first traversal, we generate candidates for `vcopy` when we
    # encounter symbols with the appropriate characteristics.
    sym.name <- as.character(x)
    sym.local.cmp <- sym.name %in% data[[B.LOC.CMP]]
    sym.local <- sym.name %in% data[[B.LOC]]
    sym.global <- sym.name %in% data[[B.ALL]]

    # For symbols matching candidate(s): promote candidate if allowed.
    cand <- data[[CAND]]
    cand.prom.i <- which(names(cand) == sym.name & !sym.local)
    cand.prom <- cand[cand.prom.i]

    # Effect promotions
    data[[ACT]] <- c(data[[ACT]], cand.prom)
    data[[CAND]][cand.prom.i] <- NULL

    # Symbol is passive unless we add an actual promotion (those set passive to
    # FALSE), or it already is a local computed symbol
    data[['passive']] <- !sym.local.cmp

    # Generate new candidates/actuals if warranted (see callptr docs).
    if(in.branch) {
      if(branch.res) {
        if(!length(assign.to) && !sym.local.cmp) {
          # Auto-promote b/c branch result will be used
          data <-
            add_actual_callptr(data, index, copy=data[['passive']], rec=TRUE)
        } else if(length(assign.to)) {
          # If part of binding only promote if binding is used (see
          # `if(call.assign) ...` in the call section which potentially adds
          # a `vcopy` around the entire assignemnt expression).
          data <- add_candidate_callptr(
            data, index, triggers=assign.to, copy=data[['passive']], rec=TRUE
          )
        }
      } else if (length(assign.to)) {
        # All non-local symbols being bound to other symbols are candidates
        data <- add_candidate_callptr(
          data, index, triggers=assign.to, copy=!sym.local.cmp, rec=TRUE
        )
      }
    } else if (last) {
      # Last symbol in r2c exp referencing external symbol auto-promoted.
      # Outside of branch so reconciliation not required (`rec=FALSE`).
      if(!sym.name %in% data[[B.ALL]])
        data <- add_actual_callptr(data, index, rec=FALSE)
    }
  } else if(is.call(x)) {
    # Recursion, except special handling for if/else and for assignments
    call.sym <- get_lang_name(x)
    call.assign <- call.sym %in% ASSIGN.SYM
    call.passive <- call.sym %in% PASSIVE.SYM
    first.assign <- length(assign.to) == 0L && call.assign

    if(call.sym == 'r2c_if') {
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
        tar.sym <- get_target_symbol(x, call.sym)
        assign.to <- union(assign.to, tar.sym)
        rec.skip <- 1:2
      }
      # Recurse on language subcomponents
      for(i in seq_along(x)[-rec.skip]) {
        if(is.language(x[[i]])) {
          # assign.to is forwarded in passive calls
          next.last <- i == length(x) && call.passive
          assign.to.next <- if(!next.last) character() else assign.to
          data[['passive']] <- passive.now
          data <- copy_branchdat_rec(
            x[[i]], index=c(index, i),
            assign.to=assign.to.next, last=last && next.last,
            branch.res=branch.res && next.last,
            in.compute=in.compute || !call.passive,
            in.branch=in.branch,
            data=data
          )
      } }
      data[['passive']] <- call.passive && data[['passive']]

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
          # non-computing local expressions make global bindings non-global
          data[[B.LOC]] <- union(data[[B.LOC]], tar.sym)
          data[[B.ALL]] <- setdiff(data[[B.ALL]], tar.sym)
        }
        data[['assigned.to']] <- assign.to
      }
    }
    passive.now <- data[['passive']]  # add_actual changes passive status
    if(branch.res) {
      if(
        !call.passive || first.assign || call.sym == 'r2c_if'
      ) {
        data <- add_actual_callptr(data, index, rec=TRUE, copy=FALSE)
        if(first.assign && !passive.now) {
          data <- add_candidate_callptr(
            data, index, triggers=data[['assigned.to']], rec=FALSE, copy=TRUE
          )
        } else if (first.assign) {
          data <- add_actual_callptr(data, index, rec=FALSE, copy=TRUE)
        }
      }
    } else if (in.branch && !call.passive && length(assign.to)) {
      data <- add_candidate_callptr(
        data, index, triggers=data[['assigned.to']], rec=TRUE, copy=FALSE
      )
    }
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
# @param index the location of the expression to vcopy; if the index ends in 0L
#   then a `x <- vcopy(x)` (assuming expression is `x`) is added at the
#   beginning of the branch the index points to as this represents an external
#   symbol missing from one branch but used in the other.
# @param rec whether this is a value that will require reconciliation across
#   branches.
# @param copy whether this is a value that need to be `vcopy`ed (see
#   copy_branchdat`).
# @param candidate TRUE if element is a candidate that requires explicit
#   promotion via a later reference to its name, as opposed to it
#   automatically became an actual vcopy because e.g. it was last.
# @param global when the object to vcopy is a symbol (i.e. in `x <- y`, the
#   `y`), whether that symbol has a potential valid global binding (i.e. is not
#   external), for the special case where we generate a candidate due to
#   assignment in if/else, but then just return the symbol as the last
#   expression.

callptr <- function(name, index, rec=TRUE, copy=FALSE) {
  list(name=name, index=index, rec=rec, copy=copy)
}
add_actual_callptr  <- function(data, index, rec=TRUE, copy=TRUE) {
  new.act <- callptr(NA_character_, index, copy=copy, rec=rec)
  data[[ACT]] <- c(data[[ACT]], list(new.act))
  data[['passive']] <- FALSE
  data
}
add_candidate_callptr <- function(data, index, triggers, rec=TRUE, copy=TRUE) {
  new.cand <- gen_callptrs(triggers, index, copy=copy, rec=rec)
  data[[CAND]] <- c(data[[CAND]], new.cand)
  data
}
gen_callptrs <- function(names, index, rec, copy)
  sapply(names, callptr, copy=copy, rec=rec, index=index, simplify=FALSE)

# Compare Two `callptr` Indices
#
# TRUE if the index a points to `a` position later in the tree than `b` in a
# depth-first traversal order.

index_greater <- function(a, b) {
  if(length(a) > length(b)) length(b) <- length(a)
  else length(a) <- length(b)
  a[is.na(a)] <- 0L
  b[is.na(b)] <- 0L
  all(a >= b) & any(a > b)
}
# Merge Candidates between Branches
#
# Both `a` and `b` contain lists at index CAND and ACT.  These are named lists
# with the names being those of symbols in the expression, and the elements
# each produced by `callptr` (essentially a "pointer" to the location of a symbol
# that might need to be `vcopy`ed, see `callptr`).
#
# The pointer lists are named for convenience, but we rely on each "pointer"
# element also containing the name.  This allows us to use `unique` and similar
# on the lists to recongize that elements with different names are different
# despite pointing to the same location (because each `vcopy` candidate might
# have multiple triggers, e.g. if `x` is a candidate in `y <- z <- x`, then
# either use of `y` or `z` could result in promotion).
#
# @param a list should be `data` from `copy_branchdat_rec` (see details).
# @param index integer vector the index into the call returning the expression
#   setting the current if/else context.

merge_copy_dat <- function(old, a, b, index) {
  # Removed shared history from both branches.  This is for the case where a
  # pre-existing candidate is cleared in one branch, but not the other.  If we
  # didn't remove shared history, the old candidate would be seen as new.
  a.cand <- setdiff(a[[CAND]], old[[CAND]])
  b.cand <- setdiff(b[[CAND]], old[[CAND]])
  # Track shared history to add back later.
  old.cand <- intersect(a[[CAND]], b[[CAND]])

  # Find candidates added in one branch missing in the other to inject e.g.
  # `x <- vcopy(x)` at start (hence 0L; so branch return value unchanged).
  a.miss <- setdiff(names(b.cand), names(a.cand))
  b.miss <- setdiff(names(a.cand), names(b.cand))
  a.miss.list <- gen_callptrs(a.miss, c(index, 2L, 0L), copy=TRUE, rec=TRUE)
  b.miss.list <- gen_callptrs(b.miss, c(index, 3L, 0L), copy=TRUE, rec=TRUE)

  # Recombine all the pieces into the new set of candidates
  copy.cand <- unique(
    c(old.cand, a.cand, b.cand, a.miss.list, b.miss.list) )

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
#   be wrapped in `vcopy`.

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
      if(i[["index"]][length(i[["index"]])]) {
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
      if(i[['rec']]) x[[i[["index"]]]] <- en_rec(x[[i[["index"]]]])
    }
  }
  x
}
