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

#' @include preprocess.R

NULL

#' Find Latest Symbol Instance
#'
#' This currently allows control/flag parameter symbols to count, even though
#' we're settling on them having different lookup semantics.  So this will be
#' cause some inefficiency in some cases where a symbol will not be released as
#' early as it should just because it's used as a control/flag.
#'
#' @noRd
#' @param x object as produced by preproc

latest_symbol_instance <- function(x) {
  call <- x
  # Compute symbol lifetime.  We only need to find the latest time it exists as
  # a leaf because it will be protected by its expression level in expressions.
  # This is to support protecting assignments across sub-expressions.
  sym.steps <- vapply(
    call, function(x) if(is.symbol(x)) as.character(x) else "", ""
  )
  sym.max <- tapply(seq_along(sym.steps), sym.steps, max)

  # Unfortunately loops complicate matters as an early symbol can be re-used
  # after first loop iteration, so get all the loops, and extract all the leaf
  # symbols used by each loop.
  loop.syms <- lapply(call, collect_loop_call_symbols)
  loop.reps <- rep(seq_along(loop.syms), lengths(loop.syms))
  loop.max <- tapply(loop.reps, unlist(loop.syms), max)

  # Return max instance
  max.names <- union(names(loop.max), names(sym.max))
  max.instance <- integer(length(max.names))
  names(max.instance) <- max.names
  max.instance[names(sym.max)] <- sym.max
  max.instance[names(loop.max)] <- pmax(max.instance[names(loop.max)], loop.max)
  max.instance[nzchar(names(max.instance))]
}
#' Find Last Call That Computes
#'
#' Braces just return the last value (as do assignments and control structures).
#' This is required to ensure the last computing call is written to the result
#' vector and not to a temporary allocation.
#'
#' @noRd
#' @param x linearized call list

latest_action_call <- function(x) {
  calls <- vapply(x, is.call, TRUE)
  call.sym <- vapply(
    x[calls],
    function(x) length(x) && (is.name(x[[1L]]) || is.character(x[[1L]])),
    TRUE
  )
  call.active <- vapply(
    x[calls][call.sym],
    function(x) !as.character(x[[1L]]) %in% PASSIVE.SYM,
    TRUE
  )
  call.active.i <- seq_along(x)[calls][call.sym][call.active]
  if(!length(call.active.i))
    stop("Internal Error: no active return call found.")
  max(call.active.i)
}

#' Allocate Required Storage
#'
#' For each call, we allocate a vector for the result in the storage list,
#' possibly re-using a previously allocated but "freed" vector.  Additionally,
#' we record the indices into the storage list for the result of each call, as
#' well as for each argument to it.
#'
#' Leaf symbols are looked for through (in order):
#'
#' * Previously assigned symbols in the expression.
#' * The naked data vectors (hmm..., another semantic wrinkle).
#' * The outside environment.
#'
#' The calls have been linearized by `preprocess` (see that for details).
#'
#' This process converts `preproc$call` to `alloc$call.dat` (and `$depth`, and
#' more).  `alloc$call.dat` should have as many entries as there are C calls
#' in `preproc$code`.
#'
#' @section Depth:
#'
#' An allocation is considered "freed" as soon as we emerge above the call
#' level (depth) for which it was originally made, unless it is also referenced
#' by a symbol that is still in use (designated by the "protected" vector).  The
#' `depth` variable semantics are strongly shaped by how it is generated (i.e.
#' by incrementing it with each level of recursion in the call tree it
#' originates from).  For example, for each call, it is a given that all of it's
#' parameters will have a `depth` of `depth+1`.  We use `stack` as a mechanism
#' for tracking the current call's parameters.
#'
#' For the specific case of braces and assignments, we let `alloc_dat` "promote"
#' the depth of their results so that there does not need to be a new allocation
#' for what should be a no-op.  This will cause the `depth` values in the
#' allocation data and the `stack` to diverge, but those should converge after
#' the stack reduces.
#'
#' @param x preprocess data as produced by `preprocess`
#' @return an alloc object:
#'
#' alloc
#'   $alloc: see `alloc_dat`.
#'   $call.dat: each actual call with a C counterpart
#'     $call: the R call
#'     $ids: ids in `alloc$dat` for parameters, and then result
#'     $ctrl: evaluated control parameters
#'     $flag: computed flag parameter value
#'   $stack:
#'     matrix used to track parameter sizes, but the time it's returned it
#'     should just have the size of the final return
#'
#' @noRd
#' @param x the result of preprocessing an expression

alloc <- function(x, data, gmax, par.env, MoreArgs, .CALL) {
  call.len <- length(x[['call']])
  call.outer <- x[['call']][[call.len]]

  # - Initialize ---------------------------------------------------------------
  env <- list2env(MoreArgs, parent=par.env)

  # Call indices where various interesting things happen
  meta <- list(
    i.call.max=length(x[['call']]),               # call count
    i.sym.max=latest_symbol_instance(x[['call']]),# last symbol maybe touched
    i.last.action=latest_action_call(x[['call']]) # last computing call
  )
  alloc <- init_dat(x[['call']], meta=meta, scope=0L)
  # Status control placeholder.
  alloc <- append_dat(
    alloc, new=list(numeric(IX[['STAT.N']])),
    sizes=IX[['STAT.N']], depth=0L, type="sts"
  )
  # Result placeholder, to be alloc'ed once we know group sizes.
  alloc <- append_dat(
    alloc, new=list(numeric()), sizes=0L, depth=0L, type="res"
  )
  # Add group data.
  if(!all(nzchar(names(data)))) stop("All data must be named.")
  data.naked <- data[is.num_naked(data)]
  data.used <- integer()
  alloc <- append_dat(
    alloc, new=data.naked, sizes=rep(gmax, length(data.naked)),
    depth=0L, type="grp"
  )
  # Bump scope so the data cannot be overwritten
  alloc[['scope']] <- alloc[['scope']] + 1L

  # - Process ------------------------------------------------------------------

  # Objective is to compute how much temporary storage we need to track all the
  # intermediate calculations in the call tree, and to allocate all the vectors
  # into a data structure.  This data structure will also include references to
  # external vectors (if there are such references), and the result of
  # evaluating the control/flag parameters.

  stack <- init_stack()
  stack.ctrl <- stack.flag <- list()
  call.dat <- list()

  for(i in seq_along(x[['call']])) {
    type <- x[['type']][[i]]
    call <- x[['call']][[i]]
    depth <- x[['depth']][[i]]
    argn <- x[['argn']][[i]]
    name <-
      if(length(call) < 2L) as.character(call)
      else as.character(call[[1L]])
    # - Reduce Call ------------------------------------------------------------
    if(type == "call") {
      # Based on previously accrued stack and function type, compute call result
      # size, allocate for it, etc.
      check_fun(name, env)
      ftype <- VALID_FUNS[[c(name, "type")]]

      # If all non-control, non-flag inputs were know to be integer, and we're
      # dealing with a function that returns integer for those, make it known
      # the result should be integer.
      res.typeof <- if(
        VALID_FUNS[[c(name, "preserve.int")]] &&
        all(alloc[['typeof']][stack['id', ]] == "integer")
      ) "integer" else "double"
      # If this is an assignment, protect the corresponding symbol
      if(ftype[[1L]] == "constant") {
        # Always constant size, e.g. 1 for `sum`
        size <- ftype[[2L]]
        alloc <- alloc_dat(
          alloc, depth, size=size, call=call, typeof=res.typeof, i.call=i
        )
        group <- 0
      } else if(ftype[[1L]] %in% c("arglen", "vecrec")) {
        # Length of a specific argument, like `probs` for `quantile`
        stack.cand <- stack['depth',] == depth + 1L
        stack.ids <-
          if(is.character(ftype[[2L]])) colnames(stack)[stack.cand]
          else seq_len(ncol(stack))[stack.cand]
        if(!all(ftype[[2L]] %in% stack.ids))
          stop(
            simpleError(
              paste0(
                "Parameter(s) ",
                deparse1(ftype[[2L]][!ftype[[2L]] %in% stack.ids]),
                " missing but required for sizing in ",
                deparse1(call)
              ),
              .CALL
          ) )
        # Get parameter data. depth + 1L should be params to current call, note
        # this is not generally true for the stuff in `x`, only for the stack.
        param.cand <- if(is.integer(ftype[[2L]])) {
          seq_len(ncol(stack))[stack.cand][ftype[[2L]]]
        } else if (is.character(ftype[[2L]])) {
          which(colnames(stack) %in% ftype[[2L]] & stack.cand)
        } else stop("Internal Error: bad sizing parameters.")

        # Arglen needs to disambiguate multiple params (e.g. `...` may show up
        # multiple times in `colnames(stack)` for any given depth).
        if(length(ftype) > 2L && is.function(ftype[[3L]])) {
          param.cand.tmp <- ftype[[3L]](param.cand)
          if(
            !is.integer(param.cand.tmp) ||
            !all(param.cand.tmp %in% param.cand)
          )
            stop("Internal Error: parameter disambiguation for sizing failed.")
          param.cand <- param.cand.tmp
        }
        sizes.tmp <- stack[c('size', 'group'), param.cand, drop=FALSE]
        alloc <- alloc_dat(
          alloc, depth, size=vec_rec_max_size(sizes.tmp, gmax), call=call,
          typeof=res.typeof, i.call=i
        )
        size <- vec_rec_known_size(sizes.tmp[1L,])  # knowable sizes
        group <- max(sizes.tmp[2L,])                # any group size in the lot?
      } else stop("Internal Error: unknown function type.")

      id <- alloc[['i']]

      # Reduce call data, including parameter and result ids, and reduce stack
      flag <- VALID_FUNS[[c(name, "ctrl.validate")]](
        stack.ctrl, stack.flag, call
      )
      call.dat <- append_call_dat(
        call.dat, call=call, stack=stack, depth=depth, alloc=alloc,
        ctrl=stack.ctrl, flag=flag
      )
      stack <- append_stack(
        stack[,stack['depth',] <= depth, drop=FALSE], id=id,
        alloc=alloc, depth=depth, size=size, group=group, argn=argn
      )
      stack.ctrl <- stack.flag <- list()

    # - Assigned-To Symbol -----------------------------------------------------
    } else if(x[['assign']][i]) {
      # We want to record this on the stack for consistency, but it doesn't
      # really need to be there as it's never used
      alloc <- append_dat(
        alloc, new=numeric(), sizes=0L, depth=depth, type="tmp"
      )
      stack <- append_stack(
        stack, alloc=alloc, id=alloc[['i']], depth=depth,
        size=0L, group=0, argn=argn
      )
    # - Control Parameter ------------------------------------------------------
    } else if (
      type %in% c("control", "flag") || !name %in% colnames(alloc[['names']])
    ) {
      # Need to eval parameter
      tryCatch(
        arg.e <- eval(call, envir=data, enclos=env),
        error=function(e) stop(simpleError(conditionMessage(e), call.outer))
      )
      # Validate non-control external args after eval
      if(type == "leaf" && !is.num_naked(list(arg.e))) {
        # Next call, if any
        next.call.v <- which(seq_along(x[['call']]) > i & x[['type']] == 'call')
        err.call <-
          if(length(next.call.v)) x[['call']][[next.call.v[1L]]]
          else call
        stop(
          simpleError(
            paste0(
              "External argument `", name, "` for `", deparse1(err.call),
              "` is not unclassed numeric ", not_num_naked_err(name, arg.e), "."
            ),
            .CALL
      ) ) }
      size <- length(arg.e)
      if(type == "control") {
        if(!nzchar(argn)) stop("Internal Error: missing arg name for control.")
        ctrl <- list(arg.e)
        names(ctrl) <- argn
        stack.ctrl <- c(stack.ctrl, ctrl)
      } else if (type == "flag") {
        if(!nzchar(argn)) stop("Internal Error: missing arg name for flag")
        flag <- list(arg.e)
        names(flag) <- argn
        stack.flag <- c(stack.flag, flag)
      } else {
        alloc <- append_dat(
          alloc, new=list(arg.e), sizes=size, depth=depth, type="ext"
        )
        stack <- append_stack(
          stack, alloc=alloc, id=alloc[['i']], depth=depth, size=size, group=0,
          argn=argn
        )
      }

    # - Match a Symbol In Data -------------------------------------------------
    } else if (id <- name_to_id(alloc, name)) {
      is.grp <- alloc[['type']][id] == 'grp'
      # Record size (note `id` computed in conditional)
      stack <- append_stack(
        stack, alloc=alloc, id=id, depth=depth,
        size=if(is.grp) NA_real_ else alloc[['size']][id],
        group=is.grp + 0, argn=argn
      )
      # Update symbol depth (needed for assigned-to symbols)
      if(alloc[['depth']][id] > depth) alloc[['depth']][id] <- depth
      data.used <- union(data.used, id)
    } else stop("Internal Error: unexpected token.")
  }
  # Remove unused data, and re-index to account for that
  ids.all <- seq_along(alloc[['dat']])
  ids.keep <- ids.all[
    alloc[['type']] %in% c("res", "ext", "tmp", "sts") |
    ids.all %in% data.used
  ]
  call.dat <- lapply(
    call.dat, function(x) {
      x[['ids']] <- match(x[['ids']][x[['ids']] %in% ids.keep], ids.keep)
      x
  } )
  stack['id',] <- match(stack['id',], ids.keep)

  alloc.fin <- lapply(
    alloc[c('dat', 'alloc', 'depth', 'type', 'typeof')], "[", ids.keep
  )
  alloc.fin[['i']] <- match(alloc[['i']], ids.keep)
  list(alloc=alloc.fin, call.dat=call.dat, stack=stack)
}

## Compute Max Possible Size
##
## This is affected by maximum group size as well as any non-group parameters.
## This allows us to keep track of what the most significant size resulting from
## prior calls is in the presence of some calls affected by group sizes.  That
## way, if e.g. at some point we have a small group, but the limiting size is
## from the non-group data, that info isn't lost.
##
## IMPORTANT: the result of this alone _must_ be combined with preserving
## whether the data was group data or not.

vec_rec_known_size <- function(x) {
  tmp <- x[!is.na(x)]             # non-group sizes
  if(!length(tmp)) NA_real_
  else if(any(tmp == 0)) 0
  else max(tmp)
}
# only difference with above is we use the max group size for alloc

vec_rec_max_size <- function(x, gmax) {
  if(
    !is.numeric(x) || !is.matrix(x) || nrow(x) != 2 ||
    any(is.na(x[1L,] & !x[2L,]))
  )
    stop("Internal error, malformed size data.")

  size <- x[1L,]
  group <- as.logical(x[2L,])
  size[is.na(size) | group] <- gmax
  if(any(size == 0)) 0 else max(size)
}
## Track Required Allocations for Intermediate vectors
##
## r2c keeps a list of every vector that it uses in computations, including the
## original data vectors, any referenced external vectors, and any temporary
## allocated vectors.  The temporary allocated vectors usage is tracked so that
## they may be re-used if their previously values are no longer needed.  The
## tracking is done by keeping track of their depth in the call tree, and also
## whether any symbols were bound to them.  Once allocated, a vector is never
## truly freed until the whole function execution ends.
##
## `append_dat` is similar except it is used when the data already comes with
## memory pre-allocated (e.g. the original iteration varying data).
##
## This is a description of the input `dat` (which is also the returned value
## after update).
##
## * dat: (this is `dat[['dat']]`) the actual data, for "tmp" type (i.e.
##   generated by alloc_dat) this will be written to so should not be accessible
##   via R.
## * ids: an integer identifier for each item in `dat` (I think this is just
##   `seq_along(dat)`, but that seems silly and not sure now).
## * ids0: a unique identifier for each allocation, differs from `ids`
##   as soon as there is a re-use of a previous allocation.  This is to detect
##   deviations between the stack and allocations.
## * alloc: the true size of the vector (should be equivalent to
##   `lengths(dat[['dat']])`?).
## * size: the size of the vector in use (can be less than 'alloc' if a smaller
##   vector is assigned to a freed bigger slot).
## * type: one of "tmp" (allocated), "grp" (from the data we're generating
##   groups from, "ext" (any other data vector), "res" (the result), "sts"
##   (status flags, e.g. recycle warning).
## * typeof: the intended data format at the end of the computation.  Used to
##   try to track integer-ness (if any).
## * depth: the depth at which allocation occurred, only relevant for
##   `type == "tmp"`
## * i: scalar integer the first index of the most recently allocated/appended
##   item(s).  This will point to the result of the most recent calculation.
## * names: a matrix where the (possibly duplicated) column names are symbols,
##   row `ids` are the ids in `dat` each symbol is bound to, `scope` is the
##   scope level the symbol was created in, and `i.max` is the largest index in
##   the linearized call list that the symbol exists in as a leaf (indicating
##   that beyond that the name binding need not prevent release of memory).
##
## We're mixing return value elements and params, a bit, but there are some
## differences, e.g.:
##
## @param i.call the index of the element being processed in the linearized call
##   tree.

alloc_dat <- function(dat, depth, size, call, typeof='double', i.call) {
  if(depth == .Machine$integer.max)
    stop("Expression max depth exceeded for alloc.") # exceedingly unlikely

  call.name <-
    if(is.call(call) && (is.symbol(call[[1L]]) || is.character(call[[1L]])))
      as.character(call[[1L]])
    else ""

  # Cleanup expired symbols
  dat <- names_clean(dat, i.call)

  # Non-computing (passive) calls don't generate allocations, which means
  # `append_call_dat` will re-use the result slot of the previous expression
  # (conveniently these are the semantics of "{" and other non-computing
  # expressions}.
  if(call.name %in% ASSIGN.SYM) {
    # Remove protection from prev assignment to same name, and bind previous
    # computation (`dat[[i]]`) to it.
    name <- get_target_symbol(call)
    dat <- names_free(dat, name)
    dat <- names_bind(dat, name, dat[['i']])
  } else if(i.call == dat[['meta']][['i.last.action']]) {
    # Last call that computes anything should be written to result
    slot <- which(dat[['type']] == "res")
    dat[['i']] <- slot
    dat[['typeof']][slot] <- typeof
  } else if(!call.name %in% PASSIVE.SYM) {
    # We have a computing expression in need of a free slots
    free <-
      !is.finite(dat[['depth']]) &
      !dat[['ids']] %in% dat[['names']]['ids',]
    fit <- free & dat[['type']] == "tmp" & dat[['alloc']] >= size
    if(!any(fit)) {
      # New allocation required
      new <- list(numeric(size))
      id <- if(length(dat[['ids']])) max(dat[['ids']]) + 1L else 1L
      dat <- append_vec_dat(
        dat, new=new, ids=id, sizes=size, depth=depth, type="tmp",
        typeof=typeof
      )
      dat[['dat']] <- c(dat[['dat']], new)
      dat[['i']] <- id
    } else {
      # Re-use smallest available that will fit
      target <- which.min(dat[['alloc']][fit])
      slot <- seq_along(dat[['alloc']])[fit][target]

      dat[['depth']][slot] <- depth
      dat[['size']][slot] <- size
      dat[['typeof']][slot] <- typeof
      dat[['ids0']][slot] <- dat[['id0']] <- dat[['id0']] + 1L
      dat[['i']] <- dat[['ids']][slot]
    }
  } else if (call.name %in% PASSIVE.SYM) {
    # Don't do anything for these, effectively causing `dat[[i]]` to remain
    # unchanged for use by the next call.
  }
  # "free" the data produced by arguments.  Not in the malloc sense, just an
  # indication that next time this function is called it can re-use the
  # slot, provided the slot is not referenced in 'names'.
  dat[['depth']][dat[['depth']] > depth & dat[['type']] == 'tmp'] <- Inf

  dat
}
## Add naked numeric data, including group varying data, result, and other, to
## our r2c data tracking structure.  Temporary variables are added by
## `alloc_dat`, not this function.

append_dat <- function(dat, new, sizes, depth, type) {
  if(length(new)) { # it's possible `data` has no numeric nums
    if(is.null(names(new))) names(new) <- character(length(new))
    if(!is.list(new)) stop("Internal Error: `new` must be list.")
    if(!all(is.num_naked(new))) stop("Internal Error: bad data column.")
    if(!type %in% c("res", "grp", "ext", "sts"))
      stop("Internal Error: bad type.")

    # Append dat should never overwrite names in the existing scope
    names <- dat[['names']]
    if(
      any(
        names(new) %in%
        colnames(names[,names['scope',] == dat[['scope']], drop=FALSE])
    ) )
      stop("Internal error: cannot append names existing in current scope.")

    new.ids <- seq_along(new) + length(dat[['dat']])
    dat[['dat']] <- c(
      dat[['dat']],
      lapply(new, function(x) if(is.integer(x)) as.numeric(x) else x)
    )
    dat <- append_vec_dat(
      dat, new, ids=new.ids, sizes=sizes, depth=depth, type=type,
      typeof=vapply(new, typeof, "character")
    )

    # Bind names
    has.name <- nzchar(names(new))
    dat <- names_bind(dat, names(new)[has.name], new.ids[has.name])

    dat[['i']] <- new.ids[1L]
  }
  dat
}
append_vec_dat <- function(
  dat, new, ids, ids0, sizes, depth, type, typeof
) {
  if(length(new) != length(ids))
    stop("Internal Error: invalid ids.")

  dat[['ids']] <- c(dat[['ids']], ids)
  ids0.new <- dat[['id0']] + seq_along(ids)
  dat[['ids0']] <- c(dat[['ids0']], ids0.new)
  dat[['id0']] <- ids0.new[length(ids0.new)]

  dat[['alloc']] <- c(dat[['alloc']], sizes)
  dat[['size']] <- c(dat[['size']], sizes)
  dat[['depth']] <- c(dat[['depth']], rep(depth, length(new)))
  dat[['type']] <- c(dat[['type']], rep(type, length(new)))
  dat[['typeof']] <- c(dat[['typeof']], typeof)
  dat
}

#' Helper Functions for Symbol Management
#'
#' * `names_clean` drops name that have no future references to them, which
#'   makes it safe to release any memory that they've been bound to.
#' * `names_free` frees any allocation previously bound to a symbol, use this
#'   when you give a symbol a new binding in the same scope.
#' * `names_bind` record new symbols and their binding to the data allocation.
#'
#' @noRd

names_clean <- function(alloc, i.call) {
  names <- alloc[['names']]
  # Drop out ouf scope names
  names <- names[, names['scope',] <= alloc[['scope']], drop=FALSE]
  # Drop expired names
  alloc[['names']] <- names[, names['i.max',] >= i.call, drop=FALSE]
  alloc
}
names_free <- function(alloc, new.names) {
  names <- alloc[['names']]
  to.free <-
    names['scope', ] == alloc[['scope']] &
    colnames(names) %in% new.names
  alloc[['names']] <- names[, !to.free, drop=FALSE]
  alloc
}
names_bind <- function(alloc, new.names, new.ids) {
  new.names.mx <- rbind(
    ids=new.ids,
    scope=rep(alloc[['scope']], length(new.names)),
    i.max=alloc[['meta']][['i.sym.max']][new.names]
  )
  new.names.mx['i.max', is.na(new.names.mx['i.max',])] <- 0L
  colnames(new.names.mx) <- new.names
  alloc[['names']] <- cbind(alloc[['names']], new.names.mx)
  alloc
}
name_to_id <- function(alloc, name) {
  if (id <- match(name, rev(colnames(alloc[['names']])), nomatch=0)) {
    # Reconvert the name id into data id (`rev` simulates masking)
    id <- ncol(alloc[['names']]) - id + 1L
    alloc[['names']]['ids', id]
  }
}



init_dat <- function(call, meta, scope) {
  list(
    # Allocation data
    dat=list(),
    names=rbind(ids=integer(), scope=integer(), i.max=integer()),

    # Equal length vector data
    alloc=numeric(),
    depth=integer(),
    ids=integer(),
    ids0=integer(),
    type=character(),
    typeof=character(),

    # Other data
    i=0L,
    id0=0L,
    scope=scope,
    meta=meta
  )
}
## Stack used to track parameters ahead of reduction when processing call.
init_stack <- function() {
  matrix(
    numeric(), nrow=5L,
    dimnames=list(
      c(
        'id',      # id in our allocated data structure
        'id0',     # unique id for integrity checks
        'depth',
        'size',    # size, possibly NA if unknown
        'group'    # size affected by group
      ),
      NULL
) ) }
append_stack <- function(stack, alloc, id, depth, size, group, argn) {
  # ctrl and flag have id = 0
  id0 <- if(!id) id else alloc[['ids0']][id]
  stack <- cbind(stack, c(id, id0, depth, size, group))
  colnames(stack)[ncol(stack)] <- argn
  stack
}
## Record Call Data
##
## Associates each call with the allocation data used, along with control and
## flag data.
##
## @return list containing:
##
## * call: the call
## * ids: the allocation ids of the parameter, followed by that of the result.
## * ctrl, flag: evaluated control and flag data

append_call_dat <- function(call.dat, call, stack, alloc, depth, ctrl, flag) {
  param.ids <- stack['id', stack['depth',] > depth]
  param.ids0 <- stack['id0', stack['depth',] > depth]
  # Make sure that parameter ids tie out
  param.check <- alloc[['ids0']][param.ids] == param.ids0
  # For braces only the last parameter matters
  if(is.brace_call(call)) param.check <- param.check[length(param.check)]
  if(!all(param.check))
    stop(
      "Internal Error: alloc/stack mismatch for ",
      toString(names(which(!param.check)))
    )
  # alloc[['i']] is the last made allocation, which will be the result
  ids <- c(param.ids, alloc[['i']])
  c(call.dat, list(list(call=call, ids=ids, ctrl=ctrl, flag=flag)))
}
## Check function validity
check_fun <- function(x, env) {
  if(!x %in% names(VALID_FUNS))
    stop("`", as.character(call[[1L]]), "` is not a supported function.")
  if(identical(x, 'r2c_copy')) {
    # waive check for `r2c_copy` so that `r2c` does not need to be attached
    # to the search path given user doesn't control when this gets injected.
  } else if(
    !identical(
      got.fun <- try(get(x, envir=env, mode="function"), silent=TRUE),
      VALID_FUNS[[c(x, "fun")]]
  ) ) {
    tar.fun <- VALID_FUNS[[c(x, "fun")]]
    env.fun <-
      if(is.null(environment(tar.fun))) getNamespace("base")
      else environment(tar.fun)
    stop(
      "Symbol `", x, "` does not resolve to the expected function from ",
      format(env.fun),
      if(is.function(got.fun) && !identical(env.fun,  environment(got.fun)))
        paste0(" (resolves to one from ", format(environment(got.fun)), ")")
) } }

