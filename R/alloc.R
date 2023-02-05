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

collect_call_symbols <- function(x) {
  syms <- character()
  if(is.call(x) && length(x) > 1) {
    syms <- c(syms, collect_call_symbols(x))
  } else if (is.symbol(x)) {
    syms <- as.character(x)
  }
  syms
}
collect_loop_call_symbols <- function(x) {
  syms <- character()
  if(is.call(x) && length(x) > 1) {
    name <- as.character(x[[1L]])
    syms <-
      if(name == "for" && length(x) == 4L) collect_call_symbols(x[[4L]])
      else if(name == "while" && length(x) == 3L) collect_call_symbols(x[[3L]])
      else if(name == "repeat" && length(x) == 2L) collect_call_symbols(x[[2L]])
      else character()
  }
  syms
}

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
  call <- x[['call']]
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
#' Allocate Required Storage
#'
#' For each call, we allocate a vector for the result in the storage list,
#' possibly re-using a previously allocated but "freed" vector.  Additionally,
#' we record the indices into the storage list for the result of each call, as
#' well as for each argument to it.
#'
#' Leaf symbols are looked for through (in order):
#'
#' * Previously assigned symbols in the expression
#' * The naked data vectors
#' * The outside environment
#'
#' The calls have been linearized by `preprocess` (see that for details).
#'
#' This process converts `preproc$call` to `alloc$call.dat` (and `$depth`, and
#' more).  `alloc$call.dat` should have as many entries as there are C calls
#' in `preproc$code`.
#'
#' An allocation is considered "freed" as soon as we emerge above the call
#' level for which it was originally made, unless it is also referenced by a
#' symbol that is still in use (designated by the "protected" vector).
#'
#' @param x preprocess data as produced by `preprocess`
#' @return an alloc object:
#'
#' alloc
#'   $alloc
#'     $dat: storage list of status vectors, result vector, data vectors, and
#'       allocated vectors.
#'     $alloc: the used size of the vectors in `$dat` (note that for iterated
#'       data it will be the size of the largest iteration, which may/will be
#'       less than the true size).
#'     $depth: tree level at which the vectors are occupied, note it should
#'       really be "height" under a tree metaphor.  Here depth = 0 means root,
#'       whereas leaves will have highest depth values.
#'     $type: is it a result vector, data vector, etc.
#'     $typeof: double/integer to track type for possible conversion to int
#'     $i:
#'   $call.dat: each actual call with a C counterpart
#'     $call: the R call
#'     $ids: ids in `alloc$dat` for parameters, and then result
#'     $ctrl: evaluated control parameters
#'     $flag: computed flag paramteter value
#'   $stack:
#'     matrix used to track parameter sizes, but the time it's returned it
#'     should just have the size of the final return
#'
#' @noRd
#' @param x the result of preprocessing an expression

alloc <- function(x, data, gmax, par.env, MoreArgs, .CALL) {
  # - Initialize ---------------------------------------------------------------
  env <- list2env(MoreArgs, parent=par.env)
  call.outer <- x[['call']][[length(x[['call']])]]
  i.call.max <- length(x[['call']])
  sym.max <- latest_symbol_instance(x)

  # Status control placeholder.
  alloc <- append_dat(
    init_dat(x[['call']]), new=list(numeric(IX[['STAT.N']])),
    sizes=IX[['STAT.N']],
    depth=0L, type="sts", i.call.max=i.call.max
  )
  # Result placeholder, to be alloc'ed once we know group sizes.
  alloc <- append_dat(
    alloc, new=list(numeric()), sizes=0L, depth=0L, type="res",
    i.call.max=i.call.max
  )
  # Add group data.
  if(!all(nzchar(names(data)))) stop("All data must be named.")
  data.naked <- data[is.num_naked(data)]
  data.used <- integer()
  alloc <- append_dat(
    alloc, new=data.naked, sizes=rep(gmax, length(data.naked)),
    depth=0L, type="grp", i.call.max=i.call.max
  )
  # - Process ------------------------------------------------------------------
  #
  # Objective is to compute how much temporary storage we need to track all the
  # intermediate calculations in the call tree, and to allocate all the vectors
  # into a data structure.  This data structure will also include references to
  # external vectors (if there are such references), and the result of
  # evaluating the control/flag parameters.
  #
  # From the data structure, For each call we want to record:
  # * The ids of the parameters in the data.
  # * The id of the result in the data.

  stack <- init_stack()
  stack.ctrl <- stack.flag <- list()
  call.dat <- list()

  for(i in seq_along(x[['call']])) {
    # Assigned symbol is processed as part of the assignment call, not here
    if(x[['assign']][i]) next

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
      if(name %in% ASSIGN.SYM) {
        assign.name <- get_target_symbol(call, name)
        protected <- sym.max[assign.name]
      } else {
        assign.name <- ""
        protected <- 0L
      }
      if(ftype[[1L]] == "constant") {
        # Always constant size, e.g. 1 for `sum`
        size <- ftype[[2L]]
        alloc <- alloc_dat(
          alloc, depth, size=size, call, typeof=res.typeof, i.call=i,
          protected=protected, name=assign.name
        )
        group <- 0
      } else if(ftype[[1L]] %in% c("arglen", "vecrec")) {
        # Length of a specific argument, like `probs` for `quantile`
        if(!all(ftype[[2L]] %in% colnames(stack)))
          stop(
            simpleError(
              paste0(
                "Parameter(s) ",
                deparse1(ftype[[2L]][!ftype[[2L]]%in% colnames(stack)]),
                " missing but required for sizing."
              ),
              .CALL
          ) )
        # Get parameter data. depth + 1L should be params to current call, note
        # this is not generally true for the stuff in `x`, only for the stack.
        param.cand <- which(
          colnames(stack) %in% ftype[[2L]] & stack['depth',] == depth + 1L
        )
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
          alloc, depth, size=vec_rec_max_size(sizes.tmp, gmax), call,
          typeof=res.typeof, i.call=i, protected=protected, name=assign.name
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
        call.dat, call=call,
        ids=c(stack['id', stack['depth',] > depth], result=id),
        ctrl=stack.ctrl, flag=flag
      )
      stack <- append_stack(
        stack[,stack['depth',] <= depth, drop=FALSE], id=id, depth=depth,
        size=size, group=group, argn=argn
      )
      stack.ctrl <- stack.flag <- list()

    # - Control Parameter ------------------------------------------------------
    } else if (
      type %in% c("control", "flag") || !name %in% alloc[['name']]
    ) {
      # Need to eval parameter
      tryCatch(
        arg.e <- eval(call, envir=data, enclos=env),
        error=function(e) stop(simpleError(conditionMessage(e), call.outer))
      )
      # Validate external args after eval
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
        ctrl <- list(arg.e)
        names(ctrl) <- argn
        stack.ctrl <- c(stack.ctrl, ctrl)
        id <- 0L
      } else if (type == "flag") {
        flag <- list(arg.e)
        names(flag) <- argn
        stack.flag <- c(stack.flag, flag)
        id <- 0L
      } else {
        alloc <- append_dat(
          alloc, new=list(arg.e), sizes=size, depth=depth, type="ext",
          i.call.max=i.call.max
        )
        id <- alloc[['i']]
      }
      stack <- append_stack(
        stack, id=id, depth=depth, size=size, group=0, argn=argn
      )

    # - Match a Symbol In Data -------------------------------------------------
    } else if (id <- match(name, rev(alloc[['name']]), nomatch=0)) {
      # Reconvert the id into data order, matched rev to simulate masking
      id <- length(alloc[['name']]) - id + 1L
      is.grp <- alloc[['type']][id] == 'grp'
      # Record size (note `id` computed in conditional)
      stack <- append_stack(
        stack, id=id, depth=depth,
        size=if(is.grp) NA_real_ else alloc[['size']][id],
        group=is.grp + 0, argn=argn
      )
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
    alloc[c('dat', 'alloc', 'depth', 'type', 'typeof', 'name')], "[", ids.keep
  )
  alloc.fin[['i']] <- match(alloc[['i']], ids.keep)
  list(alloc=alloc.fin, call.dat=call.dat, stack=stack)
}
## Convert Group Varying data ID to Alloc ID
##
## Group (iteration) varying data is offset from beginning of allocation data,
## so we need to translate.

get_gdat_id <- function(gd_id) {
  gd_id + IX[['I.GRP']]  # starting column for group varying data
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
## List of all allocated temporary vector sizes, and which of those are free,
## along with identifiers, and a scalar noting which of the slots the most
## recent(?) request was assigned to.
##
## * dat: the actual data, for "tmp" type (i.e. generated by alloc_dat) this
##   will be written to so should not be accessible via R, or at least hidden.
##   Doesn't have to be done this way but it simplifies things.
## * ids: an integer identifier for each item in `dat`.
## * alloc: the true size of the vector (should be equivalent to
##   `lengths(dat[['dat']])`?).
## * size; the size of the vector in use (can be less than 'alloc' if a smaller
##   vector is assigned to a freed bigger slot).
## * type: one of "tmp" (allocated), "grp" (from the data we're generating
##   groups from, "ext" (any other data vector), "res" (the result), "sts"
##   (status flags, e.g. recycle warning).
## * typeof: the intended data format at the end of the computation.  Used to
##   try to track integer-ness (if any).
## * depth: the depth at which allocation occurred, only relevant for
##   `type == "tmp"`
## * i: scalar integer the first index of the appended items.
## * protected: the latest use of the symbol in the linearized call list,
##   non-zero only for allocations bound to a symbol (i.e. assigned to), can be
##   released once we're past that step in the linearized call list (as tracked
##   by `i.call`).
##
## We're mixing return value elements and params, a bit, but there are some
## differences, e.g.:
##
## @param i.call the index of the element being processed in the linearized call
##   tree.

alloc_dat <- function(
  dat, depth, size, call, typeof='double', i.call, protected, name=""
) {
  if(depth == .Machine$integer.max)
    stop("Expression max depth exceeded for alloc.") # exceedingly unlikely

  if(depth > 0) {
    # Remove protection from prev assignment to but not currently in-use name
    if(nzchar(name)) {
      unprotect <- dat[['name']] == name & !is.finite(dat[['depth']])
      dat[['protected']][unprotect] <- 0L
      dat[['name']][unprotect] <- ''
    }
    # Look for free slots
    free <- !is.finite(dat[['depth']]) & dat[['protected']] < i.call
    fit <- free & dat[['type']] == "tmp" & dat[['alloc']] >= size
    if(!any(fit)) {
      # New allocation, then sort by size
      dat[['alloc']] <- c(dat[['alloc']], size)
      dat[['size']] <- c(dat[['size']], size)
      dat[['depth']] <- c(dat[['depth']], depth)
      dat[['type']] <- c(dat[['type']], 'tmp')
      dat[['typeof']] <- c(dat[['typeof']], typeof)
      dat[['protected']] <- c(dat[['protected']], protected)
      dat[['name']] <- c(dat[['name']], name)

      id <- if(length(dat[['ids']])) max(dat[['ids']]) + 1L else 1L
      dat[['ids']] <- c(dat[['ids']], id)
      dat[['dat']] <- c(dat[['dat']], list(numeric(size)))
      dat[['i']] <- id
    } else {
      # Allocate to smallest available that will fit
      target <- which.min(dat[['alloc']][fit])
      slot <- seq_along(dat[['alloc']])[fit][target]
      dat[['depth']][slot] <- depth
      dat[['size']][slot] <- size
      dat[['typeof']][slot] <- typeof
      dat[['protected']][slot] <- protected
      dat[['name']][slot] <- name
      dat[['i']] <- dat[['ids']][slot]
    }
  } else {
    # Result
    slot <- which(dat[['type']] == "res")
    dat[['i']] <- slot
    dat[['typeof']][slot] <- typeof
  }
  # Free the data we no longer need for the next go-around.  This is not a
  # "free" in the malloc sense, just an indication that next time this function
  # is called it can re-use the slot.  Data is only truly freed if any
  # protection it has has expired.
  dat[['depth']][dat[['depth']] > depth & dat[['type']] == 'tmp'] <- Inf
  dat
}
init_dat <- function(call) {
  list(
    dat=list(), alloc=numeric(), depth=integer(), ids=integer(), type=character(),
    typeof=character(), i=0L
  )
}
# Naked numeric data, including group varying data, result, and other in that
# order, where other is external data and temporary variables, although the
# temorary variables are added by `alloc_dat`, not this function.

append_dat <- function(dat, new, sizes, depth, type, i.call.max) {
  if(length(new)) { # it's possible `data` has no numeric nums
    if(is.null(names(new))) names(new) <- character(length(new))
    if(!is.list(new)) stop("Internal Error: `new` must be list.")
    if(!all(is.num_naked(new))) stop("Internal Error: bad data column.")
    if(!type %in% c("res", "grp", "ext", "sts"))
      stop("Internal Error: bad type.")
    id.max <- length(dat[['dat']])
    dat[['dat']] <- c(
      dat[['dat']],
      lapply(new, function(x) if(is.integer(x)) as.numeric(x) else x)
    )
    dat[['ids']] <- c(dat[['ids']], seq_along(new) + id.max)
    dat[['alloc']] <- c(dat[['alloc']], sizes)
    dat[['size']] <- c(dat[['size']], sizes)
    dat[['depth']] <- c(dat[['depth']], rep(depth, length(new)))
    dat[['type']] <- c(dat[['type']], rep(type, length(new)))
    dat[['typeof']] <- c(dat[['typeof']], vapply(new, typeof, "character"))
    dat[['protected']] <- c(dat[['protected']], rep(i.call.max, length(new)))
    dat[['name']] <- c(dat[['name']], names(new))
    dat[['i']] <- id.max + 1L
  }
  dat
}
## Stack used to track parameters ahead of reduction when processing call.
init_stack <- function() {
  matrix(
    numeric(), nrow=4L,
    dimnames=list(
      c(
        'id',      # id in our allocated data structure
        'depth',
        'size',    # size, possibly NA if unknown
        'group'    # size affected by group
      ),
      NULL
) ) }
append_stack <- function(stack, id, depth, size, group, argn) {
  stack <- cbind(stack, c(id, depth, size, group))
  colnames(stack)[ncol(stack)] <- argn
  stack
}
append_call_dat <- function(call.dat, call, ids, ctrl, flag) {
  c(call.dat, list(list(call=call, ids=ids, ctrl=ctrl, flag=flag)))
}
## Check function validity
check_fun <- function(x, env) {
  if(!x %in% names(VALID_FUNS))
    stop("`", as.character(call[[1L]]), "` is not a supported function.")
  if(
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

