## Copyright (C) 2022 Brodie Gaslam
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

#' Allocate Required Storage
#'
#' Group data goes first, then result, then any other temporary or external
#' data.  For each call, we record indices into the storage list for each
#' argument and the result of evaluating the call.
#'
#' @noRd
#' @param x the result of preprocessing an expression

alloc <- function(x, data, gmax, par.env, MoreArgs) {
  # - Initialize ---------------------------------------------------------------
  env <- list2env(MoreArgs, parent=par.env)
  # will track pending parameters to process

  # Add group data.
  if(!all(nzchar(names(data)))) stop("All data must be named.")
  data.naked <- data[is.num_naked(data)]
  data.used <- logical(length(data.naked))
  names(data.used) <- names(data.naked)
  alloc <- append_dat(
    init_dat(), new=data.naked, sizes=rep(gmax, length(data.naked)),
    depth=0L, type="grp"
  )
  # Put in a dummy for the result, to be alloc'ed once we know group sizes.
  alloc <- append_dat(
    alloc, new=list(numeric()), sizes=0L, depth=0L, type="res"
  )
  # - Process ------------------------------------------------------------------
  #
  # Objective is to compute how much temporary storage we need to track all the
  # intermediate calculations in the call tree.
  #
  # The call tree has been linearized depth first, so the parameters are
  # recorded before the call they belong to.  To process we will accumulate
  # parameters in a stack, until we see a call at which point reduce.
  #
  # For each call we want to record:
  # * The ids of the parameters.
  # * The id of the result.
  # * The list of evaluated control parameters.

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

      if(ftype[[1L]] == "constant") {
        # Always constant size, e.g. 1 for `sum`
        size <- ftype[[2L]]
        alloc <- alloc_dat(alloc, depth, size=size, call, typeof=res.typeof)
        group <- 0
      } else if(ftype[[1L]] %in% c("arglen", "vecrec")) {
        # Length of a specific argument, like `probs` for `quantile`
        if(!all(ftype[[2L]] %in% colnames(stack)))
          stop(
            "Parameter(s) ",
            deparse1(ftype[[2L]][!ftype[[2L]]%in% colnames(stack)]),
            " missing but required for sizing."
          )
        # Get parameter data (depth + 1L should be params to current call)
        sizes.tmp <- stack[
          c('size', 'group'),
          colnames(stack) %in% ftype[[2L]] & stack['depth',] == depth + 1L,
          drop=FALSE
        ]
        alloc <- alloc_dat(
          alloc, depth, size=vec_rec_max_size(sizes.tmp, gmax), call,
          typeof=res.typeof
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
    } else if (
      type %in% c("control", "flag") || !name %in% names(data.naked)
    ) {
      # Need to eval parameter
      arg.e <- eval(call, envir=data, enclos=env)
      # Validate external args after eval
      if(type == "leaf" && !is.num_naked(list(arg.e))) {
        # Next call, if any
        next.call.v <- which(seq_along(x[['call']]) > i & x[['type']] == 'call')
        err.call <-
          if(length(next.call.v)) x[['call']][[next.call.v[1L]]]
          else call
        stop(
          "External argument `", name, "` for `", deparse1(err.call),
          "` is not unclassed numeric ", not_num_naked_err(name, arg.e), "."
      ) }
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
          alloc, new=list(arg.e), sizes=size, depth=depth, type="ext"
        )
        id <- alloc[['i']]
      }
      stack <- append_stack(
        stack, id=id, depth=depth, size=size, group=0, argn=argn
      )
    } else if (id <- match(name, names(data.naked), nomatch=0)) {
      # Record size (note `id` computed in conditional)
      stack <- append_stack(
        stack, id=id, depth=depth, size=NA_real_, group=1, argn=argn
      )
      data.used[id] <- TRUE
    } else stop("Internal Error: unexpected token.")
  }
  # Remove unused data, and re-index to account for that
  ids.all <- seq_along(alloc[['dat']])
  ids.keep <- ids.all[
    alloc[['type']] %in% c("res", "ext", "tmp") | ids.all %in% which(data.used)
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
  group <- which(as.logical(x[2L,]))
  size[is.na(size) | group] <- gmax
  if(any(size == 0)) 0 else max(size)
}
## Track Required Allocations for Intermediate vectors
##
## List of all allocated temporary vector sizes, and which of those are free,
## along with identifiers, and a scalar noting which of the slots the most
## request request was assigned to.
##
## * dat: the actual data, for "tmp" type (i.e. generated by alloc_dat) this
##   will be written to so should not be accessible via R, or at least hidden.
##   Doesn't have to be done this way but it simplifies things.
## * ids: an integer identifier for each item in `dat`.
## * alloc: the "true" size of the vector.
## * type: one of "tmp" (allocated), "grp" (from the data we're generating
##   groups from, "ext" (any other data vector), or "res" (the result)
## * typeof: the intended data format at the end of the computation.  Used to
##   try to track integer-ness (if any).
## * depth: the depth at which allocation occurred, only relevant for
##   `type == "tmp"`
## * i: scalar integer the first index of the appended items.

alloc_dat <- function(dat, depth, size, call, typeof='double') {
  # writeLines(sprintf("  d: %d s: %d c: %s", depth, size, deparse1(call)))
  if(depth == .Machine$integer.max)
    stop("Expression max depth exceeded for alloc.") # exceedingly unlikely

  if(depth > 0) {
    free <- !is.finite(dat[['depth']])
    fit <- free & dat[['type']] == "tmp" & dat[['alloc']] >= size
    if(!any(fit)) {
      # New allocation, then sort by size
      dat[['alloc']] <- c(dat[['alloc']], size)
      dat[['depth']] <- c(dat[['depth']], depth)
      id <- if(length(dat[['ids']])) max(dat[['ids']]) + 1L else 1L
      dat[['ids']] <- c(dat[['ids']], id)
      dat[['type']] <- c(dat[['type']], 'tmp')
      dat[['typeof']] <- c(dat[['typeof']], typeof)
      dat[['dat']] <- c(dat[['dat']], list(numeric(size)))
      # writeLines(paste0("    alloc new: ", size))
      dat[['i']] <- id
    } else {
      # Allocate to smallest available that will fit
      target <- which.min(dat[['alloc']][fit])
      slot <- seq_along(dat[['alloc']])[fit][target]
      # writeLines(
      #   sprintf("    re-use slot: %d (size %d)", slot, dat[['alloc']][slot])
      # )
      dat[['depth']][slot] <- depth
      dat[['typeof']][slot] <- typeof
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
  # is called it can re-use the slot.

  dat[['depth']][dat[['depth']] > depth & dat[['type']] == 'tmp'] <- Inf
  dat
}
init_dat <- function() list(
  dat=list(), alloc=numeric(), depth=integer(), ids=integer(), type=character(),
  typeof=character(), i=0L
)
# Naked numeric data, including group varying data, result, and other in that
# order, where other is external data and temporary variables, although the
# temorary variables are added by `alloc_dat`, not this function.

append_dat <- function(dat, new, sizes, depth, type) {
  if(length(new)) { # it's possible `data` has no numeric nums
    if(!is.list(new)) stop("Internal Error: `new` must be list.")
    if(!all(is.num_naked(new))) stop("Internal Error: bad data column.")
    if(!type %in% c("res", "grp", "ext")) stop("Internal Error: bad type.")
    id.max <- length(dat[['dat']])
    dat[['dat']] <- c(
      dat[['dat']],
      lapply(new, function(x) if(is.integer(x)) as.numeric(x) else x)
    )
    dat[['ids']] <- c(dat[['ids']], seq_along(new) + id.max)
    dat[['alloc']] <- c(dat[['alloc']], sizes)
    dat[['depth']] <- c(dat[['depth']], rep(depth, length(new)))
    dat[['type']] <- c(dat[['type']], rep(type, length(new)))
    dat[['typeof']] <- c(dat[['typeof']], vapply(new, typeof, "character"))
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
      capture.output(print(env.fun)),
      if(is.function(got.fun))
      paste0(
        " (resolves to one from ", capture.output(print(environment(got.fun))), ")"
      )
    )
  }
}

