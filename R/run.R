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

## Compute Group Meta Data
##
## @param go ordered integer vector of groups ids
## @return list containing group sizes, group start offset (i.e. 0 for first
##   group), and largest group size.
##
group_sizes <- function(go) .Call(R2C_group_sizes, go)

#' Execute r2c Function Iteratively on Groups in Data
#'
#' Organizes `data` into row groups according to `groups`, and calls `r2c`
#' functions for each group with the arguments bound to the portion of the data
#' corresponding to each group.
#'
#' @export
#' @param fun a function produced by `r2c`.
#' @param groups an integer vector, or a list of equal-length integer vectors,
#'   the interaction of which defines individual groups to organize the data
#'   into.  Support for non-integer vectors will be added in the future.
#' @param data a numeric vector, or a list of numeric vectors, each vector the
#'   same length as the vector(s) in `groups`.  If a named list, the vectors
#'   will be matched to `fun` parameters by those names.  Elements without names
#'   are matched positionally.
#' @param MoreArgs a list of R objects to pass on as non-data parameters to
#'   `fun`.  Unlike with `data`, the entire object will be passed for each
#'   group.  This is useful for arguments that are intended to remain constant
#'   group to group.
#' @param sorted TRUE or FALSE (default), whether the vectors in `data` and
#'   `groups` are already sorted by `groups`.  If set to TRUE, the `data` will
#'   not be sorted prior to computation; if the data is truly sorted this
#'   produces the same results while avoiding the cost of sorting.
#' @return a if `groups` and `data` are atomic vectors, a named numeric vector
#'   with the results of executing `fun` on each group and the names set to the
#'   groups.  Otherwise, a "data.frame" with the group vectors as columns and
#'   the result column last.

group_exec <- function(fun, groups, data, MoreArgs=list(), sorted=FALSE) {
  # FIXME: add validation for shlib
  vetr(
    fun=is.function(.) && inherits(., 'r2c_base_fun'),
    groups=integer() || (list() && all(vapply(., is.integer, TRUE))) || NULL,
    data=(numeric() || (list() && all(is.num_naked(.)))),
    MoreArgs=list(),
    sorted=LGL.1
  )
  obj <- get_r2c_dat(fun)
  group_exec_int(
    obj, formals=formals(fun), env=environment(fun),
    groups=groups, data=data, MoreArgs=MoreArgs, sorted=sorted
  )
}
group_exec_int <- function(obj, formals, env, groups, data, MoreArgs, sorted) {
  preproc <- obj[['preproc']]
  shlib <- obj[['so']]

  # Make all arguments into lists
  mode <- if(!is.list(groups)) "vec" else "list"
  if(!is.list(data)) data <- list(data)
  if(length(d.len <- unique(lengths(data))) > 1L)
    stop("All `data` vectors must be the same length.")
  if(!is.null(groups)) {
    if(!is.list(groups)) groups <- list(groups)
    if(length(g.len <- unique(lengths(groups))) != 1L)
      stop("All `groups` vectors must be the same length.")
    if(length(groups) != 1L)
      stop("Only one grouping variable supported at the moment.")
    if(length(d.len) == 0 || !identical(d.len, g.len))
      stop("`groups` vectors must be the same length as `data` vectors.")
  } else {
    mode <- "ungrouped"
    if(!length(data))
      stop("NULL `groups` allowed only if `data` is non-empty.")
  }
  if (mode != "ungrouped") {
    if(!sorted) {
      o <- do.call(order, groups)
      go <- lapply(groups, "[", o)
      do <- lapply(data, "[", o)
    } else  {
      go <- groups
      do <- data
    }
    # return group lenghts, offsets, and max group size
    group.dat <- group_sizes(go[[1L]])
  } else {
    # Fake a single group
    do <- data
    group.dat <- list(as.numeric(length(d.len)), 0, as.numeric(length(d.len)))
  }
  # Match data against arguments
  params <- names(formals)
  args.dummy <- as.list(seq_len(length(do) + length(MoreArgs)))
  fun.dummy <- function() NULL
  formals(fun.dummy) <- formals
  environment(fun.dummy) <- env

  names(args.dummy) <- c(names(do), names(MoreArgs))
  call.dummy <- as.call(c(list(fun.dummy), as.list(args.dummy)))
  call.dummy.m <- unlist(
    # envir might not be righ there, test by forwarding dots
    as.list(match.call(fun.dummy, call.dummy, envir=env))[-1L]
  )
  dat.match <- call.dummy.m[call.dummy.m <= length(do)]
  names(do)[dat.match] <- names(dat.match)
  more.match <- call.dummy.m[call.dummy.m > length(do)]
  names(MoreArgs)[more.match - length(do)] <- names(more.match)

  # Prepare temporary memory allocations
  alloc <- alloc(
    x=preproc, data=do, gmax=group.dat[[3L]], par.env=env,
    MoreArgs=MoreArgs
  )
  stack <- alloc[['stack']]

  # Compute result size
  if(ncol(stack) != 1L)
    stop("Internal Error: unexpected stack state at exit.")

  group.sizes <- group.dat[[1L]]
  if(!stack['group', 1L]) { # constant group size
    group.res.sizes <- rep(stack['size', 1L], length(group.dat[[1L]]))
  } else if (is.na(stack['size', 1L])) {
    group.res.sizes <- group.sizes
  } else if (stack['size', 1L]) {
    group.res.sizes <- group.sizes
    group.res.sizes[stack['group', 1L] < stack['size', 1L]] <- stack['size', 1L]
  } else { # zero size
    stop("figure out what to do with zero size")
  }
  # Allocate result vector, this will be modified by reference
  if(length(alloc[['alloc']][['dat']][[alloc[['alloc']][['i']]]]))
    stop("Internal Error: result should be zero length when uninitialized.")
  alloc[['alloc']][['dat']][[alloc[['alloc']][['i']]]] <-
    numeric(sum(group.res.sizes))

  # Extract control parameters, and run sanity checks (not fool proof)
  dat <- alloc[['alloc']][['dat']]
  control <- lapply(alloc[['call.dat']], "[[", "ctrl")
  flag <- vapply(alloc[['call.dat']], "[[", 0L, "flag")
  ids <- lapply(alloc[['call.dat']], "[[", "ids")
  if(!all(unlist(ids) %in% seq_along(dat)))
    stop("Internal Error: Invalid data indices.")
  ids <- lapply(ids, "-", 1L) # 0-index for C

  dat_cols <- sum(alloc[['alloc']][['type']] == "grp")
  handle <- dyn.load(shlib)
  run_int(
    handle[['name']],
    alloc[['interface']],
    dat,
    dat_cols,
    ids,
    flag,
    control,
    group.sizes,
    group.res.sizes
  )
  # Result vector is modified by reference
  res <- dat[[which(alloc[['alloc']][['type']] == "res")]]

  # Generate and attach group labels
  if(mode != "ungrouped") {
    g.lab <- rep(go[[1L]][group.dat[[2L]] + 1L], group.res.sizes)
    # Attach group labels
    if(mode == 'list') {
      data.frame(group=g.lab, V1=res)
    } else if (mode == 'vec') {
      names(res) <- g.lab
    } else stop("Unknown return format mode")
  }
  res
}

run_int <- function(
  handle, interface, dat, dat_cols, ids, flag, control, group.sizes,
  group.res.sizes
) {
  .Call(
    R2C_run_internal,
    handle,
    interface,
    dat,
    dat_cols,
    ids,
    flag,
    control,
    group.sizes,
    group.res.sizes
  )
}
