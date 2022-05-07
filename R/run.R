## Copyright (C) 2022 Brodie Gaslam
##
## This file is part of "fapply - DSL For Fast Groupwise Numeric Calculations"
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

#' Experimental Code Runner
#'
#' @export

run_code <- function(code, fun, x) {
  shlib <- make_shlib(code)
  handle <- dyn.load(shlib)
  .Call(FAPPLY_run, handle[['name']], fun, x)
}
#' @rdname run_code
#' @export
run_code2 <- function(code, fun, x) {
  shlib <- make_shlib(code)
  handle <- dyn.load(shlib)
  .Call(FAPPLY_run2, handle[['name']], fun, x)
}
#' @rdname run_code
#' @export
run_code3 <- function(code, fun, x, g, flag=0L) {
  shlib <- make_shlib(code)
  handle <- dyn.load(shlib)
  o <- order(g)
  .Call(FAPPLY_run3, handle[['name']], fun, list(x[[1]][o]), g[o], flag)
}
#' @rdname run_code
#' @export

run_group <- function(shlib, fun, x, g, flag) {
  handle <- dyn.load(shlib)
  o <- order(g)
  .Call(FAPPLY_run3, handle[['name']], fun, list(x[[1]][o]), g[o], flag)
}
#' @rdname run_code
#' @export

run_group1 <- function(shlib, fun, x, g, flag) {
  handle <- dyn.load(shlib)
  o <- order(g)
  .Call(FAPPLY_run3a, handle[['name']], fun, list(x[[1]][o]), g[o], list(flag))
}
#' @rdname run_code
#' @export

run_group2 <- function(shlib, fun, x, g) {
  handle <- dyn.load(shlib)
  o <- order(g)
  .Call(FAPPLY_run3b, handle[['name']], fun, list(x[[1]][o], x[[2]][o]), g[o])
}
group_sizes <- function(go) .Call(FAPPLY_group_sizes, go)

#' @export

fapply <- function(data, groups, preproc, shlib) {
  # FIXME: add validation for shlib
  vetr(data.frame(), INT && length(.) == nrow(data))
  o <- order(groups)
  do <- data[o,]
  # return group lenghts, offsets, and max group size
  group.dat <- group_sizes(g[o])
  alloc <- alloc(x=preproc, data=do, gmax=group.dat[[3L]])
  stack <- alloc[['stack']]

  # Compute result size
  if(ncol(stack) != 1L)
    stop("Internal Error: unexpected stack state at exit.")

  group.sizes <- group.dat[[1L]]
  if(!stack['group', 1L]) { # constant group size
    group.res.sizes <- rep(stack['size', 1L], length(group.dat[[1L]]))
  } else if (is.na(stack['size', 1L])) {
    group.res.sizes <- group.sizes
  } else if (is.na(stack['size', 1L])) {
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
  ids <- lapply(alloc[['call.dat']], "[[", "ids")
  if(!all(unlist(ids) %in% seq_along(dat)))
    stop("Internal Error: Invalid data indices.")
  ids <- lapply(ids, "-", 1L) # 0-index for C

  dat_cols <- sum(alloc[['alloc']][['type']] == "grp")
  handle <- dyn.load(shlib)
  .Call(
    FAPPLY_run_internal,
    handle[['name']],
    alloc[['interface']],
    dat,
    dat_cols,
    ids,
    control,
    group.sizes,
    group.res.sizes
  )
  # Result vector is modified by reference
  dat[[which(alloc[['alloc']][['type']] == "res")]]
}

