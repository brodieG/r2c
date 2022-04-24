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


