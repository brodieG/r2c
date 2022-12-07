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

#' Basic Split Apply Combine
#'
#' Evaluates quoted expressions in the context of data split by group.  Intended
#' purely for testing against r2c calculations.
#'
#' @export
#' @inheritParams group_exec
#' @param call quoted R call to apply to each group.
#' @return numeric vector

bsac <- function(call, groups, data, MoreArgs=list(), enclos=parent.frame()) {
  data <- if(!is.data.frame(data)) as.data.frame(data) else data
  d.s <- split(data, groups)

  more <- list2env(MoreArgs, parent=enclos)
  res <- lapply(
    d.s,
    function(x, call) eval(call, envir=x, enclos=more), call=call
  )
  unlist(res)
}
#' Retrieve First Vector from Data
#'
#' Designed to handle the case where `data` can be either a numeric vector or a
#' list of numeric vectors.
#'
#' @export
#' @param x a numeric vector, or a (possibly empty) list of numeric vectors.
#' @return if `data` is a list, the first element if it is a numeric vector, or
#'   an empty numeric vector if the list is empty.  If `data` is a numeric
#'   vector, then `data`.  Otherwise an error is thrown.
#' @examples
#' first_vec(1:5)
#' first_vec(runif(5))
#' first_vec(mtcars)
#' first_vec(matrix(1:4, 2))  # matrices treated as vectors

first_vec <- function(x) {
  vetr(is.list(.) || is.numeric(.))
  if(is.list(x) && !length(x)) x <- numeric()
  if(is.list(x)) x <- x[[1L]]
  if(!is.numeric(x))
    stop("Argument `data` must be numeric, or a list containing numeric vectors.")
  x
}

is.num_naked <- function(x)
  vapply(x, is.vector, TRUE, "numeric") |
  vapply(x, is.vector, TRUE, "integer")

not_num_naked_err <- function(name, val) {
  has.class <- length(class(val)) && !identical(class(val), typeof(val))
  sprintf(
    "(type: %s%s%s)",
    typeof(val),
    if(has.class) ", class: " else "",
    if(has.class) toString(class(val)) else ""
) }

## Run an Expression with Random Seed Unset
##
## In cases where we would really like something to be random and not dictated
## by a set seed.

without_seed <- function(expr, env=parent.frame()) {
  prev.seed <- mget(
    ".Random.seed", envir=.GlobalEnv, ifnotfound=list(NULL)
  )[[1L]]
  on.exit({
    if(is.null(prev.seed) && exists(".Random.seed", envir=.GlobalEnv))
      rm(".Random.seed", envir=.GlobalEnv)
    else
      assign(".Random.seed", prev.seed, envir=.GlobalEnv)
  })
  set.seed(NULL)
  expr
}


