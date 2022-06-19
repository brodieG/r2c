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

## Used to be an S3 method, but that makes reloading package a problem.

#' Basic Split Apply Combine
#'
#' Evaluates quoted expressions in the context of data split by group.  Intended
#' purely for testing against r2c calculations.
#'
#' @export
#' @param data a data frame with numeric columns
#' @param call quoted R call to evaluate
#' @param env environment to use as enclosure
#' @return numeric vector

bsac <- function(call, g, data, MoreArgs=list(), env=parent.frame()) {
  data <- if(!is.data.frame(data)) as.data.frame(data) else data
  d.s <- split(data, g)

  more <- list2env(MoreArgs, parent=env)
  res <- lapply(
    d.s,
    function(x, call) eval(call, envir=x, enclos=more), call=call
  )
  unlist(res)
}


is.num_naked <- function(x)
  vapply(x, is.vector, TRUE, "numeric") |
  vapply(x, is.vector, TRUE, "integer")

not_num_naked_err <- function(name, val) {
  sprintf(
    "(type: %s %s%s)",
    typeof(val),
    if(length(class(val))) "class: " else "",
    if(length(class(val))) toString(class(val)) else ""
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


