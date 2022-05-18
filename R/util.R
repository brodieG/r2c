## Copyright (C) 2022 Brodie Gaslam
##
## This file is part of "r2c - Fast Iterated Statistics in R"
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

#' @export

print_code_text <- function(x, ...) {
  y <- unlist(strsplit(x[['code-text']], "\n"))
  writeLines(paste0(format(seq_along(y)), "| ", y))
  invisible(x)
}

#' Basic Split Apply Combine
#'
#' Evaluates quoted expressions in the context of data split by group.  Intended
#' purely for testing against C calculations.
#'
#' @param data a data frame with numeric columns
#' @param call quoted R call to evaluate
#' @param env environment to use as enclosure
#' @export

base_grp_eval <- function(data, g, call, env=parent.frame()) {
  dg <- split(data, g)
  unlist(
    unname(
      lapply(dg, function(x, call) eval(call, envir=x, enclos=env), call=call)
  ) )
}

