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

#' Create a Library of Pre-Compiled "r2c_fun" Objects
#'
#' @param xs named list of R functions to convert to "r2c_fun" objects.
#' @return and "r2c_lib" object.
#' @export
#' @examples
#' lib.file <- tempfile()
#' saveRDS(
#'   r2c_lib(list(sum=function(x) sum(x), mean=function(x) mean(x))),
#'   lib.file
#' )
#' lib <- readRDS(lib.file)
#' lib$sum(1:10)
#' lib$mean(1:10)
#' with(mtcars, group_exec(lib$mean, hp, cyl))

r2c_lib <- function(
  xs,
  check=getOption('r2c.check.result', FALSE),
  quiet=getOption('r2c.quiet', TRUE),
  optimize=getOption('r2c.optimize', TRUE),
  envir=environment(x)
) {
  vetr(
    list() && all(vapply(., typeof, "") == "closure") && length(.) >= 1L,
  )
  if(
    !is.character(names(xs)) || !all(nzchar(xs)) ||
    !all(grepl("^[a-zA-Z][a-zA-Z0-9_]{,19}", names(xs)))
  ) {
    stop(
      "`xs` must all have ASCII names starting with a letter, composed with
      letters, numbers, and underscores, and be 20 or fewer characters in
      length."
    )
  }
  body.all <- lapply(xs, body)
  formals.all <- lapply(as.list, lapply(xs, formals))

  funs <- list(...)
  funs.nm <- names(funs)
  if(
    is.null(funs.nm) || !all(nzchar(funs.nm)) || anyNA(funs.nm) ||
    anyDuplicated(funs.nm)
  )
    stop("`...` elements must all be uniquely named.")
  if(!all(vapply(funs, typeof, "") == "closure"))
    stop("`...` elements must all be closures.")

  r2c_core(
    body(x), formals=as.list(formals(x)),
    check=check, quiet=quiet, optimize=optimize,
    envir=envir
  )
}

