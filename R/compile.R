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

## Generate a Shared Library Object From C Code
##
## @param x character the C code to compile
## @return character file name of the SO; will be in a temporary directory, it is
##   the users responsibility to preserve and/or discard the file.

make_shlib <- function(x) {
  if(!is.character(x) || anyNA(x))
    stop("Argument `x` must be character and free of NAs.")

  dir <- tempfile()
  dir.create(dir)
  file.base <- file.path(dir, sprintf('code-%s', rand_string(8)))
  file.src <- paste0(file.base, ".c")
  file.obj <- paste0(file.base, ".so")
  writeLines(x, file.src)
  system2(R.home("bin/R"), c("CMD", "SHLIB", file.src))
  # is this what's returned on windows (we can specify, but should make sure if
  # the extension matters)?
  file.obj
}

rand_string <- function(len, pool=c(letters, 0:9))
  paste0(sample(pool, len, replace=TRUE), collapse="")

#' Compile an R Call Into Machine Instructions
#'
#' Translates an R call into C and compiles it into native instructions using 
#' `R CMD SHLIB`.
#'
#' @export
#' @param call an R expression, for `compileq` it is captured unevaluated, for
#'   `compile` it should be pre-quoted.
#' @param dir character(1L) name of a temporary directory to store the shared
#'   object file in.
#' @return the full path to the compiled shared object file.
#' @examples
#' compileq(sum(x + y))
#' compile(quote(sum(x + y))

r2c <- function(call, dir=tempfile()) {
  vetr(is.language(.))  # in theory could be e.g. 5L...
  preproc <- preprocess(call)
  so <- make_shlib(preproc[['code-text']])
  list(preproc=preproc, so=so)
}
#' @export
#' @rdname r2c

r2cq <- function(call, dir=tempfile()) r2c(substitute(call))
