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

#' Generate a Shared Library Object From C Code
#'
#' @param x character the C code to compile
#' @return character file name of the SO; will be in a temporary directory, it is
#'   the users responsibility to preserve and/or discard the file.

make_shlib <- function(x) {
  if(!is.character(x) || anyNA(x))
    stop("Argument `x` must be character and free of NAs.")

  dir <- tempfile()
  dir.create(dir)
  file <- file.path(dir, 'code.c')
  writeLines(x, file)
  # what if we're running with RD or similar?
  system2(R.home("bin/R"), c("CMD", "SHLIB", file))
  # is this what's returned on windows?
  file.path(dir, 'code.so')
}
