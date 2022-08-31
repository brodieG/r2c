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

#' Fast Iterated Statistic Computation in R
#'
#' Compiles a subset of R into machine code so that expressions composed with
#' that subset can be applied repeatedly on varying data without interpreter
#' overhead.
#'
#' @docType package
#' @name r2c-package
#' @import vetr
#' @importFrom utils globalVariables
#' @useDynLib r2c, .registration=TRUE, .fixes="R2C_"

NULL

utils::globalVariables(".")  # for vetr .
