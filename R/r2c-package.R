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

#' Fast Iterated Statistics in R
#'
#' Compiles a selected subset of R into native instructions so that that
#' expressions composed from that subset can be executed repeatedly on varying
#' data without interpreter overhead.
#'
#' @docType package
#' @name r2c
#' @import vetr
#' @useDynLib r2c, .registration=TRUE, .fixes="R2C_"

NULL
