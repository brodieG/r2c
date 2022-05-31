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

test1 <- function(x, y, z, w) .Call(R2C_test1, x, y, z, w)
test2 <- function(x, y, z, w) .Call(R2C_test2, x, y, z, w)
test3 <- function(x, y, z, w) .Call(R2C_test3, x, y, z, w)
test4 <- function(x, y, z, w) .Call(R2C_test4, x, y, z, w)
test5 <- function(x, y, z, w) .Call(R2C_test5, x, y, z, w)

