## Copyright (C) 2022 Brodie Gaslam
##
## This file is part of "r2c - A DSL for Fast Statistic Computation in R"
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

unitizer_sect("slope", {
  r2c_slope(x, y)
  r2c_slope(z, y)
  r2c_slope(z, x=y)

  r2c_slope(x.weird, y)
  r2c_slope(x.na, y)
  r2c_slope(x.inf, y)
  r2c_slope(y, x.weird)
  r2c_slope(y, x.na)
  r2c_slope(y, x.inf)
  r2c_slope(x, numeric())
  r2c_slope(numeric(), x)
  r2c_slope(x, 1:3)
  r2c_slope(1:3, x)
  r2c_slope(x, 1:101)
  r2c_slope(1:101, x)
})
