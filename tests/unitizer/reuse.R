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

unitizer_sect("re-use", {
  slope <- quote(((x - mean(x)) * (y - mean(y))) / (x - mean(x))^2)
  (slope.r <- reuse_calls(slope))
  identical(eval(slope), eval(slope.r))

  intercept <- quote(
    mean(y) - mean(x) * ((x - mean(x)) * (y - mean(y))) / (x - mean(x))^2
  )
  (intercept.r <- reuse_calls(intercept))
  identical(eval(intercept), eval(intercept.r))
})
