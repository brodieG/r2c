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

unitizer_sect("uplus", {
  uplus <- r2cq(+x, check=TRUE)
  get_r_code(uplus)
  uplus(c(1, NA, Inf, -Inf, 0))
  uplus(NA)
  uplus(c(TRUE, FALSE))
})
unitizer_sect("uminus", {
  uminus <- r2cq(-x, check=TRUE)
  get_r_code(uminus)
  uminus(c(1, NA, Inf, -Inf, 0))
  uminus(NA)
  uminus(c(TRUE, FALSE))
})
unitizer_sect("negate", {
  negate <- r2cq(!x, check=TRUE)
  negate(c(1, NA, Inf, -Inf, 0))
  negate(c(TRUE, FALSE, NA))
})
