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

library(r2c)

## We'll add up whole powers of two so that we can tell by the bit-components of
## things that all the correct window elements were added.

wvec <- 2^(0:4)

unitizer_sect('Equivalence', {
  identical(
    rolli_exec(r2c_sum, wvec, n=3, by=1),
    rollby_exec(r2c_sum, wvec, width=2, by=1)
  )

})
