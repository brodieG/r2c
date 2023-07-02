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

unitizer_sect("Subset", {
  # zero length subset
  # subset in groups
  # out of bounds subset
  # different index types (including logical)
  f1a <- r2cq(x[y])
  f1a(11:15, c(2, 4))
  f1a(11:15, rep(5:1, 2))

  # error
  f1a(11:15, 1:5 > 2L)
  f1a(11:15, 6)
  f1a(11:15, -1)
  f1a(11:15, numeric())
  f1a(11:15, c(3, NA))
  f1a(11:15, c(3, Inf))

  # groups
  group_exec(f1a, list(1:6), rep(1:2, each=3), MoreArgs=list(c(1L,3L)))
})
unitizer_sect("Sub-assign", {
  f2a <- r2cq({x[s] <- y; x})
  f2a(11:15, c(2, 4), 98:99)
  f2a(11:15, c(2, 4), 97:99)
  f2a(11:15, -c(2, 4), 97:99)
  f2a(11:15, 0, 97:99)
  f2a(11:15, integer(), 97:99)
  f2a(11:15, c(3, NA), 97:99)
  f2a(11:15, c(3, Inf), 97:99)
  f2a(11:15, 1:5 > 2L, 97:99)

  # In groups
  group_exec(f2a, list(1:6, c(3,1,1,1,1,3), -(1:6)), rep(1:2, each=3))
})
