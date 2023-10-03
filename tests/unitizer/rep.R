## Copyright (C) Brodie Gaslam
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

f1a <- r2cq(rep(x, times, length.out, each), check=TRUE)
f1b <- r2cq(rep(x, times, length.out, each)) # for errors w/o check

unitizer_sect("times", {
  f1a(1:3, 2, NA_real_, 1)
  f1a(1:3, 1:3, NA_real_, 1)
  f1a(numeric(), 10, NA_real_, 1)

  # logical out?
  f1a(TRUE, 3, NA_real_, 1)

  # bad times values
  f1b(1:3, 1:2, NA_real_, 1)
  f1b(1:3, -1, NA_real_, 1)
  f1b(1:3, numeric(), NA_real_, 1)
  f1b(1:3, Inf, NA_real_, 1)
  f1b(1:3, 1e300, NA_real_, 1)
  f1b(1:3, NA_real_, NA_real_, 1)
})
unitizer_sect("each", {
  # interaction of each and times
  f1a(1:3, 1:6, NA_real_, 2)
  f1a(1:3, 3, NA_real_, 2)
  f1a(1:3, 1:6, 5, 2)

  # Errors
  f1b(1:3, 1:3, NA_real_, 2)
  f1b(1:3, 1:6, NA_real_, 0)
  f1b(1:3, 1:6, NA_real_, Inf)
  f1b(1:3, 1:6, NA_real_, 1e300)  # 1e300 > R_XLEN_T_MAX
  f1b(1:3, 1, NA_real_, 1e300)
  f1b(1:3, 1:6, NA_real_, NA_real_)
  f1b(1:3, numeric(), NA_real_, 2)   # error
})
unitizer_sect("length.out", {
  # length.out overrides times completely
  f1a(1:3, 1:2, 1, 1)
  f1a(1:3, 1:3, 6, 1)
  f1a(1:3, 1:3, 8, 1)
  f1a(numeric(), 1, 3, 1)
  f1a(1:3, 1:3, 6, 2)
  f1a(1:3, 1:3, 8, 2)

  # Error
  f1b(1:3, 1:3, Inf, 2)
  f1b(1:3, 1:3, -1, 2)
  f1b(1:3, 1:3, 1e300, 2)
})
unitizer_sect("group_exec", {
  group_exec(f1a, 1:6, rep(1:2, 3), MoreArgs=list(2, NA_real_, 1))
  group_exec(f1a, 1:6, rep(1:2, 3), MoreArgs=list(1:3, NA_real_, 1))
  (res1 <- group_exec(f1a, 1:6, rep(1:2, 3), MoreArgs=list(1:6, NA_real_, 2)))
  res2 <- bsac(quote(rep(x, times = 1:6, each = 2)), list(x=1:6), rep(1:2, 3))
  identical(as.vector(res1), as.vector(res2))
  # Error
  group_exec(f1a, 1:5, c(rep(1L,3), rep(2L, 2)), MoreArgs=list(1:3, NA_real_, 1))
})

