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

unitizer_sect("Basic Logicals", {
  f_and <- r2cq(a & b, check=TRUE)
  f_or <- r2cq(a | b, check=TRUE)
  a <- rep(c(TRUE, FALSE, NA), 3)
  b <- rep(c(TRUE, FALSE, NA), each=3)
  c <- rep(c(1, 0, Inf, -Inf, NA_real_), 5)
  d <- rep(c(1, 0, Inf, -Inf, NA_real_), each=5)
  f_and(a, b)
  f_and(c, d)
  f_or(a, b)
  f_or(c, d)

  f_all <- r2cq(all(a), check=TRUE)
  f_all_n <- r2cq(all(a, b), check=TRUE)
  f_any <- r2cq(any(a), check=TRUE)
  f_any_n <- r2cq(any(a, b), check=TRUE)

  f_all(c(TRUE, NA))
  f_all(c(TRUE, NA, FALSE))
  f_all(c(TRUE, TRUE))
  f_all(c(FALSE, FALSE))
  f_all(c(NA, FALSE))
  f_all(c(FALSE, NA))

  f_all_n(TRUE, NA)
  f_all_n(TRUE, FALSE)
  f_all_n(NA, TRUE)
  f_all_n(NA, FALSE)
  f_all_n(TRUE, TRUE)
  f_all_n(TRUE, c(TRUE, FALSE))

  f_any(c(TRUE, NA))
  f_any(c(TRUE, NA, FALSE))
  f_any(c(TRUE, TRUE))
  f_any(c(FALSE, FALSE))
  f_any(c(FALSE, NA))
  f_any(c(NA, FALSE))

  f_any_n(TRUE, NA)
  f_any_n(TRUE, FALSE)
  f_any_n(NA, TRUE)
  f_any_n(NA, FALSE)
  f_any_n(FALSE, FALSE)
  f_any_n(FALSE, c(TRUE, FALSE))
})
unitizer_sect("na.rm", {
  f_all_narm <- r2cq(all(a, b, na.rm=na.rm), check=TRUE)
  f_any_narm <- r2cq(any(a, b, na.rm=na.rm), check=TRUE)

  f_all_narm(TRUE, NA, na.rm=FALSE)
  f_all_narm(TRUE, NA, na.rm=TRUE)
  f_all_narm(TRUE, c(FALSE, NA), na.rm=TRUE)
  f_all_narm(TRUE, c(NA, FALSE), na.rm=TRUE)
  f_all_narm(c(NA, TRUE), NA, na.rm=TRUE)

  f_any_narm(FALSE, NA, na.rm=FALSE)
  f_any_narm(FALSE, NA, na.rm=TRUE)
  f_any_narm(FALSE, c(FALSE, NA), na.rm=TRUE)
  f_any_narm(FALSE, c(NA, FALSE), na.rm=TRUE)
  f_any_narm(FALSE, c(NA, TRUE), na.rm=TRUE)
  f_any_narm(c(NA, FALSE), NA, na.rm=TRUE)
})
unitizer_sect("Relops", {
  f_gt <- r2cq(a > b, check=TRUE)
  f_gte <- r2cq(a >= b, check=TRUE)
  f_lt <- r2cq(a < b, check=TRUE)
  f_lte <- r2cq(a <= b, check=TRUE)
  f_eq <- r2cq(a == b, check=TRUE)
  f_neq <- r2cq(a != b, check=TRUE)

  f_gt(a, b)
  f_gte(a, b)
  f_lt(a, b)
  f_lte(a, b)
  f_eq(a, b)
  f_neq(a, b)

  f_gt(c, d)
  f_gte(c, d)
  f_lt(c, d)
  f_lte(c, d)
  f_eq(c, d)
  f_neq(c, d)
})
