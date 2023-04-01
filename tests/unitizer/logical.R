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
unitizer_sect("'Double' Logicals", {
  f_and2 <- r2cq(a && b, check=TRUE)
  f_or2 <- r2cq(a || b, check=TRUE)

  f_and2(TRUE, TRUE)
  f_and2(TRUE, FALSE)
  f_and2(FALSE, FALSE)
  f_and2(TRUE, NA)
  f_and2(NA, TRUE)
  f_and2(FALSE, NA)
  f_and2(NA, FALSE)
  f_and2(NA, NA)
  f_and2(logical(), TRUE)
  f_and2(TRUE, logical())
  f_and2(logical(), FALSE)
  f_and2(FALSE, logical())

  f_or2(TRUE, TRUE)
  f_or2(TRUE, FALSE)
  f_or2(FALSE, FALSE)
  f_or2(TRUE, NA)
  f_or2(NA, TRUE)
  f_or2(FALSE, NA)
  f_or2(NA, FALSE)
  f_or2(NA, NA)
  f_or2(logical(), TRUE)
  f_or2(TRUE, logical())
  f_or2(logical(), FALSE)
  f_or2(FALSE, logical())

  # To avoid errors from the R code in the check proper
  f_and22 <- r2cq(a && b, check=FALSE)
  f_or22 <- r2cq(a || b, check=FALSE)

  f_and22(TRUE, c(TRUE, FALSE))
  f_and22(c(TRUE, FALSE), TRUE)
  f_and22(logical(), c(TRUE, FALSE))
  f_and22(c(TRUE, FALSE), logical())
  f_or22(TRUE, c(TRUE, FALSE))
  f_or22(c(TRUE, FALSE), TRUE)
  f_or22(logical(), c(TRUE, FALSE))
  f_or22(c(TRUE, FALSE), logical())
})
unitizer_sect("ifelse", {
  f_ifelse <- r2cq(ifelse(test, yes, no), check=TRUE)
  f_ifelse(c(TRUE, FALSE, NA), 1:3, 1:3 * 10L)
  f_ifelse(c(TRUE, FALSE, NA), 1:4, 1:4 * 10L)
  f_ifelse(c(TRUE, FALSE, NA), 1:2, 1:2 * 10L)
  f_ifelse(c(TRUE, FALSE, NA), 1:3, 1:3 * 10)
  f_ifelse(logical(), 1:3, 1:3 * 10)
  f_ifelse(c(TRUE, FALSE), numeric(), numeric())
  f_ifelse(TRUE, numeric(), 1)
  f_ifelse(c(TRUE, FALSE), 1, 3)
  f_ifelse(c(TRUE, FALSE), 1, numeric())

  # r2c ifelse doesn't match the types exactly
  f_ifelse(TRUE, TRUE, 1)
  f_ifelse(TRUE, TRUE, 1L)
  f_ifelse(FALSE, TRUE, 1)
  f_ifelse(FALSE, TRUE, 1L)
  f_ifelse(1, TRUE, 1)
  f_ifelse(1, TRUE, 1L)
})
