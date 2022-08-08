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

unitizer_sect("Parenthesis Remova", {
  fp0 <- r2cq(((1 + 2) * x))
  args(fp0)
  fp0(10)
  fp1 <- r2cq((1 + (2 * x)))
  args(fp1)
  fp1(10)
})
unitizer_sect("Symbol Resolution", {
  xghf <- 42
  local(r2c_sum(xghf))
  local(r2c_sum(xghf), list2env(list(xghf=51)))
})
unitizer_sect("Bad Args", {
  r2c_sum('hello')
})
unitizer_sect("Square Transform", {
  grep("square(", get_c_code(r2c_slope), fixed=TRUE, value=TRUE)
})
unitizer_sect("Dots in Def", {
  fsum_add0 <- r2cq(sum(..., x, na.rm=na.rm) - y, check=TRUE)
  fsum_add0(1:2, 3:4, y=10, x=3, na.rm=TRUE)
  fsum_add0(1:2, 3:4, 10, 3, TRUE)  # error

  fsum_add1 <- r2cq(y - sum(x, na.rm=na.rm, ...), check=TRUE)
  fsum_add1(1:2, 3:4, y=10, x=3, na.rm=TRUE)
  fsum_add1(1:2, 3:4, 1, 3, na.rm=TRUE)  # okay (na.rm must be specified)

  # multiple sets of dots
  fsum_add2 <- r2cq(sum(..., x, na.rm=na.rm) - sum(...) * 2, check=TRUE)
  fsum_add2(1:2, x=10, na.rm=TRUE)
})
unitizer_sect("Forwarded Dots", {
  x <- 1:10
  y <- c(20, 100)

  f1 <- function(...) {
    x <- 1
    y <- 2
    r2c_add(...)
  }
  f1(x, y)
  f2 <- function(...) {
    x <- 1
    y <- 2
    group_exec(r2c_add, rep(1:2, each=5), list(...))
  }
  f2(x, rep(y, 5))

  r2c_sumdots <- r2cq(sum(...))
  f2a <- function(...) {
    x <- 1
    y <- 2
    group_exec(r2c_sumdots, rep(1:2, each=5), list(...))
  }
  f2a(x, rep(y, 5))

  # Compile with dots in the code
  f3 <- (function(...) r2cq(sum(...)))(1, 2)
  f3()

  f4 <- (function(...) r2cq(sum(...)))(x, y)
  f4(x, y)
  (function(...) f4(...))(x, y)
  f4(x)

  # Make sure r2cq doesn't get confused about dots
  f5 <- (function(...) r2cq(sum(...)))(1:10, 2)
  f5()
  f5(1:10)
  f5(1:10, 2)
})
unitizer_sect("Self Check", {
  sum_check <- r2cq(sum(x), check=TRUE)
  sum(x)
})
