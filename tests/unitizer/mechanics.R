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

unitizer_sect("Dots, etc.", {
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

  # Compile with dots in the code
  f3 <- (function(...) r2cq(sum(...)))(1, 2)
  f3()

  f4 <- (function(...) r2cq(sum(...)))(x, y)
  f4(x, y)                       # error
  (function(...) f4(...))(x, y)  # okay

  # This generates a single argument?
  f5 <- (function(...) r2cq(sum(...)))(1:10, 2)
  f5()
  (function(...) f5(...))(1:10)  # okay

})
