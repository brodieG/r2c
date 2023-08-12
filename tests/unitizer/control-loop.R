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

unitizer_sect("Basic Loop", {
  f1a <- r2cq({
    y <- 99
    for(i in seq_along(x)) y <- i
    y
  })
  f1a(1:2)
  f1a(numeric())

  f1b <- r2cq({
    y <- 99
    for(i in seq_along(x)) y <- i
  })
  # Error mismatch return value size
  f1b(1:2)

})
unitizer_sect('convolve', {
  # based on WRE 5.2 convolution example, originally intended for benchmarking
  # purposes but revealed several bugs in for loops along the way.
  f2a.r <- function(a, b, c) {
    ab <- numeric(c)
    for(i in seq_along(a)) {
      for(j in seq_along(b)) {
        ab[i + j - 1] <- ab[i + j - 1] + a[i] * b[j]
      }
    }
    ab
  }
  f2a <- r2cf(f2a.r)
  a <- (1:3) / 2
  b <- c(.75, 2, 1/3)
  (res0 <- f2a(a, b, length(a) + length(b) - 1))
  identical(res0, r2c:::convolve(a, b))
  identical(res0,  f2a(a, b, length(a) + length(b) - 1))
})
