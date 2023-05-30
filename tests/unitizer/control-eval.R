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

unitizer_sect("basic", {

  f1a <- r2cq(if(a) mean(x) else sum(x))
  f1a(TRUE, 1:3)
  f1a(FALSE, 1:3)
  f1a(NA, 1:3)

  f2a <- r2cq({
      z <-
        if(a) {y <- mean(x); sum(z)}
        else y <- sum(x)
      y + z
    },
    check=TRUE
  )
  f2a(a=TRUE, x=1:3, z=1:3 * 10)
  # Not identical b/c one branch returns numeric (`* 10`)
  f2a(a=FALSE, x=1:3, z=1:3 * 10)

  f3a <- r2cq({
    x0 <- x + y
    x1 <- x * y
    x4 <- sum(y)
    x5 <- x1

    x2 <- if(a == 1) {
      x3 <- mean(x)
      x5 <- x0 + x1
    } else if (a == 2) {
      x3 <- sum(x)
      x1
    } else {
      x3 <- x4 <- sum(x) * sum(x)
      x0
    }
    if(b) {
      x3 + x2
    } else {
      x3 + x4 + x5
    }
  })
  f3a(1:3, 5:7, 1, 1)

  f4a <- r2cq({
    x0 <- x + y
    x1 <- x * y
    x4 <- sum(y)
    x5 <- x1

    x2 <- if(a == 1) {
      x3 <- mean(x)
      x5 <- x0 + x1
    } else if (a == 2) {
      x3 <- sum(x)
      x1
    } else {
      x3 <- x4 <- sum(x) * sum(x)
      x0
    }
  })
  f4a(1:3, 5:7, 1)

})
