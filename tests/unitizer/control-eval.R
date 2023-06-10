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
  f1a(NaN, 1:3)

  # Branch return value unused, implicit else
  f1b <- r2cq({
      if(a) x <- y
      x - y
    },
    check=TRUE
  )
  f1b(TRUE, 1, 2)
  f1b(FALSE, 1, 2)

  # Branch return value unused, re-bound x
  f1c <- r2cq({
      x <- x
      if(a) x <- y
      x - y
    },
    check=TRUE
  )
  f1c(a=TRUE, 1, 2)
  f1c(a=FALSE, 1, 2)

  # Branch return value used
  f1d <- r2cq(if(a) x <- y, check=TRUE)
  # Error from length mismatch with implicit
  f1d(TRUE, 1)
  f1d(FALSE, 1)

  # Implicit branch return should work
  f1d(TRUE, numeric())
  f1d(FALSE, numeric())


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
      if (b) r2c::rec(x0 + y)
      else r2c::rec(y + y)
    }, check=TRUE
  )
  f3a(1:3, 5:7, 1)
  f3a(1:3, 5:7, 0)

  f4a.r <-function(x, a) {
    x2 <- if(a == 1) mean(x)
    else if (a == 2) sum(x)
    else a
    x2
  }
  f4a <- r2cf(f4a.r, check=TRUE)
  f4a(1:3, 1)
  f4a(1:3, 2)
  f4a(1:3, 3)

  f4b.r <-function(x, a) {
    x2 <- if(a == 1) x3 <- mean(x)
    else if (a == 2) x3 <- sum(x)
    else x3 <- a
    x2 + x3
  }
  f4b <- r2cf(f4b.r, check=TRUE)
  f4b(1:3, 1)
  f4b(1:3, 2)
  f4b(1:3, 3)

  f4c.r <-function(x, a) {
    x2 <- if(a == 1) {
      x3 <- mean(x)
      a
    } else if (a == 2) {
      x3 <- sum(x)
      a + a
    } else {
      x3 <- a
      sum(x)
    }
    x2 + x3
  }
  f4c <- r2cf(f4c.r, check=TRUE)
  f4c(1:3, 1)
  f4c(1:3, 2)
  f4c(1:3, 3)

  f4d.r <-function(x, a, b) {
    x1 <- mean(x * a)
    x2 <- if(a == 1) {
      x3 <- mean(x)
      a
    } else if (a == 2) {
      x3 <- sum(x)
      a + a
    } else {
      x3 <- a
      sum(x)
    }
    if(b) {
      x3 + x2
    } else {
      x3 * x1
    }
  }
  f4d <- r2cf(f4d.r, check=TRUE)
  f4d(1:3, 1, 1)
  f4d(1:3, 2, 1)
  f4d(1:3, 3, 1)
  f4d(1:3, 1, 0)
  f4d(1:3, 2, 0)
  f4d(1:3, 3, 0)

  f5a.r <-function(x, a) {
    x5 <- x * a
    if(a == 1) x5 <- x
    else x
    x5
  }
  f5a <- r2cf(f5a.r, check=TRUE)
  f5a(1:3, 2)

  f5b.r <-function(x, y, a, b) {
    x5 <- x * y
    x2 <- if(a == 1) x5 <- y
    else x
    x5
  }
  f5b <- r2cf(f5b.r, check=TRUE)
  f5b(1:3, 5:7, 2, 0)


  f5a(1:3, 5:7, 1, 1)
  f5a(1:3, 5:7, 2, 1)
  f5a(1:3, 5:7, 3, 1)
  f5a(1:3, 5:7, 1, 0)
  f5a(1:3, 5:7, 3, 0)

  f5b.r <-function(x, y, a, b) {
    x5 <- x * y
    if(a == 1) {
      x5 <- x + y
    } else {
      x3 <- sum(x) * sum(x)
      x
    }
    x5
  }
  f5b <- r2cf(f5b.r, check=TRUE)
  f5b(1:3, 5:7, 2, 0)

  f5c.r <-function(x, y, a, b) {
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
  }
  f5c <- r2cf(f5c.r, check=TRUE)
  f5c(1:3, 5:7, 1, 1)
  f5c(1:3, 5:7, 2, 1)
  f5c(1:3, 5:7, 3, 1)
  f5c(1:3, 5:7, 1, 0)
  f5c(1:3, 5:7, 2, 0)
  f5c(1:3, 5:7, 3, 0)


})
