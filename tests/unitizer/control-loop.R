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
  # Not an error b/c loops always return 0 size
  f1b(1:2)

  f1c <- r2cq({
    y <- 99
    for(i in seq_along(x)) y <- i
    i
  })
  # i value should persist past loop
  f1c(1:2)
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
unitizer_sect('Use before set', {
  call3a0 <- quote({
    res <- 0
    for(i in x) {
      tmp <- res + i
      res <- tmp
    }
    res
  })
  f3a0 <- r2cl(call3a0, check=TRUE)
  f3a0(1:4)
  f3a0(numeric())

  f3a1 <- r2cq(
    {
      res <- 0
      for(i in x) {
        tmp <- res + i
        res <- tmp
      }
      tmp
    },
    check=TRUE
  )
  # Need to supply tmp for the balancing branch
  f3a1(c(2, 3, 5), pi)
  f3a1(numeric(), pi)

  # use before set in same line
  f3b <- r2cq({
      res <- 0
      for(xi in x) res <- res + xi
      res
    },
    check=TRUE
  )
  f3b(c(2, 3, 5))

  # Copy and use use before set (b should remain constant in loop despite being
  # an alias, i should e unaffected).
  call3c <- quote({
    a <- mean(x)
    b <- a
    for(i in seq_along(x)) {
      c <- a + b
      a <- i <- c + 1
    }
    a
  })
  f3c <- r2cl(call3c, check=TRUE)
  f3c(1:3)
  f3c(numeric())
})

unitizer_sect('multiple use before set', {
  f4 <- r2cf(
    function(x, b, d) {
      for(i in x) {
        a <- b + 1
        c <- d
        d <- i
        b <- a + d + c
      }
      b
    },
    check=TRUE
  )
  f4(1:2, 0, 5)

  # Both b and a are used b4 set, and set to the same value.
  f4a <- r2cf(
    function(x, a, b, c) {
      for(i in x) {
        c <- a
        a <- b <- a * b + c
      }
      a
    },
    check=TRUE
  )
  f4a(c(2,3,5), 2, 5, 10)
})

unitizer_sect('nested loop/branch', {
  f5a.r <- function (a, x, c, b) {
    for(i in x) {
      b <- a + 1
      if(i > 1) {
        a <- b
      } else {
        a <- c
      }
    }
    b + a
  }
  f5a <- r2cf(f5a.r, check=TRUE)
  f5a(a=11, x=pi, c=-5, b=898)

  # Super naive dot product
  f5b.r <- function(A, B) {
    res <- numeric_along(A)
    for(ai in seq_along(A)) {
      for(bi in seq_along(B)) {
        if(ai == bi) {
          res[ai] <- res[ai] + A[ai] + B[bi]
        }
      }
    }
    res
  }
  f5b <- r2cf(f5b.r, check=TRUE)
  f5b(1:3, 4:6)

  # Super naive matrix multiplication (see also big.R)
  f5c.r <- function(A, B, rs.A, cs.A, rs.B, cs.B) {
    res <- numeric_alongn(rs.A, cs.B)
    for(r.A in rs.A) {
      for(c.B in cs.B) {
        for(c.A in cs.A) {
          for(r.B in rs.B) {
            if(c.A == r.B) {
              res[r.A + (c.B - 1) * length(rs.A)] <-
                res[r.A + (c.B - 1) * length(rs.A)] +
                A[r.A + (c.A - 1) * length(rs.A)] *
                B[r.B + (c.B - 1) * length(rs.B)]
            }
          }
        }
      }
    }
    res
  }
  f5c <- r2cf(f5c.r, check=TRUE)
  A <- matrix(1:6, 3)
  B <- matrix(4:1, 2)
  rs.A <- seq_len(nrow(A))
  cs.A <- seq_len(ncol(A))
  rs.B <- seq_len(nrow(B))
  cs.B <- seq_len(ncol(B))
  (res.5c <- f5c(c(A), c(B), rs.A, cs.A, rs.B, cs.B))
  identical(c(res.5c), c(A%*%B))

  # Naive matrix mult followed by rowsum/rowprod (see also big.R)
  f5d.r <- function(A, B, rs.A, cs.A, rs.B, cs.B, sum=TRUE) {
    tmp <- numeric_alongn(rs.A, cs.B)
    for(r.A in rs.A) {
      for(c.B in cs.B) {
        for(c.A in cs.A) {
          for(r.B in rs.B) {
            if(c.A == r.B) {
              tmp[r.A + (c.B - 1) * length(rs.A)] <-
                tmp[r.A + (c.B - 1) * length(rs.A)] +
                A[r.A + (c.A - 1) * length(rs.A)] *
                B[r.B + (c.B - 1) * length(rs.B)]
            }
          }
        }
      }
    }
    res <- numeric_along(rs.A)
    # force if/else to outside
    if(sum) {
      for(r.A in rs.A) {
        for(c.B in cs.B) {
          res[r.A] <- res[r.A] + tmp[r.A + (c.B - 1) * length(rs.A)]
        }
      }
    } else {
      res <- res + 1
      for(r.A in rs.A) {
        for(c.B in cs.B) {
          res[r.A] <- res[r.A] * tmp[r.A + (c.B - 1) * length(rs.A)]
        }
      }
    }
    res
  }
  f5d <- r2cf(f5d.r)
  (res.5d1 <- f5d(c(A), c(B), rs.A, cs.A, rs.B, cs.B))
  (res.5d2 <- f5d(c(A), c(B), rs.A, cs.A, rs.B, cs.B, FALSE))
  identical(res.5d1, rowSums(A%*%B))
  identical(res.5d2, apply(A%*%B, 1, prod))

  # Iteration variable inside if/else
  call5e <- quote({
    if(a) for(i in x) 1
    else i <- 43
    i
  })
  f5e <- r2cl(call5e)
  f5e(TRUE, c(1, 42))
  f5e(FALSE, c(1, 42))
})
