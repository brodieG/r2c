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

library(r2c)

slope <- function(x, y)
  sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2)
slope2 <- function(x, y) {
  mux <- mean(x)
  x_mux <- x - mux
  sum(x_mux * (y - mean(y))) / sum(x_mux^2)
}
intercept <- function(x, y)
  (
    mean(y) -
    mean(x) * (sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2))
  )

r2c_slope <- r2cf(slope, check=TRUE)
r2c_slope2 <- r2cf(slope2)
r2c_int <- r2cf(intercept, check=TRUE)
r2c_len <- r2cq(length(x))

rbslope <- lcurry(rollby_exec, fun=r2c_slope, start=0, offset=0)

# Bigger tests more likely to triggers segfaults, etc.  These tests are here so
# they don't end up stored in the unitizers as they end up being multi-MB.
n <- 1e5
w <- 20
set.seed(1)
x.big <- runif(n) * runif(n)
y.big <- runif(n) * runif(n)
pos <- cumsum(x.big)

ri.slope <- rolli_exec(r2c_slope, list(x.big, y.big), n=w)
round(range(ri.slope, na.rm=TRUE), 4)
# Check entry distribution
rby.pos <- rollby_exec(r2c_len, x.big, position=pos, by=1, width=2)
table(rby.pos)
# Should be ~8 entries per window
rby.slope <- rbslope(list(x.big, y.big), position=pos, by=1, width=2)
round(range(rby.slope, na.rm=TRUE), 4)
length(rby.slope)
# spot check
sixth.window <- which(pos >= 5 & pos < 7)
identical(slope(x.big[sixth.window], y.big[sixth.window]), rby.slope[6])

# More complex expression
r2c.int <- r2c_int(x=x.big, y=y.big)
if(!require("stats")) stop("Package stats required for tests")
stats.coef <- unname(coefficients(lm(y ~ x, data.frame(x=x.big, y=y.big))))
all.equal(c(r2c.int, r2c_slope(x.big, y.big)), stats.coef)
ri.int <- rolli_exec(r2c_int, list(x.big, y.big), n=w)

# With intermediate assignments
identical(ri.slope, rolli_exec(r2c_slope2, list(x.big, y.big), n=w))

# Naive matrix multiplication testing deeply nested loops and branches
mat_mult <- function(A, B, rs.A, cs.A, rs.B, cs.B) {
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
r2c_mat_mult <- r2cf(mat_mult)

# Naive matrix mult followed by rowsum/rowprod
mat_mult_rowsum <- function(A, B, rs.A, cs.A, rs.B, cs.B, sum=TRUE) {
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
r2c_mat_mult_rowsum <- r2cf(mat_mult_rowsum)

mx <- matrix(runif(1e4), 1000)
xr <- seq_len(nrow(mx))
xc <- seq_len(ncol(mx))
my <- matrix(runif(1e4), 10)
yr <- seq_len(nrow(my))
yc <- seq_len(ncol(my))

all.equal(
  r2c_mat_mult(c(mx), c(my), xr, xc, yr, yc),
  c(mx %*% my)
)
all.equal(
  r2c_mat_mult_rowsum(c(mx), c(my), xr, xc, yr, yc),
  rowSums(mx %*% my)
)

