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

rbslope <- lcurry(rollby_exec, fun=r2c_slope, start=0, offset=0)
unitizer_sect("Big Tests", {
  # Bigger tests more likely to triggers segfaults, etc.
  n <- 1e5
  w <- 20
  set.seed(1)
  x <- runif(n) * runif(n)
  y <- runif(n) * runif(n)

  ri.slope <- rolli_exec(r2c_slope, list(x, y), n=w)
  round(range(ri.slope, na.rm=TRUE), 4)
  pos <- cumsum(x)
  # Check entry distribution
  rby.pos <- rollby_exec(r2c_len, x, x=pos, by=1, width=2)
  table(rby.pos)
  # Should be ~8 entries per window
  rby.slope <- rbslope(list(x, y), x=pos, by=1, width=2)
  round(range(rby.slope, na.rm=TRUE), 4)
  length(rby.slope)
  # spot check
  sixth.window <- which(pos >= 5 & pos < 7)
  identical(slope(x[sixth.window], y[sixth.window]), rby.slope[6])

  # More complex expression
  r2c_int <- r2cq(
    (
      sum(y) -
      sum(x) * (sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2))
    ) / length(x),
    check=TRUE
  )
  r2c.int <- r2c_int(x=x, y=y)
  if(!require("stats")) stop("Package stats required for tests")
  stats.coef <- unname(coefficients(lm(y ~ x, data.frame(x, y))))
  all.equal(c(r2c.int, r2c_slope(x, y)), stats.coef)
  ri.int <- rolli_exec(r2c_int, list(x, y), n=w)

})
