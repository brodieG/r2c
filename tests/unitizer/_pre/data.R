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

library(r2c)

set.seed(1)
n <- 100
x <- runif(n) * runif(n) - .5
y <- runif(n) * runif(n) - .5
z <- runif(n) * runif(n) - .5

# ensure we have 1 and 2 size groups, note we use the 2 size group to cause the
# double overflow later
g <- sample(10, n, replace=TRUE)
g[1:3] <- c(11L, 12L, 12L)

x.na <- x.inf <- x.ovrf.dbl <- x.ovrf.ldbl <- x
x.na[sample(n, 3)] <- NA_real_

g.na <- g
g.na[sample(n, 3)] <- NA_integer_
x.inf[sample(n, 4)] <- c(Inf, -Inf, Inf, -Inf)

x.weird <- x
x.weird[sample(n, 3)] <- c(
  .Machine$double.xmax, .Machine$double.xmin,
  2^(.Machine$double.min.exp - 1) # denormal
)

x.ovrf.dbl[g == 12L] <- .Machine$double.xmax
# this one is for when we don't do by group, will overflow 80
# bit long doubles (2^4 would do it, going to 2^5 to be safe for negatives)
ldbl.na <- sample(n, 2^5)
x.ovrf.ldbl[ldbl.na] <- .Machine$double.xmax
x.ovrf.dbl.na <- x.ovrf.dbl
x.ovrf.dbl.na[which(g == 12L)[1L]] <- NA_real_
x.ovrf.ldbl.na <- x.ovrf.ldbl
x.ovrf.ldbl.na[ldbl.na[1L]] <- NA_real_

# Functions used in multiple places

r2c_slope <- r2cq(
  sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2),
  check=TRUE
)
r2c_sum <- r2cq(sum(x))
r2c_add <- r2cq(x + y)

