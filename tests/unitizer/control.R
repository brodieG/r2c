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

x1 <- 1
x2 <- c(1, 2)

r2cq(if(a) x1 else x2)
r2cq(if(a) x1)

# Presumably `check` runs on the pre-processed expression and thus not subject
# to the if/else substitution?
r2cq(if(a) x1 else x2, check=TRUE)


# No else
y <- 1
r2cq({
  x <- y
  if(mean(y) > .5) x <- mean(y) / 2
})

# No else, length mismatch
y <- 1:2
r2cq({
  x <- y
  if(mean(y) > .5) x <- mean(y) / 2
})
# No else, length mismatch but unused so irrelevant

test <- TRUE
y <- 1
r2cq({
  x <- if(test) y
  y
})

# With else, okay to mismatch first value
r2cq({
  x <- z
  x <- if(test) y else y + 1
  x
})(z = 1:2, y=1, test=TRUE)

# With else, no length mistmatch

x <- y
r2cq(if(mean(y) > .5) x <- mean(y) / 2)

# if / else if
