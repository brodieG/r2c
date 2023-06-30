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


unitizer_sect('subassign', {
  # External x must be copied before
  call8a <- quote({x[a] <- y; x})
  r2c:::pp_clean(call8a)

  # Make sure sub-assign in correct spot
  call8a1 <- quote({
    if(a) {
      s <- mean(y)
      x[a] <- s
      x
    } else {
      y <- x + 1
    }
  })
  r2c:::pp_clean(call8a1)

  # `x` is internal, but assigning from self requires copy
  call8b <- quote({
    x <- x + 1
    x[a] <- x
    x
  })
  r2c:::pp_clean(call8b)

  # Can't use sub-assign result
  call8c0 <- quote(x[a] <- y)
  r2c:::pp_clean(call8c0)

  call8c1 <- quote(if(b) x[a] <- y)
  r2c:::pp_clean(call8c1)

  call8c2 <- quote({
    y <- (x[a] <- y)
    y
  })
  r2c:::pp_clean(call8c2)

})

