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

unitizer_sect("re-use", {
  # we made a mistake with the slope call, but it caught a bug
  notslope <- quote(((x - mean(x)) * (y - mean(y))) / (x - mean(x))^2)
  (notslope.r <- reuse_calls(notslope))
  identical(eval(notslope), eval(notslope.r))

  (slope.r <- reuse_calls(body(slope)))
  identical(eval(body(slope)), eval(slope.r))

  (intercept.r <- reuse_calls(body(intercept)))
  identical(eval(body(intercept)), eval(intercept.r))

  # Ok to hoist `mean(x)`
  a <- 1
  ru0 <- quote({
    a
    {
      y <- (x - mean(x))
      z <- mean(x) + y
    }
  })
  (ru0.r <- reuse_calls(ru0))
  identical(eval(ru0), eval(ru0.r))

  # Not ok to hoist `mean(x)` first time, ok second time
  ru1 <- quote({
    a
    y <- ((x <- a) - mean(x) * mean(x))
    z <- mean(x) + mean(x)
  })
  (ru1.r <- reuse_calls(ru1))
  identical(eval(ru1), eval(ru1.r))

  # w * mean(x) can only be hoisted after first expr
  ru2 <- quote({
    a
    y <- (w <- a) - (w * mean(x))
    z <- mean(x) + (w * mean(x))
    z * (w * mean(x))
  })
  (ru2.r <- reuse_calls(ru2))
  identical(eval(ru2), eval(ru2.r))

  # multi braces (note the x's should be different across if branches!)
  ru3 <- quote({
    a
    if (TRUE) {
      y <- (x <- a) - mean(x)
      mean(x)
    } else {
      mean(x) * mean(x)
    }
  })
  reuse_calls(ru3)

  # multi braces, but now we should be able to sub
  b <- 2
  ru4 <- quote({
    a
    {
      y <- (x <- a) - mean(x)
      mean(x)
    }
    b
    {
      mean(x) * mean(x)
    }
  })
  (ru4.r <- reuse_calls(ru4))
  identical(eval(ru4), eval(ru4.r))
})
unitizer_sect("Complex Hoisting", {
  ru5a <- quote({
    if(a == 2) {
      sum(x)
    } else sum(x) * sum(x)
  })
  reuse_calls(ru5a)

  ru5a1 <- quote({
    if(a == 2) {
      y <- sum(x)
    } else sum(x) * sum(x)
  })
  reuse_calls(ru5a1)

  ru5a2 <- quote({
    if(a == 2) y <- sum(x)
    else if(a == 3) sum(x) * sum(x)
    else sum(x) * 2
  })
  reuse_calls(ru5a2)

  # re-use broken by intervening re-assignment; in theory the last three
  # `sum(x)` could be hoisted and re-used, but the failure is okay.
  ru5a3 <- quote({
    if(a == 2) {
      x <- y
      sum(x)
    }
    else if(a == 3) sum(x) * sum(x)
    else sum(x) * 2
  })
  reuse_calls(ru5a3)

  # In this case we're able to reuse the first few, and correctly not the last
  # ones.
  ru5a4 <- quote({
    if(a == 2) sum(x)
    else if(a == 3) sum(x) * sum(x)
    else {
      x <- y
      sum(x)
    }
    sum(x)
  })
  reuse_calls(ru5a4)

})

