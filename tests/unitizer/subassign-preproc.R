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

  # There should be reconcile and copy for both self copies, and both results
  # need reconcile, and TRUE branch result also must copy again (we can't
  # reconcile the same memory twice).
  call8a2 <- quote({
    z <- if(a) {
      s <- mean(y)
      x[a] <- s
      x
    } else {
      y <- x + 1
    }
    x + z
  })
  r2c:::pp_clean(call8a2)

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

  call8c2 <- quote({
    y <- (x[a] <- y)
    y
  })
  r2c:::pp_clean(call8c2)
})
unitizer_sect('in branch', {
  # Can't use sub-assign result
  call9a <- quote(if(b) x[a] <- y)
  r2c:::pp_clean(call9a)

  call9a1 <- quote({
    if(b) x[a] <- y
    x
  })
  r2c:::pp_clean(call9a1)

  # Should trigger a reconcile in the alternate branch
  call9b <- quote({
    if(b) {x[a] <- y; x}
    x
  })
  r2c:::pp_clean(call9b)
  # In this case reconciliation is not required.
  call9b2 <- quote({
    x <- x + 1
    if(b) {x[a] <- y; x}
    x
  })
  r2c:::pp_clean(call9b2)

  # Copies in first branch should trigger, but if we were to remove dead code
  # then they wouldn't since `x` does not affect final return value.
  call9c <- quote({
    if(b) x <- y + 1 else x <- y
    if(c) x[a] <- z else x[a] <- y
    y
  })
  r2c:::pp_clean(call9c)
})


