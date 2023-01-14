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

library(r2c)

## We'll add up whole powers of two so that we can tell by the bit-components of
## things that all the correct window elements were added.

wvec <- 2^(0:4)

unitizer_sect('Equivalence', {
  n <- 3
  identical(
    rolli_exec(r2c_sum, wvec, n=n, by=1, align="left", partial=TRUE),
    rollby_exec(r2c_sum, wvec, width=n - 1, by=1, bounds="[]")
  )
  identical(
    rolli_exec(r2c_sum, wvec, n=n, by=1, align="left", partial=TRUE),
    rollby_exec(r2c_sum, wvec, width=n - 1, by=1, bounds="[]")
  )
  swvec <- seq_along(wvec)
  identical(
    rolli_exec(r2c_sum, wvec, n=n, by=1, align="left", partial=TRUE),
    rollat_exec(r2c_sum, wvec, width=n - 1, at=swvec, bounds="[]")
  )
  identical(
    rolli_exec(r2c_sum, wvec, n=n, by=1, align="left", partial=TRUE),
    rollbw_exec(r2c_sum, wvec, left=swvec, right=swvec + n - 1, bounds="[]")
  )
})

rbs <- lcurry(rollby_exec, fun=r2c_sum)
rbs_W1B1 <- lcurry(rollby_exec, fun=r2c_sum, width=1, by=1)
ras <- lcurry(rollat_exec, fun=r2c_sum)
rbws <- lcurry(rollbw_exec, fun=r2c_sum)

unitizer_sect('bounds', {
  show_bits(rbs(wvec, width=1, by=1))
  show_bits(rbs(wvec, width=1, by=1, bounds="[]"))
  show_bits(rbs(wvec, width=.999, by=1, bounds="[]"))
  show_bits(rbs(wvec, width=1, by=1, bounds="(]"))
  show_bits(rbs(wvec, width=1, by=1, bounds="()"))
  show_bits(rbs(wvec, width=1, by=1.001, bounds="()"))
  show_bits(rbs(wvec, width=1, by=0.999, bounds="()"))

  # Zero width must have closed bounds on both sides
  show_bits(rbs(wvec, width=0, by=1))
  show_bits(rbs(wvec, width=0, by=1, bounds="[]"))

  # Errors
  rbs(wvec, width=1, by=0.999, bounds="hello")
})
unitizer_sect('offsets and start/end', {
  show_bits(rbs_W1B1(wvec))
  show_bits(rbs_W1B1(wvec, offset=0))
  show_bits(rbs_W1B1(wvec, offset=-1))
  show_bits(rbs_W1B1(wvec, offset=-1, bounds="(]"))
  show_bits(rbs_W1B1(wvec, offset=-2))
  show_bits(rbs_W1B1(wvec, offset=1))
  show_bits(rbs_W1B1(wvec, offset=-5))

  show_bits(rbs_W1B1(wvec, start=2))
  show_bits(rbs_W1B1(wvec, start=1.001))
  identical(
    rbs_W1B1(wvec, start=2, offset=0),
    rbs_W1B1(wvec, start=1.001, offset=0)
  )
  show_bits(rbs_W1B1(wvec, end=4))
  show_bits(rbs_W1B1(wvec, start=2, end=4))

  # Errors
  rbs_W1B1(wvec, end=2, start=4)
})
unitizer_sect('complete coverage', {
  # with default bounds we exactly cover the range
  identical(sum(rbs(wvec, width=2, by=2)), sum(wvec))
  identical(sum(rbs(wvec, width=1, by=1)), sum(wvec))
})
unitizer_sect('zero lengths', {
  rbs_W1B1(numeric(), position=numeric())
  rbs_W1B1(numeric(), position=numeric(), start=0, end=1)
  rbs_W1B1(numeric(), position=numeric(1), start=0, end=1)
  rbs_W1B1(numeric(1), position=numeric(), start=0, end=1)
  rbs_W1B1(list(), position=numeric(), start=0, end=1)
  rbs_W1B1(list(), position=numeric(1), start=0, end=1)
  rbs_W1B1(list(numeric(1)), position=numeric(), start=0, end=1)
  rbs_W1B1(list(numeric()), position=numeric(), start=0, end=1)

  ras(wvec, at=numeric(), width=1, offset=0)
  ras(numeric(), at=1, width=1, offset=0)

  rbws(wvec, left=numeric(), right=numeric())
  rbws(numeric(), left=1, right=2)
})
unitizer_sect('play with x', {
  # Double width equal to half position
  identical(
    rbs(wvec, width=2, by=2),
    rbs(wvec, width=1, by=1, position=seq_along(wvec)/2)
  )
  show_bits(rbs_W1B1(wvec, position=c(1, 1.25, 2, 3.25, 5)))
  show_bits(rbs_W1B1(wvec, position=rev(6 - c(1, 1.25, 2, 3.25, 5))))
})
unitizer_sect('rollat', {
  show_bits(ras(wvec, at=c(1, 5), width=4, offset=-2))
  show_bits(ras(wvec, at=c(1, 5), width=4, offset=-2, bounds="(]"))
  show_bits(ras(wvec, at=c(1, 3, 5), width=1))
  show_bits(ras(wvec, at=c(1, 3, 5) + .001, width=1))
})
unitizer_sect('rollbw', {
  show_bits(rbws(wvec, left=rep(1, 5), right=1:5))
  show_bits(rbws(wvec, left=rep(1, 5), right=1:5, bounds="[]"))
  show_bits(rbws(wvec, right=rep(5, 5), left=1:5))
})
