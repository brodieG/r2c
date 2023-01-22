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

# Partial, and various alignments
rsum_pl <- lcurry(r2c::rolli_exec, r2c_sum, align='left', partial=TRUE)
rsum_pc <- lcurry(r2c::rolli_exec, r2c_sum, align='center', partial=TRUE)
rsum_pr <- lcurry(r2c::rolli_exec, r2c_sum, align='right', partial=TRUE)
rsum_p <- lcurry(r2c::rolli_exec, r2c_sum, partial=TRUE)

## see _pre/data.R for show_bits and other things

unitizer_sect("window partial", {
  # Left aligned
  show_bits(rsum_pl(wvec, n=1))
  show_bits(rsum_pl(wvec, n=2))
  show_bits(rsum_pl(wvec, n=3))
  show_bits(rsum_pl(wvec, n=4))
  show_bits(rsum_pl(wvec, n=5))
  show_bits(rsum_pl(wvec, n=6))
  show_bits(rsum_pl(wvec, n=.Machine[['integer.max']]))
  show_bits(rsum_pl(wvec, n=0))
  # Error
  show_bits(rsum_pl(wvec, n=.Machine[['integer.max']] + 1))

  # Centered
  show_bits(rsum_pc(wvec, n=1))
  show_bits(rsum_pc(wvec, n=2))
  show_bits(rsum_pc(wvec, n=3))
  show_bits(rsum_pc(wvec, n=4))
  show_bits(rsum_pc(wvec, n=5))
  show_bits(rsum_pc(wvec, n=6))
  show_bits(rsum_pc(wvec, n=8))
  show_bits(rsum_pc(wvec, n=9))
  show_bits(rsum_pc(wvec, n=.Machine[['integer.max']]))

  # Right aligned
  show_bits(rsum_pr(wvec, n=1))
  show_bits(rsum_pr(wvec, n=2))
  show_bits(rsum_pr(wvec, n=3))
  show_bits(rsum_pr(wvec, n=4))
  show_bits(rsum_pr(wvec, n=5))
  show_bits(rsum_pr(wvec, n=6))
  show_bits(rsum_pr(wvec, n=.Machine[['integer.max']]))

  # Numeric align
  show_bits(rsum_p(wvec, n=5, align=0))
  show_bits(rsum_p(wvec, n=5, align=-4))
  show_bits(rsum_p(wvec, n=5, align=-2))

  # Numeric align OOB (valid)
  show_bits(rsum_p(wvec, n=5, align=1))
  show_bits(rsum_p(wvec, n=5, align=-5))
})
unitizer_sect("by", {
  show_bits(rsum_pl(wvec, n=1, by=2))
  show_bits(rsum_pl(wvec, n=1, by=3))
  show_bits(rsum_pl(wvec, n=1, by=4))
  show_bits(rsum_pl(wvec, n=1, by=5))

  show_bits(rsum_pl(wvec, n=3, by=2))
  show_bits(rsum_pl(wvec, n=3, by=3))
  show_bits(rsum_pl(wvec, n=3, by=4))
  show_bits(rsum_pl(wvec, n=3, by=5))

  show_bits(rsum_pc(wvec, n=3, by=2))
  show_bits(rsum_pc(wvec, n=3, by=3))
  show_bits(rsum_pc(wvec, n=3, by=4))
  show_bits(rsum_pc(wvec, n=3, by=5))

  show_bits(rsum_pr(wvec, n=3, by=2))
  show_bits(rsum_pr(wvec, n=3, by=3))
  show_bits(rsum_pr(wvec, n=3, by=4))
  show_bits(rsum_pr(wvec, n=3, by=5))
})
unitizer_sect("complete", {
  rolli_exec(r2c_sum, wvec, n=3)
  show_bits(rolli_exec(r2c_sum, wvec, n=3))
})
unitizer_sect("Variable Width", {
  show_bits(rsum_pl(wvec, n=c(3, 1, 3, 1, 3)))
  show_bits(rsum_pl(wvec, n=c(3, 1, 0, 1, 3)))
  # Errors
  show_bits(rsum_pl(wvec, n=c(3, 1, NA, 1, 3)))
  show_bits(rsum_pl(wvec, n=c(3, 1, 1, 3)))
})
unitizer_sect("Zero Len", {
  rolli_exec(r2c_sum, numeric(), 3)
  rolli_exec(r2c_sum, list(), 3, MoreArgs=list(1:3))
  rolli_exec(r2c_sum, list(numeric()), 3, MoreArgs=list(1:3))
})

