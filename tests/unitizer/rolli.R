## Copyright (C) 2022 Brodie Gaslam
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
rsum_pl <- Curry(r2c::rolli_exec, r2c_sum, align='left', partial=TRUE)
rsum_pc <- Curry(r2c::rolli_exec, r2c_sum, align='center', partial=TRUE)
rsum_pr <- Curry(r2c::rolli_exec, r2c_sum, align='right', partial=TRUE)
rsum_p <- Curry(r2c::rolli_exec, r2c_sum, partial=TRUE)

## see _pre/data.R for show_bits and other things

unitizer_sect("window partial", {
  show_bits(rsum_pl(wvec, n=1))
  show_bits(rsum_pl(wvec, n=2))
  show_bits(rsum_pl(wvec, n=3))
  show_bits(rsum_pl(wvec, n=4))
  show_bits(rsum_pl(wvec, n=5))
  show_bits(rsum_pl(wvec, n=6))
  show_bits(rsum_pl(wvec, n=.Machine[['integer.max']]))

  show_bits(rsum_pc(wvec, n=1))
  show_bits(rsum_pc(wvec, n=2))
  show_bits(rsum_pc(wvec, n=3))
  show_bits(rsum_pc(wvec, n=4))
  show_bits(rsum_pc(wvec, n=5))
  show_bits(rsum_pc(wvec, n=6))
  show_bits(rsum_pc(wvec, n=8))
  show_bits(rsum_pc(wvec, n=9))
  show_bits(rsum_pc(wvec, n=.Machine[['integer.max']]))

  show_bits(rsum_pr(wvec, n=1))
  show_bits(rsum_pr(wvec, n=2))
  show_bits(rsum_pr(wvec, n=3))
  show_bits(rsum_pr(wvec, n=4))
  show_bits(rsum_pr(wvec, n=5))
  show_bits(rsum_pr(wvec, n=6))
  show_bits(rsum_pr(wvec, n=.Machine[['integer.max']]))

  # Numeric align
  show_bits(rsum_p(wvec, n=5, align=0))
  show_bits(rsum_p(wvec, n=5, align=4))
  show_bits(rsum_p(wvec, n=5, align=2))

  # Numeric align OOB (valid)
  show_bits(rsum_p(wvec, n=5, align=-1))
  show_bits(rsum_p(wvec, n=5, align=5))
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

unitizer_sect("errors", {
  show_bits(rsum_pl(wvec, n=0))
  show_bits(rsum_pl(wvec, n=.Machine[['integer.max']] + 1))
})

