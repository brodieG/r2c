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

## We'll add up whole powers of two so that we can tell by the bit-components of
## things that all the correct window elements were added.

we_p_sum <- function(x, width, align='left', by=1L)
  window_exec(r2c_sum, x, width=width, partial=TRUE, align=align, by=by)
we_sum <- function(x, width, align='left', by=1L)
  window_exec(r2c_sum, x, width=width, partial=FALSE, align=align, by=by)
wvec <- 2^(0:4)

unitizer_sect("window partial", {
  show_bits(we_p_sum(wvec, width=1, align='left'))
  show_bits(we_p_sum(wvec, width=2, align='left'))
  show_bits(we_p_sum(wvec, width=3, align='left'))
  show_bits(we_p_sum(wvec, width=4, align='left'))
  show_bits(we_p_sum(wvec, width=5, align='left'))
  show_bits(we_p_sum(wvec, width=6, align='left'))
  show_bits(we_p_sum(wvec, width=.Machine[['integer.max']], align='left'))

  show_bits(we_p_sum(wvec, width=1, align='center'))
  show_bits(we_p_sum(wvec, width=2, align='center'))
  show_bits(we_p_sum(wvec, width=3, align='center'))
  show_bits(we_p_sum(wvec, width=4, align='center'))
  show_bits(we_p_sum(wvec, width=5, align='center'))
  show_bits(we_p_sum(wvec, width=6, align='center'))
  show_bits(we_p_sum(wvec, width=8, align='center'))
  show_bits(we_p_sum(wvec, width=9, align='center'))
  show_bits(we_p_sum(wvec, width=.Machine[['integer.max']], align='center'))

  show_bits(we_p_sum(wvec, width=1, align='right'))
  show_bits(we_p_sum(wvec, width=2, align='right'))
  show_bits(we_p_sum(wvec, width=3, align='right'))
  show_bits(we_p_sum(wvec, width=4, align='right'))
  show_bits(we_p_sum(wvec, width=5, align='right'))
  show_bits(we_p_sum(wvec, width=6, align='right'))
  show_bits(we_p_sum(wvec, width=.Machine[['integer.max']], align='right'))

  show_bits(we_p_sum(wvec, width=5, align=0))
  show_bits(we_p_sum(wvec, width=5, align=4))
  show_bits(we_p_sum(wvec, width=5, align=2))

  show_bits(we_p_sum(wvec, width=5, align=-1))
  show_bits(we_p_sum(wvec, width=5, align=5))

})
unitizer_sect("by", {
  show_bits(we_p_sum(wvec, width=1, by=2))
  show_bits(we_p_sum(wvec, width=1, by=3))
  show_bits(we_p_sum(wvec, width=1, by=4))
  show_bits(we_p_sum(wvec, width=1, by=5))

  show_bits(we_p_sum(wvec, width=3, by=2))
  show_bits(we_p_sum(wvec, width=3, by=3))
  show_bits(we_p_sum(wvec, width=3, by=4))
  show_bits(we_p_sum(wvec, width=3, by=5))

  show_bits(we_p_sum(wvec, width=3, by=2, align='center'))
  show_bits(we_p_sum(wvec, width=3, by=3, align='center'))
  show_bits(we_p_sum(wvec, width=3, by=4, align='center'))
  show_bits(we_p_sum(wvec, width=3, by=5, align='center'))

  show_bits(we_p_sum(wvec, width=3, by=2, align='right'))
  show_bits(we_p_sum(wvec, width=3, by=3, align='right'))
  show_bits(we_p_sum(wvec, width=3, by=4, align='right'))
  show_bits(we_p_sum(wvec, width=3, by=5, align='right'))
})

unitizer_sect("errors", {
  we_p_sum(wvec, width=0, align='left')
  we_p_sum(wvec, width=.Machine[['integer.max']] + 1, align='left')

  we_p_sum(wvec, width=1, align=1)
  we_p_sum(wvec, width=1, by=-1)

  we_p_sum(numeric(), width=1)
})

unitizer_sect("complete", {
  show_bits(we_sum(wvec, width=3, align='center'))
})

