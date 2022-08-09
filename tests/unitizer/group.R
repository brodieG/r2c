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

unitizer_sect("group sum", {
  identical(group_exec(r2c_sum, g, x), c(tapply(x, g, sum)))
  identical(
    group_exec(r2c_sum, process_groups(sort(g), sorted=TRUE), x),
    c(tapply(x, sort(g), sum))
  )
  group_exec(r2c_sum, list(g), x)
  group_exec(r2c_sum, g, list(x))
  group_exec(r2c_sum, list(g), list(y=x))
  group_exec(r2c_sum, g, list(na.rm=x))

  identical(group_exec(r2c_sum, g, x.na), c(tapply(x.na, g, sum)))
  identical(group_exec(r2c_sum, g, x.weird), c(tapply(x.weird, g, sum)))
  identical(group_exec(r2c_sum, g, x.ovrf.dbl), c(tapply(x.ovrf.dbl, g, sum)))
  identical(
    group_exec(r2c_sum, g, x.ovrf.dbl.na),
    c(tapply(x.ovrf.dbl.na, g, sum))
  )
  identical(group_exec(r2c_sum, g, x.ovrf.ldbl), c(tapply(x.ovrf.ldbl, g, sum)))
  identical(
    group_exec(r2c_sum, g, x.ovrf.ldbl.na),
    c(tapply(x.ovrf.ldbl.na, g, sum))
  )

  # tapply drops the NA
  identical(group_exec(r2c_sum, g.na, x)[-13], c(tapply(x, g.na, sum)))
  group_exec(r2c_sum, g.na, x)
})
unitizer_sect("group_slope", {
  slope <- quote(sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2))
  identical(
    group_exec(r2c_slope, g, list(x, y)), bsac(slope, g, list(x=x, y=y))
  )
  identical(
    group_exec(r2c_slope, g, list(x.na, y)),
    bsac(slope, g, list(x=x.na, y=y))
  )
  identical(
    group_exec(r2c_slope, g, list(x.weird, y)),
    bsac(slope, g, list(x=x.weird, y=y))
  )
  identical(
    group_exec(r2c_slope, g, x, MoreArgs=list(1:3)),
    bsac(slope, g, list(x=x), list(y=1:3))
  )
  identical(
    group_exec(r2c_slope, g, x, MoreArgs=list(1:110)),
    bsac(slope, g, list(x=x), list(y=1:110))
  )
  identical(
    group_exec(r2c_slope, g, x, MoreArgs=list(x=1:3)),
    bsac(slope, g, list(y=x), list(x=1:3))
  )
  identical(
    group_exec(r2c_slope, g, y, MoreArgs=list(x=1:110)),
    bsac(slope, g, list(y=y), list(x=1:110))
  )
})
unitizer_sect('weird groups', {
  # Testing non sequential groups
  g2 <- g
  g2[g2 == 1L] <- 501L
  g2[g2 == 8L] <- 50L
  identical(group_exec(r2c_sum, g2, x), c(tapply(x, g2, sum)))
})



