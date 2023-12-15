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

unitizer_sect("group sum", {
  identical(group_exec(r2c_sum, x, g), c(tapply(x, g, sum)))
  identical(
    group_exec(r2c_sum, x, process_groups(sort(g), sorted=TRUE)),
    c(tapply(x, sort(g), sum))
  )
  group_exec(r2c_sum, x, list(g))
  group_exec(r2c_sum, list(x), g)
  group_exec(r2c_sum, list(y=x), list(g))
  group_exec(r2c_sum, list(na.rm=x), g)

  identical(group_exec(r2c_sum, x.na, g), c(tapply(x.na, g, sum)))
  identical(group_exec(r2c_sum, x.weird, g), c(tapply(x.weird, g, sum)))
  identical(group_exec(r2c_sum, x.ovrf.dbl, g), c(tapply(x.ovrf.dbl, g, sum)))
  identical(
    group_exec(r2c_sum, x.ovrf.dbl.na, g),
    c(tapply(x.ovrf.dbl.na, g, sum))
  )
  identical(group_exec(r2c_sum, x.ovrf.ldbl, g), c(tapply(x.ovrf.ldbl, g, sum)))
  identical(
    group_exec(r2c_sum, x.ovrf.ldbl.na, g),
    c(tapply(x.ovrf.ldbl.na, g, sum))
  )

  # tapply drops the NA
  identical(group_exec(r2c_sum, x, g.na)[-13], c(tapply(x, g.na, sum)))
  group_exec(r2c_sum, x, g.na)
})
unitizer_sect("group_slope", {
  slope <- quote(sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2))
  identical(
    group_exec(r2c_slope, list(x, y), g), bsac(slope, list(x=x, y=y), g)
  )
  identical(
    group_exec(r2c_slope, list(x.na, y), g),
    bsac(slope, list(x=x.na, y=y), g)
  )
  identical(
    group_exec(r2c_slope, list(x.weird, y), g),
    bsac(slope, list(x=x.weird, y=y), g)
  )
  identical(
    group_exec(r2c_slope, x, g, MoreArgs=list(1:3)),
    bsac(slope, list(x=x), g, list(y=1:3))
  )
  identical(
    group_exec(r2c_slope, x, g, MoreArgs=list(1:110)),
    bsac(slope, list(x=x), g, list(y=1:110))
  )
  identical(
    group_exec(r2c_slope, x, g, MoreArgs=list(x=1:3)),
    bsac(slope, list(y=x), g, list(x=1:3))
  )
  identical(
    group_exec(r2c_slope, y, g, MoreArgs=list(x=1:110)),
    bsac(slope, list(y=y), g, list(x=1:110))
  )
})
unitizer_sect("factor groups", {
  fg.r2c <- with(iris, group_exec(r2c_sum, groups=Species, Sepal.Width))
  fg.base <- with(iris, tapply(Sepal.Width, Species, sum))
  identical(fg.r2c, c(fg.base))
})
unitizer_sect('weird groups', {
  # Testing non sequential groups
  g2 <- g
  g2[g2 == 1L] <- 501L
  g2[g2 == 8L] <- 50L
  identical(group_exec(r2c_sum, x, g2), c(tapply(x, g2, sum)))
})
unitizer_sect('preprocess groups', {
  g.r2c <- process_groups(c(1L, 2L, 2L), sorted=TRUE)
  group_exec(r2c_sum, c(1, 2, 3), groups=g.r2c)
})
unitizer_sect('zero groups', {
  group_exec(r2c_sum, list(), rep(1:2, each=2), MoreArgs=list(x=1:3))
  group_exec(r2c_add, list(), rep(1:2, each=2), MoreArgs=list(x=1:3, y=10))
  group_exec(r2c_sum, numeric(), rep(1:2, each=2))
  group_exec(r2c_sum, list(numeric()), rep(1:2, each=2))

  group_exec(r2c_sum, list(), integer(), MoreArgs=list(x=1:3))
  group_exec(r2c_add, list(), integer(), MoreArgs=list(x=1:3, y=1:3))
  group_exec(r2c_sum, numeric(), integer(), MoreArgs=list(x=1:3))
  group_exec(r2c_sum, list(numeric()), integer(), MoreArgs=list(x=1:3))
})
unitizer_sect("param mismatch error prettyfying", {
  n0 <- numeric()
  group_exec(r2c_sum, n0, integer(), MoreArgs=list(x=1:3))
  group_exec(r2c_sum, list(n0), integer(), MoreArgs=list(x=1:3))
  group_exec(r2c_sum, list(y=n0), integer(), MoreArgs=list(x=1:3))
  group_exec(r2c_sum, list(y=n0, x=n0), integer(), MoreArgs=list(x=1:3))
})
unitizer_sect("vecrec sizing", {
  # Vecrec with fixed size vs varying smaller and larger
  x <- 1:6
  g <- rep(1:3, 1:3)
  group_exec(r2c_add, x, g, MoreArgs=list(y=c(5,10)))
})
