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

unitizer_sect('basic', {
  r2c_arith <- r2cq(((x + y * z - 3) / w ^ v))
  base_arith <- function(x, y, z, w, v) (x + y * z - 3) / w ^ v

  # Exponents
  identical(r2c_arith(x, y, z, rev(x), 2), base_arith(x, y, z, rev(x), 2))
  identical(r2c_arith(x, y, z, rev(x), 3), base_arith(x, y, z, rev(x), 3))
  identical(r2c_arith(x, y, z, rev(x), y), base_arith(x, y, z, rev(x), y))

  # Arg order
  identical(r2c_arith(y, z, x, rev(x), 2), base_arith(y, z, x, rev(x), 2))

  # Unusual values
  identical(
    r2c_arith(x.weird, y, z, rev(x), 2), base_arith(x.weird, y, z, rev(x), 2)
  )
  identical(
    r2c_arith(x.inf, y, z, rev(x), 2), base_arith(x.inf, y, z, rev(x), 2)
  )
  identical(
    r2c_arith(z, x.ovrf.dbl, x.ovrf.dbl, rev(x), 2),
    base_arith(z, x.ovrf.dbl, x.ovrf.dbl, rev(x), 2)
  )

  # NA
  identical(r2c_arith(x.na, y, z, x, 2), base_arith(x.na, y, z, x, 2))
  identical(r2c_arith(y, x.na, z, x, 2), base_arith(y, x.na, z, x, 2))
  identical(r2c_arith(y, z, x.na, x, 2), base_arith(y, z, x.na, x, 2))
  identical(r2c_arith(y, z, x, x.na, 2), base_arith(y, z, x, x.na, 2))
  identical(r2c_arith(y, z, x, 2, x.na), base_arith(y, z, x, 2, x.na))

  # Lengths
  identical(r2c_arith(x, y, z, 1:3, 2), base_arith(x, y, z, 1:3, 2))
  n0 <- numeric()
  identical(r2c_arith(n0, y, z, x, 2), base_arith(n0, y, z, x, 2))
  identical(r2c_arith(y, n0, z, x, 2), base_arith(y, n0, z, x, 2))
  identical(r2c_arith(y, z, n0, x, 2), base_arith(y, z, n0, x, 2))
  identical(r2c_arith(y, z, x, n0, 2), base_arith(y, z, x, n0, 2))
  identical(r2c_arith(y, z, x, 2, n0), base_arith(y, z, x, 2, n0))

  # Currently unimplemented but works as a constant exp
  r2c_mod <- r2cq(x %% y)
  identical(r2c_mod(x, 1:5), x %% (1:5))
  # But doesn't work as varying
  group_exec(r2c_mod, x, rep(1:2, 50), MoreArgs=list(1:5))
})

