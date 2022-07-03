## Copyright (C) 2022 Brodie Gaslam
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

unitizer_sect('sum', {
  r2c_sum_na <- r2cq(sum(x, na.rm=na.rm))
  r2c_sum_narm <- r2cq(sum(x, na.rm=TRUE))
  r2c_sum_n <- r2cq(sum(x, y))
  r2c_sum_n_na <- r2cq(sum(x, y, na.rm=na.rm))
  r2c_sum_n_narm <- r2cq(sum(x, y, na.rm=TRUE))

  identical(r2c_sum(x), sum(x))
  identical(r2c_sum(y), sum(y))
  identical(r2c_sum(x), sum(x))
  identical(r2c_sum(x.inf), sum(x.inf))
  identical(r2c_sum(x.na), sum(x.na))
  identical(r2c_sum(x.weird), sum(x.weird))

  identical(r2c_sum_na(x.na, na.rm=TRUE), sum(x.na, na.rm=TRUE))
  identical(r2c_sum_na(na.rm=TRUE, x.na), sum(x.na, na.rm=TRUE))
  identical(r2c_sum_na(x.na, na.rm=FALSE), sum(x.na, na.rm=FALSE))
  identical(r2c_sum_narm(x.na), sum(x.na, na.rm=TRUE))

  identical(r2c_sum_n(x, y), sum(x, y))
  identical(r2c_sum_n_narm(x.na, y), sum(x.na, y, na.rm=TRUE))
  identical(r2c_sum_n_na(z, x.na, na.rm=TRUE), sum(z, x.na, na.rm=TRUE))

  ## integers
  r2c_sum(1:4)
  typeof(r2c_sum(1:4))
  r2c_sum_n(1, 1:4)         # mix int, real
  typeof(r2c_sum_n(1, 1:4))

  r2c_sum(c(1L, NA_integer_, 2L))
  typeof(r2c_sum(c(1L, NA_integer_, 2L)))

  ## Odd inputs
  r2c_sum(numeric())
  r2c_sum()                  # Unike `sum`, this requires and arg
})
unitizer_sect('mean', {
  r2c_mean <- r2cq(mean(x))
  r2c_mean_na <- r2cq(mean(x, na.rm=na.rm))
  r2c_mean_trim <- r2cq(mean(x, trim=.2))
  r2c_mean_trim_na <- r2cq(mean(x, na.rm=na.rm, trim=.2))
  r2c_mean_trim_0 <- r2cq(mean(x, trim=0))
  r2c_mean_trim_0_na <- r2cq(mean(x, na.rm=na.rm, trim=0))

  identical(r2c_mean(x), mean(x))
  identical(r2c_mean(y), mean(y))
  identical(r2c_mean(x.inf), mean(x.inf))
  identical(r2c_mean(x.na), mean(x.na))
  identical(r2c_mean(x.weird), mean(x.weird))
  identical(r2c_mean(x.ovrf.dbl), mean(x.ovrf.dbl))
  identical(r2c_mean(x.ovrf.ldbl), mean(x.ovrf.ldbl))

  identical(r2c_mean_na(x.na, na.rm=TRUE), mean(x.na, na.rm=TRUE))
  identical(r2c_mean_na(na.rm=TRUE, x.na), mean(x.na, na.rm=TRUE))
  identical(r2c_mean_na(x.na, na.rm=FALSE), mean(x.na, na.rm=FALSE))
  identical(
    r2c_mean_na(x.ovrf.dbl.na, na.rm=TRUE), mean(x.ovrf.dbl.na, na.rm=TRUE)
  )
  identical(
    r2c_mean_na(x.ovrf.ldbl.na, na.rm=TRUE), mean(x.ovrf.ldbl.na, na.rm=TRUE)
  )

  identical(r2c_mean_trim_0(x), mean(x))
  identical(r2c_mean_trim_0_na(x.na, na.rm=TRUE), mean(x.na, na.rm=TRUE))
})
unitizer_sect('mean0', {
  r2c_mean0 <- r2cq(mean0(x))
  r2c_mean0_na <- r2cq(mean0(x, na.rm=na.rm))

  identical(r2c_mean0(x), mean0(x))
  identical(r2c_mean0(y), mean0(y))
  identical(r2c_mean0(x), mean0(x))
  identical(r2c_mean0(x.inf), mean0(x.inf))
  identical(r2c_mean0(x.na), mean0(x.na))
  identical(r2c_mean0(x.weird), mean0(x.weird))

  identical(r2c_mean0_na(x.na, na.rm=TRUE), mean0(x.na, na.rm=TRUE))
})
unitizer_sect('length', {
  r2c_len <- r2cq(length(x))
  r2c_len(numeric())
  r2c_len(integer())
  r2c_len(x)
})
unitizer_sect('Errors', {
  r2c_sum(x, y)
  r2c_sum(letters)
  r2c_sum_n(x, z=letters)

  r2c_mean_na(x, y)

  # Only default trim value allowed
  r2c_mean_trim(x)
  r2c_mean_trim(x, na.rm=TRUE)
})

