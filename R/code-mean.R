## Copyright (C) 2022 Brodie Gaslam
##
## This file is part of "r2c - Fast Iterated Statistics in R"
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
##
## The code in this file is adapted from R's implementation of the real_mean
## function in src/main/summary.c as of -r82326, with the following copyright
## notice:
##
##  R : A Computer Language for Statistical Data Analysis
##  Copyright (C) 1997--2021  The R Core Team
##  Copyright (C) 1995, 1996  Robert Gentleman and Ross Ihaka
## 
##  This program is free software; you can redistribute it and/or modify
##  it under the terms of the GNU General Public License as published by
##  the Free Software Foundation; either version 2 of the License, or
##  (at your option) any later version.
## 
##  This program is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##  GNU General Public License for more details.
## 
##  You should have received a copy of the GNU General Public License
##  along with this program; if not, a copy is available at
##  https://www.R-project.org/Licenses/

## Changes from R implementation:
## * Adds na removal, R's implementation does that in R
## * Replaces LDOUBLE by long double
## * Removes ITERATE_BY_REGION

loop_mean <-
'  if(!narm)
    for (R_xlen_t k = 0; k < n; k++) %1$s;
  else
    for (R_xlen_t k = 0; k < n; k++) if(!isnan(dx[k])) %1$s;
'
loop_mean1 <- sprintf(loop_mean, 's += dx[k]')
loop_mean2 <- sprintf(loop_mean, 's += dx[k]/n')
loop_mean3 <- sprintf(loop_mean, 't += (dx[k] - s)')
loop_mean4 <- sprintf(loop_mean, 't += (dx[k] - s)/n')

f_mean <- sprintf('
static void %%s(%%s) {
  int di1 = datai[0];
  int di2 = datai[1];
  R_xlen_t n = lens[di1];
  double * dx = data[di1];
  int narm = flag;  // only one possible flag parameter

  long double s = 0.0;
  %s

  Rboolean finite_s = R_FINITE((double) s);
  if (finite_s) {
    s /= n;
  } else { // infinite s, maybe just overflowed; try to use smaller terms:
    s = 0.;
    %s
  }
  // Second precision enhancing pass
  if (finite_s && R_FINITE((double) s)) {
    long double t = 0.0;
    %s
    s += t/n;
  }
  else if (R_FINITE((double) s)) { // was infinite: more careful
    long double t = 0.0;
    %s
    s += t;
  }
  *data[di2] = s;
  lens[di2] = 1;
}', loop_mean1, loop_mean2, loop_mean3, loop_mean4)


