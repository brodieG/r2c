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

loop.mean.base <- '
if(!narm)
  for (R_xlen_t k = 0; k < n; k++) %%1$s;
else
  for (R_xlen_t k = 0; k < n; k++) if(!isnan(dx[k])) %s'

lp.mn <- sprintf(loop.mean.base, "%1$s;")
# this one is to set the count of non-na elements
lp.mn.0 <- sprintf(loop.mean.base, "%1$s; else --m;")

make_loop_mean <- function(base, term, pad=2) {
  sprintf(
    paste0(
      strrep(' ', pad),
      unlist(strsplit(base, '\n', fixed=TRUE))[-1L],
      collapse="\n"
    ),
    term
  )
}
loop_mean1 <- make_loop_mean(lp.mn.0, 's += dx[k]')
loop_mean2 <- make_loop_mean(lp.mn, 's += dx[k]/m', 4)
loop_mean3 <- make_loop_mean(lp.mn, 't += (dx[k] - s)', 4)
loop_mean4 <- make_loop_mean(lp.mn, 't += (dx[k] - s)/m', 4)

f_mean <- sprintf('
static void %%s(%%s) {
  int di1 = di[0];
  int di2 = di[1];
  R_xlen_t n, m;
  n = m = lens[di1];
  double * dx = data[di1];
  int narm = flag;  // only one possible flag parameter

  long double s = 0.0;
%s

  if(!narm) m = n;   // reset non-na counter if we do not care

  Rboolean finite_s = R_FINITE((double) s);
  if (finite_s) {
    s /= m;
  } else { // infinite s, maybe just overflowed; try to use smaller terms:
    s = 0.;
%s
  }

  // Second precision enhancing pass
  if (finite_s && R_FINITE((double) s)) {
    long double t = 0.0;
%s
    s += t / m;
  }
  else if (R_FINITE((double) s)) { // was infinite: more careful
    long double t = 0.0;
%s
    s += t;
  }
  *data[di2] = s;
  lens[di2] = 1;
}', loop_mean1, loop_mean2, loop_mean3, loop_mean4)

