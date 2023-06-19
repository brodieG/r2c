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

f_subset <- '
static void %s(%s) {
  double * res = data[di[2]];
  double * dat = data[di[0]];
  double * index = data[di[1]];

  R_xlen_t len = lens[di[1]];

  for(R_xlen_t i = 0; i < len; ++i) {
    double ival = index[i];
    int isna = ISNAN(ival);
    if(!isna && ival > 0) {
      res[i] = dat[(R_xlen_t)ival - 1];
    } else if (isna) {
      res[i] = NA_REAL;
    } else
      Rf_error(
        "Only strictly positive index values allowed, found: %%jd",
        (intmax_t) ival
      );
  }
  lens[2] = len;
}'

code_gen_subset <- function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    identical(., "["),
    args.reg=list(NULL, NULL),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_subset, name, toString(F.ARGS.BASE))
  code_res(defn=defn, name=name)
}

f_subset_assign <- '
static void %s(%s) {
  double * res = data[di[2]];
  double * dat = data[di[0]];
  double * index = data[di[1]];

  R_xlen_t len = lens[di[1]];

  for(R_xlen_t i = 0; i < len; ++i) {
    double ival = index[i];
    int isna = ISNAN(ival);
    if(!isna && ival > 0) {
      res[i] = dat[(R_xlen_t)ival - 1];
    } else if (isna) {
      res[i] = NA_REAL;
    } else
      Rf_error(
        "Only strictly positive index values allowed, found: %%jd",
        (intmax_t) ival
      );
  }
  lens[2] = len;
}'




