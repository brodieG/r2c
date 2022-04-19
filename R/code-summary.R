## Copyright (C) 2022 Brodie Gaslam
##
## This file is part of "fapply - DSL For Fast Groupwise Numeric Calculations"
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

## We'll be changing this to be less like sum?
##
## Maybe we should also have a low error sum.

f_mean_base <- '
static void %%s(%%s%%s%%s) {
  double * res = data[1][off[1]];
  *res = 0;
  R_xlen_t len_n = len[0];
  double * dat = data[0][off[0]];
  for(R_xlen_t i = 0; i < len_n; ++i) %s*res += dat[i];
}
'
f_mean_1_narm <- sprintf(f_mean_base, 'if(!isnan(dat[i])) ')
f_mean_1_naok <- sprintf(f_mean_base, '')

f_sum_n_base <- '
static void %%s(%%s%%s%%s) {
  double * res = data[narg][off[narg]];
  *res = 0;
  for(int arg = 0; arg < narg; ++arg) {
    R_xlen_t len_n = len[narg];
    double * dat = data[arg][off[arg]];
    for(R_xlen_t i = 0; i < len_n; ++i) %s*res += dat[i];
  }
}
'
f_sum_n_narm <- sprintf(f_sum_n_base, 'if(!isnan(dat[i])) ')
f_sum_n_naok <- sprintf(f_sum_n_base, '')

## Special case when only one data parameter
f_sum_1_base <- '
static void %%s(%%s%%s%%s) {
  double * res = data[1][off[1]];
  *res = 0;
  R_xlen_t len_n = len[0];
  double * dat = data[0][off[0]];
  for(R_xlen_t i = 0; i < len_n; ++i) %s*res += dat[i];
}
'
f_sum_1_narm <- sprintf(f_sum_1_base, 'if(!isnan(dat[i])) ')
f_sum_1_naok <- sprintf(f_sum_1_base, '')

## Structure all strings into a list for ease of selection

f_summary <- list(
  sum_n_narm=f_sum_n_narm,
  sum_1_narm=f_sum_1_narm,
  sum_n_naok=f_sum_n_naok,
  sum_1_naok=f_sum_1_naok,
  mean_1_narm=f_mean_1_narm,
  mean_1_naok=f_mean_1_naok
)

code_gen_summary <- function(op, sizes, ctrl) {
  # needs "include <math.h>" for isnan
  if('trim' %in% ctrl[['trim']] && !isTRUE(ctrl[['trim']] == 0))
    stop("`trim` may only take on its default 0 value")
  sizes <- sizes[!names(sizes) %in% names(ctrl)]
  ctrl[['trim']] <- NULL

  na.rm <- if(isTRUE(ctrl[['na.rm']][1L] == TRUE)) "narm" else "naok"
  n <- if(length(sizes) == 1L) "1" else "n"
  name <- paste(op, n, na.rm, sep="_")
  list(
    defn=sprintf(
      f_summary[[name]], name,
      ARGS.BASE, if(n == "n") ARGS.VAR else "", ""
    ),
    name=name,
    call=sprintf(
      "%s(%s%s%s);", name, CALL.BASE, if(n == "n") ARGS.VAR else "", ""
    )
  )
}

