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

f_concat <- '
static void %s(%s) {
  R_xlen_t len_tot = 0;
  double * res = data[di[narg]];

  for(int arg = 0; arg < narg; ++arg) {
    int din = di[arg];
    double * dat = data[din];
    R_xlen_t len_n = lens[din];
    // Probably should be R level check
    if(R_XLEN_T_MAX - len_n < len_tot)
      Rf_error("Concatenating arguments would overflow R_xlen_t.");
    len_tot += len_n;

    memcpy(res, dat, sizeof(double) * len_n);
    res += len_n;
  }
  lens[di[narg]] = len_tot;
}'

code_gen_concat <- function(fun, pars, par.types) {
  vetr(
    identical(., 'c'),
    pars=list(),
    par.types=
      character() && length(.) == length(pars) &&
      all(. %in% PAR.TYPES)
  )
  name <- FUN.NAMES[fun]
  code_res(
    defn=sprintf(
      f_concat, name,
      toString(c(CF.ARGS.BASE, CF.ARGS.VAR))
    ),
    name=name, narg=TRUE
  )
}
