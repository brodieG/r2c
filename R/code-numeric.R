## Copyright (C) Brodie Gaslam
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

f_numeric <- '
static void %s(%s) {
  if(lens[0] != 1) Rf_error("invalid length argument");
  double lend = data[di[0]][0];
  double * res = data[di[1]];

  if(lend >= 0 && lend < R_XLEN_T_MAX) {
    R_xlen_t len = data[di[0]][0];
    R_xlen_t i;
    // AFAICT 0.0 not guaranteed to be all zeroes in binary representation, so
    // cannot just memset.  If we used a simple loop instead of macro
    // compiler can replace with memset in common case where zero is zeroes.
    // So something to consider here.
    LOOP_W_INTERRUPT1(len, {res[i] = 0;});
    lens[1] = len;
  } else {
    Rf_error("Invalid vector size (got %%f)", data[di[0]][0]);
  }
}'
# Sizing fun for things like `numeric(x)`

numeric_size <- function(vals) {
  if(!is.list(vals) || length(vals) != 1L) stop(
    "There should only be one sizing param, got ", length(vals)
  )
  val <- vals[[1L]]
  if(!is.numeric(val) || length(val) != 1L || is.na(val) || val < 0)
    stop("Invalid size parameter.")
  list(val)
}

code_gen_numeric <- function(fun, pars, par.types) {
  vetr(
    identical(., "numeric"),
    pars=list(NULL),
    par.types=character() && all(. %in% PAR.INT)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_numeric, name, toString(F.ARGS.BASE))
  code_res(defn=defn, name=name)
}



