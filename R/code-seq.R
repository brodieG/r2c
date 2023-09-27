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

#' @include code-numeric.R

f_seq_core <- '
    R_xlen_t i;
    LOOP_W_INTERRUPT1(len, {res[i] = ++counter;});
    lens[di[1]] = len;'

f_seq_len_size_check <- sprintf('
  if(lend >= 0 && lend < R_XLEN_T_MAX) {
    double counter = 0.0;
    R_xlen_t len = (R_xlen_t) lend;%s
  } else {
    Rf_error("Invalid vector size (got %%%%%%%%f)", data[di[0]][0]);
  }', f_seq_core
)

f_seq_along <- sprintf('
static void %%s(%%s) {
  double * res = data[di[1]];
  R_xlen_t lend = lens[di[0]];%s
}', f_seq_len_size_check
)

f_seq_len <- sprintf('
static void %%s(%%s) {
  if(lens[di[0]] != 1) Rf_error("invalid length argument");
  double * res = data[di[1]];
  double lend = data[di[0]][0];%s
}', f_seq_len_size_check
)

code_gen_seq_along <- function(fun, pars, par.types) {
  vetr(
    identical(., "seq_along"),
    pars=list(NULL),
    par.types=character() && all(. %in% PAR.INT)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_seq_along, name, toString(F.ARGS.BASE))
  code_res(defn=defn, name=name)
}
code_gen_seq_len <- function(fun, pars, par.types) {
  vetr(
    identical(., "seq_len"),
    pars=list(NULL),
    par.types=character(1) && all(. %in% PAR.EXT.NUM)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_seq_len, name, toString(F.ARGS.BASE))
  code_res(defn=defn, name=name)
}
