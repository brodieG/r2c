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

#' @include code-summary.R

NULL

## all/any are in code-summary.R, relops and &/| are in code-bin.R

f_lgl2 <- '
static void %s(%s) {
  if(lens[di[0]] == 1 && lens[di[1]] == 1) {
    *data[di[2]] = %3$s(*data[di[0]], *data[di[1]]);
  } else if(lens[di[0]] <= 1 && lens[di[1]] <= 1) {
    // Zero length
    if(lens[di[0]] == 1) *data[di[2]] = %3$s(*data[di[0]], NA_REAL);
    else if(lens[di[1]] == 1) *data[di[2]] = %3$s(NA_REAL, *data[di[1]]);
    else *data[di[2]] = NA_REAL;
  } else {
    // Error (allow warning, report iteration this happened in)
    intmax_t bad_len = lens[di[0]];
    const char * bad_side = "LHS";
    if(lens[di[0]] <= 1) { bad_len = lens[di[1]]; bad_side = "RHS"; }
    Rf_error("%%s length > 1 (%%jd) for `%4$s`.", bad_side, bad_len);
  }
  lens[di[1]] = 1;
}'

LGL.OP <- c("&&"="AND", "||"="OR")
LGL.DEFN <- c("&&"=unname(OP.DEFN['&']), "||"=unname(OP.DEFN['|']))
stopifnot(all(names(LGL.OP) %in% names(FUN.NAMES)))

code_gen_lgl2 <- function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    . %in% c("&&", "||"),
    args.reg=list(NULL, NULL),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_lgl2, name, toString(F.ARGS.BASE), LGL.OP[fun], fun)
  code_res(defn=defn, name=name, defines=LGL.DEFN[fun])
}


f_ifelse <- '
static void %s(%s) {
  int di0 = di[0];
  int di1 = di[1];
  int di2 = di[2];
  int dires = di[3];
  double * cond = data[di0];
  double * yes = data[di1];
  double * no = data[di2];

  R_xlen_t cond_len = lens[di0];
  R_xlen_t yes_len = lens[di1];
  R_xlen_t no_len = lens[di1];
  double * res = data[dires];

  if(cond_len == yes_len && cond_len == no_len) {
    for(R_xlen_t i = 0; i < cond_len; ++i) {
      if(!ISNAN(cond[i])) res[i] = cond[i] ? yes[i] : no[i];
      else res[i] = NA_REAL;
    }
  } else {
    R_xlen_t j = 0;
    R_xlen_t k = 0;
    for(R_xlen_t i = 0; i < cond_len; ++i, ++j, ++k) {
      if(j > yes_len) j = 0;
      if(k > no_len) k = 0;
      if(!ISNAN(cond[i])) res[i] = cond[i] ? yes[j] : no[k];
      else res[i] = NA_REAL;
  } }
  lens[dires] = cond_len;
}'
code_gen_ifelse <- function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    identical(., "ifelse"),
    args.reg=list(NULL, NULL, NULL),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  name <- fun
  defn <- sprintf(f_ifelse, name, toString(F.ARGS.BASE))
  code_res(defn=defn, name=name)
}


