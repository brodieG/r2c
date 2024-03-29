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

code_gen_lgl2 <- function(fun, pars, par.types) {
  vetr(
    . %in% c("&&", "||"),
    pars=list(NULL, NULL),
    par.types=character() && all(. %in% PAR.IVARY)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_lgl2, name, toString(CF.ARGS.BASE), LGL.OP[fun], fun)
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
  R_xlen_t no_len = lens[di2];
  double * res = data[dires];
  R_xlen_t i;

  if(cond_len == yes_len && cond_len == no_len) {
    LOOP_W_INTERRUPT1(cond_len, {
      if(!ISNAN(cond[i])) res[i] = cond[i] ? yes[i] : no[i];
      else res[i] = NA_REAL;
    });
  } else if (yes_len > 0 && no_len > 0) {
    R_xlen_t j, k;
    LOOP_W_INTERRUPT3(cond_len, yes_len, no_len, {
      if(!ISNAN(cond[i])) res[i] = cond[i] ? yes[j] : no[k];
      else res[i] = NA_REAL;
    });
  } else if (yes_len > 0) {
    R_xlen_t j;
    LOOP_W_INTERRUPT2(cond_len, yes_len, {
      if(!ISNAN(cond[i])) res[i] = cond[i] ? yes[j] : NA_REAL;
      else res[i] = NA_REAL;
    });
  } else if (no_len > 0) {
    R_xlen_t j;
    LOOP_W_INTERRUPT2(cond_len, no_len, {
      if(!ISNAN(cond[i])) res[i] = cond[i] ? NA_REAL : no[j];
      else res[i] = NA_REAL;
    });
  } else LOOP_W_INTERRUPT1(cond_len, res[i] = NA_REAL;);
  lens[dires] = cond_len;
}'
code_gen_ifelse <- function(fun, pars, par.types) {
  vetr(
    identical(., "ifelse"),
    pars=list(NULL, NULL, NULL),
    par.types=character() && all(. %in% PAR.IVARY)
  )
  name <- fun
  defn <- sprintf(f_ifelse, name, toString(CF.ARGS.BASE))
  code_res(defn=defn, name=name)
}


