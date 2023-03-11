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
    if(lens[di[0]] == 1) *data[di[2]] = %3$s(*data[di[0]], NAN);
    else if(lens[di[1]] == 1) *data[di[2]] = %3$s(NAN, *data[di[1]]);
    else *data[di[2]] = NAN;
  } else {
    // Error (allow warning, report iteration this happened in)
    intmax_t bad_len = lens[di[0]];
    const char * bad_side = "LHS";
    if(lens[di[0]] <= 1) { bad_len = lens[di[1]]; bad_side = "RHS"; }
    Rf_error("%%s length > 1 (%%jd) for `%4$s`.", bad_side, bad_len);
  }
  lens[di[1]] = 1;
}'

LGL.NAMES <- c("&&"="and2", "||"="or2")
LGL.OP <- c("&&"="AND", "||"="OR")
LGL.OP <- c("&&"="AND", "||"="OR")
LGL.DEFN <- c("&&"=unname(OP.DEFN['&']), "||"=unname(OP.DEFN['|']))

code_gen_lgl2 <- function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    . %in% c("&&", "||"),
    args.reg=list(NULL, NULL),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  name <- LGL.NAMES[fun]
  defn <- sprintf(f_lgl2, name, toString(F.ARGS.BASE), LGL.OP[fun], fun)
  code_res(defn=defn, name=name, defines=LGL.DEFN[fun])
}


