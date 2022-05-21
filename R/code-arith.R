## Copyright (C) 2022 Brodie Gaslam
##
## This file is part of "r2c - A DSL for Fast Statistics in R"
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

OP.NAMES <- c(
  "+"="add", "-"="subtract", "*"="multiply", "/"="divide", "%"="modulo"
)
## Binary Opertors or Functions with Vecto Recycling
##
## Use %3$s for functions, %4$s for operators.
##
## This supports unequal sizes.  We looked at having specialized functions for
## each of the possible length pairings, but that didn't seem to improve things
## too much (at least single core without any contention).

bin_op_vec_rec <- '
static void %1$s(%2$s) {
  int di1 = datai[0];
  int di2 = datai[1];
  int dires = datai[2];

  double * res = data[dires];

  double * e1;
  double * e2;
  R_xlen_t len1 = lens[di1];
  R_xlen_t len2 = lens[di2];
  R_xlen_t len12 = 0;

  if(len1 == 0 || len2 == 0) { // empty recycle is zero
    lens[dires] = 0;
    return;
  }
  // Ensure longest arg is first
  if(len1 >= len2) {
    e1 = data[di1];
    e2 = data[di2];
  } else {
    e1 = data[di2];
    e2 = data[di1];
    len12 = len2;
    len2 = len1;
    len1 = len12;
  }
  // Mod iterate by region?
  R_xlen_t i, j;
  if(len1 == len2) {
    for(i = 0; i < len1; ++i) res[i] = %3$s(e1[i] %4$s e2[i]);
  } else if (len2 == 1) {
    for(i = 0; i < len1; ++i) res[i] = %3$s(e1[i] %4$s *e2);
  } else {
    for(i = 0, j = 0; i < len1; ++i, ++j) {
      if(j > len2) j = 0;
      res[i] = %3$s(e1[i] %4$s e2[j]);
    }
  }
  lens[dires] = len1;
}'
code_gen_arith <- function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    CHR.1 && . %in% names(OP.NAMES),
    args.reg=list(),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  args <- ARGS.NM.BASE
  name <- OP.NAMES[fun]
  defn <- sprintf(bin_op_vec_rec, name, toString(F.ARGS.BASE), "", fun)
  code_res(defn=defn, name=name, args=args, headers=character())
}


