## Copyright (C) 2022 Brodie Gaslam
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

#' @include constants.R

NULL


OP.NAMES <- c(
  "+"="add", "-"="subtract", "*"="multiply", "/"="divide", "%%"="modulo"
)
OP.OP <- c("+"="+", "-"="-", "*"="*", "/"="/", "%%"="%")

## Binary Opertors or Functions with Vecto Recycling
##
## Use %3$s for functions, %4$s for operators.
##
## This supports unequal sizes.  We looked at having specialized functions for
## each of the possible length pairings, but that didn't seem to improve things
## too much (at least single core without any contention).

bin_op_vec_rec <- paste0('
static void %1$s(%2$s) {
  int di0 = di[0];
  int di1 = di[1];
  int dires = di[2];
  double * e1 = data[di0];
  double * e2 = data[di1];
  R_xlen_t len1 = lens[di0];
  R_xlen_t len2 = lens[di1];
  double * res = data[dires];

  if(len1 == 0 || len2 == 0) { // empty recycle is zero
    lens[dires] = 0;
    return;
  }
  // Not all "arith" operators are commutative
  // so we cannot play tricks with switching parameter order

  // Mod iterate by region?
  R_xlen_t i, j;
  if(len1 == len2) {
    for(i = 0; i < len1; ++i) res[i] = %3$s(e1[i] %4$s e2[i]);
    lens[dires] = len1;
  } else if (len2 == 1) {
    for(i = 0; i < len1; ++i) res[i] = %3$s(e1[i] %4$s *e2);
    lens[dires] = len1;
  } else if (len1 == 1) {
    for(i = 0; i < len2; ++i) res[i] = %3$s(*e1 %4$s e2[i]);
    lens[dires] = len2;
  } else if (len1 > len2) {
    for(i = 0, j = 0; i < len1; ++i, ++j) {
      if(j >= len2) j = 0;
      res[i] = %3$s(e1[i] %4$s e2[j]);
    }
    if(j != len2) data[%5$s][%6$s] = 1.;   // bad recycle
    lens[dires] = len1;
  } else if (len2 > len1) {
    for(i = 0, j = 0; i < len2; ++i, ++j) {
      if(j >= len1) j = 0;
      res[i] = %3$s(e1[j] %4$s e2[i]);
    }
    if(j != len1) data[%5$s][%6$s] = 1.;   // bad recycle
    lens[dires] = len2;
  }
}')
code_gen_arith <- function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    CHR.1 && . %in% names(OP.NAMES),
    args.reg=list(),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  name <- OP.NAMES[fun]
  op <- OP.OP[fun]      # needed for modulo
  defn <- sprintf(
    bin_op_vec_rec, name, toString(F.ARGS.BASE), "", op,
    IX[['I.STAT']], IX[['STAT.RECYCLE']]
  )
  code_res(defn=defn, name=name, headers=character())
}


