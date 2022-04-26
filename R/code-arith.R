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

OP.NAMES <- c(
  "+"="add", "-"="subtract", "*"="multiply", "/"="divide", "%"="modulo"
)
## This supports unequal sizes.  We looked at having specialized functions for
## each of the possible length pairings, but that didn't seem to improve things
## too much (at least single core without any contention).

arith_n_m <- '
static void %s(%s) {
  double * res = data[datai[2]] + off[2];
  double * e1;
  double * e2;
  R_xlen_t len1 = len[0];
  R_xlen_t len2 = len[1];
  R_xlen_t len12 = 0;

  if(len1 == 0 || len2 == 0) return; // empty recycle is zero

  // Ensure longest arg is first
  if(len1 >= len2) {
    e1 = data[datai[0]] + off[0];
    e2 = data[datai[1]] + off[1];
  } else {
    e1 = data[datai[1]] + off[1];
    e2 = data[datai[0]] + off[0];
    len12 = len2;
    len2 = len1;
    len1 = len12;
  }
  // Mod iterate by region?

  R_xlen_t i, j;
  if(len1 == len2) {
    for(i = 0; i < len1; ++i) res2[i] = e1[i] %s e2[i];
  } else if (len2 == 1) {
    for(i = 0; i < len1; ++i) res2[i] = e1[i] %s *e2;
  } else {
    for(i = 0, j = 0; i < len1; ++i, ++j) {
      if(j > len2) j = 0;
      res2[i] = e1[i] %s e2[j];
    }
  }
}
'
code_gen_arith <- function(op, sizes, ctrl) {
  vetr(
    CHR.1 && . %in% names(OP.NAMES),
    numeric(2L) && all(is.na(.) | . >= 0),
    list() && !length(.)
  )
  args <- ARGS.NM.BASE
  name <- paste0(OP.NAMES[op], "_n_m")
  defn <- sprintf(arith_n_m, name, toString(F.ARGS.BASE), op)
  list(
    defn=defn,
    name=name,
    call=sprintf("%s(%s);", name, toString(CALL.BASE)),
    args=args,
    headers=character()
  )
}


