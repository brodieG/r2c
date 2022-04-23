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
## Generate Arithmetic Functions

arith_1_1 <- "
static void %s(%s) {
  data[datai[2]][off[datai[2]]] =
    data[datai[0]][off[0]] + data[datai[1]][off[1]];

  len[datai[2]] = 1;
  off[datai[2]] = 0;
}
"
arith_n_1_x <- "
static void %%s(%%s) {
  double * res = data[datai[2]][off[2]];
  double * e%d = data[datai[0]][off[0]];
  double e%d = *(data[1][off[1]]);
  R_xlen_t len1 = len[0];

  for(R_xlen_t i = 0; i < len1; ++i) res[i] = e%d[i] %%s e%d;

  len[datai[2]] = len1;
  off[datai[2]] = 0;
}
"
arith_n_1 <- sprintf(arith_n_1_x, 1L, 2L, 1L, 2L)
arith_1_n <- sprintf(arith_n_1_x, 2L, 1L, 2L, 1L)

## If both are group data, use this (maybe of both are external equal size)
## We will not run calculations that return zero length vectors.
arith_n_n <- '
static void %s(%s) {
  double * res = data[datai[2]][off[2]];
  double * e1 = data[datai[0]][off[0]];
  double * e2 = data[datai[1]][off[1]];
  R_xlen_t len1 = len[0];
  for(R_xlen_t i = 0; i < len1; ++i) res[i] = e1[i] %s e2[i];

  len[datai[2]] = len1;
  off[datai[2]] = 0;
}
'
## Unequal size, we're assuming at least one is from group data otherwise it
## doesn't really make sense to do this, but this means which is the larger of
## the two can change group to group.
##
## This will be less efficient for the special case where an external vector is
## exactly the same size as every group.
arith_n_m <- '
static void %s(%s) {
  double * res = data[datai[2]][off[2]];
  double * e1;
  double * e2;
  R_xlen_t len1 = len[0];
  R_xlen_t len2 = len[1];
  R_xlen_t len12 = 0;

  if(len1 == 0 || len2 == 0) return; // empty recycle is zero

  // Ensure longest arg is first
  if(len1 >= len2) {
    e1 = data[datai[0]][off[0]];
    e2 = data[datai[1]][off[1]];
  } else {
    e1 = data[datai[1]][off[1]];
    e2 = data[datai[0]][off[0]];
    len12 = len2;
    len2 = len1;
    len1 = len12;
  }
  // Mod iterate by region?
  for(R_xlen_t i = 0; i < len1; ++i, ++j) {
    if(j > len2) j = 0;
    res[i] = e1[i] %s e2[j];
  }

  len[datai[2]] = len1;
  off[datai[2]] = 0;
}
'
code_gen_arith <- function(op, sizes, ctrl) {
  vetr(
    CHR.1 && . %in% names(OP.NAMES),
    numeric(2L) && all(is.na(.) | . >= 0),
    list() && !length(.)
  )
  args <- ARGS.BASE
  args.s <- toString(args)
  defn <- if(all(is.na(sizes))) {
    name <- paste0(OP.NAMES[op], "_n_n")
    sprintf(arith_n_n, name, args.s, op)
  } else if(isTRUE(sizes[1L] == 1) && isTRUE(sizes[2L] == 1)) {
    name <- paste0(OP.NAMES[op], "_1_1")
    sprintf(arith_1_1, name, args.s, op)
  } else if(isTRUE(sizes[1L] == 1L)) {
    name <- paste0(OP.NAMES[op], "_1_n")
    sprintf(arith_1_n, name, args.s, op)
  } else if(isTRUE(sizes[2L] == 1L)) {
    name <- paste0(OP.NAMES[op], "_n_1")
    sprintf(arith_n_1, name, args.s, op)
  } else {
    name <- paste0(OP.NAMES[op], "_n_m")
    sprintf(arith_n_m, name, args.s, op)
  }
  list(
    defn=defn,
    name=name,
    call=sprintf("%s(%s);", name, toString(CALL.BASE)),
    args=args,
    headers=character()
  )
}


