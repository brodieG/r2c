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
static void %s(%s%s%s) {
  data[2][off[2]] = data[0][off[0]] + data[1][off[1]];
}
"
arith_n_1 <- "
static void %s(%s%s%s) {
  double * res = data[2][off[2]];
  double * e1 = data[0][off[0]];
  double e2 = *(data[1][off[1]]);
  R_xlen_t len1 = len[0];
  for(R_xlen_t i = 0; i < len1; ++i) res[i] = e1[i] %s e2;
}
"
arith_1_n <- "
static void %s(%s%s%s) {
  double * res = data[2][off[2]];
  double e1 = *(data[0][off[0]]);
  double * e2 = data[1][off[1]];
  R_xlen_t len2 = len[1];
  for(R_xlen_t i = 0; i < len2; ++i) res[i] = e1 %s e2[i];
}
"
## If both are group data, use this (maybe of both are external equal size)
## We will not run calculations that return zero length vectors.
arith_n_n <- '
static void %s(%s%s%s) {
  double * res = data[2][off[2]];
  double * e1 = data[0][off[0]];
  double * e2 = data[1][off[1]];
  R_xlen_t len1 = len[0];
  for(R_xlen_t i = 0; i < len1; ++i) res[i] = e1[i] %s e2[i];
}
'
## Unequal size, we're assuming at least one is from group data otherwise it
## doesn't really make sense to do this, but this means which is the larger of
## the two can change group to group.
##
## This will be less efficient for the special case where an external vector is
## exactly the same size as every group.
arith_n_m <- '
static void %s(%s%s%s) {
  double * res = data[2][off[2]];
  double * e1;
  double * e2;
  R_xlen_t len1 = len[0];
  R_xlen_t len2 = len[1];
  R_xlen_t len12 = 0;

  if(len1 == 0 || len2 == 0) return; // empty recycle is zero

  // Ensure longest arg is first
  if(len1 >= len2) {
    e1 = data[0][off[0]];
    e2 = data[1][off[1]];
  } else {
    e1 = data[1][off[1]];
    e2 = data[0][off[0]];
    len12 = len2;
    len2 = len1;
    len1 = len12;
  }
  // Mod iterate by region?
  for(R_xlen_t i = 0; i < len1; ++i, ++j) {
    if(j > len2) j = 0;
    res[i] = e1[i] %s e2[j];
  }
}
'
code_gen_arith <- function(op, sizes, ctrl) {
  vetr(
    CHR.1 && . %in% names(OP.NAMES),
    numeric(2L) && all(is.na(.) | . >= 0),
    list() && !length(.)
  )
  defn <- if(all(is.na(sizes))) {
    name <- paste0(OP.NAMES[op], "_n_n")
    sprintf(arith_n_n, name, ARGS.BASE, "", "", op)
  } else if(isTRUE(sizes[1L] == 1) && isTRUE(sizes[2L] == 1)) {
    name <- paste0(OP.NAMES[op], "_1_1")
    sprintf(arith_1_1, name, ARGS.BASE, "", "", op)
  } else if(isTRUE(sizes[1L] == 1L)) {
    name <- paste0(OP.NAMES[op], "_1_n")
    sprintf(arith_1_n, name, ARGS.BASE, "", "", op)
  } else if(isTRUE(sizes[2L] == 1L)) {
    name <- paste0(OP.NAMES[op], "_n_1")
    sprintf(arith_n_1, name, ARGS.BASE, "", "", op)
  } else {
    name <- paste0(OP.NAMES[op], "_n_m")
    sprintf(arith_n_m, name, ARGS.BASE, "", "", op)
  }
  list(
    defn=defn,
    name=name,
    call=sprintf("%s(%s%s%s);", name, CALL.BASE, "", "")
  )
}


