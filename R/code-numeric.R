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
  double * res = data[di[1]];
  if(data[di[0]][0] < R_XLEN_T_MAX) {
    R_xlen_t len = data[di[0]][0];
    R_xlen_t i;
    // AFAICT 0.0 not guaranteed to be all zeroes.  Should consider just using
    // a simple for loop so that compiler can replace with memset in common
    // case where it can be used.
    LOOP_W_INTERRUPT1(len, {res[i] = 0;});
    lens[1] = len;
  } else {
    Rf_error(
      "Cannot create vector larger than R_XLEN_T_MAX (got %%f)",
      data[di[0]][0]
    );
  }
}'

code_gen_numeric <- function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    identical(., "numeric"),
    args.reg=list(NULL),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_numeric, name, toString(F.ARGS.BASE))
  code_res(defn=defn, name=name)
}



