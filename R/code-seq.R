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

f_seq_along <- '
// Copy a vector, intended to be copied to result
static void %s(%s) {
  double * res = data[di[1]];
  double * input = data[di[0]];
  R_xlen_t len0 = lens[di[0]];
  R_xlen_t ri = 0;
  double counter = 0.0;

  for(R_xlen_t i = 0; i < len0; ++i) res[i] = ++counter;

  lens[1] = len0;
}'

code_gen_seq_along <- function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    identical(., "seq_along"),
    args.reg=list(NULL),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  name <- fun
  defn <- sprintf(f_seq_along, name, toString(F.ARGS.BASE))
  code_res(defn=defn, name=name)
}



