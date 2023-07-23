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
static void %s(%s) {
  double * res = data[di[1]];
  R_xlen_t len0 = lens[di[0]];
  double counter = 0.0;

  for(R_xlen_t i = 0; i < len0; ++i) res[i] = ++counter;

  lens[1] = len0;
}'

code_gen_seq_along <- function(fun, par, par.types) {
  vetr(
    identical(., "seq_along"),
    pars=list(NULL),
    par.types=character() && all(. %in% PAR.INT)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_seq_along, name, toString(F.ARGS.BASE))
  code_res(defn=defn, name=name)
}



