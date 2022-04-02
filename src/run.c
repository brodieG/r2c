/*
 * Copyright (C) 2022  Brodie Gaslam
 *
 * This file is part of "fapply - Fast Apply"
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 or 3 of the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Go to <https://www.r-project.org/Licenses> for a copies of the licenses.
 */

/*
 * Given an so name, retrieve and run the function
 */

SEXP compile(SEXP code) {
  cmd


}
SEXP run(SEXP so, SEXP fun, SEXP x) {
  if(TYPEOF(so) != STRSXP || XLENGTH(so) != 1)
    error("Argument `so` should be a scalar string.");
  if(TYPEOF(fun) != STRSXP || XLENGTH(fun) != 1)
    error("Argument `fun` should be a scalar string.");
  if(TYPEOF(x) != VECSXP)
    error("Argument `x` should be a list.");

  R_xlen_t xlen = XLENGTH(x);
  if(xlen > INTMAX)
    error("Arguent `x` may not contain more than INT_MAX items.");
  double ** xvals = R_alloc((size_t)xlen, sizeof(*double));
  for(R_xlen_t i = 0; i < xlen; ++i) {
    if(TYPEOF(VECTOR_ELT(x, i)) != REALSXP)
      error("Argument `x[%d]` must be a double.", %d);
    *(xvals + i) = REAL(VECTOR_ELT(x, i));
  }
  void * handle = dynload()



}
