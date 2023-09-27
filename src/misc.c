/*
 * Copyright (C) Brodie Gaslam
 *
 * This file is part of "r2c - Fast Iterated Statistic Computation in R"
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

// System headers if any go above ^^
#include "r2c.h"
#include <R_ext/Rdynload.h>

/* Dummy convolution adapted from WRE 5.10.1 for benchmarking vs
 * the r2c version.
 */

SEXP R2C_convolve(SEXP a, SEXP b) {
  int na, nb, nab;
  double *xa, *xb, *xab;
  SEXP ab;

  a = PROTECT(Rf_coerceVector(a, REALSXP));
  b = PROTECT(Rf_coerceVector(b, REALSXP));
  na = Rf_length(a); nb = Rf_length(b); nab = na + nb - 1;
  ab = PROTECT(Rf_allocVector(REALSXP, nab));
  xa = REAL(a); xb = REAL(b); xab = REAL(ab);
  for(int i = 0; i < nab; i++) xab[i] = 0.0;  // not necessary?
  for(int i = 1; i <= na; i++)
    for(int j = 1; j <= nb; j++) xab[i + j - 2] += xa[i - 1] * xb[j -1];

  UNPROTECT(3);
  return ab;
}
