/*
 * Copyright (C) 2022  Brodie Gaslam
 *
 * This file is part of "r2c - A DSL for Fast Repeated Numeric Calculations"
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

SEXP R2C_test1(SEXP x, SEXP y, SEXP z, SEXP w) {
  if(
    XLENGTH(x) != XLENGTH(y) || XLENGTH(x) != XLENGTH(x) ||
    XLENGTH(x) != XLENGTH(w)
  )
    error("Mismatched lengths.\n");

  R_xlen_t xlen = XLENGTH(x);
  SEXP res = PROTECT(allocVector(REALSXP, XLENGTH(x)));
  double * xr = REAL(x);
  double * yr = REAL(y);
  double * zr = REAL(z);
  double * wr = REAL(w);
  double * resr = REAL(res);

  for(R_xlen_t i = 0; i < xlen; ++i) resr[i] = xr[i] + yr[i];
  for(R_xlen_t i = 0; i < xlen; ++i) resr[i] += zr[i];
  for(R_xlen_t i = 0; i < xlen; ++i) resr[i] += wr[i];

  UNPROTECT(1);
  return res;
}
SEXP R2C_test2(SEXP x, SEXP y, SEXP z, SEXP w) {
  if(
    XLENGTH(x) != XLENGTH(y) || XLENGTH(x) != XLENGTH(x) ||
    XLENGTH(x) != XLENGTH(w)
  )
    error("Mismatched lengths.\n");

  R_xlen_t xlen = XLENGTH(x);
  SEXP res = PROTECT(allocVector(REALSXP, XLENGTH(x)));
  double * xr = REAL(x);
  double * yr = REAL(y);
  double * zr = REAL(z);
  double * wr = REAL(w);
  double * resr = REAL(res);

  for(R_xlen_t i = 0; i < xlen; ++i)
    resr[i] = xr[i] + yr[i] + zr[i] + wr[i];

  UNPROTECT(1);
  return res;
}

#include <R_ext/Itermacros.h>
#define NINTERRUPT 1000000U

SEXP R2C_test3(SEXP x, SEXP y, SEXP z, SEXP w) {
  if(
    XLENGTH(x) != XLENGTH(y) || XLENGTH(x) != XLENGTH(x) ||
    XLENGTH(x) != XLENGTH(w)
  )
    error("Mismatched lengths.\n");

  R_xlen_t xlen = XLENGTH(x);
  SEXP res = PROTECT(allocVector(REALSXP, XLENGTH(x)));
  double * xr = REAL(x);
  double * yr = REAL(y);
  double * zr = REAL(z);
  double * wr = REAL(w);
  double * resr = REAL(res);

  R_xlen_t n, n1, n2, i, i1, i2;
  n = n1 = n2 = xlen;
  MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
                     resr[i] = xr[i1] + yr[i2];);
  MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
                     resr[i] = resr[i1] + zr[i2];);
  MOD_ITERATE2_CHECK(NINTERRUPT, n, n1, n2, i, i1, i2,
                     resr[i] = resr[i1] + wr[i2];);

  UNPROTECT(1);
  return res;
}
SEXP R2C_test4(SEXP x, SEXP y, SEXP z, SEXP w) {
  if(
    XLENGTH(x) != XLENGTH(y) || XLENGTH(x) != XLENGTH(x) ||
    XLENGTH(x) != XLENGTH(w)
  )
    error("Mismatched lengths.\n");

  R_xlen_t xlen = XLENGTH(x);
  SEXP res = PROTECT(allocVector(REALSXP, XLENGTH(x)));
  double * xr = REAL(x);
  double * yr = REAL(y);
  double * zr = REAL(z);
  double * wr = REAL(w);
  double * resr = REAL(res);

  R_xlen_t n, n1, n2, i, i1, i2;
  n = n1 = n2 = xlen;
  MOD_ITERATE2(n, n1, n2, i, i1, i2,
                     resr[i] = xr[i1] + yr[i2];);
  MOD_ITERATE2(n, n1, n2, i, i1, i2,
                     resr[i] = resr[i1] + zr[i2];);
  MOD_ITERATE2(n, n1, n2, i, i1, i2,
                     resr[i] = resr[i1] + wr[i2];);

  UNPROTECT(1);
  return res;
}
SEXP R2C_test5(SEXP x, SEXP y, SEXP z, SEXP w) {
  if(
    XLENGTH(x) != XLENGTH(y) || XLENGTH(x) != XLENGTH(x) ||
    XLENGTH(x) != XLENGTH(w)
  )
    error("Mismatched lengths.\n");

  R_xlen_t xlen = XLENGTH(x);
  SEXP res = PROTECT(allocVector(REALSXP, XLENGTH(x)));
  double * xr = REAL(x);
  double * yr = REAL(y);
  double * zr = REAL(z);
  double * wr = REAL(w);
  double * resr = REAL(res);

  R_xlen_t n, i;
  n = xlen;
  R_ITERATE_CHECK(NINTERRUPT, n, i, resr[i] = xr[i] + yr[i];);
  R_ITERATE_CHECK(NINTERRUPT, n, i, resr[i] = resr[i] + zr[i];);
  R_ITERATE_CHECK(NINTERRUPT, n, i, resr[i] = resr[i] + wr[i];);
  UNPROTECT(1);
  return res;
}

