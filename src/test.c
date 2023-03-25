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

// Testing

#include <math.h>
#include "r2c-const.h"
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Utils.h>
#include "loop-interrupt.h"

static void add(double ** data, R_xlen_t * lens, int * di) {
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
  // Not all "bin" operators are commutative
  // so we cannot play tricks with switching parameter order
  R_xlen_t i, j;
  if(len1 == len2) {
    LOOP_W_INTERRUPT1(len1, {res[i] = (e1[i] + e2[i]);});
    lens[dires] = len1;
  } else if (len2 == 1) {
    LOOP_W_INTERRUPT1(len1, {res[i] = (e1[i] + *e2);});
    lens[dires] = len1;
  } else if (len1 == 1) {
    LOOP_W_INTERRUPT1(len2, {res[i] = (*e1 + e2[i]);});
    lens[dires] = len2;
  } else if (len1 > len2) {
    LOOP_W_INTERRUPT2(len1, len2, res[i] = (e1[i] + e2[j]););
    if(j != len2) data[0][0] = 1.;   // bad recycle
    lens[dires] = len1;
  } else if (len2 > len1) {
    LOOP_W_INTERRUPT2(len2, len1, res[i] = (e1[j] + e2[i]););
    if(j != len1) data[0][0] = 1.;   // bad recycle
    lens[dires] = len2;
  }
}
static void sum(double ** data, R_xlen_t * lens, int * di, int flag) {
  int di0 = di[0];
  int di1 = di[1];

  R_xlen_t len_n = lens[di0];
  double * dat = data[di0];
  int narm = flag;  // only one possible flag parameter

  long double tmp = 0;
  R_xlen_t i;
  if(!narm) LOOP_W_INTERRUPT1(len_n, tmp += dat[i];);
  else LOOP_W_INTERRUPT1(len_n, {if(!ISNAN(dat[i])) tmp += dat[i];});

  *data[di1] = (double) tmp;
  lens[di1] = 1;
}

void run(double ** data, R_xlen_t * lens, int ** di) {
  // a + b
  add(data, lens, *di++);

  // sum(... = a + b, na.rm = FALSE)
  sum(data, lens, *di++, 0.);
}
#define ITEMS  10000000
#define GROUPS  1000000

SEXP R2C_sum_add(SEXP x, SEXP y) {
  if(XLENGTH(x) != ITEMS) Rf_error("bad length.");
  if(XLENGTH(y) != ITEMS) Rf_error("bad length.");
  R_xlen_t gsize = ITEMS/GROUPS;

  SEXP res = PROTECT(Rf_allocVector(REALSXP, GROUPS));
  double * tmp = (double *) R_alloc(gsize, sizeof(double));
  double ** data = (double **) R_alloc(4, sizeof(double*));
  data[0] = REAL(x);
  data[1] = REAL(y);
  data[2] = tmp;
  data[3] = REAL(res);

  R_xlen_t * lens = (R_xlen_t *) R_alloc(4, sizeof(R_xlen_t));
  lens[0] = lens[1] = lens[2] = gsize;
  lens[3] = 1;

  int ** di = (int **) R_alloc(2, sizeof(int*));
  di[0] = (int*) R_alloc(3, sizeof(int));
  di[1] = (int*) R_alloc(2, sizeof(int));
  di[0][0] = 0;
  di[0][1] = 1;
  di[0][2] = 2;
  di[1][0] = 2;
  di[1][1] = 3;

  for(R_xlen_t g = 0; g < GROUPS; g += 1) {
    run(data, lens, di);
    data[0] += gsize;
    data[1] += gsize;
  }
  UNPROTECT(1);
  return res;
}
