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

#include <math.h>
// System headers if any go above ^^
#include "r2c.h"

extern struct const_dat consts[];
extern int CONST_N;

SEXP R2C_assumptions(void) {
#ifndef INFINITY
  Rf_error("C implementation does not define INFINITY.");
#endif
  // This should generate a compiler warning if it is not infinite
  float inf = INFINITY;
  // Per C99 6.3.1.5 promotion to double does not change values, so if (float)
  // inf is infinite, so must (double) inf.  We rely on infinity being present
  // so it is defined that long double values can always be cast to double.
  if(isfinite(inf)) Rf_error("C implementation does not support infinity.");

  double xlen_test_dbl = R_XLEN_T_MAX;
  intmax_t xlen_test_int = (intmax_t) xlen_test_dbl;
  if(xlen_test_int != R_XLEN_T_MAX)
    Rf_error("Double cannot hold R_LEN_T_MAX without precision loss.");

  // Need to make sure the full range of R_xlen_t values can be accomodated by
  // double as we use doubles to store vector indices, specifically that there
  // is enough precision that all sequential integers are representable and
  // we're not skipping e.g. with a stride of 2.
  if(
    ((R_xlen_t)((double) R_XLEN_T_MAX) != R_XLEN_T_MAX) ||
      ((R_xlen_t)((double) R_XLEN_T_MAX - 1) != R_XLEN_T_MAX - 1)
  )
    Rf_error("Double cannot hold R_LEN_T_MAX without precision loss.");

  // Repeat same tests for integer, would be a weird system to fail this one.
  double intlen_test_dbl = INT_MAX;
  intmax_t intlen_test_int = (intmax_t) intlen_test_dbl;
  if(intlen_test_int != INT_MAX)
    Rf_error("Double cannot hold INT_MAX without precision loss.");

  // Need to make sure the full range of R_xlen_t values can be accomodated by
  // double as we use doubles to store vector indices, specifically that there
  // is enough precision that all sequential integers are representable and
  // we're not skipping e.g. with a stride of 2.
  if(
    ((int)((double) INT_MAX) != INT_MAX) ||
      ((int)((double) INT_MAX - 1) != INT_MAX - 1)
  )
    Rf_error("Double cannot hold INT_MAX without precision loss.");

  // For window application, we want to remap the window starts and ends to an
  // all-positive domain.  With a max window size of int we can definitely use
  // R_xlen_t for that (in the positive range).  But what if we want to
  // ultimately allow R_xlen_t window sizes?  That will never make sense except
  // for very large `by` values.

  return Rf_ScalarLogical(1);
}

