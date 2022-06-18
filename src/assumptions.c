/*
 * Copyright (C) 2022  Brodie Gaslam
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

SEXP R2C_assumptions() {
#ifndef INFINITY
  Rf_error("C implementation does not define INFINITY.");
#endif
  // This should generate a compiler warning if it is not infinite
  float inf = INFINITY;
  // Per C99 6.3.1.5 promotion to double does not change values, so if (float)
  // inf is infinite, so must (double) inf.  We rely on infinity being present
  // so it is defined that long double values can always be cast to double.
  if(isfinite(inf)) Rf_error("C implementation does not support infinity.");
  return Rf_ScalarLogical(1);
}
