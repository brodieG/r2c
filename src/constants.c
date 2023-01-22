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

const struct const_dat consts[] = {
  { "I.STAT",       I_STAT },
  { "I.RES",        I_RES },
  { "I.GRP",        I_GRP },
  { "STAT.N",       STAT_N },
  { "STAT.RECYCLE", STAT_RECYCLE }
};
const int CONST_N = sizeof(consts) / sizeof(struct const_dat);

//; Return shared constants from C to R
SEXP R2C_constants() {
  SEXP res = PROTECT(Rf_allocVector(VECSXP, CONST_N));
  SEXP res_names = PROTECT(Rf_allocVector(STRSXP, CONST_N));
  for(int i = 0; i < CONST_N; ++i) {
    SET_VECTOR_ELT(res, i, PROTECT(Rf_ScalarInteger(consts[i].value)));
    SET_STRING_ELT(res_names, i, PROTECT(Rf_mkChar(consts[i].name)));
    UNPROTECT(2);
  }
  Rf_setAttrib(res, R_NamesSymbol, res_names);
  UNPROTECT(2);
  return res;
}
