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

// System headers if any go above ^^
#include "r2c.h"
#include <R_ext/Rdynload.h>

/*
 * Compute Group Data
 *
 * @param g sorted group indices
 */

SEXP R2C_group_sizes(SEXP g) {
  if(TYPEOF(g) != INTSXP)
    Rf_error("Argument `g` should be an integer vector.");

  int prt = 0;
  int *g_int = INTEGER(g);
  R_xlen_t glen = XLENGTH(g);
  R_xlen_t gn = 1;  // At least one group, possibly zero sized

  if (glen > 1) {
    int g_int_val = *g_int;
    for(R_xlen_t gi = 1; gi < glen; ++gi) {
      int g_int_prev_val = g_int_val;
      g_int_val = *(++g_int);
      if(g_int_val != g_int_prev_val) ++gn;
  } }
  SEXP gsize_sxp = PROTECT(Rf_allocVector(REALSXP, gn)); ++prt;
  SEXP glabs_sxp = PROTECT(Rf_allocVector(INTSXP, gn)); ++prt;
  g_int = INTEGER(g);

  double *gsize = REAL(gsize_sxp);
  int *glabs = INTEGER(glabs_sxp);
  double gmax = 0;

  if(glen == 0) {
    glabs[0] = 0;
    *gsize = 0.;
  } else if(glen == 1) {
    *gsize = (double) glen;
    *glabs = *g_int;
  } else if (glen > 1) {
    double gsize_i = 0;
    int g_int_val = *g_int;
    *(glabs++) = g_int_val;
    for(R_xlen_t gi = 1; gi < glen; ++gi) {
      int g_int_prev_val = g_int_val;
      g_int_val = *(++g_int);
      ++gsize_i;
      // Group changed, record prior group size (recall we start one lagged)
      if(g_int_val != g_int_prev_val) {
        *(gsize++) = gsize_i;
        if(gsize_i > gmax) gmax = gsize_i;
        *(glabs++) = g_int_val;
        gsize_i = 0;
      }
    }
    // One extra item in the trailing group we will not have counted
    *gsize = gsize_i + 1;
    if(*gsize > gmax) gmax = *gsize;
  }
  SEXP res = PROTECT(Rf_allocVector(VECSXP, 3)); ++prt;
  SEXP gmax_sxp = PROTECT(Rf_ScalarReal(gmax)); ++prt;
  SET_VECTOR_ELT(res, 0, gsize_sxp);
  SET_VECTOR_ELT(res, 1, glabs_sxp);
  SET_VECTOR_ELT(res, 2, gmax_sxp);
  UNPROTECT(prt);
  return res;
}

