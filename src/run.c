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

// System headers if any go above ^^
#include "fapply.h"
#include <R_ext/Rdynload.h>

// Taken from Rdynpriv.h; we'll need to work around this
// Dummy struct, per C99 all pointers to struct are the same size, and since
// we're just going to pass a NULL pointer which R_FindSymbol checks for this
// will work.  Of course not part of the APIS so this can't be how we do the
// final implementation.

struct Rf_RegisteredNativeSymbol {
    int dummy;
};

/*
 * Given an so and function name, retrieve and run the function
 */

SEXP FAPPLY_run(SEXP so, SEXP fun_name, SEXP x) {
  if(TYPEOF(so) != STRSXP || XLENGTH(so) != 1)
    error("Argument `so` should be a scalar string.");
  if(TYPEOF(fun_name) != STRSXP || XLENGTH(fun_name) != 1)
    error("Argument `fun_name` should be a scalar string.");
  if(TYPEOF(x) != REALSXP)
    error("Argument `x` should be a numeric vector.");
  // if(TYPEOF(x) != VECSXP)
  //   error("Argument `x` should be a list.");

  // R_xlen_t xlen = XLENGTH(x);
  // if(xlen > INT_MAX)
  //   error("Arguent `x` may not contain more than INT_MAX items.");
  // double ** xvals = (double**) R_alloc((size_t)xlen, sizeof(double*));
  // for(R_xlen_t i = 0; i < xlen; ++i) {
  //   if(TYPEOF(VECTOR_ELT(x, i)) != REALSXP)
  //     error("Argument `x[%d]` must be a double.", i);
  //   *(xvals + i) = REAL(VECTOR_ELT(x, i));
  // }
  DL_FUNC fun = NULL;
  const char * fun_char = CHAR(STRING_ELT(fun_name, 0));
  const char * dll_char = CHAR(STRING_ELT(so, 0));
  struct Rf_RegisteredNativeSymbol * symbol = NULL;
  Rprintf("finding symbol for '%s' '%s'", fun_char, dll_char);
  fun = R_FindSymbol(fun_char, dll_char, symbol);
  Rprintf("Calling fun");
  return (*fun)(x);
}
/*
 * This one passes naked C objects
 */
SEXP FAPPLY_run2(SEXP so, SEXP fun_name, SEXP x) {
  if(TYPEOF(so) != STRSXP || XLENGTH(so) != 1)
    error("Argument `so` should be a scalar string.");
  if(TYPEOF(fun_name) != STRSXP || XLENGTH(fun_name) != 1)
    error("Argument `fun_name` should be a scalar string.");
  if(TYPEOF(x) != VECSXP)
    error("Argument `x` should be a list.");

  R_xlen_t xlen = XLENGTH(x);
  if(xlen > INT_MAX)
    error("Arguent `x` may not contain more than INT_MAX items.");
  double **xvals = (double**) R_alloc((size_t)xlen, sizeof(double*));
  R_xlen_t *xlens = (R_xlen_t*) R_alloc((size_t)xlen, sizeof(R_xlen_t));
  for(R_xlen_t i = 0; i < xlen; ++i) {
    SEXP elt = VECTOR_ELT(x, i);
    if(TYPEOF(elt) != REALSXP)
      error("Argument `x[%d]` must be a double.", i);
    *(xvals + i) = REAL(elt);
    *(xlens + i) = XLENGTH(elt);
  }
  const char * fun_char = CHAR(STRING_ELT(fun_name, 0));
  const char * dll_char = CHAR(STRING_ELT(so, 0));
  struct Rf_RegisteredNativeSymbol * symbol = NULL;
  DL_FUNC fun = R_FindSymbol(fun_char, dll_char, symbol);
  double res = 0;
  (*fun)(xvals, xlens, (int) xlen, &res);
  return ScalarReal(res);
}
/*
 * Initial experiment with groups
 */
SEXP FAPPLY_run3(SEXP so, SEXP fun_name, SEXP x, SEXP g) {
  if(TYPEOF(so) != STRSXP || XLENGTH(so) != 1)
    error("Argument `so` should be a scalar string.");
  if(TYPEOF(fun_name) != STRSXP || XLENGTH(fun_name) != 1)
    error("Argument `fun_name` should be a scalar string.");
  if(TYPEOF(x) != VECSXP)
    error("Argument `x` should be a list.");
  if(TYPEOF(g) != INTSXP)
    error("Argument `g` should be an integer vector.");

  R_xlen_t xlen = XLENGTH(x);

  // Generate Naked Data structure
  if(xlen > INT_MAX)
    error("Arguent `x` may not contain more than INT_MAX items.");
  double **xvals = (double**) R_alloc((size_t)xlen, sizeof(double*));
  R_xlen_t *xlens = (R_xlen_t*) R_alloc((size_t)xlen, sizeof(R_xlen_t));
  for(R_xlen_t i = 0; i < xlen; ++i) {
    SEXP elt = VECTOR_ELT(x, i);
    if(TYPEOF(elt) != REALSXP)
      error(
        "Argument `x[%d]` must be a double is (%s).", i, type2char(TYPEOF(elt))
      );
    *(xvals + i) = REAL(elt);
    *(xlens + i) = XLENGTH(elt);
    if(i > 0 && *(xlens + i) != *(xlens + i -1))
      error("Argument `x` must contain equal length vectors.");  // for now
  }
  // compute group sizes (g should be ordered), two passes, first # of groups
  // then their sizes.  Could do one pass but then we need to alloc the whole
  // vector.
  int *g_int = INTEGER(g);
  R_xlen_t glen = XLENGTH(g);
  R_xlen_t gn = 1;  // At least one group, possibly zero sized

  // assumes all vectors are same length
  if(*xlens != glen) error("Argument `x` and `g` must be of equal length");
  if (glen > 1) {
    for(R_xlen_t gi = 1; gi < glen; ++gi)
      if(*(g_int + gi) != *(g_int + gi - 1)) ++gn;
  }
  R_xlen_t *gsize = (R_xlen_t*) R_alloc(gn, sizeof(R_xlen_t));
  R_xlen_t *goff = (R_xlen_t*) R_alloc(gn, sizeof(R_xlen_t));
  R_xlen_t *gsize_track = gsize;
  *goff = 0;
  R_xlen_t *goff_track = goff;
  if(glen == 1) *gsize = *xlens; // assumes all vectors are same length
  else if (glen > 1) {
    R_xlen_t gsize_i = 0;
    R_xlen_t gi;
    for(gi = 1; gi < glen; ++gi) {
      ++gsize_i;
      if(*(g_int + gi) != *(g_int + gi - 1)) {
        *(gsize_track++) = gsize_i;
        goff_track++;
        *goff_track = *(goff_track - 1) + gsize_i;
        gsize_i = 0;
      }
    }
    // One extra item in the trailing group we will not have counted
    *gsize_track = gsize_i + 1;
  }
  // Run for each group

  const char * fun_char = CHAR(STRING_ELT(fun_name, 0));
  const char * dll_char = CHAR(STRING_ELT(so, 0));
  struct Rf_RegisteredNativeSymbol * symbol = NULL;
  DL_FUNC fun = R_FindSymbol(fun_char, dll_char, symbol);
  SEXP res_sxp = PROTECT(allocVector(REALSXP, gn));
  double * res = REAL(res_sxp);

  for(R_xlen_t ggi = 0; ggi < gn; ++ggi) {
    (*fun)(xvals, *(gsize + ggi), *(goff + ggi), (int) xlen, res + ggi);
  }
  UNPROTECT(1);
  return res_sxp;
}

