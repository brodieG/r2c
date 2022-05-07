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
SEXP FAPPLY_run3(SEXP so, SEXP fun_name, SEXP x, SEXP g, SEXP flag) {
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
    (*fun)(
      xvals, *(gsize + ggi), *(goff + ggi), (int) xlen, res + ggi,
      asInteger(flag)
    );
  }
  UNPROTECT(1);
  return res_sxp;
}
/*
 * Initial experiment with groups
 */
SEXP FAPPLY_run3a(SEXP so, SEXP fun_name, SEXP x, SEXP g, SEXP flag) {
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
    goff_track++;
    *goff_track = *(goff_track - 1) + *gsize_track;
  }
  // Run for each group

  const char * fun_char = CHAR(STRING_ELT(fun_name, 0));
  const char * dll_char = CHAR(STRING_ELT(so, 0));
  struct Rf_RegisteredNativeSymbol * symbol = NULL;
  DL_FUNC fun = R_FindSymbol(fun_char, dll_char, symbol);
  SEXP res_sxp = PROTECT(allocVector(REALSXP, gn));
  double * res = REAL(res_sxp);

  for(R_xlen_t ggi = 0; ggi < gn; ++ggi) {
    (*fun)(
      xvals, *(gsize + ggi), *(goff + ggi), (int) xlen, res + ggi,
      flag
    );
  }
  UNPROTECT(1);
  return res_sxp;
}
/*
 * Test bed for arithmetic operators
 */
SEXP FAPPLY_run3b(SEXP so, SEXP fun_name, SEXP x, SEXP g) {
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
      // Group changed, record prior group size (recall we start one lagged)
      if(*(g_int + gi) != *(g_int + gi - 1)) {
        *(gsize_track++) = gsize_i;
        goff_track++;
        *goff_track = *(goff_track - 1) + gsize_i;
        gsize_i = 0;
      }
    }
    // One extra item in the trailing group we will not have counted
    *gsize_track = gsize_i + 1;
    goff_track++;
    *goff_track = *(goff_track - 1) + *gsize_track;
  }
  // Run for each group

  const char * fun_char = CHAR(STRING_ELT(fun_name, 0));
  const char * dll_char = CHAR(STRING_ELT(so, 0));
  struct Rf_RegisteredNativeSymbol * symbol = NULL;
  DL_FUNC fun = R_FindSymbol(fun_char, dll_char, symbol);
  SEXP res_sxp = PROTECT(allocVector(REALSXP, glen));
  double * res = REAL(res_sxp);
  // Rprintf("results size %d\n", (int) XLENGTH(res_sxp));

  for(R_xlen_t ggi = 0; ggi < gn; ++ggi) {
    R_xlen_t size = *(gsize + ggi);
    (*fun)(xvals, size, size, *(goff + ggi), (int) xlen, res);
  }
  UNPROTECT(1);
  return res_sxp;
}
/*
 * Compute Group Data
 *
 * @param g sorted group indices
 */

SEXP FAPPLY_group_sizes(SEXP g) {
  if(TYPEOF(g) != INTSXP)
    error("Argument `g` should be an integer vector.");

  int prt = 0;
  int *g_int = INTEGER(g);
  R_xlen_t glen = XLENGTH(g);
  R_xlen_t gn = 1;  // At least one group, possibly zero sized

  if (glen > 1) {
    for(R_xlen_t gi = 1; gi < glen; ++gi)
      if(*(g_int + gi) != *(g_int + gi - 1)) ++gn;
  }
  SEXP gsize_sxp = PROTECT(allocVector(REALSXP, gn)); ++prt;
  SEXP goff_sxp = PROTECT(allocVector(REALSXP, gn)); ++prt;

  double *gsize = REAL(gsize_sxp);
  double *goff = REAL(goff_sxp);
  double gmax = 0;
  *goff = 0;
  if(glen == 1) *gsize = (double) glen;
  else if (glen > 1) {
    double gsize_i = 0;
    R_xlen_t gi;
    for(gi = 1; gi < glen; ++gi) {
      ++gsize_i;
      // Group changed, record prior group size (recall we start one lagged)
      if(*(g_int + gi) != *(g_int + gi - 1)) {
        *(gsize++) = gsize_i;
        if(gsize_i > gmax) gmax = gsize_i;
        goff++;
        *goff = *(goff - 1) + gsize_i;
        gsize_i = 0;
      }
    }
    // One extra item in the trailing group we will not have counted
    *gsize = gsize_i + 1;
    if(*gsize > gmax) gmax = *gsize;
  }
  SEXP res = PROTECT(allocVector(VECSXP, 3)); ++prt;
  SEXP gmax_sxp = PROTECT(ScalarReal(gmax)); ++prt;
  SET_VECTOR_ELT(res, 0, gsize_sxp);
  SET_VECTOR_ELT(res, 1, goff_sxp);
  SET_VECTOR_ELT(res, 2, gmax_sxp);
  PrintValue(res);
  UNPROTECT(prt);
  return res;
}


SEXP FAPPLY_run_internal(
  SEXP so,
  SEXP interface,  // what type of call interface into the runner
  SEXP dat,
  SEXP dat_cols,
  SEXP ids,
  SEXP ctrl,
  SEXP grp_lens,
  SEXP res_lens
) {
  Rprintf("start\n");
  if(TYPEOF(so) != STRSXP || XLENGTH(so) != 1)
    error("Argument `so` should be a scalar string.");
  if(TYPEOF(dat_cols) != INTSXP || XLENGTH(dat_cols) != 1)
    error("Argument `dat_cols` should be a scalar integer.");
  if(TYPEOF(interface) != INTSXP || XLENGTH(interface) != 1)
    error("Argument `interface` should be a scalar integer.");
  if(TYPEOF(grp_lens) != REALSXP)
    error("Argument `grp_lens` should be a real vector.");
  if(TYPEOF(res_lens) != REALSXP || XLENGTH(grp_lens) != XLENGTH(res_lens))
    error("Argument `res_lens` should REALSXP and same length as `grp_lens`.");
  if(TYPEOF(dat) != VECSXP)
    error("Argument `data` should be a list.");
  if(TYPEOF(ids) != VECSXP)
    error("Argument `ids` should be a list.");
  if(TYPEOF(ctrl) != VECSXP)
    error("Argument `ctrl` should be a list.");

  const char * fun_char = "run";
  const char * dll_char = CHAR(STRING_ELT(so, 0));
  struct Rf_RegisteredNativeSymbol * symbol = NULL;
  DL_FUNC fun = R_FindSymbol(fun_char, dll_char, symbol);
  int dat_count = asInteger(dat_cols);
  R_xlen_t g_count = XLENGTH(grp_lens);
  double * g_lens = REAL(grp_lens);
  double * r_lens = REAL(grp_lens);
  int intrf = asInteger(interface);

  // Data columns are first, followed by result, and then external cols.
  // This check incompleted if there are external cols.
  if(dat_count < 0 || dat_count > XLENGTH(dat) - 1)
    error("Internal Error: bad data col count.");

  Rprintf("restructure\n");
  // Retructure data to be seakable without overhead of VECTOR_ELT
  // R_alloc not guaranteed to align to pointers, blergh. FIXME.
  double ** data = (double **) R_alloc(XLENGTH(dat), sizeof(double*));
  R_xlen_t * lens = (R_xlen_t *) R_alloc(XLENGTH(dat), sizeof(R_xlen_t));
  for(R_xlen_t i = 0; i < XLENGTH(dat); ++i) {
    SEXP elt = VECTOR_ELT(dat, i);
    if(TYPEOF(elt) != REALSXP)
      error(
        "Internal Error: non-real data at %jd (%s).\n",
        (intmax_t) i, type2char(TYPEOF(elt))
      );
    *(data + i) = REAL(elt);
    *(lens + i) = XLENGTH(elt);
  }
  // Indices into data, should be as many as there are calls in the code
  int ** datai = (int **) R_alloc(XLENGTH(ids), sizeof(int*));
  int * narg = (int *) R_alloc(XLENGTH(ids), sizeof(int));
  for(R_xlen_t i = 0; i < XLENGTH(ids); ++i) {
    SEXP elt = VECTOR_ELT(ids, i);
    if(TYPEOF(elt) != INTSXP)
      error(
        "Internal Error: non-integer data at %jd (%s).\n", 
        (intmax_t) i, type2char(TYPEOF(elt))
      );
    *(datai + i) = INTEGER(elt);
    *(narg + i) = (int) XLENGTH(elt);
  }
  // Compute.  Resuls is in `data[dat_count]` and is updated by reference
  Rprintf("go\n");
  for(R_xlen_t i = 0; i < g_count; ++i) {
    R_xlen_t g_len = (R_xlen_t) g_lens[i];  // rows in group
    R_xlen_t r_len = (R_xlen_t) r_lens[i];  // final output size

    // Update group length and result length
    for(int j = 0; j < dat_count; ++j) lens[j] = g_len;
    lens[dat_count] = r_len;

    // There are four possible interfaces (this is to avoid unused argument
    // compiler warnings; now wondering if there is a better way to do that).
    Rprintf("interface %d\n", intrf);
    switch(intrf) {  // Hopefully compiler unrolls this out of the loop
      case 1: (*fun)(data, datai, lens); break;
      case 2: (*fun)(data, datai, lens, narg); break;
      case 3: (*fun)(data, datai, lens, ctrl); break;
      case 4: (*fun)(data, datai, lens, narg, ctrl); break;
      default: error("Internal Error: invalid interface specified.");
    }
    // Increment the data pointers by group size; the last increment will be
    // one past end of data, but it will not be dereferenced so okay
    for(int j = 0; j < dat_count; ++j) *(data + j) += g_len;
    *(data + dat_count) += r_len;
  }
  return R_NilValue;
}
