/*
 * Copyright (C) 2022  Brodie Gaslam
 *
 * This file is part of "r2c - Fast Iterated Statistics in R"
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

// Taken from Rdynpriv.h; we'll need to work around this
// Dummy struct, per C99 all pointers to struct are the same size, and since
// we're just going to pass a NULL pointer which R_FindSymbol checks for this
// will work.  Of course not part of the APIS so this can't be how we do the
// final implementation.

struct Rf_RegisteredNativeSymbol {
    int dummy;
};

/*
 * Compute Group Data
 *
 * @param g sorted group indices
 */

SEXP R2C_group_sizes(SEXP g) {
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
  UNPROTECT(prt);
  return res;
}

SEXP R2C_run_internal(
  SEXP so,
  SEXP interface,  // what type of call interface into the runner
  SEXP dat,
  SEXP dat_cols,
  SEXP ids,
  SEXP flag,
  SEXP ctrl,
  SEXP grp_lens,
  SEXP res_lens
) {
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
  if(TYPEOF(flag) != INTSXP)
    error("Argument `flag` should be an integer vector.");
  if(XLENGTH(ids) != XLENGTH(ctrl))
    error("Argument `ids` and `ctrl` should be the same length.");
  if(XLENGTH(flag) != XLENGTH(ctrl))
    error("Argument `flag` and `ctrl` should be the same length.");

  const char * fun_char = "run";
  const char * dll_char = CHAR(STRING_ELT(so, 0));
  struct Rf_RegisteredNativeSymbol * symbol = NULL;
  DL_FUNC fun = R_FindSymbol(fun_char, dll_char, symbol);
  int dat_count = asInteger(dat_cols);
  R_xlen_t g_count = XLENGTH(grp_lens);
  double * g_lens = REAL(grp_lens);
  double * r_lens = REAL(res_lens);
  int intrf = asInteger(interface);

  // Data columns are first, followed by result, and then external cols.
  // This check incompleted if there are external cols.
  if(dat_count < 0 || dat_count > XLENGTH(dat) - 1)
    error("Internal Error: bad data col count.");

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
  R_xlen_t call_count = XLENGTH(ids);
  for(R_xlen_t i = 0; i < call_count; ++i) {
    SEXP elt = VECTOR_ELT(ids, i);
    if(TYPEOF(elt) != INTSXP)
      error(
        "Internal Error: non-integer data at %jd (%s).\n",
        (intmax_t) i, type2char(TYPEOF(elt))
      );
    *(datai + i) = INTEGER(elt);
    *(narg + i) = (int) XLENGTH(elt);
  }
  // Integer flags representing TRUE/FALSE control parameters
  int * flag_int = INTEGER(flag);

  // Compute.  Resuls is in `data[dat_count]` and is updated by reference
  for(R_xlen_t i = 0; i < g_count; ++i) {
    R_xlen_t g_len = (R_xlen_t) g_lens[i];  // rows in group
    R_xlen_t r_len = (R_xlen_t) r_lens[i];  // final output size

    // Update group length and result length; note: `lens` should be updated by
    // the C functions to reflect the size of the intermediate results
    for(int j = 0; j < dat_count; ++j) lens[j] = g_len;

    // This got out of hand, was trying to avoid compiler warnings
    switch(intrf) {
      case 1: (*fun)(data, lens, datai); break;
      case 2: (*fun)(data, lens, datai, narg); break;
      case 3: (*fun)(data, lens, datai, flag_int); break;
      case 4: (*fun)(data, lens, datai, ctrl); break;
      case 5: (*fun)(data, lens, datai, flag_int, ctrl); break;
      case 6: (*fun)(data, lens, datai, narg, flag_int); break;
      case 7: (*fun)(data, lens, datai, narg, ctrl); break;
      case 8: (*fun)(data, lens, datai, narg, flag_int, ctrl); break;
      default: error("Internal Error: invalid interface specified.");
    }
    // Increment the data pointers by group size; the last increment will be
    // one past end of data, but it will not be dereferenced so okay
    for(int j = 0; j < dat_count; ++j) *(data + j) += g_len;
    if(lens[dat_count] != r_len)
      error(
        "Group result size does not match expected (%ju vs expected %ju).",
        lens[dat_count], r_len
      );
    *(data + dat_count) += r_len;
  }
  return R_NilValue;
}
