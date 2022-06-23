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

extern struct const_dat consts[];
extern int CONST_N;

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
    Rf_error("Argument `g` should be an integer vector.");

  int prt = 0;
  int *g_int = INTEGER(g);
  R_xlen_t glen = XLENGTH(g);
  R_xlen_t gn = 1;  // At least one group, possibly zero sized

  if (glen > 1) {
    for(R_xlen_t gi = 1; gi < glen; ++gi)
      if(*(g_int + gi) != *(g_int + gi - 1)) ++gn;
  }
  SEXP gsize_sxp = PROTECT(Rf_allocVector(REALSXP, gn)); ++prt;
  SEXP goff_sxp = PROTECT(Rf_allocVector(REALSXP, gn)); ++prt;

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
  SEXP res = PROTECT(Rf_allocVector(VECSXP, 3)); ++prt;
  SEXP gmax_sxp = PROTECT(Rf_ScalarReal(gmax)); ++prt;
  SET_VECTOR_ELT(res, 0, gsize_sxp);
  SET_VECTOR_ELT(res, 1, goff_sxp);
  SET_VECTOR_ELT(res, 2, gmax_sxp);
  UNPROTECT(prt);
  return res;
}

SEXP R2C_run_internal(
  SEXP so,
  // starts with the status vector, followed by the result vector, then followed
  // by the group varying data, and finally any collected external references
  // afterwards.
  SEXP dat,
  // How many of the columns of `dat` are of the group varying type.
  SEXP dat_cols,
  SEXP ids,
  SEXP flag,
  SEXP ctrl,
  SEXP grp_lens,
  SEXP res_lens
) {
  if(TYPEOF(so) != STRSXP || XLENGTH(so) != 1)
    Rf_error("Argument `so` should be a scalar string.");
  if(TYPEOF(dat_cols) != INTSXP)
    Rf_error("Argument `dat_cols` should be scalar integer.");
  if(TYPEOF(grp_lens) != REALSXP)
    Rf_error("Argument `grp_lens` should be a real vector.");
  if(TYPEOF(res_lens) != REALSXP || XLENGTH(grp_lens) != XLENGTH(res_lens))
    Rf_error("Argument `res_lens` should REALSXP and same length as `grp_lens`.");
  if(TYPEOF(dat) != VECSXP)
    Rf_error("Argument `data` should be a list.");
  if(TYPEOF(ids) != VECSXP)
    Rf_error("Argument `ids` should be a list.");
  if(TYPEOF(ctrl) != VECSXP)
    Rf_error("Argument `ctrl` should be a list.");
  if(TYPEOF(flag) != INTSXP)
    Rf_error("Argument `flag` should be an integer vector.");
  if(XLENGTH(ids) != XLENGTH(ctrl))
    Rf_error("Argument `ids` and `ctrl` should be the same length.");
  if(XLENGTH(flag) != XLENGTH(ctrl))
    Rf_error("Argument `flag` and `ctrl` should be the same length.");

  const char * fun_char = "run";
  const char * dll_char = CHAR(STRING_ELT(so, 0));
  struct Rf_RegisteredNativeSymbol * symbol = NULL;
  DL_FUNC fun = R_FindSymbol(fun_char, dll_char, symbol);
  int dat_count = Rf_asInteger(dat_cols);
  R_xlen_t g_count = XLENGTH(grp_lens);
  if(g_count >= R_XLEN_T_MAX)
    Rf_error("Maximum allowed group count of %jd exceeded.", R_XLEN_T_MAX - 1);
  double * g_lens = REAL(grp_lens);
  double * r_lens = REAL(res_lens);

  // Not a foolproof check, but we need at least group varying cols + 2 data
  if(dat_count < 0 || dat_count > XLENGTH(dat) - 2)
    Rf_error("Internal Error: bad data col count.");

  // Retructure data to be seakable without overhead of VECTOR_ELT
  // R_alloc not guaranteed to align to pointers, blergh. FIXME.
  double ** data = (double **) R_alloc(XLENGTH(dat), sizeof(double*));
  R_xlen_t * lens = (R_xlen_t *) R_alloc(XLENGTH(dat), sizeof(R_xlen_t));
  for(R_xlen_t i = 0; i < XLENGTH(dat); ++i) {
    SEXP elt = VECTOR_ELT(dat, i);
    if(TYPEOF(elt) != REALSXP)
      Rf_error(
        "Internal Error: non-real data at %jd (%s).\n",
        (intmax_t) i, Rf_type2char(TYPEOF(elt))
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
      Rf_error(
        "Internal Error: non-integer data at %jd (%s).\n",
        (intmax_t) i, Rf_type2char(TYPEOF(elt))
      );
    // Each call accesses some numbers of elements from data, where the last one
    // is the result of evaluating the call (by convention)
    *(datai + i) = INTEGER(elt);
    *(narg + i) = (int) XLENGTH(elt) - 1;
  }
  // Integer flags representing TRUE/FALSE control parameters
  int * flag_int = INTEGER(flag);
  int dat_start = I_GRP;
  int dat_end = I_GRP + dat_count - 1;
  R_xlen_t grp_recycle_warn = 0;  // these will be stored 1-index

  // Compute.  Result will be in `data[I_RES]` and is updated by reference
  for(R_xlen_t i = 0; i < g_count; ++i) {
    R_xlen_t g_len = (R_xlen_t) g_lens[i];  // rows in current group
    R_xlen_t r_len = (R_xlen_t) r_lens[i];  // group result size

    // Update group length and result length; note: `lens` should be updated by
    // the C functions to reflect the size of the intermediate results
    for(int j = dat_start; j <= dat_end; ++j) lens[j] = g_len;

    (*fun)(data, lens, datai, narg, flag_int, ctrl);

    // Record recycling error if any
    if(data[I_STAT][STAT_RECYCLE] && !grp_recycle_warn)
      grp_recycle_warn = i + 1; // g_count < R_XLEN_T_MAX

    // Increment the data pointers by group size; the last increment will be
    // one past end of data, but it will not be dereferenced so okay.
    for(int j = dat_start; j <= dat_end; ++j) *(data + j) += g_len;
    if(lens[I_RES] != r_len)
      Rf_error(
        "Group result size does not match expected (%jd vs expected %jd).",
        lens[I_RES], r_len
      );

    // Increment to the next result slot; the last increment will be
    // one past end of data, but it will not be dereferenced so okay.
    *(data + I_RES) += r_len;
  }
  // Return the recycle status flag; if we add more in the future we'll need to
  // adapt the logic a little for multiple flags
  return Rf_ScalarReal((double) grp_recycle_warn);
}
