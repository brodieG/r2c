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
 * Common Data Restructure Steps
 *
 * Shared by group and window functions.  Uses a small amount of R_alloc memory.
 */
struct R2C_dat prep_data(
  SEXP dat, SEXP dat_cols, SEXP ids, SEXP extn, SEXP so
) {
  if(TYPEOF(so) != STRSXP || XLENGTH(so) != 1)
    Rf_error("Argument `so` should be a scalar string.");
  if(TYPEOF(dat_cols) != INTSXP && XLENGTH(dat_cols) != 1)
    Rf_error("Argument `dat_cols` should be scalar integer.");
  if(TYPEOF(dat) != VECSXP)
    Rf_error("Argument `data` should be a list.");
  if(TYPEOF(ids) != VECSXP)
    Rf_error("Argument `ids` should be a list.");
  if(TYPEOF(extn) != VECSXP)
    Rf_error("Argument `extn` should be a list.");
  if(XLENGTH(ids) != XLENGTH(extn))
    Rf_error("Argument `ids` and `extn` should be the same length.");

  const char * fun_char = "run";
  const char * dll_char = CHAR(STRING_ELT(so, 0));
  struct Rf_RegisteredNativeSymbol * symbol = NULL;
  r2c_dl_fun fun = (r2c_dl_fun) R_FindSymbol(fun_char, dll_char, symbol);
  int dat_count = Rf_asInteger(dat_cols);

  // Not a foolproof check, but we need at least group varying cols + I_GRP data
  if(dat_count < 0 || dat_count > XLENGTH(dat) - I_GRP)
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
    // Each call accesses elements from data, where the last element receives
    // the result of the call (by convention).  datai is 0-indexed.
    *(datai + i) = INTEGER(elt);
    *(narg + i) = (int) XLENGTH(elt) - 1; // last elt result, so exclude
  }
  struct R2C_dat res = {
    .data = data,
    .datai = datai,
    .dat_start = I_GRP,
    .dat_end = I_GRP + dat_count - 1,
    .dat_count = dat_count,
    .narg = narg,
    .lens = lens,
    .extn = extn,
    .fun = fun
  };
  return res;
}
/*
 * Single run runner
 *
 * See prep_data and R2C_dat struct in r2c.h for more detail on what the
 * paramters correspond to.  See also R2C_run_group and R2C_run_window*.
 */

SEXP R2C_run_one(
  SEXP so,
  SEXP dat,
  SEXP dat_cols,
  SEXP ids,
  SEXP extn,
  SEXP res_len
) {
  if(TYPEOF(res_len) != REALSXP || XLENGTH(res_len) != 1)
    Rf_error("Argument `res_len` should be a scalar REALSXP.");

  struct R2C_dat dp = prep_data(dat, dat_cols, ids, extn, so);

  // ***************************************************************
  // ** LOOK AT group.c FOR MORE COMMENTS ON WHAT'S GOING ON HERE **
  // ***************************************************************

  double r_len = REAL(res_len)[0];
  double recycle_warn = 0;

  (*(dp.fun))(dp.data, dp.lens, dp.datai, dp.narg, dp.extn);
  if(dp.data[I_STAT][STAT_RECYCLE] && !recycle_warn) recycle_warn = 1;

  if(dp.lens[I_RES] != r_len)
    Rf_error(
      "Result size does not match expected (%jd vs expected %.0f).",
      (intmax_t) dp.lens[I_RES], r_len
    );

  return Rf_ScalarReal((double) recycle_warn);
}
