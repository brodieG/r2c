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
/*
 * Apply function by groups.
 *
 * See prep_data and R2C_dat struct for what many of the parameters are.
 */

SEXP R2C_run_group(
  SEXP so,
  SEXP dat,
  // How many of the columns of `dat` are of the group varying type.
  SEXP dat_cols,
  SEXP ids,
  SEXP flag,
  SEXP ctrl,
  // Size of each window / group
  SEXP grp_lens,
  SEXP res_lens
) {
  if(TYPEOF(grp_lens) != REALSXP)
    Rf_error("Argument `grp_lens` should be a real vector.");
  if(TYPEOF(res_lens) != REALSXP || XLENGTH(grp_lens) != XLENGTH(res_lens))
    Rf_error("Argument `res_lens` should REALSXP and same length as `grp_lens`.");

  struct R2C_dat dp = prep_data(dat, dat_cols, ids, flag, ctrl, so);

  double * g_lens = REAL(grp_lens);
  double * r_lens = REAL(res_lens);
  R_xlen_t g_count = XLENGTH(grp_lens);
  // these will be stored 1-index, so double (see assumptions.c)
  double recycle_warn = 0;

  if(g_count >= R_XLEN_T_MAX)
    Rf_error("Maximum allowed group count of %jd exceeded.", R_XLEN_T_MAX - 1);

  // Compute.  Result will be in `data[I_RES]` and is updated by reference
  for(R_xlen_t i = 0; i < g_count; ++i) {
    R_xlen_t g_len = (R_xlen_t) g_lens[i];  // rows in current group
    R_xlen_t r_len = (R_xlen_t) r_lens[i];  // group result size

    // Update group length and result length; note: `lens` will be updated by
    // each C function invoaked within (*fun), in the end leaving the final
    // result size in `lens[I_RES]`.
    for(int j = dp.dat_start; j <= dp.dat_end; ++j) dp.lens[j] = g_len;

    // Showtime.  This runs the compiled version of `get_c_code` output:
    (*(dp.fun))(dp.data, dp.lens, dp.datai, dp.narg, dp.flags, dp.ctrl);
    // Record recycling error if any, g_count < R_XLEN_T_MAX, so could use
    // R_xlen_t here, but being safe in case code changes in future
    if(dp.data[I_STAT][STAT_RECYCLE] && !recycle_warn)
      recycle_warn = (double)i + 1;

    // Increment the data pointers by group size; the last increment will be
    // one past end of data, but it will not be dereferenced so okay.
    for(int j = dp.dat_start; j <= dp.dat_end; ++j)
        *(dp.data + j) += g_len;
    if(dp.lens[I_RES] != r_len)
      Rf_error(
        "Group result size does not match expected (%jd vs expected %jd).",
        dp.lens[I_RES], r_len
      );

    // Increment to the next result slot; the last increment will be
    // one past end of data, but it will not be dereferenced so okay.
    *(dp.data + I_RES) += r_len;
  }
  return Rf_ScalarReal((double) recycle_warn);
}

