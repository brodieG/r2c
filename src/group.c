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
#include "loop-interrupt-basic.h"
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
  R_xlen_t gn = 0;
  // we increment gn from 1 up, so need to start at 1 if have groups
  gn = glen > 0;

  int g_int_val = *g_int;
  for(R_xlen_t gi = 1; gi < glen; ++gi) {
    int g_int_prev_val = g_int_val;
    g_int_val = *(++g_int);
    if(g_int_val != g_int_prev_val) ++gn;
  }
  SEXP gsize_sxp = PROTECT(Rf_allocVector(REALSXP, gn)); ++prt;
  SEXP glabs_sxp = PROTECT(Rf_allocVector(INTSXP, gn)); ++prt;
  g_int = INTEGER(g);

  double *gsize = REAL(gsize_sxp);
  int *glabs = INTEGER(glabs_sxp);
  double gmax = 0;
  double gmin = glen;

  if(glen == 1) {
    *gsize = (double) glen;
    *glabs = *g_int;
  } else if (glen > 1) {
    double gsize_i = 0;
    int g_int_val = *g_int;
    *(glabs++) = g_int_val;

    R_xlen_t i = 1;
    R_xlen_t next_interrupt = INTERRUPT_AT;                            \
    LOOP_W_INTERRUPT_BASIC(
      glen, {
        int g_int_prev_val = g_int_val;
        g_int_val = *(++g_int);
        ++gsize_i;
        // Group changed, record prior group size (recall we start one lagged)
        if(g_int_val != g_int_prev_val) {
          *(gsize++) = gsize_i;
          if(gsize_i > gmax) gmax = gsize_i;
          if(gsize_i < gmin || i == 1) gmin = gsize_i;
          *(glabs++) = g_int_val;
          gsize_i = 0;
        }
      }
    );
    // One extra item in the trailing group we will not have counted
    *gsize = gsize_i + 1;
    if(*gsize > gmax) gmax = *gsize;
    if(*gsize < gmin) gmin = *gsize;
  }
  SEXP res = PROTECT(Rf_allocVector(VECSXP, 4)); ++prt;
  SEXP gmax_sxp = PROTECT(Rf_ScalarReal(gmax)); ++prt;
  SEXP gmin_sxp = PROTECT(Rf_ScalarReal(gmin)); ++prt;
  SET_VECTOR_ELT(res, 0, gsize_sxp);
  SET_VECTOR_ELT(res, 1, glabs_sxp);
  SET_VECTOR_ELT(res, 2, gmax_sxp);
  SET_VECTOR_ELT(res, 3, gmin_sxp);
  UNPROTECT(prt);
  return res;
}
/*
 * Given vectors of possible sizes, for each set of elements compute the result
 * size according to vecrec semantics.
 *
 * Not strictly group specific, but only used by groups so far.
 */
SEXP R2C_vecrec_pmax(SEXP sizes) {
  if(TYPEOF(sizes) != VECSXP)
    Rf_error("Internal Error: `sizes` should be a list.");

  R_xlen_t slen = XLENGTH(sizes);
  SEXP elt0 = VECTOR_ELT(sizes, 0);
  if(TYPEOF(elt0) != REALSXP)
    Rf_error("Internal Error: `sizes` may contain only real vectors.");
  SEXP res;

  if(slen < 1) {
    Rf_error("Internal Error: `sizes` must contain at least one element.");
  } else if(slen == 1) {
    // Simplest case, no ambiguity
    res = PROTECT(elt0);
  } else {
    // Pick largets value, unless zero, in which case zero
    R_xlen_t elt0_len = XLENGTH(elt0);
    for(R_xlen_t i = 1; i < slen; ++i) {
      SEXP elt = VECTOR_ELT(sizes, i);
      if(TYPEOF(elt) != TYPEOF(elt0))
        Rf_error("Internal Error: `sizes` may contain only real vectors.");
      if(XLENGTH(elt) != elt0_len)
        Rf_error("Internal Error: `sizes` may contain only equal-size vectors.");
    }
    res = PROTECT(Rf_allocVector(REALSXP, elt0_len));
    double * dres = REAL(res);
    // Simple logic, iterate across every vector, first iteration is between
    // first two, each subsequent one is between result and next one.

    R_xlen_t next_interrupt = INTERRUPT_AT;                            \
    for(R_xlen_t j = 1; j < slen; ++j) {
      double *a, *b;
      if(j > 1) a = REAL(res); else a = REAL(elt0);
      b = REAL(VECTOR_ELT(sizes, j));

      // Lose some efficiency trying to make sure the interrupt count carries
      // across the elements (needs ioff).
      R_xlen_t i, ioff;
      i = ioff = (j - 1) * elt0_len;
      LOOP_W_INTERRUPT_BASIC(elt0_len * j, {
        R_xlen_t i0 = i - ioff;
        double da = *(a + i0);
        double db = *(b + i0);
        if(da && db) {
          if(da >=  db) *(dres + i0) = da; else *(dres + i0) = db;
        } else *(dres + i0) = 0;
      });
    }
  }
  UNPROTECT(1);
  return res;
}
/*
 * Apply function by groups.
 *
 * See prep_data and R2C_dat struct in r2c.h for more detail on what the
 * paramters correspond to.
 */

SEXP R2C_run_group(
  SEXP so,
  SEXP dat,
  SEXP dat_cols,
  SEXP ids,
  SEXP extn,
  // Size of each window / group
  SEXP grp_lens,
  SEXP res_lens
) {
  if(TYPEOF(grp_lens) != REALSXP)
    Rf_error("Argument `grp_lens` should be a real vector.");
  if(TYPEOF(res_lens) != REALSXP || XLENGTH(grp_lens) != XLENGTH(res_lens))
    Rf_error(
      "Argument `res_lens` should be REALSXP and same length as `grp_lens`."
    );

  struct R2C_dat dp = prep_data(dat, dat_cols, ids, extn, so);

  double * g_lens = REAL(grp_lens);
  double * r_lens = REAL(res_lens);
  R_xlen_t g_count = XLENGTH(grp_lens);
  // these will be stored 1-index, so double (see assumptions.c)
  double recycle_warn = 0;
  R_xlen_t interrupt_i = 0;

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

    // Check for interrupts (too complicated for _INTERRUPT_BASIC)
    if(
      g_len <= INTERRUPT_AT &&  /* we don't know what R_XLEN_T_MIN is */
      interrupt_i <= INTERRUPT_AT - g_len && interrupt_i <= R_XLEN_T_MAX - g_len
    ) {
      interrupt_i += g_len;
    } else  {
      interrupt_i = 0;
      R_CheckUserInterrupt();
    }
    // Showtime.  This runs the compiled version of `get_c_code` output:
    (*(dp.fun))(dp.data, dp.lens, dp.datai, dp.narg, dp.extn);
    // Record recycling error if any, g_count < R_XLEN_T_MAX, so could use
    // R_xlen_t here, but being safe in case code changes in future
    if(dp.data[I_STAT][STAT_RECYCLE] && !recycle_warn)
      recycle_warn = (double)i + 1;

    // Increment the data pointers by group size; the last increment will be
    // one past end of data, but it will not be dereferenced so okay.
    for(int j = dp.dat_start; j <= dp.dat_end; ++j)
        *(dp.data + j) += g_len;
    // This warning is too late but if it works it is helpful in debugging
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

