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
 * Initial implementation requires size 1 output
 */
SEXP R2C_run_window(
  SEXP so,
  SEXP dat,
  // How many of the columns of `dat` are of the group varying type.
  SEXP dat_cols,
  SEXP ids,
  SEXP flag,
  SEXP ctrl,
  SEXP width,
  SEXP offset,
  SEXP by,
  SEXP partial
) {
  if(TYPEOF(width) != INTSXP || XLENGTH(width) != 1L)
    Rf_error("Argument `width` should be scalar integer.");
  if(TYPEOF(offset) != INTSXP || XLENGTH(offset) != 1L)
    Rf_error("Argument `offset` should be scalar integer.");
  if(TYPEOF(by) != INTSXP || XLENGTH(by) != 1L)
    Rf_error("Argument `by` should be scalar integer.");
  if(TYPEOF(partial) != LGLSXP || XLENGTH(partial) != 1L)
    Rf_error("Argument `partial` should be scalar logical.");

  int w_int = Rf_asInteger(width);
  int o_int = Rf_asInteger(offset);
  int by_int = Rf_asInteger(by);
  int part_int = Rf_asInteger(partial);

  // This should be validated R level, but bad if wrong
  if(w_int < 0 || o_int < 0 || o_int >= w_int || by_int < 1)
    Rf_error(
      "Internal Error: bad window values w %d o %d b %d.", w_int, o_int, by_int
    );

  struct R2C_dat dp = prep_data(dat, dat_cols, ids, flag, ctrl, so);

  // these will be stored 1-index, so double (see assumptions.c)
  double recycle_warn = 0;

  // Make a copy of the base data pointers
  double ** dat_base = (double **) R_alloc(dp.dat_end, sizeof(double *));
  for(int j = dp.dat_start; j <= dp.dat_end; ++j) dat_base[j] = dp.data[j];

  // ***************************************************************
  // ** LOOK AT group.c FOR MORE COMMENTS ON WHAT'S GOING ON HERE **
  // ***************************************************************
  //
  // Logic internal to the loops (all the pointer and offset stuff) is copied
  // from there, and it is documented there.

  R_xlen_t d_size = dp.lens[dp.dat_start];
  R_xlen_t start = 0; // start of window, reset to 0 if OOB based on offset
  R_xlen_t len = 0;   // length of window, trim data if OOB based on start
  R_xlen_t i = 0;     // base index, will scan through the data vector(s)

  // This is not super optimized.  There are common special cases (e.g. window
  // fully in bounds) that require less work than those partially out of bounds,
  // so we could split the loop.  In testing though there does not seem to be
  // that much overhead, as it's only a significant portion of the work with big
  // windows, and if windows are big, the actual function evaluation will be
  // slow relative to the bookkeeping of the start/len variables.

  for(i = 0; i < d_size; i += by_int) {
    int incomplete = 0;
    start = i - o_int;
    len = w_int;
    if(start < 0) {             // OOB due to offset
      len += start;
      start = 0;
      incomplete = 1;
    }
    if(len + start > d_size) {  // OOB due to start/width/offset
      len = d_size - start;
      incomplete = 1;
    }
    if(part_int || !incomplete) {
      for(int j = dp.dat_start; j <= dp.dat_end; ++j) {
        dp.data[j] = dat_base[j] + start;
        dp.lens[j] = len;
      }
      (*(dp.fun))(dp.data, dp.lens, dp.datai, dp.narg, dp.flags, dp.ctrl);
      // Unlike with groups, either none or all would warn, so we shouldn't have
      // to check every single step.
      if(dp.data[I_STAT][STAT_RECYCLE] && !recycle_warn)
        recycle_warn = (double)start + 1;
      if(dp.lens[I_RES] != 1)
        Rf_error("Window result size is not 1 (is %jd).", dp.lens[I_RES]);
    } else {
      **(dp.data + I_RES) = NA_REAL;
    }
    ++(*(dp.data + I_RES));
  }
  return Rf_ScalarReal((double) recycle_warn);
}

SEXP R2C_run_window_i(
  SEXP so,
  SEXP dat,
  // How many of the columns of `dat` are of the group varying type.
  SEXP dat_cols,
  SEXP ids,
  SEXP flag,
  SEXP ctrl,
  SEXP width,
  SEXP offset,
  SEXP by_sxp,
  SEXP index_sxp,
  SEXP start_sxp,
  SEXP end_sxp
) {
  if(TYPEOF(width) != REALSXP || XLENGTH(width) != 1L)
    Rf_error("Argument `width` should be scalar integer.");
  if(TYPEOF(offset) != REALSXP || XLENGTH(offset) != 1L)
    Rf_error("Argument `offset` should be scalar integer.");
  if(TYPEOF(by_sxp) != REALSXP || XLENGTH(by_sxp) != 1L)
    Rf_error("Argument `by` should be scalar integer.");
  if(TYPEOF(index_sxp) != REALSXP)
    Rf_error("Argument `index` should be numeric.");

  double w = Rf_asReal(width);
  double o = Rf_asReal(offset);
  double by = Rf_asReal(by_sxp);
  double start = Rf_asReal(start_sxp);
  double end = Rf_asReal(end_sxp);

  // This should be validated R level, but bad if wrong
  if(
    w < 0 || o < 0 || o > w || by < 0 || start > end ||
    isnan(w) || isnan(o) || isnan(by) || isnan(start) || isnan(end) ||
    isnan(w) || isnan(o) || isnan(by) || isnan(start) || isnan(end) ||
    !isfinite(w) || !isfinite(o) || !isfinite(by) || !isfinite(start) ||
    !isfinite(end)
  )
    Rf_error(
      "Internal Error: bad window values w %f o %f b %f start %f end %f.",
      w, o, by, start, end
    );

  // Shift start/end by offset, should be equivalent
  start = start - o;
  end = end - o;
  if(!isfinite(start))
    Rf_error(
      "`start` and `align` values create a negative infinity left bound, ",
      "which is disallowed."
    );

  struct R2C_dat dp = prep_data(dat, dat_cols, ids, flag, ctrl, so);
  double * index = REAL(index_sxp);
  R_xlen_t ilen = XLENGTH(index_sxp);

  // these will be stored 1-index, so double (see assumptions.c)
  double recycle_warn = 0;

  // Make a copy of the base data pointers
  double ** dat_base = (double **) R_alloc(dp.dat_end, sizeof(double *));
  for(int j = dp.dat_start; j <= dp.dat_end; ++j) dat_base[j] = dp.data[j];

  // *******************************************************************
  // ** LOOK AT window_exec FOR MORE COMMENTS ON WHAT'S GOING ON HERE **
  // *******************************************************************

  R_xlen_t ileft = 0; // base index, will scan through the data vector(s)
  R_xlen_t iright = 0;// base index, will scan through the data vector(s)

  // This is not optimized:
  // * Ordered nature of the index should allow binary search
  // * Could have many empty windows at start and end that should be handled in
  //   different loops.

  double left, right;

  for(left = start; left <= end; left += by) {
    while(ileft < ilen && index[ileft] < left) ++ileft;
    iright = ileft;
    right = left + w;
    while(iright < ilen && index[iright] < right) ++iright;

    if(index[ileft] > right || index[iright] < left) {
      // Could have long periods when this is true, could have separate loops
      // for those at the beginning and end
      for(int j = dp.dat_start; j <= dp.dat_end; ++j) dp.lens[j] = 0;
    } else {
      R_xlen_t len = iright - ileft + 1;
      for(int j = dp.dat_start; j <= dp.dat_end; ++j) {
        dp.data[j] = dat_base[j] + ileft;
        dp.lens[j] = len;
    } }
    (*(dp.fun))(dp.data, dp.lens, dp.datai, dp.narg, dp.flags, dp.ctrl);
    // Unlike with groups, either none or all would warn, so we shouldn't have
    // to check every single step.
    if(dp.data[I_STAT][STAT_RECYCLE] && !recycle_warn)
      recycle_warn = (double)start + 1;
    if(dp.lens[I_RES] != 1)
      Rf_error("Window result size is not 1 (is %jd).", dp.lens[I_RES]);
    ++(*(dp.data + I_RES));
  }
  return Rf_ScalarReal((double) recycle_warn);
}


