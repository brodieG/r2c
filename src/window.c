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

  // This should be validated R level, but bad if wrong
  int w_int = Rf_asInteger(width);
  int o_int = Rf_asInteger(offset);
  int b_int = Rf_asInteger(by);
  int part_int = Rf_asInteger(partial);

  struct R2C_dat dp = prep_data(dat, dat_cols, ids, flag, ctrl, so);

  double *res = dp.data[I_RES];

  R_xlen_t i_max = dp.lens[I_RES];
  R_xlen_t i;
  R_xlen_t recycle_warn = 0;  // these will be stored 1-index

  if(w_int > i_max) w_int = i_max;  // Window larger than data

  // We process first incomplete windows at the beginning (left) of the data,
  // then complete windows, and finally incomplete windows on the end (right).
  // The loop code is copied from group.c, look there for comments as to what
  // each line is doing.
  //
  // Incomplete windows are NA unless `partial` (part_int) is TRUE.

  // - Partial Left Stub -------------------------------------------------------

  int wleft_int = w_int - o_int;
  if(part_int) {
    for(i = 0; wleft_int < w_int; ++i, ++wleft_int) {
      for(int j = dp.dat_start; j <= dp.dat_end; ++j) dp.lens[j] = wleft_int;
      (*(dp.fun))(dp.data, dp.lens, dp.datai, dp.narg, dp.flags, dp.ctrl);
      if(dp.data[I_STAT][STAT_RECYCLE] && !recycle_warn)
        recycle_warn = i + 1;
      for(int j = dp.dat_start; j <= dp.dat_end; ++j) *(dp.data + j) += 1;
      if(dp.lens[I_RES] != 1)
        Rf_error("Window result size is not 1 (is %jd).", dp.lens[I_RES]);
      (*(dp.data + I_RES))++;
    }
  } else {
    for(i = 0; wleft_int < w_int; ++i, ++wleft_int) res[i] = NA_REAL;
  }
  // - Complete Windows --------------------------------------------------------

  R_xlen_t i_max_main = i_max;
  if(o_int < w_int - 1) i_max_main = i_max - (w_int - o_int);

  // Window size always teh same, so set this once
  for(int j = dp.dat_start; j <= dp.dat_end; ++j) dp.lens[j] = w_int;

  for(; i < i_max_main; ++i) {
    (*(dp.fun))(dp.data, dp.lens, dp.datai, dp.narg, dp.flags, dp.ctrl);
    if(dp.data[I_STAT][STAT_RECYCLE] && !recycle_warn)
      recycle_warn = i + 1; // g_count < R_XLEN_T_MAX
    for(int j = dp.dat_start; j <= dp.dat_end; ++j) *(dp.data + j) += 1;
    if(dp.lens[I_RES] != 1)
      Rf_error("Window result size is not 1 (is %jd).", dp.lens[I_RES]);
    (*(dp.data + I_RES))++;
  }
  // - Partial Right Stub ------------------------------------------------------

  int wright_int = w_int - o_int;
  if(part_int) {
    for(; i < i_max; ++i, --wright_int) {
      for(int j = dp.dat_start; j <= dp.dat_end; ++j) dp.lens[j] = wright_int;
      (*(dp.fun))(dp.data, dp.lens, dp.datai, dp.narg, dp.flags, dp.ctrl);
      if(dp.data[I_STAT][STAT_RECYCLE] && !recycle_warn)
        recycle_warn = i + 1;
      for(int j = dp.dat_start; j <= dp.dat_end; ++j) *(dp.data + j) += 1;
      if(dp.lens[I_RES] != 1)
        Rf_error("Window result size is not 1 (is %jd).", dp.lens[I_RES]);
      (*(dp.data + I_RES))++;
    }
  } else {
    for(; i < i_max; ++i, --wright_int) res[i] = NA_REAL;
  }

  return Rf_ScalarReal((double) recycle_warn);
}


