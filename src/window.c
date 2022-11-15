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
  Rprintf("w %d o %d b %d\n", w_int, o_int, by_int);

  // This should be validated R level, but bad if wrong
  if(w_int < 0 || o_int < 0 || o_int >= w_int || by_int < 1)
    Rf_error(
      "Internal Error: bad window values w %d o %d b %d.", w_int, o_int, by_int
    );

  struct R2C_dat dp = prep_data(dat, dat_cols, ids, flag, ctrl, so);

  // Varying data index (not result index).  This always references the first
  // element of the window in-bounds of the data, so if `o_int` is 0
  // (align="left"), then this is also the base index of the window.
  R_xlen_t i = 0;
  R_xlen_t i_max = dp.lens[I_RES] * by_int; // `* by` to convert to data index

  // these will be stored 1-index, so double (see assumptions.c)
  double recycle_warn = 0;

  if(w_int > i_max) w_int = i_max;         // Window larger than data
  if(o_int > w_int - 1) o_int = w_int - 1; // Offset larger than data

  // Process incomplete windows on left, then complete, windows, then incomplete
  // on the right.  Hope branch pred / compiler will do right thing for(if())

  // ***************************************************************
  // ** LOOK AT group.c FOR MORE COMMENTS ON WHAT'S GOING ON HERE **
  // ***************************************************************

  // - Partial Left Stub -------------------------------------------------------

  // Index always at zero, and adjust varying vector length as more of the
  // window comes into range.

  double * dat_base = *(dp.data + I_RES);
  int w_left = w_int - o_int;
  Rprintf("w %d imax %d wl %d\n", w_int, i_max, w_left);
  Rprintf("  Data at %d\n", *(dp.data + I_RES) - dat_base);
  for(; w_left < w_int; i += by_int, w_left += by_int) {
    if(part_int) {
      for(int j = dp.dat_start; j <= dp.dat_end; ++j) dp.lens[j] = w_left;
      (*(dp.fun))(dp.data, dp.lens, dp.datai, dp.narg, dp.flags, dp.ctrl);
      if(dp.data[I_STAT][STAT_RECYCLE] && !recycle_warn)
        recycle_warn = (double)i + 1;
      if(dp.lens[I_RES] != 1)
        Rf_error("Window result size is not 1 (is %jd).", dp.lens[I_RES]);
    } else {
      **(dp.data + I_RES) = NA_REAL;
    }
    ++(*(dp.data + I_RES));
  }
  Rprintf(
    "  Data at %d ds %d de %d ires %d\n", *(dp.data + I_RES) - dat_base,
    dp.dat_start, dp.dat_end, I_RES
  );
  // - Complete Windows --------------------------------------------------------

  // Window size always the same, so set this once
  for(int j = dp.dat_start; j <= dp.dat_end; ++j) dp.lens[j] = w_int;
  // Possibly skipped some initial indices b/c of `by`
  int complete_start = w_left - w_int;
  for(int j = dp.dat_start; j <= dp.dat_end; ++j)
    *(dp.data + j) += complete_start;
  // Find limit of complete windows
  R_xlen_t i_max_main = i_max - (w_int - o_int - 1);

  Rprintf("w %d imax_main %d i %d\n", w_int, i_max_main, i);

  Rprintf("  Data at %d\n", *(dp.data + I_RES) - dat_base);
  for(; i < i_max_main; i += by_int) {
    (*(dp.fun))(dp.data, dp.lens, dp.datai, dp.narg, dp.flags, dp.ctrl);
    if(dp.data[I_STAT][STAT_RECYCLE] && !recycle_warn)
      recycle_warn = (double)i + 1;
    // For `by` == 1, we don't need this check since we don't dereference past
    // the end, but if `by > 1`, we end up beyond +1 of end so need to check
    if(i + by_int <= i_max)
      for(int j = dp.dat_start; j <= dp.dat_end; ++j) *(dp.data + j) += by_int;
    if(dp.lens[I_RES] != 1)
      Rf_error("Window result size is not 1 (is %jd).", dp.lens[I_RES]);
    ++(*(dp.data + I_RES));
  }
  Rprintf("  Data at %d\n", *(dp.data + I_RES) - dat_base);

  // - Partial Right Stub ------------------------------------------------------

  // Index shifting, reduce window size

  int w_right = i_max - i + o_int;  // in-bounds window shrinks

  Rprintf(
    "w %d imax %d imax_main %d i %d w_right %d\n", w_int, i_max, i_max_main, i,
    w_right
  );
  for(; i < i_max; i += by_int, w_right -= by_int) {
    if(part_int) {
      for(int j = dp.dat_start; j <= dp.dat_end; ++j) dp.lens[j] = w_right;
      Rprintf(
        "Woohoo len %d res %f\n", dp.lens[dp.dat_start], *(dp.data[I_RES])
      );
      (*(dp.fun))(dp.data, dp.lens, dp.datai, dp.narg, dp.flags, dp.ctrl);
      Rprintf(
        "Woohoo len %d res %f\n", dp.lens[dp.dat_start], *(dp.data[I_RES])
      );
      if(dp.data[I_STAT][STAT_RECYCLE] && !recycle_warn)
        recycle_warn = (double)i + 1;
      if(i + by_int <= i_max)
        for(int j = dp.dat_start; j <= dp.dat_end; ++j) *(dp.data + j) += by_int;
      if(dp.lens[I_RES] != 1)
        Rf_error("Window result size is not 1 (is %jd).", dp.lens[I_RES]);
    } else {
      **(dp.data + I_RES) = NA_REAL;
    }
    ++(*(dp.data + I_RES));
  }
  Rprintf("Data at %d\n", *(dp.data + I_RES) - dat_base);

  return Rf_ScalarReal((double) recycle_warn);
}


