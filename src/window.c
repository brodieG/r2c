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

  // these will be stored 1-index, so double (see assumptions.c)
  double recycle_warn = 0;

  // There are (at least) two possible solutions for dealing with the window
  // being OOB on either side of the data.  Either we handle each of the four
  // possibilities (1 - both ends OOB, 2,3 - one end OOB, 4 - neither), or we
  // remap the ends into a domain that is all inbounds, and for each loop
  // iteration remap back to the data domain.
  //
  // We chose the first approach so we don't have to worry about overflows and
  // data type conversions / compatibility, etc.  This comes at the cost of code
  // repeated four times and all the drawbacks that has.
  //
  // Remap width to end point.  Offset (o_int) then represents how many window
  // elements are oob to the "left" of the data in the first iteration.

  // ***************************************************************
  // ** LOOK AT group.c FOR MORE COMMENTS ON WHAT'S GOING ON HERE **
  // ***************************************************************
  //
  // Logic internal to the loops (all the pointer and offset stuff) is copied
  // from there, and it is documented there.

  R_xlen_t d_size = dp.lens[dp.dat_start];
  R_xlen_t end = w_int - o_int;
  R_xlen_t start = 0;
  R_xlen_t len = 0;
  R_xlen_t i = 0;

  // Adjust initial value of the window and offset to fit data
  if(end >= d_size) end = d_size - 1;
  if(o_int >= d_size) o_int = d_size - 1;

  Rprintf(
    "d_size %d o_int %d start %d end %d len %d\n",
    d_size, o_int, start, end, len
  );
  // At most three of the sections will execute for any given window/offset/data
  // combination (e.g. you can't have a combo that will be Both OOB and Both IB)
  // We have not spent much time trying to optimize what follows.  Hope compiler
  // and branch prediction are good with all the nested business.

  // - Left OOB Right IB -------------------------------------------------------

  Rprintf("L-OOB R-IB\n");
  cond1 = (d_size - len) / by_int;
  cond2 = o_int / by_int;


  len = end + 1;
  for(; o_int > 0 && len <= d_size; o_int -= by_int, len += by_int) {
    Rprintf("  o %d start %d end %d len %d\n", o_int, start, end, len);
    if(part_int) {
      for(int j = dp.dat_start; j <= dp.dat_end; ++j) dp.lens[j] = len;
      (*(dp.fun))(dp.data, dp.lens, dp.datai, dp.narg, dp.flags, dp.ctrl);
      if(dp.data[I_STAT][STAT_RECYCLE] && !recycle_warn)
        recycle_warn = (double)start + 1;
      if(dp.lens[I_RES] != 1)
        Rf_error("Window result size is not 1 (is %jd).", dp.lens[I_RES]);
    } else {
      **(dp.data + I_RES) = NA_REAL;
    }
    ++(*(dp.data + I_RES));
  }
  if(len > d_size) len = d_size;
  end = len - 1;
  i = 

  // - Both OOB ----------------------------------------------------------------

  // PROBLEM WITH CURRENT APPROACH IS WE'RE NOT TRACKING HOW FAR THROUGH THE
  // DATA OUR INDEX HAS MOVED, SO WE DON'T KNOW WHEN TO STOP.  THIS IS REALLY A
  // HUGE MESS, SERIOUSLY RECONSIDER WHETHER WE NEED TO RECAST EVERYTHING INTO A
  // SPACE WE CAN SAFELY ITERATE AND JUST RECOMPUTE THE ENDS AT EACH
  // ITERATION...  Otherwise we have to carefully pre-compute how many steps we
  // need to do at each point, and keep track of the base index, and know when
  // to stop.

  Rprintf("BOTH-OOB\n");
  // len constant of *data* size, either all NA, or all the same number
  for(int j = dp.dat_start; j <= dp.dat_end; ++j) dp.lens[j] = d_size;
  if(o_int > 0) {
    Rprintf("  o %d start %d end %d len %d\n", o_int, start, end, len);
    if(part_int) {
      (*(dp.fun))(dp.data, dp.lens, dp.datai, dp.narg, dp.flags, dp.ctrl);
      if(dp.data[I_STAT][STAT_RECYCLE] && !recycle_warn)
        recycle_warn = (double)start + 1;
      if(dp.lens[I_RES] != 1)
        Rf_error("Window result size is not 1 (is %jd).", dp.lens[I_RES]);
    } else {
      **(dp.data + I_RES) = NA_REAL;
    }
    double res_val = **(dp.data + I_RES);
    ++(*(dp.data + I_RES));
    o_int -= by_int;

    // Copy result into whole result vector (use memcpy?)
    for(; o_int > 0; o_int -= by_int, ++(*(dp.data + I_RES)))
      **(dp.data + I_RES) = res_val;

    if(o_int > 0 || o_int <= -by_int)
      Rf_error("Internal Error: bad window start calc 1 (offset %d).", o_int);
  }
  // - Both IB -----------------------------------------------------------------

  Rprintf("BOTH-IB\n");
  start = -o_int;  // o_int <= 0; by could have caused skipping some values
  R_xlen_t end_max = d_size - 1;
  // Window fully in bounds, len constant *window* size
  for(int j = dp.dat_start; j <= dp.dat_end; ++j) dp.lens[j] = w_int;
  for(; end < end_max; end += by_int, start += by_int) {
    Rprintf("  o %d start %d end %d len %d\n", o_int, start, end, len);
    (*(dp.fun))(dp.data, dp.lens, dp.datai, dp.narg, dp.flags, dp.ctrl);
    if(dp.data[I_STAT][STAT_RECYCLE] && !recycle_warn)
      recycle_warn = (double)start + 1;
    // For `by` == 1, we don't need this check since we don't dereference past
    // the end, but if `by > 1`, we end up beyond +1 of end so need to check
    if(start + by_int <= d_size)
      for(int j = dp.dat_start; j <= dp.dat_end; ++j) *(dp.data + j) += by_int;
    if(dp.lens[I_RES] != 1)
      Rf_error("Window result size is not 1 (is %jd).", dp.lens[I_RES]);
    ++(*(dp.data + I_RES));
  }
  // - Right OOB Left IB -------------------------------------------------------

  Rprintf("R-OOB L-IB\n");
  len = d_size - start;
  for(; start < d_size; start += by_int, len -= by_int) {
    Rprintf("  o %d start %d end %d len %d\n", o_int, start, end, len);
    if(part_int) {
      for(int j = dp.dat_start; j <= dp.dat_end; ++j) dp.lens[j] = len;
      (*(dp.fun))(dp.data, dp.lens, dp.datai, dp.narg, dp.flags, dp.ctrl);
      if(dp.data[I_STAT][STAT_RECYCLE] && !recycle_warn)
        recycle_warn = (double)start + 1;
      if(start + by_int <= d_size)
        for(int j = dp.dat_start; j <= dp.dat_end; ++j) *(dp.data + j) += by_int;
      if(dp.lens[I_RES] != 1)
        Rf_error("Window result size is not 1 (is %jd).", dp.lens[I_RES]);
    } else {
      **(dp.data + I_RES) = NA_REAL;
    }
    ++(*(dp.data + I_RES));
  }
  return Rf_ScalarReal((double) recycle_warn);
}


