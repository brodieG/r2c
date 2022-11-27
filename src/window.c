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
  SEXP by_sxp,
  SEXP partial
) {
  if(TYPEOF(width) != INTSXP || XLENGTH(width) != 1L)
    Rf_error("Argument `width` should be scalar integer.");
  if(TYPEOF(offset) != INTSXP || XLENGTH(offset) != 1L)
    Rf_error("Argument `offset` should be scalar integer.");
  if(TYPEOF(by_sxp) != INTSXP || XLENGTH(by_sxp) != 1L)
    Rf_error("Argument `by` should be scalar integer.");
  if(TYPEOF(partial) != LGLSXP || XLENGTH(partial) != 1L)
    Rf_error("Argument `partial` should be scalar logical.");

  int w = Rf_asInteger(width);
  int o = Rf_asInteger(offset);
  int by = Rf_asInteger(by_sxp);
  int part_int = Rf_asInteger(partial);

  // This should be validated R level, but bad if wrong
  if(w < 0 || by < 1)
    Rf_error("Internal Error: bad window values w %d o %d b %d.", w, o, by);

  struct R2C_dat dp = prep_data(dat, dat_cols, ids, flag, ctrl, so);

  // these will be stored 1-index, so double (see assumptions.c)
  double recycle_warn = 0;
  double * recycle_flag = dp.data[I_STAT] + STAT_RECYCLE;

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

  if(o < 0 && R_XLEN_T_MAX + o < d_size)
    Rf_error(
      "`align`, `window`, and `data` cause counter to overflow R_XLEN_T_MAX."
    );

  // Avoid theoretical potential for R_xlen_t overflow in loop counter in case
  // where `by` stride is wide and last step would be well past end of data.
  R_xlen_t imax = R_XLEN_T_MAX - by;
  if(d_size < imax) imax = d_size;

  // This is not super optimized.  There are common special cases (e.g. window
  // fully in bounds) that require less work than those partially out of bounds,
  // so we could split the loop.  In testing though there does not seem to be
  // that much overhead, as it's only a significant portion of the work with big
  // windows, and if windows are big, the actual function evaluation will be
  // slow relative to the bookkeeping of the start/len variables.

  for(i = 0; i < imax; i += by) {
    int incomplete = 0;
    start = i - o;
    len = w;
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
      // Windows can have varying number of elements
      if(*recycle_flag && !recycle_warn) recycle_warn = (double)i + 1;
      if(dp.lens[I_RES] != 1)
        Rf_error("Window result size is not 1 (is %jd).", dp.lens[I_RES]);
    } else {
      **(dp.data + I_RES) = NA_REAL;
    }
    ++(*(dp.data + I_RES));
  }
  return Rf_ScalarReal((double) recycle_warn);
}
#define SLIDE_WINDOW(LEFT_COND, RIGHT_COND, IB_COND) do {                     \
  /* `+ by * i` instead of `+= by` for precision */                           \
  for(R_xlen_t i = 0; i < rlen; ++i, left = start + by * i) {                 \
    while((LEFT_COND) && ileft < ilen) ++ileft;                               \
    if(ileft < ilen) {                                                        \
      right = left + w;                                                       \
      /* better iright_prev if window >> by, but otherwise ileft is better. */\
      /* relying on branch pred figuring out this is constant for duration  */\
      /* + 1 because we do --iright.                                        */\
      if(ileft > iright_prev) iright = ileft + 1;                             \
      else iright = iright_prev + 1;                                          \
      /* overshoot and step back */                                           \
      while((RIGHT_COND) && iright < ilen) ++iright;                          \
      --iright;                                                               \
      iright_prev = iright;                                                   \
    } else {                                                                  \
      ileft = iright = ilen - 1;                                              \
    }                                                                         \
    if((IB_COND)) { /* at least one value */                                  \
      R_xlen_t len = iright - ileft + 1;                                      \
      for(int j = dp.dat_start; j <= dp.dat_end; ++j) {                       \
        dp.data[j] = dat_base[j] + ileft;                                     \
        dp.lens[j] = len;                                                     \
      }                                                                       \
    } else {        /* empty window */                                        \
      for(int j = dp.dat_start; j <= dp.dat_end; ++j) dp.lens[j] = 0;         \
    }                                                                         \
  (*(dp.fun))(dp.data, dp.lens, dp.datai, dp.narg, dp.flags, dp.ctrl);        \
  if(*recycle_flag && !recycle_warn)                                          \
    recycle_warn = (double)i + 1;                                             \
                                                                              \
  /* checks not be necessary if all the calcs are right  */                   \
  if(dp.lens[I_RES] != 1)                                                     \
    Rf_error("Window result size is not 1 (is %jd).", dp.lens[I_RES]);        \
                                                                              \
  ++(*(dp.data + I_RES));                                                     \
  }                                                                           \
} while (0)

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
  SEXP end_sxp,
  SEXP interval_sxp
) {
  if(TYPEOF(interval_sxp) != INTSXP || XLENGTH(interval_sxp) != 1L)
    Rf_error("Argument `interval` should be scalar integer.");
  if(TYPEOF(width) != REALSXP || XLENGTH(width) != 1L)
    Rf_error("Argument `width` should be scalar numeric.");
  if(TYPEOF(offset) != REALSXP || XLENGTH(offset) != 1L)
    Rf_error("Argument `offset` should be scalar numeric.");
  if(TYPEOF(by_sxp) != REALSXP || XLENGTH(by_sxp) != 1L)
    Rf_error("Argument `by` should be scalar numeric.");
  if(TYPEOF(index_sxp) != REALSXP)
    Rf_error("Argument `index` should be numeric.");

  double w = Rf_asReal(width);
  double o = Rf_asReal(offset);
  double by = Rf_asReal(by_sxp);
  double start = Rf_asReal(start_sxp);
  double end = Rf_asReal(end_sxp);

  // This should be validated R level, but bad if wrong
  if(
    isnan(w) || !isfinite(w) || isnan(o) || !isfinite(o) ||
    isnan(by) || !isfinite(by) || isnan(start) || !isfinite(start) ||
    isnan(end) || !isfinite(end) || w < 0 || by < 0 || start > end
  )
    Rf_error(
      "Internal Error: bad window values w %f o %f b %f start %f end %f.",
      w, o, by, start, end
    );

  // Shift start/end by o, so we can treat window start as the base index
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
  R_xlen_t rlen = dp.lens[I_RES];

  // these will be stored 1-index, so double (see assumptions.c)
  double recycle_warn = 0;
  double * recycle_flag = dp.data[I_STAT] + STAT_RECYCLE;

  // Make a copy of the base data pointers
  double ** dat_base = (double **) R_alloc(dp.dat_end, sizeof(double *));
  for(int j = dp.dat_start; j <= dp.dat_end; ++j) dat_base[j] = dp.data[j];

  // *******************************************************************
  // ** LOOK AT window_exec FOR MORE COMMENTS ON WHAT'S GOING ON HERE **
  // *******************************************************************

  // This is not optimized, e.g. take advantage of ordered input, explicitly
  // handle empty windows at ends, etc.  But because not optimized, it is
  // simpler and won' behave badly if input is not actually ordered.

  // Because of potential precision issues, we do not test left < end despite
  // that being the more natural condition.  We also have the precision problem
  // with e.g. index[ileft] < left, but there it won't cause us to write one too
  // many or too few values into our result vector, so we ignore it.

  R_xlen_t ileft, iright, iright_prev;  // indices of ends of window
  ileft = iright = iright_prev = 0;
  double left, right;
  left = start;
  int interval = INTEGER(interval_sxp)[0];
  int lclose, rclose;
  lclose = interval & 1;
  rclose = interval & 2;

  if(lclose && !rclose) {
    SLIDE_WINDOW(
      index[ileft] < left, index[iright] < right,
      index[ileft] < right && index[iright] >= left
    );
  } else if (!lclose && rclose) {
    SLIDE_WINDOW(
      index[ileft] <= left, index[iright] <= right,
      index[ileft] <= right && index[iright] > left
    );
  } else if (lclose && rclose) {
    SLIDE_WINDOW(
      index[ileft] < left, index[iright] <= right,
      index[ileft] <= right && index[iright] >= left
    );
  } else if (!lclose && !rclose) {
    SLIDE_WINDOW(
      index[ileft] < left, index[iright] < right,
      index[ileft] < right && index[iright] > left
    );
  }
  return Rf_ScalarReal((double) recycle_warn);
}


