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

// This is not super optimized.  There are common special cases (e.g. window
// fully in bounds) that require less work than those partially out of bounds,
// so we could split the loop.  In testing though there does not seem to be
// that much overhead, as it's only a significant portion of the work with big
// windows, and if windows are big, the actual function evaluation will be
// slow relative to the bookkeeping of the start/len variables.

#define ROLL_WINDOW_I(NEXP) do {                                               \
  double * recycle_flag = dp.data[I_STAT] + STAT_RECYCLE;                      \
  double recycle_warn = 0;      /* stored 1-index */                           \
                                                                               \
  for(i = 0; i < imax; i += by) {                                              \
    int incomplete = 0;                                                        \
    start = i - o;                                                             \
    NEXP;                                                                      \
    if(start < 0) {             /* OOB due to offset */                        \
      len += start;                                                            \
      if(len < 0) len = 0;                                                     \
      start = 0;                                                               \
      incomplete = 1;                                                          \
    }                                                                          \
    if(len + start > d_size) {  /* OOB due to start/n/offset */                \
      len = d_size - start;                                                    \
      incomplete = 1;                                                          \
    }                                                                          \
    if(part_int || !incomplete) {                                              \
      for(int j = dp.dat_start; j <= dp.dat_end; ++j) {                        \
        dp.data[j] = dat_base[j] + start;                                      \
        dp.lens[j] = len;                                                      \
      }                                                                        \
      (*(dp.fun))(dp.data, dp.lens, dp.datai, dp.narg, dp.flags, dp.ctrl);     \
      /* Windows can have varying number of elements */                        \
      if(!recycle_warn && *recycle_flag) recycle_warn = (double)i + 1;         \
      if(dp.lens[I_RES] != 1)                                                  \
        Rf_error("Window result size is not 1 (is %jd).", dp.lens[I_RES]);     \
    } else {                                                                   \
      **(dp.data + I_RES) = NA_REAL;                                           \
    }                                                                          \
    ++(*(dp.data + I_RES));                                                    \
  }                                                                            \
  return Rf_ScalarReal((double) recycle_warn);                                 \
} while(0)
/*
 * Simple Integer Rank Position Implementation
 */
SEXP R2C_run_window(
  SEXP so,
  SEXP dat,
  SEXP dat_cols,
  SEXP ids,
  SEXP flag,
  SEXP ctrl,
  // ^^ See group.c, paramaters above shared with group_exec
  SEXP n_sxp,
  SEXP offset,
  SEXP by_sxp,
  SEXP partial
) {
  if(TYPEOF(n_sxp) != INTSXP)
    Rf_error("Argument `n` should be scalar integer.");
  if(TYPEOF(offset) != INTSXP || XLENGTH(offset) != 1L)
    Rf_error("Argument `offset` should be scalar integer.");
  if(TYPEOF(by_sxp) != INTSXP || XLENGTH(by_sxp) != 1L)
    Rf_error("Argument `by` should be scalar integer.");
  if(TYPEOF(partial) != LGLSXP || XLENGTH(partial) != 1L)
    Rf_error("Argument `partial` should be scalar logical.");

  int o = Rf_asInteger(offset);
  int by = Rf_asInteger(by_sxp);
  int part_int = Rf_asInteger(partial);

  // This should be validated R level, but bad if wrong
  if(by < 1) Rf_error("Internal Error: by less than 1 (%d).", by);

  struct R2C_dat dp = prep_data(dat, dat_cols, ids, flag, ctrl, so);

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

  // Roll, different case for scalar vs. vector n (is it worth having a
  // special scalar case?
  if(XLENGTH(n_sxp) == 1) {
    int n = Rf_asInteger(n_sxp);
    if(n < 0 || n == NA_INTEGER)
      Rf_error("Argument `n` may contain only positive non-NA values.");

    ROLL_WINDOW_I(len = n);
  } else {
    if(XLENGTH(n_sxp) != d_size)
      Rf_error(
        "Argument `n` must be scalar or have the same element count as `data` "
        "(%jd vs %jd)", (intmax_t) XLENGTH(n_sxp), (intmax_t) d_size
      );
    int * n = INTEGER(n_sxp);

    ROLL_WINDOW_I(
      if(n[i] >= 0 && n[i] != NA_INTEGER) len = n[i];
      else Rf_error("Argument `n` contains negative or NA values.");
    );
  }
}

struct win_args {
  double w;         // `width`
  double o;         // `offset`
  double by;
  double start;
  double end;
  int partial;
  int bounds;
  double ** dbase;  // base of `data` pointers
  double * x;       // same as R level `x`
  double * at;      // same as R level `at`
  double * l_end;   // same as R level `left`
  double * r_end;   // same as R level `right`
  R_xlen_t xlen;    // Length of `x`
  R_xlen_t rlen;    // Length of result vector
};
/* Check and Initialize Shared Window Arguments
 *
 * Allows centralizing the checking and setting across most of the window
 * functions.  Arguments not used by a particular function are passed in as
 * NULL.  Those will not be checked and will set their corresponding member in
 * the returned struct to 0 (including the pointers, so don't go dereferencing
 * those).
 *
 * Following macros are for exclusive use with this function.  SET_MBR* are
 * responsible for checking SEXPs, converting to C basic types, and setting them
 * in the `wa` struct.  The MACROS expect very specific types of inputs and are
 * not safe to use generally (e.g. because they can't enclose some of the
 * "arguments" in parentheses).
 */
#define IERR(msg) Rf_error("Internal Error: " msg)
// Test Scalar-Type
#define TSCL(obj, type) TYPEOF((obj)) != (type) || XLENGTH((obj)) != 1L
// Test Vector-Type
#define TVEC(obj, type) TYPEOF((obj)) != (type)
// If not NULL, Test SEXP and set Corresponding Member in struct `wa`
#define SET_MBR(OBJ, SYM, NAME, TYPE, SETTER, TYPEN, TEST) do {         \
  if(TYPEOF((OBJ)) != NILSXP) {                                         \
    if(TEST(OBJ, TYPE))                                                 \
      IERR("Argument `" NAME "` should be " TYPEN " vector.");          \
    wa.SYM = SETTER((OBJ));                                             \
  }                                                                     \
} while(0)
#define SET_MBR_REAL_SCL(OBJ, SYM, NAME) \
  SET_MBR(OBJ, SYM, NAME, REALSXP, Rf_asReal, "numeric(1)", TSCL)
#define SET_MBR_INT_SCL(OBJ, SYM, NAME) \
  SET_MBR(OBJ, SYM, NAME, INTSXP, Rf_asInteger, "integer(1)", TSCL)
#define SET_MBR_LGL_SCL(OBJ, SYM, NAME) \
  SET_MBR(OBJ, SYM, NAME, LGLSXP, Rf_asInteger, "logical(1)", TSCL)
#define SET_MBR_REAL(OBJ, SYM, NAME) \
  SET_MBR(OBJ, SYM, NAME, REALSXP, REAL, "numeric", TVEC);

// See comments before MACROS ^^
static struct win_args prep_win_args(
  SEXP width, SEXP offset, SEXP x_sxp, SEXP by_sxp,
  SEXP at_sxp, SEXP left_sxp, SEXP right_sxp,
  SEXP start_sxp, SEXP end_sxp, SEXP bounds_sxp, SEXP partial_sxp,
  struct R2C_dat dp
) {
  struct win_args wa = {.w = 0};

  // Scalar members
  //               SEXP             Struct Member    Name for Error
  SET_MBR_REAL_SCL(width,           w,               "width");
  SET_MBR_REAL_SCL(offset,          o,               "offset");
  SET_MBR_REAL_SCL(by_sxp,          by,              "by");
  SET_MBR_REAL_SCL(start_sxp,       start,           "start");
  SET_MBR_REAL_SCL(end_sxp,         end,             "end");
  SET_MBR_LGL_SCL( partial_sxp,     partial,         "partial");
  SET_MBR_INT_SCL( bounds_sxp,      bounds,          "bounds");

  // Vector members
  SET_MBR_REAL(    x_sxp,           x,               "x");
  SET_MBR_REAL(    at_sxp,          at,              "at");
  SET_MBR_REAL(    left_sxp,        l_end,           "left");
  SET_MBR_REAL(    right_sxp,       r_end,           "right");

  // This should be validated R level, but bad if wrong.  Because `wa` is zero
  // initialized we can safely tests things that aren't set
  if(
    isnan(wa.w) || !isfinite(wa.w) || isnan(wa.o) || !isfinite(wa.o) ||
    isnan(wa.start) || !isfinite(wa.start) ||
    isnan(wa.end) || !isfinite(wa.end) || wa.w < 0 || wa.start > wa.end
  )
    Rf_error(
      "Internal Error: bad window values w %f o %f b %f start %f end %f.",
      wa.w, wa.o, wa.by, wa.start, wa.end
    );

  wa.xlen = XLENGTH(x_sxp);
  wa.rlen = dp.lens[I_RES];

  // Make a copy of the base data pointers to use as references as we advance
  // through the data
  wa.dbase = (double **) R_alloc(dp.dat_end + 1, sizeof(double *));
  for(int j = dp.dat_start; j <= dp.dat_end; ++j) wa.dbase[j] = dp.data[j];

  return wa;
}

/*
 * Generate the Window Application Loop
 *
 * Algorithm is to advance through data positions in `x` until finding first
 * element that is inside window range.  Then, continue advancing (either from
 * the first in-range element, or the previous iteration last in-range element)
 * until we find first out of range element, and then back up one to presumed
 * last in-range element.
 *
 * All the _OP "variables" are comparison operators in <, >, <=, >=.  See
 * ROLL_WINDOW for more details.
 *
 * R_EXP expression used to define the left end of the window
 * L_EXP expression used to define the right end of the window
 * L_OP operator to compare element position to left end of window
 * R_OP operator to compare element position to right end of window
 * IBL_OP determines whether there are any items to the left of the right end of
 *   the window.  Together with IBR_OP can be used to determine empty windows.
 * IBR_OP determines whether there are any items to the right of the left end of
 *   the window.  Together with IBR_OP can be used to determine empty windows.
 */
#define ROLL_BOUND(L_EXP, R_EXP, L_OP, R_OP, IBL_OP, IBR_OP) do {           \
  R_xlen_t ileft, iright, iright_prev;  /* indices of ends of window */     \
  ileft = iright = iright_prev = 0;                                         \
  double * recycle_flag = dp.data[I_STAT] + STAT_RECYCLE;                   \
  /* left/right set by L_EXP and R_EXP respectively */                      \
  double left, right;                                                       \
  R_xlen_t i = 0; /* need initial vals for L_EXP and R_EXP */               \
  L_EXP; R_EXP;                                                             \
  for(; i < wa.rlen; ++i, (L_EXP), (R_EXP)) {                               \
    /* Find first in-range element */                                       \
    while(wa.x[ileft] L_OP left && ileft < wa.xlen) ++ileft;                \
    if(ileft < wa.xlen) {                                                   \
      /* Small optim: reset to iright_prev if window >> by */               \
      if(ileft > iright_prev) iright = ileft + 1;                           \
      else iright = iright_prev + 1;                                        \
      /* Find first oob element  to right */                                \
      while(wa.x[iright] R_OP right && iright < wa.xlen) ++iright;          \
      --iright;   /* step back to last in-range element */                  \
      iright_prev = iright;                                                 \
    } else {      /* ran out of vector */                                   \
      ileft = iright = wa.xlen - 1;                                         \
    }                                                                       \
    if(wa.x[ileft] IBL_OP right && wa.x[iright] IBR_OP left) {              \
      R_xlen_t len = iright - ileft + 1;                                    \
      for(int j = dp.dat_start; j <= dp.dat_end; ++j) {                     \
        dp.data[j] = wa.dbase[j] + ileft;                                   \
        dp.lens[j] = len;                                                   \
      }                                                                     \
    } else {      /* empty window */                                        \
      for(int j = dp.dat_start; j <= dp.dat_end; ++j) dp.lens[j] = 0;       \
    }                                                                       \
    /* RUN r2c FUN */                                                       \
    (*(dp.fun))(dp.data, dp.lens, dp.datai, dp.narg, dp.flags, dp.ctrl);    \
    /* Should be debug-mode only check */                                   \
    if(dp.lens[I_RES] != 1)                                                 \
      Rf_error("Window result size is not 1 (is %jd).", dp.lens[I_RES]);    \
                                                                            \
    if(*recycle_flag && !recycle_warn) recycle_warn = (double)i + 1;        \
    ++(*(dp.data + I_RES));                                                 \
  }                                                                         \
} while (0)

/* Generate the window iteration loop depending on the permutations of the
 * bounds types (open or closed).
 *
 * For example for "[)", the first option, we advance in `x` until `x` is not
 * strictly less than `left` (x[ileft] < left), and then we advance `x` until it
 * is strictly greater than right (x[iright] R_OP right), and step back one
 * element (--iright).  See ROLL_BOUND.
 *
 * L_END the expression that defines the left end of the window
 * R_END the expression that defines the left end of the window
 *
 * L_END R_END are reset at each iterations of the ROLL_BOUND loop.
 *
 * `bounds` is integer in 0-3, to be interpreted such that the value of the
 * first bit designates the closedness of the left end, and the second bit the
 * closedness of the right end.
 */

#define ROLL_WINDOW(L_END, R_END) do {                      \
  int lclosed, rclosed;                                     \
  lclosed = wa.bounds & 1;                                  \
  rclosed = wa.bounds & 2;                                  \
  /* 1-index, so use double (see assumptions.c) */          \
  double recycle_warn = 0;                                  \
                                                            \
  if(lclosed && !rclosed) {                                 \
    ROLL_BOUND(left = L_END, right = R_END, <, <, <, >=);   \
  } else if (!lclosed && rclosed) {                         \
    ROLL_BOUND(left = L_END, right = R_END, <=, <=, <=, >); \
  } else if (lclosed && rclosed) {                          \
    ROLL_BOUND(left = L_END, right = R_END, <, <=, <=, >=); \
  } else if (!lclosed && !rclosed) {                        \
    ROLL_BOUND(left = L_END, right = R_END, <, <, <, >);    \
  }                                                         \
  return Rf_ScalarReal((double) recycle_warn);              \
} while(0)

SEXP R2C_run_window_by(
  SEXP so,
  SEXP dat,
  SEXP dat_cols,
  SEXP ids,
  SEXP flag,
  SEXP ctrl,
  // ^^ See group.c, paramaters above shared with group_exec
  SEXP width,
  SEXP offset,
  SEXP by_sxp,
  SEXP x_sxp,
  SEXP start_sxp,
  SEXP end_sxp,
  SEXP bounds_sxp
) {
  struct R2C_dat dp = prep_data(dat, dat_cols, ids, flag, ctrl, so);
  struct win_args wa = prep_win_args(
    width, offset, x_sxp, by_sxp,
    R_NilValue, R_NilValue, R_NilValue,
    start_sxp, end_sxp,
    bounds_sxp, R_NilValue, dp
  );
  // Shift start by o, so we can treat window start as the base x
  double base0 = wa.start - wa.o;
  double end0 = wa.end - wa.o;
  if(!isfinite(base0) || !isfinite(end0))
    Rf_error(
      "`start`/`end` and `offset` values create infinite bound(s), ",
      "which is disallowed."
    );

  // `+ by * i` instead of `+= by` for precision
  ROLL_WINDOW(base0 + wa.by * i, left + wa.w);
}
SEXP R2C_run_window_at(
  SEXP so,
  SEXP dat,
  SEXP dat_cols,
  SEXP ids,
  SEXP flag,
  SEXP ctrl,
  // ^^ See group.c, paramaters above shared with group_exec
  SEXP width,
  SEXP offset,
  SEXP at_sxp,
  SEXP x_sxp,
  SEXP bounds_sxp
) {
  struct R2C_dat dp = prep_data(dat, dat_cols, ids, flag, ctrl, so);
  struct win_args wa = prep_win_args(
    width, offset, x_sxp, R_NilValue,
    at_sxp, R_NilValue, R_NilValue,
    R_NilValue, R_NilValue,
    bounds_sxp, R_NilValue, dp
  );
  ROLL_WINDOW(wa.at[i] - wa.o, left + wa.w);
}
SEXP R2C_run_window_bw(
  SEXP so,
  SEXP dat,
  SEXP dat_cols,
  SEXP ids,
  SEXP flag,
  SEXP ctrl,
  // ^^ See group.c, paramaters above shared with group_exec
  SEXP left_sxp,
  SEXP right_sxp,
  SEXP x_sxp,
  SEXP bounds_sxp
) {
  struct R2C_dat dp = prep_data(dat, dat_cols, ids, flag, ctrl, so);
  struct win_args wa = prep_win_args(
    R_NilValue, R_NilValue, x_sxp, R_NilValue,
    R_NilValue, left_sxp, right_sxp,
    R_NilValue, R_NilValue,
    bounds_sxp, R_NilValue, dp
  );
  ROLL_WINDOW(wa.l_end[i], wa.r_end[i]);
}

