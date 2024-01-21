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

/*-----------------------------------------------------------------------------\
|                                                                              |
|                              DISCRETE ROLL FUN                               |
|                                                                              |
\-----------------------------------------------------------------------------*/

// This is not super optimized.  There are common special cases (e.g. window
// fully in bounds) that require less work than those partially out of bounds,
// so we could split the loop.  In testing though there does not seem to be
// that much overhead, as it's only a significant portion of the work with big
// windows, and if windows are big, the actual function evaluation will be
// slow relative to the bookkeeping of the start/len variables.
//
// Conversely, we probably don't need a dedicated version for scalar `n` vs
// vector `n`.  Even with the additional branch it probably doesn't warrant the
// extra complexity of the macro here.
//
// NEXP: set `len` to window width.

#define ROLL_WINDOW_I(NEXP) do {                                               \
  double * recycle_flag = dp.data[I_STAT] + STAT_RECYCLE;                      \
  double recycle_warn = 0;      /* stored 1-index */                           \
  R_xlen_t interrupt_i = 0;                                                    \
                                                                               \
  for(i = 0; i < imax; i += by) {                                              \
    int incomplete = 0;                                                        \
    start = i - o;                                                             \
    NEXP;                       /* Vector vs Scalar `n` */                     \
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
    /* Check for interrupts */                                                 \
    if(                                                                        \
      len <= INTERRUPT_AT &&  /* we don't know what R_XLEN_T_MIN is */         \
      interrupt_i <= INTERRUPT_AT - len &&                                     \
      interrupt_i <= R_XLEN_T_MAX - len                                        \
    ) {                                                                        \
      interrupt_i += len;                                                      \
    } else  {                                                                  \
      interrupt_i = 0;                                                         \
      R_CheckUserInterrupt();                                                  \
    }                                                                          \
    /* If window complete, run the code */                                     \
    if(part_int || !incomplete) {                                              \
      for(int j = dp.dat_start; j <= dp.dat_end; ++j) {                        \
        dp.data[j] = dat_base[j] + start;                                      \
        dp.lens[j] = len;                                                      \
      }                                                                        \
      (*(dp.fun))(dp.data, dp.lens, dp.datai, dp.narg, dp.extn);               \
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
  SEXP extn,
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

  struct R2C_dat dp = prep_data(dat, dat_cols, ids, extn, so);

  // Make a copy of the base data pointers
  double ** dat_base = (double **) R_alloc(dp.dat_end + 1, sizeof(double *));
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

  // Roll, different case for scalar vs. vector n.
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
/*-----------------------------------------------------------------------------\
|                                                                              |
|                            ROLL FUN SHARED CODE                              |
|                                                                              |
\-----------------------------------------------------------------------------*/

struct win_args {
  double w;         // `width`
  double o;         // `offset`
  double by;
  double start;
  double end;
  int partial;
  int bounds;
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
  R_xlen_t rlen
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
  wa.rlen = rlen;
  return wa;
}
// Make a copy of the base data pointers to use as references as we advance
// through the data

static double ** copy_dat(struct R2C_dat dp) {
  double ** dbase = (double **) R_alloc(dp.dat_end + 1, sizeof(double *));
  for(int j = dp.dat_start; j <= dp.dat_end; ++j) dbase[j] = dp.data[j];
  return dbase;
}
/*
 * Actual window application
 *
 * IBL_OP determines whether there are any items to the left of the right end of
 *   the window.  Together with IBR_OP can be used to determine empty windows.
 * IBR_OP determines whether there are any items to the right of the left end of
 *   the window.  Together with IBR_OP can be used to determine empty windows.
 *
 * See below for params.
 */
#define ROLL_CALL(IBL_OP, IBR_OP) do{                                       \
  if(wa.x[ileft] IBL_OP right && wa.x[iright] IBR_OP left) {                \
    R_xlen_t len = iright - ileft + 1;                                      \
    for(int j = dp.dat_start; j <= dp.dat_end; ++j) {                       \
      dp.data[j] = dbase[j] + ileft;                                        \
      dp.lens[j] = len;                                                     \
    }                                                                       \
  } else {      /* empty window */                                          \
    for(int j = dp.dat_start; j <= dp.dat_end; ++j) dp.lens[j] = 0;         \
  }                                                                         \
  /* RUN r2c FUN */                                                         \
  (*(dp.fun))(dp.data, dp.lens, dp.datai, dp.narg, dp.extn);      \
  /* Should be debug-mode only check */                                     \
  if(dp.lens[I_RES] != 1)                                                   \
    Rf_error("Window result size is not 1 (is %jd).", dp.lens[I_RES]);      \
  if(*recycle_flag && !recycle_warn) recycle_warn = (double)i + 1;          \
  ++(*(dp.data + I_RES));                                                   \
} while(0)
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
 * It's arguable whether we should use macros for this or explicit branches.
 * This started as an experiment to see how to do it with macros out of the
 * potential concern of having branches nested (L_EXP and R_EXP are evaluated
 * for each window, L_OP and R_OP are evaualted for every step within every
 * window).  We haven't gone back and tested the cost of adding those branches,
 * but it seems likely it could be expensive (e.g. small windows branch pred
 * will fail often, and big windows you still are evaluating the condition n^2).
 *
 * R_EXP expression used to define the left end of the window
 * L_EXP expression used to define the right end of the window
 * L_OP operator to compare element position to left end of window
 * R_OP operator to compare element position to right end of window
 * F_EXP the function evaluation expression, or we can sub-in the width
 *   calculation expression for the max window size estimation.
 */
#define ROLL_BOUND(L_EXP, R_EXP, L_OP, R_OP, F_EXP) do {                     \
  R_xlen_t ileft, iright, iright_prev;  /* indices of ends of window */      \
  ileft = iright = iright_prev = 0;                                          \
  /* left/right set by L_EXP and R_EXP respectively */                       \
  double left, right;                                                        \
  R_xlen_t i = 0; /* need initial vals for L_EXP and R_EXP */                \
  R_xlen_t interrupt_i = 0;                                                  \
  for(; i < wa.rlen; ++i) {                                                  \
    L_EXP; R_EXP;\
    /* Find first in-range element */                                        \
    while(ileft < wa.xlen && wa.x[ileft] L_OP left) ++ileft;                 \
    if(ileft < wa.xlen) {                                                    \
      /* Small optim: reset to iright_prev if window >> by */                \
      if(ileft > iright_prev) iright = ileft + 1;                            \
      else iright = iright_prev + 1;                                         \
      /* Find first oob element  to right */                                 \
      while(iright < wa.xlen && wa.x[iright] R_OP right) ++iright;           \
      --iright;   /* step back to last in-range element */                   \
      iright_prev = iright;                                                  \
    } else {      /* ran out of vector */                                    \
      ileft = iright = wa.xlen - 1;                                          \
    }                                                                        \
    /* ileft max is R_XLEN_T_MAX - 1, so + 1 cannot overflow */              \
    R_xlen_t len = iright - ileft + 1;                                       \
    /* Check for interrupts (too complicated for _INTERRUPT_BASIC) */        \
    if(                                                                      \
      len <= INTERRUPT_AT &&  /* we don't know what R_XLEN_T_MIN is */       \
      interrupt_i <= INTERRUPT_AT - len && interrupt_i <= R_XLEN_T_MAX - len \
    ) {                                                                      \
      interrupt_i += len;                                                    \
    } else  {                                                                \
      interrupt_i = 0;                                                       \
      R_CheckUserInterrupt();                                                \
    }                                                                        \
    /* Run expression */                                                     \
    F_EXP;                                                                   \
  }                                                                          \
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

#define ROLL_WINDOW(L_END, R_END) do {                                 \
  int lclosed, rclosed;                                                \
  lclosed = wa.bounds & 1;                                             \
  rclosed = wa.bounds & 2;                                             \
  /* 1-index, so use double (see assumptions.c) */                     \
  double recycle_warn = 0;                                             \
  double * recycle_flag = dp.data[I_STAT] + STAT_RECYCLE;              \
                                                                       \
  if(lclosed && !rclosed) {                                            \
    ROLL_BOUND(left = L_END, right = R_END, <, <, ROLL_CALL(<, >=));   \
  } else if (!lclosed && rclosed) {                                    \
    ROLL_BOUND(left = L_END, right = R_END, <=, <=, ROLL_CALL(<=, >)); \
  } else if (lclosed && rclosed) {                                     \
    ROLL_BOUND(left = L_END, right = R_END, <, <=, ROLL_CALL(<=, >=)); \
  } else if (!lclosed && !rclosed) {                                   \
    ROLL_BOUND(left = L_END, right = R_END, <, <, ROLL_CALL(<, >));    \
  }                                                                    \
  return Rf_ScalarReal((double) recycle_warn);                         \
} while(0)
/*
 * Max Window Size Estimation
 *
 * This one run first to count maximum number of elements in a window.  We're a
 * little sloppy because we implicitly assume the smallest possible window size
 * is 1 (ileft==iright), but it's okay to overestimate here.
 *
 * The macro assumes iright >= ileft (should be true).
 *
 * See ROLL_WINDOW for L_END, R_END
 */
#define WIN_SIZE do{                                           \
  if(len > len_max) len_max = len;                             \
  if(len < len_min) len_min = len;                             \
} while(0)

#define SIZE_WINDOW(L_END, R_END) do {                         \
  int lclosed, rclosed;                                        \
  lclosed = wa.bounds & 1;                                     \
  rclosed = wa.bounds & 2;                                     \
  R_xlen_t len_max = 0;                                        \
  R_xlen_t len_min = R_XLEN_T_MAX;                             \
                                                               \
  if(lclosed && !rclosed) {                                    \
    ROLL_BOUND(left = L_END, right = R_END, <, <, WIN_SIZE);   \
  } else if (!lclosed && rclosed) {                            \
    ROLL_BOUND(left = L_END, right = R_END, <=, <=, WIN_SIZE); \
  } else if (lclosed && rclosed) {                             \
    ROLL_BOUND(left = L_END, right = R_END, <, <=, WIN_SIZE);  \
  } else if (!lclosed && !rclosed) {                           \
    ROLL_BOUND(left = L_END, right = R_END, <, <, WIN_SIZE);   \
  }                                                            \
  SEXP size_fin = PROTECT(Rf_allocVector(REALSXP, 2));         \
  REAL(size_fin)[0] = (double) len_max;                        \
  REAL(size_fin)[1] = (double) len_min;                        \
  UNPROTECT(1);                                                \
  return size_fin;                                             \
} while(0)

// ROLL_XX: one of ROLL_BY, ROLL_AT, ROLL_BW

#define ROLL(ROLL_XX) do{                                                \
  struct R2C_dat dp = prep_data(dat, dat_cols, ids, extn, so);           \
  double ** dbase = copy_dat(dp);                                        \
  ROLL_XX;                                                               \
} while(0)

/*-----------------------------------------------------------------------------\
|                                                                              |
|                       CONTINOUS ROLL FUN INTERFACES                          |
|                                                                              |
\-----------------------------------------------------------------------------*/

// Macro structure is designed to allow sharing code across similar tasks (I
// hope I don't end up regretting this...).
//
// For each of the roll types (by/at/bw), we need to be able to compute window
// sizes (SIZE_WINDOW), as well as to run the r2c function on each window
// (ROLL_WINDOW).  We provide an interface for each of those actions via the
// R2C_run_window_xx and R2C_size_window_xx.
//
// Both actions share the same window seeking code (i.e. identifying which data
// elements fall within a window).  We use two passes (size and calc)
// because we need to know max rolling window for our temporary vector memory
// allocations.  We could just guess and then grow when we're wrong, which might
// be worth it given our currently dumb seeking algorithm could be the bottlneck
// in some cases.
//
// `x_sxp` corresponds to `position` at the R level.
//
// ACTION: one of SIZE_WINDOW or ROLL_WINDOW
// RLEN: result length

// - By ------------------------------------------------------------------------

#define ROLL_BY(ACTION, RLEN) do {                                    \
  struct win_args wa = prep_win_args(                                 \
    width, offset, x_sxp, by_sxp,                                     \
    R_NilValue, R_NilValue, R_NilValue,                               \
    start_sxp, end_sxp,                                               \
    bounds_sxp, R_NilValue, RLEN                                      \
  );                                                                  \
  /* Shift start by o, so we can treat window start as the base x */  \
  double base0 = wa.start - wa.o;                                     \
  double end0 = wa.end - wa.o;                                        \
  if(!isfinite(base0) || !isfinite(end0))                             \
    Rf_error(                                                         \
      "`start`/`end` and `offset` values create infinite bound(s), "  \
      "which is disallowed."                                          \
    );                                                                \
  /* `+ by * i` instead of `+= by` for precision */                   \
  ACTION(base0 + wa.by * i, left + wa.w);                             \
} while (0)

SEXP R2C_run_window_by(
  SEXP so, SEXP dat, SEXP dat_cols, SEXP ids, SEXP extn,
  SEXP width, SEXP offset, SEXP by_sxp, SEXP x_sxp, SEXP start_sxp,
  SEXP end_sxp, SEXP bounds_sxp
) {
  ROLL(ROLL_BY(ROLL_WINDOW, dp.lens[I_RES]));
}
SEXP R2C_size_window_by(
  SEXP rlen_sxp, SEXP width, SEXP offset, SEXP by_sxp, SEXP x_sxp,
  SEXP start_sxp, SEXP end_sxp, SEXP bounds_sxp
) {
  ROLL_BY(SIZE_WINDOW, (R_xlen_t) Rf_asReal(rlen_sxp));
}

// - At ------------------------------------------------------------------------

#define ROLL_AT(ACTION, RLEN) do {      \
  struct win_args wa = prep_win_args(   \
    width, offset, x_sxp, R_NilValue,   \
    at_sxp, R_NilValue, R_NilValue,     \
    R_NilValue, R_NilValue,             \
    bounds_sxp, R_NilValue, RLEN        \
  );                                    \
  ACTION(wa.at[i] - wa.o, left + wa.w); \
} while (0)

SEXP R2C_run_window_at(
  SEXP so, SEXP dat, SEXP dat_cols, SEXP ids, SEXP extn,
  SEXP width, SEXP offset, SEXP at_sxp, SEXP x_sxp,
  SEXP bounds_sxp
) {
  ROLL(ROLL_AT(ROLL_WINDOW, dp.lens[I_RES]));
}
SEXP R2C_size_window_at(
  SEXP rlen_sxp, SEXP width, SEXP offset, SEXP at_sxp,
  SEXP x_sxp, SEXP bounds_sxp
) {
  ROLL_AT(SIZE_WINDOW, (R_xlen_t) Rf_asReal(rlen_sxp));
}
// - Between -------------------------------------------------------------------

#define ROLL_BW(ACTION, RLEN) do {             \
  struct win_args wa = prep_win_args(          \
    R_NilValue, R_NilValue, x_sxp, R_NilValue, \
    R_NilValue, left_sxp, right_sxp,           \
    R_NilValue, R_NilValue,                    \
    bounds_sxp, R_NilValue, RLEN               \
  );                                           \
  ACTION(wa.l_end[i], wa.r_end[i]);            \
} while (0)

SEXP R2C_run_window_bw(
  SEXP so, SEXP dat, SEXP dat_cols, SEXP ids, SEXP extn,
  SEXP left_sxp, SEXP right_sxp, SEXP x_sxp, SEXP bounds_sxp
) {
  ROLL(ROLL_BW(ROLL_WINDOW, dp.lens[I_RES]));
}
SEXP R2C_size_window_bw(
  SEXP rlen_sxp, SEXP left_sxp, SEXP right_sxp, SEXP x_sxp, SEXP bounds_sxp
) {
  ROLL_BW(SIZE_WINDOW, (R_xlen_t) Rf_asReal(rlen_sxp));
}

