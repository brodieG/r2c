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


#ifndef R2C_H
#define R2C_H
#define R_NO_REMAP

// System headers go above
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>
#include <stdint.h>

// Important indices in the alloc data (0-base)
#define I_STAT      0   // status flags
#define I_RES       1   // final call result
#define I_GRP       2   // index start of group varying data

// Indices in the I_STAT element of the alloc_data
#define STAT_N       2  // STATUS entry count (not an index)
#define STAT_RECYCLE 0  // bad recycling
#define STAT_LOOP    1  // loop counter for interrupts

struct const_dat {const char * name; const int value;};

typedef SEXP (*r2c_dl_fun) (
  double ** data, R_xlen_t * lens, int ** di, int * narg, int * flag, SEXP ctrl
);

SEXP R2C_assumptions(void);
SEXP R2C_constants(void);
SEXP R2C_group_sizes(SEXP g);

/*
 * Structure containing the varying data in a format for faster access
 */
struct R2C_dat {
  double ** data;  // Full data (see next for details)
  // `data` contains some meta data columns first, result vector, followed by
  // the iterations varying data, followed by "static" data (same for every
  // call), see I_* above for precise indices.
  int dat_start;   // First "iteration varying" data column
  int dat_end;     // Last "iteration varying" data column
  int dat_count;   // dat_end - dat_start + 1 (convenience)
  int ** datai;    // For each sub-fun, which indices in data are relevant
  int * narg;      // For each sub-fun, how many arguments it takes
  int * flags;     // Flag (T/F) control parameters, one for each sub-fun
  SEXP ctrl;       // Non data, non-flag parameters
  R_xlen_t * lens; // Length of each of the data vectors
  r2c_dl_fun fun;  // function to apply
};
struct R2C_dat prep_data(
  SEXP dat,        // the data, comes back in R2C_dat.data
  SEXP dat_cols,   // how many iteration varying data cols in `data`
  // List with as many elements as sub-calls in the r2c fun, indicating for each
  // which elements in `data` should be given to the function
  SEXP ids,
  SEXP flag, SEXP ctrl, SEXP so
);


// See prep_data and R2C_dat above for details of the first 5 parameters
// for all the R2C_run_* functions
SEXP R2C_run_group(
  SEXP so, SEXP dat, SEXP dat_cols, SEXP ids, SEXP flag, SEXP ctrl,
  SEXP grp_lens, SEXP res_lens
);
SEXP R2C_run_window(
  SEXP so, SEXP dat, SEXP dat_cols, SEXP ids, SEXP flag, SEXP ctrl,
  SEXP width, SEXP offset, SEXP by, SEXP partial
);

SEXP R2C_run_window_by(
  SEXP so, SEXP dat, SEXP dat_cols, SEXP ids, SEXP flag, SEXP ctrl,
  SEXP width, SEXP offset, SEXP by_sxp, SEXP x_sxp,
  SEXP start_sxp, SEXP end_sxp, SEXP bounds_sxp
);
SEXP R2C_run_window_at(
  SEXP so, SEXP dat, SEXP dat_cols, SEXP ids, SEXP flag, SEXP ctrl,
  SEXP width, SEXP offset, SEXP at_sxp, SEXP x_sxp,
  SEXP bounds_sxp
);
SEXP R2C_run_window_bw(
  SEXP so, SEXP dat, SEXP dat_cols, SEXP ids, SEXP flag, SEXP ctrl,
  SEXP left_sxp, SEXP right_sxp, SEXP x_sxp,
  SEXP bounds_sxp
);

SEXP R2C_size_window_by(
  SEXP rlen_sxp, SEXP width, SEXP offset, SEXP by_sxp, SEXP x_sxp,
  SEXP start_sxp, SEXP end_sxp, SEXP bounds_sxp
);
SEXP R2C_size_window_at(
  SEXP rlen_sxp, SEXP width, SEXP offset, SEXP at_sxp,
  SEXP x_sxp, SEXP bounds_sxp
);
SEXP R2C_size_window_bw(
  SEXP rlen_sxp, SEXP left_sxp, SEXP right_sxp, SEXP x_sxp, SEXP bounds_sxp
);

#endif  /* R2C_H */
