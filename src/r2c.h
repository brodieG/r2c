/*
 * Copyright (C) 2022  Brodie Gaslam
 *
 * This file is part of "r2c - Fast Iterated Statistics in R"
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

// System headers go above
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>
#include <stdint.h>

SEXP R2C_group_sizes(SEXP g);
SEXP R2C_run_internal(
  SEXP so, SEXP interface, SEXP dat, SEXP dat_cols,
  SEXP ids, SEXP flag, SEXP ctrl, SEXP grp_lens, SEXP res_lens
);

#endif  /* R2C_H */
