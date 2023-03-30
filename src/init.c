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

#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include "r2c.h"

static const
R_CallMethodDef callMethods[] = {
  {"group_sizes", (DL_FUNC) &R2C_group_sizes, 1},
  {"run_group", (DL_FUNC) &R2C_run_group, 8},
  {"run_window", (DL_FUNC) &R2C_run_window, 10},

  {"run_window_by", (DL_FUNC) &R2C_run_window_by, 13},
  {"run_window_at", (DL_FUNC) &R2C_run_window_at, 11},
  {"run_window_bw", (DL_FUNC) &R2C_run_window_bw, 10},

  {"size_window_by", (DL_FUNC) &R2C_size_window_by, 8},
  {"size_window_at", (DL_FUNC) &R2C_size_window_at, 6},
  {"size_window_bw", (DL_FUNC) &R2C_size_window_bw, 5},

  {"assumptions", (DL_FUNC) &R2C_assumptions, 0},
  {"constants", (DL_FUNC) &R2C_constants, 0},

  {NULL, NULL, 0}
};

void attribute_visible R_init_r2c(DllInfo *info)
{
 /* Register the .C and .Call routines.
    No .Fortran() or .External() routines,
    so pass those arrays as NULL.
  */
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, FALSE);
}

