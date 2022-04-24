/*
 * Copyright (C) 2022 Brodie Gaslam
 *
 * This file is part of "fapply - Fast Apply"
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
#include "fapply.h"

static const
R_CallMethodDef callMethods[] = {
  {"run", (DL_FUNC) &FAPPLY_run, 3},
  {"run2", (DL_FUNC) &FAPPLY_run2, 3},
  {"run3", (DL_FUNC) &FAPPLY_run3, 5},
  {"run3a", (DL_FUNC) &FAPPLY_run3a, 5},
  {"run3b", (DL_FUNC) &FAPPLY_run3b, 4},
  {"test1", (DL_FUNC) &FAPPLY_test1, 4},
  {"test2", (DL_FUNC) &FAPPLY_test2, 4},
  {"test3", (DL_FUNC) &FAPPLY_test3, 4},
  {"test4", (DL_FUNC) &FAPPLY_test4, 4},
  {"test5", (DL_FUNC) &FAPPLY_test5, 4},
  {NULL, NULL, 0}
};

void attribute_visible R_init_fapply(DllInfo *info)
{
 /* Register the .C and .Call routines.
    No .Fortran() or .External() routines,
    so pass those arrays as NULL.
  */
  R_registerRoutines(info, NULL, callMethods, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, FALSE);
}

