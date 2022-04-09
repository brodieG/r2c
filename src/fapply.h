/*
 * Copyright (C) 2022  Brodie Gaslam
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

#ifndef FAPPLY_H
#define FAPPLY_H

// System headers go above
#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>

SEXP FAPPLY_run(SEXP so, SEXP fun, SEXP x);
SEXP FAPPLY_run2(SEXP so, SEXP fun_name, SEXP x);
SEXP FAPPLY_run3(SEXP so, SEXP fun_name, SEXP x, SEXP g);

SEXP FAPPLY_test1(SEXP x, SEXP y, SEXP z, SEXP w);
SEXP FAPPLY_test2(SEXP x, SEXP y, SEXP z, SEXP w);
SEXP FAPPLY_test3(SEXP x, SEXP y, SEXP z, SEXP w);
SEXP FAPPLY_test4(SEXP x, SEXP y, SEXP z, SEXP w);
SEXP FAPPLY_test5(SEXP x, SEXP y, SEXP z, SEXP w);

#endif  /* FAPPLY_H */
