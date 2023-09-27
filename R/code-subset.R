## Copyright (C) Brodie Gaslam
##
## This file is part of "r2c - Fast Iterated Statistic Computation in R"
##
## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 2 or 3 of the License.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## Go to <https://www.r-project.org/Licenses> for copies of the licenses.

f_subset <- '
static void %s(%s) {
  double * res = data[di[2]];
  double * dat = data[di[0]];
  double * index = data[di[1]];

  R_xlen_t len = lens[di[1]];
  R_xlen_t lend = lens[di[0]];

  for(R_xlen_t i = 0; i < len; ++i) {
    double ival = index[i];
    int isna = ISNAN(ival);
    if(!isna && ival > 0 && ival <= lend) {
      res[i] = dat[(R_xlen_t)ival - 1];
    } else if (isna || ival > 0) {
      res[i] = NA_REAL;
    } else
      Rf_error(
        "Only strictly positive index values allowed, found: [%%jd].",
        (intmax_t) ival
      );
  }
  lens[di[2]] = len;
}'

code_gen_subset <- function(fun, pars, par.types) {
  vetr(
    identical(., "["),
    pars=list(NULL, NULL),
    par.types=character() && all(. %in% PAR.INT)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_subset, name, toString(F.ARGS.BASE))
  code_res(defn=defn, name=name)
}
# Does *NOT* return the sub-assignment values, so can only be used incontext
# where the return value is not used.  This should be checked for in the
# preprocessing, allocation, etc.

f_subset_assign <- '
static void %s(%s) {
  double * res = data[di[0]];  // assigns to first input
  double * index = data[di[1]];
  double * dat = data[di[2]];

  R_xlen_t lenr = lens[di[0]];
  R_xlen_t leni = lens[di[1]];
  R_xlen_t lenv = lens[di[2]];
  R_xlen_t v = 0;

  for(R_xlen_t i = 0; i < leni; ++i, ++v) {
    if(v >= lenv) v = 0;
    double ival = index[i];
    int isna = ISNAN(ival);
    if(!isna && ival > 0 && ival <= lenr) {
      R_xlen_t ival0 = ival - 1;
      res[ival0] = dat[v];
    } else if (isna) {
      Rf_error("NAs are not allowed in subscripted assignments.");
    } else if (ival <= R_XLEN_T_MAX) {
      Rf_error(
        "Subassignments must be in bounds of target vector; oob index: [%%jd].",
        (intmax_t) ival
      );
    } else {
      Rf_error(
        "Subassignments must be in bounds of target vector; oob index: "
        "[>R_XLEN_T_MAX]."
      );
    }
  }
  if(leni && v != lenv) data[%s][%s] = 1.;   // bad recycle
}'

# Validator for Subset/Subassign
#
# Verifies that second argument is numeric (specifically not logical, which is a
# problem for subset in particular, but for now blocking it for subassign too)

type.tpl <- character()
names(type.tpl) <- character()
subset_input_val <- function(types) {
  vetr(type.tpl && all(. %in% NUM.TYPES) && length(.) %in% 2:3)
  if(!types[[2L]] %in% c('integer', 'double'))
    stop(
      "Subset and sub-assign require numeric index vectors (got ", types[[2L]],
    ").")
  TRUE
}

code_gen_subassign <- function(fun, pars, par.types) {
  vetr(
    identical(., "subassign"),
    pars=list(NULL, NULL, NULL),
    par.types=character() && all(. %in% PAR.INT)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(
    f_subset_assign, name, toString(F.ARGS.BASE),
    IX[['I.STAT']], IX[['STAT.RECYCLE']] # these are now available as defines
  )
  code_res(defn=defn, name=name)
}

#' Assign to a Subset of a Vector
#'
#' "Internal" function that implement subassignment (e.g. `x[s] <- y`) for
#' `r2c`.  Standard R subassignments are converted to `subassign` so that the
#' `r2c` [preprocessor][r2c-preprocess] can identify them as a sub-assignment
#' instead of an a subset nested in an assignment.
#'
#' Unlike the R counterpart, or the `r2c` usage of this function,
#' regular R usage of this function does not modify `x` outside of the internal
#' scope of the function.  This function is an implementation detail and is
#' documented only for the rare cases where it becomes visible to the user.
#'
#' @seealso Other [intermediate-representation][intermediate representation functions], 
#' [r2c-preprocess][`r2c` preprocessor].
#' @keywords internal
#' @param x a numeric vector
#' @export
#' @return `x` with positions designated by `s` replaced by values in `y`

subassign <- function(x, s, y) {
  x[s] <- y
  x
}

