## Copyright (C) Brodie Gaslam
##
## This file is part of "r2c - A DSL for Fast Statistic Computation in R"
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

#' @include code-summary.R

double_size_tpl <- '
  if(lend >= 0 && lend < R_XLEN_T_MAX) {
    R_xlen_t len = (R_xlen_t) lend;%s
  } else {
    Rf_error("Invalid vector size (got %%%%f)", data[di[0]][0]);
  }'

f_numeric_core <- '
    R_xlen_t i;
    // AFAICT 0.0 not guaranteed to be all zeroes in binary representation, so
    // cannot just memset.  If we used a simple loop instead of macro
    // compiler can replace with memset in common case where zero is zeroes.
    // So something to consider here.
    LOOP_W_INTERRUPT1(len, {res[i] = 0;});
    lens[di[narg]] = len;'

f_numeric_size_check <- sprintf(double_size_tpl, f_numeric_core)

# Strictly we could make do without numeric() because it only works as a
# constant expression, but because we use `numeric(0)` for the implicit branches
# we are keeping it around for now.
f_numeric <- sprintf('
static void %%s(%%s) {
  if(lens[di[0]] != 1) Rf_error("invalid length argument");
  int narg = 1;
  double lend = data[di[0]][0];
  double * res = data[di[1]];%s
}', f_numeric_size_check
)
f_numeric_along <- sprintf('
static void %%s(%%s) {
  int narg = 1;
  R_xlen_t len = lens[di[0]];
  double * res = data[di[1]];%s}',
  repad(f_numeric_core, 2)
)
f_numeric_alongn <- sprintf('
static void %%s(%%s) {
  double lend = 1;
  // assume overflow to infinity since we require infinity support
  for(int i = 0; i < narg; ++i) lend *= (double) lens[di[i]];
  double * res = data[di[narg]];%s}
', f_numeric_size_check
)
# Sizing fun for things like `numeric(x)`

numeric_size <- function(alloc, idx, gmax, gmin) {
  if(length(idx) != 1L)
    stop(
      "There should only be one sizing param, got ", length(idx)
    )
  val <- alloc[['dat']][[idx]]
  if(!is.numeric(val) || length(val) != 1L || is.na(val) || val < 0)
    stop("Invalid size parameter.")
  list(val)  # this is a size.coef
}

code_gen_numeric <- function(fun, pars, par.types) {
  vetr(
    identical(., "numeric"),
    pars=list(NULL),
    par.types=character(1) && all(. %in% PAR.ICNST.NUM)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_numeric, name, toString(CF.ARGS.BASE))
  code_res(defn=defn, name=name)
}
code_gen_numeric_along <- function(fun, pars, par.types) {
  vetr(
    identical(., "numeric_along"),
    pars=list(NULL),
    par.types=character(1) && all(. %in% PAR.IVARY)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_numeric_along, name, toString(CF.ARGS.BASE))
  code_res(defn=defn, name=name)
}
code_gen_numeric_alongn <- function(fun, pars, par.types) {
  vetr(
    identical(., "numeric_alongn"),
    pars=list(),
    par.types=character() && all(. %in% PAR.IVARY)
  )
  name <- FUN.NAMES[fun]
  defn <-
    sprintf(f_numeric_alongn, name, toString(c(CF.ARGS.BASE, CF.ARGS.VAR)))
  code_res(defn=defn, name=name, narg=TRUE)
}

#' Initialize a Numeric Vector Sized to Match Input
#'
#' Generates a numeric vector of the same size as the input.  Equivalent to
#' `numeric(length(x))`.  `numeric_alongn` supports multiple vectors in the
#' input, for which the result size is the product of the lengths of the inputs.
#'
#' @export
#' @seealso [base::numeric]
#' @param along.with vector to use for sizing the result.
#' @param ... vectors to use for sizing the result.
#' @return a zero numeric vector the same length as the product of the lengths
#'   of all the vectors in provided as inputs.
#' @examples
#' numeric_along(1:3)
#' numeric_alongn(1:3, 1:2)

numeric_along <- function(along.with) numeric(length(along.with))

#' @export
#' @rdname numeric_along

numeric_alongn <- function(...) numeric(prod(lengths(list(...))))
