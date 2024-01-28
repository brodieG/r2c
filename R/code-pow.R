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

code_gen_pow <-  function(fun, pars, par.types) {
  vetr(
    identical(., "^"),
    pars=list(NULL, NULL),
    par.types=character() && all(. %in% PAR.IVARY)
  )
  name <- FUN.NAMES[fun]
  # Test is not perfect, ideally it would match what USE_POWL_IN_R_POW
  # is set by (from src/gnuwin32/fixed/h/config.h as of ~2024-01)
  arch <- R.version[['arch']]
  os <- R.version[['os']]
  win64 <- identical(c(arch, os), c("x86_64", "mingw32"))
  pow.fun <- if(win64) "powl" else "pow"

  defn <- sprintf(
    bin_op_vec_rec, name, toString(CF.ARGS.BASE), pow.fun, ",",
    IX[['I.STAT']], IX[['STAT.RECYCLE']]
  )
  code_res(defn=defn, name=name, headers="<math.h>")
}

## Interim solution until we add the deduplication of calculations
square_code <- '
static void %1$s(%2$s) {
  int di0 = di[0];
  int dires = di[1];
  double * res = data[dires];

  double * e1 = data[di0];
  R_xlen_t len = lens[di0];

  if(len == 0) { // empty recycle is zero
    lens[dires] = 0;
    return;
  }
  R_xlen_t i;
  LOOP_W_INTERRUPT1(len, res[i] = e1[i] * e1[i];);
  lens[dires] = len;
}'
code_gen_square <- function(fun, pars, par.types) {
  vetr(
    identical(., "square"),
    pars=list(NULL),
    par.types=character() && all(. %in% PAR.IVARY)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(square_code, name, toString(CF.ARGS.BASE))
  code_res(defn=defn, name=name, headers="<math.h>")
}
#' Raise a Vector to the Power of Two
#'
#' Implemented as `x * x` to match what R does with `x ^ 2`.
#'
#' @param x a numeric vector
#' @export
#' @return `x`, squared

square <- function(x) x * x

pow_transform <- function(call) {
  if(
    !is.call(call) || !identical(as.character(call[[1L]]), "^") ||
    length(call) != 3
  )
    stop("Bad exponent call to transform: ", deparse1(call))

  exp <- call[[3L]]
  if(is.integer(exp)) exp <- as.numeric(exp)
  if(identical(exp, 2)) {
    f <- pkg_fun("square")
    call <- as.call(list(f, x=call[[2L]]))
  }
  ## # We'll add these once we implement the re-used calculations
  ## else if(identical(exp, 1)) {
  ## else if(identical(exp, 0)) {
  ## else if(identical(exp, 3)) {
  ##   val <- call[[2L]]
  ##   call <- call("*", call("*", val, val), val)
  ## } else if(identical(exp, 4)) {
  ##   val <- call[[2L]]
  ##   call <- call("*", call("*", val, val), call("*", val, val))
  ## }
  call
}


