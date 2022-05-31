## Copyright (C) 2022 Brodie Gaslam
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

code_gen_pow <-  function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    identical(., "^"),
    args.reg=list(NULL, NULL),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  name <- "power"
  defn <- sprintf(bin_op_vec_rec, name, toString(F.ARGS.BASE), "pow", ",")
  code_res(defn=defn, name=name, args=ARGS.NM.BASE, headers="<math.h>")
}

## Interim solution until we add the deduplication of calculations
square_code <- '
static void %1$s(%2$s) {
  int di1 = datai[0];
  int dires = datai[1];
  double * res = data[dires];

  double * e1 = data[di1];
  R_xlen_t len = lens[di1];

  if(len == 0) { // empty recycle is zero
    lens[dires] = 0;
    return;
  }
  // Mod iterate by region?
  R_xlen_t i;
  for(i = 0; i < len; ++i) res[i] = e1[i] * e1[i];
  lens[dires] = len;
}'
code_gen_square <- function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    identical(., "square"),
    args.reg=list(NULL),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  name <- "sqr"
  defn <- sprintf(square_code, name, toString(F.ARGS.BASE))
  code_res(defn=defn, name=name, args=ARGS.NM.BASE, headers="<math.h>")
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
    call <- call("square", call[[2L]])
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


