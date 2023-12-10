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

#' @include r2c-package.R

f_iftest <- '
// Check a test condition, mostly enforces length
static int %s(%s) {
  if(lens[di[0]] != 1) {
    if(lens[di[0]] > 1) Rf_error("the condition has length > 1");
    else if(lens[di[0]] < 1) Rf_error("the condition is of length zero");
  }
  if (ISNAN(data[di[0]][0])) Rf_error("missing value where TRUE/FALSE needed.");
  lens[di[1]] = 1;
  return (int) data[di[0]][0];
}'

f_ifother <- '
// if/else does not generate a definition
// static void %s(%s) { /* NOOP */ }'

code_gen_if_test <- function(fun, pars, par.types) {
  vetr(
    identical(., IF.TEST),
    pars=list(NULL),
    par.types=character() && all(. %in% PAR.IVARY)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_iftest, name, toString(CF.ARGS.BASE))
  code_res(
    defn=defn, name=name,
    c.call.gen=function(...)
      paste0("if(", sub(";$", "", c_call_gen(...)), ") {")
  )
}
code_gen_if_true <- function(fun, pars, par.types) {
  vetr(
    identical(., "if_true"),
    pars=list(NULL),
    par.types=character() && all(. %in% PAR.IVARY)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_ifother, name, toString(CF.ARGS.BASE))
  code_res(
    defn=defn, name=name, c.call.gen=function(...) "} else {",
    out.ctrl=CGEN.OUT.CALL
  )
}
code_gen_if_false <- function(fun, pars, par.types) {
  vetr(
    identical(., "if_false"),
    pars=list(NULL),
    par.types=character() && all(. %in% PAR.IVARY)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_ifother, name, toString(CF.ARGS.BASE))
  code_res(
    defn=defn, name=name, c.call.gen=function(...) "}", out.ctrl=CGEN.OUT.CALL
  )
}
code_gen_r2c_if <- function(fun, pars, par.types) {
  vetr(
    identical(., "r2c_if"),
    pars=list(NULL, NULL),
    par.types=character() && all(. %in% PAR.IVARY)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_ifother, name, toString(CF.ARGS.BASE))
  code_res(defn=defn, name=name, out.ctrl=CGEN.OUT.NONE)
}
code_gen_if <- function(...) {
  stop(
    "Internal Error: attempting to generate code for raw `if/else` ",
    "instead of decomposed one."
  )
}
#' Intermediate Representation Functions
#'
#' Functions used by the `r2c` preprocessor to represent control structures in a
#' format that facilitates translation to C code.
#'
#' @param x language to wrap in markers that designate it for special treatment
#'   by the memory allocator at run time.
#' @param rec.i identifier for each instance of a use before set loop variable.
#' @param cond expression to evaluate to determine which branch is taken.
#' @param true expression to evaluate if previous `if_test` is TRUE.
#' @param false expression to evaluate if previous `if_test` is FALSE
#' @param expr branch expression to evaluate.
#' @param iter expression that increments the loop iteration variable.
#' @param for.n expression to evaluate if there is at least one loop iteration.
#' @param for.0 expression to evaluate if there are no loop iterations.
#' @param seq expression that generates vector to loop over.
#' @param seq.i position in `seq` the loop is at.
#' @param var variable to hold the loop iteration value.
#' @return These functions are not intended for use at the R level thus their
#'   return values are undocumented.
#'
#' @seealso [Preprocessing][r2c-preprocess] for details on how these functions
#'   are used, [subassign] (another internal representation function).
#' @export
#' @keywords internal
#' @rdname intermediate-representation
#' @aliases r2c_if if_true if_false if_test r2c_for for_init for_iter for_n
#'   for_0 vcopy rec lset lcopy

r2c_if <- function(true, false) true

#' @export
#' @keywords internal
#' @rdname intermediate-representation

if_true <- function(expr) expr

#' @export
#' @keywords internal
#' @rdname intermediate-representation

if_false <- function(expr) expr

#' @export
#' @keywords internal
#' @rdname intermediate-representation

if_test <- function(cond) expr

