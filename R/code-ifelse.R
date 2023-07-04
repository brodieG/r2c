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

code_gen_if_test <- function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    identical(., "if_test"),
    args.reg=list(NULL),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_iftest, name, toString(F.ARGS.BASE))
  code_res(
    defn=defn, name=name,
    c.call.gen=function(...)
      paste0("if(", sub(";$", "", c_call_gen(...)), ") {")
  )
}
code_gen_if_true <- function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    identical(., "if_true"),
    args.reg=list(NULL),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_ifother, name, toString(F.ARGS.BASE))
  code_res(
    defn=defn, name=name, c.call.gen=function(...) "} else {",
    out.ctrl=CGEN.OUT.CALL
  )
}
code_gen_if_false <- function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    identical(., "if_false"),
    args.reg=list(NULL),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_ifother, name, toString(F.ARGS.BASE))
  code_res(
    defn=defn, name=name, c.call.gen=function(...) "}", out.ctrl=CGEN.OUT.CALL
  )
}
code_gen_r2c_if <-function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    identical(., "r2c_if"),
    args.reg=list(NULL, NULL),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_ifother, name, toString(F.ARGS.BASE))
  code_res(defn=defn, name=name, out.ctrl=CGEN.OUT.NONE)
}
code_gen_if <- function(...) {
  stop(
    "Internal Error: attempting to generate code for raw `if/else` ",
    "instead of decomponsed one."
  )
}
#' If / Else Counterparts
#'
#' "Internal" functions that integrate the `if / else` construct between the R
#' and C level.  These are purely an implementation detail and not intended to
#' be used directly, but are documented so users can understand what they are if
#' they encounter them when inspect "r2c_fun" objects.
#'
#' Most R level calls are converted 1-1 into C level calls.  Control structures
#' are more complicated because we need to generate the call structure itself
#' without a direct correspondence of R call to structural element.  The
#' preprocessor decomposes regular `if / else` calls such as:
#'
#' ```
#' if(a) x else y
#' ```
#' Into:
#' ```
#' if_test(a)
#' r2c_if(if_true(x), if_false(y))
#' ```
#'
#' The decomposition creates a 1-1 R-C correspondence without changing the
#' overall semantics (although the intermediate semantics are not the same due
#' to the use of implicit state to decide what branch to evaluate).  You can
#' run these functions as R functions, but there is no reason to do so, and
#' further `r2c_if` will always return the true branch as the state from
#' `if_test` is not recorded in pure R evaluation.  There is only a loose
#' correspondence between the R function names and the C code they cause to be
#' generated as we exploit how `r2c` linearizes the AST to cause the pieces of
#' the control structure to be emitted at the right spots (i.e.  this is a hack
#' to get control flow to fit into an implementation that originally did not
#' intend to allow them).
#'
#' @export
#' @param true expression to evaluate if previous `if_test` is TRUE.
#' @param false expression to evaluate if previous `if_test` is FALSE
#' @param expr an expression to evaluate
#' @return for `if_true` and `if_false`, the result of evaluating `expr`, for
#'   `if_test` the result of evaluating `test` if that is length 1 (otherwise an
#'   error) 
#' @examples
#' get_r_code(r2cq(if(a) b else c), raw=TRUE)

r2c_if <- function(true, false) true

#' @export
#' @rdname r2c_if
if_true <- function(expr) expr

#' @export
#' @rdname r2c_if
if_false <- function(expr) expr

#' @export
#' @rdname r2c_if
if_test <- function(cond) expr

