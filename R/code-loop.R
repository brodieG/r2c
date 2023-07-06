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

# See code-ifelse.R for an explanation of how this works.

f_for_init <- '
// Check whether a loop has any iterations
static int %s(%s) {
  return (int) lens[di[0]] > 0;
}'
f_for_iter <- '
// Check whether a loop has any iterations
static int %s(%s) {
  return (int) lens[di[0]] > 0;
}'
f_for_other <- '
// NO-OP, not output but needed for internal checks
// static void %s(%s) { /* NOOP */ }'

code_gen_for_init <- function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    identical(., "for_init"),
    args.reg=list(NULL, NULL),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_for_init, name, toString(F.ARGS.BASE))
  code_res(
    defn=defn, name=name,
    c.call.gen=function(...)
      paste0("if(", sub(";$", "", c_call_gen(...)), ") {")
  )
}
code_gen_for_iter <- function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    identical(., "for_iter"),
    args.reg=list(NULL, NULL, NULL),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_for_iter, name, toString(F.ARGS.BASE))
  code_res(
    defn=defn, name=name,
    c.call.gen=function(...)
      paste0("while(f_for_iter(", sub(";$", "", c_call_gen(...)), ") {")
  )
}
code_gen_for_n <- function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    identical(., "for_n"),
    args.reg=list(NULL),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_for_other, name, toString(F.ARGS.BASE))
  code_res(
    defn=defn, name=name, c.call.gen=function(...) "} } else {",
    out.ctrl=CGEN.OUT.CALL
  )
}
code_gen_for_0 <- function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    identical(., "for_0"),
    args.reg=list(NULL),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_for_other, name, toString(F.ARGS.BASE))
  code_res(
    defn=defn, name=name, c.call.gen=function(...) "}",
    out.ctrl=CGEN.OUT.CALL
  )
}
code_gen_r2c_for <-function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    identical(., "r2c_for"),
    args.reg=list(NULL, NULL),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_for_other, name, toString(F.ARGS.BASE))
  code_res(defn=defn, name=name, out.ctrl=CGEN.OUT.NONE)
}
code_gen_for <- function(...) {
  stop(
    "Internal Error: attempting to generate code for raw `for` ",
    "instead of decomposed one."
  )
}

#' Loop Stub Functions
#'
#' @keywords internal
#' @export

r2c_for <- function() NULL

#' @rdname r2c_for
#' @export

for_init <- function(seq, seq.i) NULL
#'
#' @rdname r2c_for
#' @export

for_iter <- function(var, seq, seq.i) NULL

#' @rdname r2c_for
#' @export

for_n <- function(expr.n) NULL

#' @rdname r2c_for
#' @export

for_0 <- function(expr.0) NULL



