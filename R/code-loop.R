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
  (void) data; // unused
  return (int) lens[di[0]] > 0;
}'
f_for_iter <- '
// Increment the iteration variable
static int %s(%s) {
  R_xlen_t seq_i = (R_xlen_t) data[di[2]][0];
  R_xlen_t seq_len = lens[di[1]];
  if(seq_i < seq_len) {
    // set the iteration variable to new value
    data[di[0]][0] = data[di[1]][seq_i];
    // increment the pointer into the iteration vector for next iteration
    data[di[2]][0]++;
    return 1;
  } else {
    return 0;
  }
}'
f_for_other <- '
// NO-OP, not output but needed for internal checks
// static void %s(%s) { /* NOOP */ }'

code_gen_for_init <- function(fun, pars, par.types) {
  vetr(
    identical(., "for_init"),
    pars=list(NULL, NULL),
    par.types=character() && all(. %in% PAR.INT)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_for_init, name, toString(F.ARGS.BASE))
  code_res(
    defn=defn, name=name,
    c.call.gen=function(...)
      paste0("if(", sub(";$", "", c_call_gen(...)), ") {")
  )
}
code_gen_for_iter <- function(fun, pars, par.types) {
  vetr(
    identical(., "for_iter"),
    pars=list(NULL, NULL, NULL),
    par.types=character() && all(. %in% PAR.INT)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_for_iter, name, toString(F.ARGS.BASE))
  code_res(
    defn=defn, name=name,
    c.call.gen=function(...)
      paste0("while(", sub(";$", "", c_call_gen(...)), ") {")
  )
}
code_gen_for_n <- function(fun, pars, par.types) {
  vetr(
    identical(., FOR.N),
    pars=list(NULL),
    par.types=character() && all(. %in% PAR.INT)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_for_other, name, toString(F.ARGS.BASE))
  code_res(
    defn=defn, name=name, c.call.gen=function(...) "} } else {",
    out.ctrl=CGEN.OUT.CALL
  )
}
code_gen_for_0 <- function(fun, pars, par.types) {
  vetr(
    identical(., FOR.0),
    pars=list(NULL),
    par.types=character() && all(. %in% PAR.INT)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_for_other, name, toString(F.ARGS.BASE))
  code_res(
    defn=defn, name=name, c.call.gen=function(...) "}",
    out.ctrl=CGEN.OUT.CALL
  )
}
code_gen_r2c_for <-function(fun, pars, par.types) {
  vetr(
    identical(., "r2c_for"),
    pars=list(NULL, NULL, NULL),
    par.types=character() && all(. %in% PAR.INT)
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

f_luse <- '
// marker for alloc, so noop
// static void %s(%s) { /* NOOP */ }'

code_gen_luse <- function(fun, pars, par.types) {
  vetr(
    identical(., "luse"),
    pars=list(NULL, NULL),
    par.types=character() && all(. %in% PAR.INT)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_rec, name, toString(F.ARGS.BASE))
  code_res(defn=defn, name=name, out.ctrl=CGEN.OUT.NONE)
}

f_lset <- '
// marker for alloc, so noop
// static void %s(%s) { /* NOOP */ }'

code_gen_lset <- function(fun, pars, par.types) {
  vetr(
    identical(., "lset"),
    pars=list(NULL, NULL),
    par.types=character() && all(. %in% PAR.INT)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_rec, name, toString(F.ARGS.BASE))
  code_res(defn=defn, name=name, out.ctrl=CGEN.OUT.NONE)
}

f_lrec <- '
// marker for alloc, so noop
// static void %s(%s) { /* NOOP */ }'

code_gen_luse <- function(fun, pars, par.types) {
  vetr(
    identical(., "luse"),
    pars=list(NULL, NULL),
    par.types=character() && all(. %in% PAR.INT)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_rec, name, toString(F.ARGS.BASE))
  code_res(defn=defn, name=name, out.ctrl=CGEN.OUT.NONE)
}

f_lrec <- '
// Copy loop value to a use before set variable so that the variable will be
// updated before beginning each subsequent loop iteration.
static void %s(%s) {
  R_xlen_t len0 = lens[di[0]];
  double * res = data[di[2]];  // skip rec.i used only by alloc
  double * input = data[di[0]];
  // this should be a debug mode check only
  if(res == input) Rf_error("Internal Error: copying in-place.");
  if(len0 != lens[di[2]]) Rf_error("Internal Error: mismatched copy sizes.");
  memcpy(res, input, sizeof(*res) * len0);
}'

code_gen_lrec <- function(fun, pars, par.types) {
  vetr(
    identical(., "lrec"),
    pars=list(NULL, NULL),
    par.types=character() && all(. %in% PAR.INT)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_copy, name, toString(F.ARGS.BASE))
  code_res(defn=defn, name=name)
}

#' @rdname intermediate-representation
#' @export

r2c_for <- function(iter, for.n, for.0) NULL

#' @rdname intermediate-representation
#' @export

for_init <- function(seq, seq.i) NULL

#' @rdname intermediate-representation
#' @export

for_iter <- function(var, seq, seq.i) NULL

#' @rdname intermediate-representation
#' @export

for_n <- function(expr) expr

#' @rdname intermediate-representation
#' @export

for_0 <- function(expr) expr

#' @rdname intermediate-representation
#' @export

luse <- function(x, rec.i) x

#' @rdname intermediate-representation
#' @export

lset <- function(x, rec.i) x

#' @rdname intermediate-representation
#' @export

lrec <- function(x, rec.i) x


