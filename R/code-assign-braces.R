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

# - Copy Prior Result for No-ops -----------------------------------------------

# No-op functions like assignment and braces don't compute, thus don't write to
# memory.  To ensure that their "output" is written to the result vector, we
# need to make sure it is explicitly copied (e.g. when we assign an external
# symbol, we actually want that copied into the result).

f_copy <- '
// Copy a vector, intended to be copied to result
static void %s(%s) {
  R_xlen_t len0 = lens[di[0]];
  double * res = data[di[1]];
  double * input = data[di[0]];
  // this should be a debug mode check only
  if(res == input) Rf_error("Internal Error: copying in-place.");
  memcpy(res, input, sizeof(*res) * len0);
  lens[di[1]] = len0;
}'

code_gen_copy <- function(fun, pars, par.types) {
  vetr(
    identical(., "vcopy"),
    pars=list(NULL),
    par.types=character() && all(. %in% PAR.INT)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_copy, name, toString(F.ARGS.BASE))
  code_res(defn=defn, name=name)
}
#' @rdname intermediate-representation
#' @export

vcopy <- function(x) x + 0

# No-op functions like assignment and braces don't compute, thus don't write to
# memory.  To ensure that their "output" is written to the result vector, we
# need to make sure it is explicitly copied (e.g. when we assign an external
# symbol, we actually want that copied into the result).

f_rec <- '
// Reconciliation is a no-op
// static void %s(%s) { /* NOOP */ }'

code_gen_rec <- function(fun, pars, par.types) {
  vetr(
    identical(., "rec"),
    pars=list(NULL),
    par.types=character() && all(. %in% PAR.INT)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_rec, name, toString(F.ARGS.BASE))
  code_res(defn=defn, name=name, out.ctrl=CGEN.OUT.NONE)
}
#' @rdname intermediate-representation
#' @export

rec <- function(x) x

# - Braces and Assign ----------------------------------------------------------

f_braces <- '
// Braces are a no-op; allocation handles them.
// static void %s(%s) { /* NOOP */ }'
code_gen_braces <- function(fun, pars, par.types) {
  vetr(
    identical(., "{"),
    pars=list(),
    par.types=character() && all(. %in% PAR.INT)
  )
  # Empty braces should have been preprocessed into {numeric(0)}
  if(length(pars) < 1L) stop("Empty braces expresssions disallowed.")
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_braces, name, toString(c(F.ARGS.BASE, F.ARGS.VAR)))
  code_res(defn=defn, narg=TRUE, name=name, out.ctrl=CGEN.OUT.NONE)
}
transform_braces <- function(call) {
  if(!is.brace_call(call))
    stop("Internal Error: expected braces call but got ", deparse1(call))
  # Empty braces
  if(length(call) == 1L) {
    call[[2L]] <- quote(numeric(length=0))
    dot_names(call)
  } else call
}

f_assign <- '
// Read-only assignments are a no-op; allocation handles them.
// static void %s(%s) { /* NOOP */ }'
code_gen_assign <- function(fun, pars, par.types) {
  vetr(
    isTRUE(. %in% c("=", "<-")),
    pars=list(NULL, NULL),
    par.types=character() && all(. %in% PAR.INT)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_assign, name, toString(F.ARGS.BASE))
  code_res(defn=defn, name=name, out.ctrl=CGEN.OUT.NOOP)
}
# Transform for Complex Assignments
#
# E.g. turn things like `x[s] <- y` into `subassign(x, s, y)`.
#
# See code-subset.R

assign_transform <- function(call) {
  if(!is.call(call))
    stop("Internal Error: expected call but got ", typeof(call))
  if(length(call) != 3L)
    stop(
      "Internal Error: expected length 3 assign call but got:\n",
      deparseLines(call)
    )

  call.sym <- get_lang_name(call)
  if(call.sym %in% ASSIGN.SYM.BASE) {
    if(is.call(call[[2L]]) && get_lang_name(call[[2L]]) == "[") {
      if(length(call[[2L]]) == 3L && is.symbol(call[[c(2L,2L)]])) {
        call <- call("x", call[[c(2L,2L)]], call[[c(2L,3L)]], call[[3L]])
        call[[1L]] <- call("::", quote(r2c), quote(subassign))
        names(call) <- c('', 'x', 's', 'y')
      } else {
        stop("Unsupported sub-assignment form:\n", deparseLines(call))
      }
    }
  }
  call
}

