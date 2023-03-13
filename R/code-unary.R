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

#' @include constants.R

NULL

#' Unary Operator Equivalent Functions
#'
#' Required so that the compiler internals can easily disambiguate unary
#' plus/minus from the binary forms.  These should never be needed by the user,
#' but they are generated in the compilation step so that the C counterparts can
#' be used by `{r2c}`.
#'
#' @export
#' @param x a logical numeric vector
#' @return `x` for `uplus` (possibly coerced to integer/numeric), `-x` for
#'   `uminus`

uplus <- function(x) x

#' @export
#' @rdname uplus

uminus <- function(x) -x

UOP.MAP <- c("-"="uminus", "+"="uplus")

## Convert unary +/- into the unary functions
unary_transform <- function(call) {
  name <- get_lang_name(call)
  if(name %in% c("+", "-") && length(call) == 2L) {
    call[[1L]] <- call("::", quote(r2c), as.name(UOP.MAP[name]))
    names(call) <- c("", "x")
  }
  call
}
## See OP.DEFN in code-bin.R

UOP.DEFN <- c(
  "!"="#define NEGATE(x) ((x) == 0 ? 1 : (ISNAN(x) ? NA_REAL : 0))"
)
UOP.OP <- c("uplus"="+", "uminus"="-", "!"="NEGATE")

f_unary <- paste0('
static void %1$s(%2$s) {
  int di0 = di[0];
  int dires = di[1];
  double * e1 = data[di0];
  double * res = data[dires];
  R_xlen_t len = lens[di0];

  LOOP_W_INTERRUPT(len, {res[i] = %3$s(%4$s e1[i]);});

  lens[dires] = len;
}')
code_gen_unary <- function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    CHR.1 && . %in% c("!", "uplus", "uminus"),
    args.reg=list(NULL),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  is.neg <- fun == "!"
  name <- FUN.NAMES[fun]
  op <- UOP.OP[fun]      # needed for modulo
  defn <- sprintf(
    f_unary, name, toString(F.ARGS.BASE),
    if(is.neg) op else "", if(!is.neg) op else ""
  )
  code_res(
    defn=defn, name=name, defines=if(is.neg) UOP.DEFN[fun],
    noop=fun == "uplus"
  )
}

