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

#' @param e1 a logical or numeric vector
#' @rdname intermediate-representation
#' @export

uplus <- function(e1) x

#' @param e1 a logical or numeric vector
#' @rdname intermediate-representation
#' @export

uminus <- function(e1) -x

UOP.MAP <- c("-"="uminus", "+"="uplus")

## Convert unary +/- into the unary functions
unary_transform <- function(call) {
  name <- get_lang_name(call)
  if(name %in% c("+", "-") && length(call) == 2L) {
    call[[1L]] <- pkg_fun(UOP.MAP[name])
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
  R_xlen_t i;

  LOOP_W_INTERRUPT1(len, {res[i] = %3$s(%4$s e1[i]);});

  lens[dires] = len;
}')
code_gen_unary <- function(fun, pars, par.types) {
  vetr(
    CHR.1 && . %in% c("!", "uplus", "uminus"),
    pars=list(NULL),
    par.types=character() && all(. %in% PAR.INT)
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
    out.ctrl=if(fun == "uplus") CGEN.OUT.NOOP else CGEN.OUT.DFLT
  )
}

