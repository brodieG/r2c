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

#' @include code-mean.R

NULL

f_summary_base <- '
static void %%s(%%s) {
  int di0 = di[0];
  int di1 = di[1];
  long double tmp = 0;

  R_xlen_t len_n = lens[di0];
  %sdouble * dat = data[di0];
  int narm = flag;  // only one possible flag parameter

%s%s

  *data[di1] = (double) tmp;
  lens[di1] = 1;
}'
loop.base <- '
if(!narm)
  for(R_xlen_t i = 0; i < len_n; ++i) tmp += dat[i];
else
  for(R_xlen_t i = 0; i < len_n; ++i)
    if(!isnan(dat[i])) tmp += dat[i];%s
'
make_loop_base <- function(count.na=FALSE, pad=2) {
  sprintf(
    paste0(
      strrep(' ', pad),
      unlist(strsplit(loop.base, '\n', fixed=TRUE))[-1L],
      collapse="\n"
    ),
    if(count.na) " else ++na_n;" else ""
  )
}

f_mean0 <- sprintf(
  f_summary_base,
  "R_xlen_t na_n = 0;\n  ",
  make_loop_base(count.na=TRUE),
  "\n  tmp /= (len_n - na_n);\n"
)
f_sum_1 <- sprintf(f_summary_base, "", make_loop_base(count.na=FALSE), "")
f_sum_n_base <- '
static void %%s(%%s) {
  int narm = flag;  // only one possible flag parameter
  *data[di[narg]] = 0;

  for(int arg = 0; arg < narg; ++arg) {
    long double tmp = 0;
    int din = di[arg];
    R_xlen_t len_n = lens[din];
    double * dat = data[din];
%s
    // base uses a double, not long double, intermediate acc.
    // Overflow to Inf (and we check Inf available in assumptions.c)
    *data[di[narg]] += (double) tmp;
  }
  lens[di[narg]] = 1;
}'
f_sum_n <- sprintf(f_sum_n_base, make_loop_base(count.na=FALSE, pad=4))

## Structure all strings into a list for ease of selection

f_summary <- list(
  sum=f_sum_1,
  sum_n=f_sum_n,
  mean0=f_mean0,   # single pass implementation, no infinity check
  mean=f_mean      # R implementation
)
code_gen_summary <- function(fun, args.reg, args.ctrl, args.flag) {
  vetr(
    CHR.1 && . %in% names(f_summary),
    args.reg=list(),
    args.ctrl=list(),
    args.flag=list()
  )
  multi <- length(args.reg) > 1L
  name <- paste0(fun, if(multi) "_n")

  code_res(
    defn=sprintf(
      f_summary[[name]], name,
      toString(c(F.ARGS.BASE, if(multi) F.ARGS.VAR, F.ARGS.FLAG))
    ),
    name=name, narg=multi, flag=TRUE, headers="<math.h>"
  )
}
## @param x a list of the matched parameters for the call `call`
## @call the unmatched (sub)call as provided by the user

ctrl_val_summary <- function(ctrl, flag, call) {
  # for mean.default
  trim <- ctrl[['trim']]
  if(!isTRUE(trim.test <- vet(NULL || identical(., 0), trim)))
    stop(
      paste0(
        c("`trim` must be set to default value (", trim.test, ")"),
        collapse="\n"
    ) )

  na.rm <- flag[['na.rm']]
  if(!isTRUE(na.test <- vet(LGL.1, na.rm)))
    stop(na.test, " in `", deparse1(call), "`.")

  if(flag[['na.rm']]) 1L else 0L # avoid bizarre cases of TRUE != 1L
}
#' Single Pass Mean Calculation
#'
#' [`base::mean`] does a two pass calculation that additional handles cases
#' where `sum(x)` overflows doubles but `sum(x/n)` does not.  This version is a
#' single pass one that does not protect against the overflow case.
#'
#' @inheritParams base::mean
#' @export
#' @return scalar numeric
#' @examples
#' mean0(runif(10))
#'
#' ## Overflow even 80 bit long double:
#' mean0(rep(.Machine$double.xmax, 2^4))
#' mean(rep(.Machine$double.xmax, 2^4))
#'
#' ## Reduced precision
#' x <- runif(10) ^ 2
#' mean(x) - mean0(x)

mean0 <- function(x, na.rm=FALSE)
  sum(x, na.rm=na.rm) /
    if(na.rm) sum(!is.na(x)) else length(x)

## Length

f_length <- '
static void %s(%s) {
  *data[di[1]] = (double) lens[di[0]];
  lens[di[1]] = 1;
}'
code_gen_length <- function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    identical(., "length"),
    args.reg=list(NULL),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  name <- "r2c_length"
  defn <- sprintf(f_length, name, toString(F.ARGS.BASE))
  code_res(defn=defn, name=name)
}

