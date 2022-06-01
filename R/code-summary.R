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

## We'll be changing this to be less like sum?
##
## Maybe we should also have a low error sum.

f_summary_base <- '
static void %%s(%%s) {
  int di1 = datai[0];
  int di2 = datai[1];
  long double tmp = 0;

  R_xlen_t len_n = lens[di1];
  %sdouble * dat = data[di1];
  int narm = flag;  // only one possible flag parameter
%s%s

  *data[di2] = (double) tmp;
  lens[di2] = 1;
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
  long double tmp = 0;
  *res = 0;
  int narm = flag;  // only one possible flag parameter

  for(int arg = 0; arg < narg; ++arg) {
    int din = datai[arg]
    R_xlen_t len_n = lens[din];
    double * dat = data[din];
    %s
  }
  // R checks if we would overflow, but we assume infinity exists in double so
  // no overflow (at least in IEEE-754?).
  *data[datai[narg]] = (double) tmp;
  lens[datai[narg]] = 1;
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
  args <- c(ARGS.NM.BASE, if(multi) ARGS.NM.VAR, ARGS.NM.FLAG)
  def.args <- F.ARGS.ALL[match(args, ARGS.NM.ALL)]

  code_res(
    defn=sprintf(f_summary[[name]], name, toString(def.args)),
    name=name, args=args, headers="<math.h>"
  )
}
## @param x a list of the matched parameters for the call `call`
## @call the unmatched (sub)call as provided by the user

ctrl_val_summary <- function(ctrl, flag, call) {
  # for mean.default
  if(!is.null(ctrl[['trim']]) && !identical(ctrl[['trim']], 0))
    stop(
      "Only the default value for `trim` is allowed in ",
      "`", deparse1(call), "`."
    )
  if(!flag[['na.rm']] %in% c(TRUE, FALSE))
    stop("`na.rm` must be TRUE or FALSE in ", "`", deparse1(call), "`.")

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

mean0 <- function(x, na.rm=TRUE) sum(x, na.rm=TRUE) / length(x)

#' Length

f_length <- '
static void %s(%s) {
  *data[datai[1]] = (double) lens[datai[0]];
  lens[datai[1]] = 1;
}'
code_gen_length <-  function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    identical(., "length"),
    args.reg=list(NULL),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  name <- "r2c_length"
  defn <- sprintf(f_length, name, toString(F.ARGS.BASE))
  code_res(defn=defn, name=name, args=ARGS.NM.BASE)
}

