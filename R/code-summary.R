## Copyright (C) 2022 Brodie Gaslam
##
## This file is part of "fapply - DSL For Fast Groupwise Numeric Calculations"
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

## We'll be changing this to be less like sum?
##
## Maybe we should also have a low error sum.

f_summary_base <- '
static void %%s(%%s) {
  double * res = data[datai[1]] + off[1];
  *res = 0;
  R_xlen_t len_n = len[0];
  double * dat = data[datai[0]] + off[0];
  int narm = asInteger(VECTOR_ELT(ctrl, 0));

  %s%s
}
'
loop_base <- '
  if(!narm)
    for(R_xlen_t i = 0; i < len_n; ++i) *res += dat[i];
  else
    for(R_xlen_t i = 0; i < len_n; ++i) if(!isnan(dat[i])) *res += dat[i];
'
f_mean <- sprintf(f_summary_base, loop_base, "\n  *res /= len_n;")
f_sum_1 <- sprintf(f_summary_base, loop_base, "")
f_sum_n_base <- '
static void %%s(%%s) {
  double * res = data[narg] + off[narg];
  *res = 0;

  for(int arg = 0; arg < narg; ++arg) {
    R_xlen_t len_n = len[narg];
    double * dat = data[arg] + off[arg];
    %s
  }
}
'
f_sum_n <- sprintf(f_sum_n_base, loop_base);

## Structure all strings into a list for ease of selection

f_summary <- list(
  sum=f_sum_1,
  sum_n=f_sum_n,
  mean=f_mean
)
code_gen_summary <- function(fun, args.reg, args.ctrl) {
  vetr(CHR.1 && . %in% names(f_summary), list(), list())
  multi <- length(args.reg) > 1L
  name <- paste0(fun, if(multi) "_n")
  args <- c(ARGS.NM.BASE, if(multi) ARGS.NM.VAR, ARGS.NM.CTRL)
  call.args <- c(CALL.BASE, if(multi) CALL.VAR, CALL.CTRL)
  def.args <- F.ARGS.ALL[match(args, ARGS.NM.ALL)]

  list(
    defn=sprintf(f_summary[[name]], name, toString(def.args)),
    name=name,
    call=sprintf("%s(%s);", name, toString(call.args)),
    args=args,
    headers="<math.h>"
  )
}
## @param x a list of the matched parameters for the call `call`
## @call the unmatched (sub)call as provided by the user

ctrl_val_summary <- function(x, call) {
  if(!identical(x[['trim']], 0))
    stop(
      "Only the default value for `trim` is allowed in ", 
      "`", deparse1(call), "`."
    )
  if(!x[['na.rm']] %in% c(TRUE, FALSE))
    stop("`na.rm` must be TRUE or FALSE in ", "`", deparse1(call), "`.")
  TRUE
}

