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

#' @include code-mean.R
#' @include code-bin.R

NULL

# - Sum / Base -----------------------------------------------------------------

f_summary_base <- '
static void %%s(%%s) {
  int di0 = di[0];
  int di_na = di[1];
  int di_res = di[2];

  R_xlen_t len_n = lens[di0];
  %sdouble * dat = data[di0];
  int narm = (int) *data[di_na];  // checked to be 0 or 1 by valid_narm

%s%s

  *data[di_res] = (double) tmp;
  lens[di_res] = 1;
}'
loop.base <- '
long double tmp = 0;
R_xlen_t i;
if(!narm) LOOP_W_INTERRUPT1(len_n, tmp += dat[i];);
else LOOP_W_INTERRUPT1(len_n, {if(!ISNAN(dat[i])) tmp += dat[i];%s});
'
repad <- function(x, pad=2) {
  split <- unlist(strsplit(x, '\n', fixed=TRUE))
  if(length(split) && !nzchar(split[1L])) split <- split[-1L]
  paste0(strrep(' ', pad), split, collapse="\n")
}
make_loop_base <- function(count.na=FALSE, pad=2) {
  sprintf(repad(loop.base, pad), if(count.na) " else ++na_n;" else "")
}

f_mean1 <- sprintf(
  f_summary_base,
  "R_xlen_t na_n = 0;\n  ",
  make_loop_base(count.na=TRUE),
  "\n  tmp = (double)tmp / (len_n - na_n); // (double) to match R version\n"
)
f_sum_1 <- sprintf(f_summary_base, "", make_loop_base(count.na=FALSE), "")
f_sum_n_base <- '
static void %%s(%%s) {
  int narm = (int) *data[di[narg - 1]];  // checked to be 0 or 1 by valid_narm
%s
  for(int arg = 0; arg < narg - 1; ++arg) {
    int din = di[arg];
    R_xlen_t len_n = lens[din];
    double * dat = data[din];
%s
%s
  }
  lens[di[narg]] = 1;
}'
f_sum_n <- sprintf(
  f_sum_n_base, "", make_loop_base(count.na=FALSE, pad=4),
  "*data[di[narg]] += (double) tmp; // R uses double cross-arg accumulator"
)

# - Any / All ------------------------------------------------------------------

logical.sum.base <- '
int has_nan = 0;
double tmp = %2$d;
R_xlen_t i;

if(!narm)
  LOOP_W_INTERRUPT1(len_n, {
    if(ISNAN(dat[i])) has_nan = 1;
    else if(dat[i] %1$s 0) break;
  });
else
  LOOP_W_INTERRUPT1(len_n, {
    if(!ISNAN(dat[i]) && dat[i] %1$s 0) break;
  });

if(i < len_n) tmp = dat[i] != 0;
else if(has_nan) tmp = NA_REAL;
'
# For all, if any FALSE, FALSE, otherwise if any NA, NA
# For any, if any TRUE, TRUE, otherwise if any NA, NA

make_loop_lgl <- function(op, pad, init)
  repad(sprintf(logical.sum.base, op, init), pad)
make_end_lgl <- function(op, pad)
  sprintf(
    repad(
      '*data[di[narg]] = %1$s(*data[di[narg]], tmp);\nif(i < len_n) break;',
      pad
    ),
    op
  )

f_all_1 <- sprintf(
  f_summary_base, "", make_loop_lgl("==", 2, 1), ""
)
f_all_n <- sprintf(
  f_sum_n_base, "  *data[di[narg]] = 1;\n",
  make_loop_lgl("==", 4, 1), make_end_lgl("AND", 4)
)
f_any_1 <- sprintf(
  f_summary_base, "", make_loop_lgl("!=", 2, 0), ""
)
f_any_n <- sprintf(
  f_sum_n_base, "  *data[di[narg]] = 0;\n",
  make_loop_lgl("!=", 4, 0), make_end_lgl("OR", 4)
)
SUM.DEFN <- c(any_n=unname(OP.DEFN['|']), all_n=unname(OP.DEFN['&']))

# - Gen Funs -------------------------------------------------------------------

## Structure all strings into a list for ease of selection

f_summary <- list(
  sum=f_sum_1,
  sum_n=f_sum_n,
  mean1=f_mean1,   # single pass implementation, no infinity check
  mean=f_mean,     # R implementation
  all=f_all_1,
  any=f_any_1,
  all_n=f_all_n,
  any_n=f_any_n
)
code_gen_summary <- function(fun, pars, par.types) {
  vetr(
    CHR.1 && . %in% names(f_summary),
    pars=list(),
    par.types=
      character() && length(.) == length(pars) &&
      all(. %in% c(PAR.INT, PAR.EXT))
  )
  pars.int <- pars[par.types %in% PAR.INT]

  multi <-
    length(pars.int) > 1L ||
    (length(pars.int) == 1L && identical(pars.int[[1L]], quote(.R2C.DOTS)))
  name <- paste0(FUN.NAMES[fun], if(multi) "_n")
  code_res(
    defn=sprintf(
      f_summary[[name]], name,
      toString(c(F.ARGS.BASE, if(multi) F.ARGS.VAR))
    ),
    name=name, narg=multi, headers="<math.h>",
    ext.any=FALSE,
    defines=if(name %in% names(SUM.DEFN)) SUM.DEFN[name]
  )
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
#' mean1(runif(10))
#'
#' ## Overflow even 80 bit long double:
#' mean1(rep(.Machine$double.xmax, 2^4))
#' mean(rep(.Machine$double.xmax, 2^4))
#'
#' ## Reduced precision
#' x <- runif(10) ^ 2
#' mean(x) - mean1(x)

mean1 <- function(x, na.rm=FALSE)
  sum(x, na.rm=na.rm) /
    if(na.rm) sum(!is.na(x)) else length(x)

## Length

f_length <- '
static void %s(%s) {
  *data[di[1]] = (double) lens[di[0]];
  lens[di[1]] = 1;
}'
code_gen_length <- function(fun, pars, par.types) {
  vetr(
    identical(., "length"),
    pars=list(NULL),
    par.types=character() && all(. %in% PAR.INT)
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_length, name, toString(F.ARGS.BASE))
  code_res(defn=defn, name=name)
}

