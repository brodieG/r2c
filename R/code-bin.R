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
#' @include code-unary.R

NULL

OP.OP <- c(
  "+"="+", "-"="-", "*"="*", "/"="/",
  # "%%"="%",
  ">"="GT", ">="="GTE", "<"="LT", "<="="LTE", "=="="EQ", "!="="NEQ",
  "&"="AND", "|"="OR"
)
## Some question as to whether these would re-evalute the memory fetch, but
## presumably compiler is smart enough to re-use the registers.  Also,
## presumably Only relational operators trigger exceptions, not equality (C99
## 7.12.14).  Assuming the negation for isunordered is worth allowing the most
## likely branch first, but have not tested.  Not clear that returning NA_REAL
## here is correct as one of the inputs could be regular NaN.
OP.DEFN <- c(
  ">"="#define GT(x, y) (!isunordered((x), (y)) ? (x) > (y) : NA_REAL)",
  ">="="#define GTE(x, y) (!isunordered((x), (y)) ? (x) >= (y) : NA_REAL)",
  "<"="#define LT(x, y) (!isunordered((x), (y)) ? (x) < (y) : NA_REAL)",
  "<="="#define LTE(x, y) (!isunordered((x), (y)) ? (x) <= (y) : NA_REAL)",
  "=="="#define EQ(x, y) (!isunordered((x), (y)) ? (x) == (y) : NA_REAL)",
  "!="="#define NEQ(x, y) (!isunordered((x), (y)) ? (x) != (y) : NA_REAL)",
  "&"="#define AND(x, y) ((x) == 0 || (y) == 0 ? 0 : isunordered((x), (y)) ? NA_REAL : 1)",
  "|"="#define OR(x, y) (!ISNAN(x) && (x) || !ISNAN(y) && (y) ? 1 : (x) == 0 && (y) == 0 ? 0 : NA_REAL)"
)
stopifnot(
  all(names(OP.OP) %in% names(FUN.NAMES)),
  all(names(OP.DEFN) %in% names(OP.OP))
)

## Binary Operators or Functions with Vector Recycling
##
## Use %3$s for functions like pow, %4$s for operators (which should be a comma
## when using functions).
##
## This supports unequal sizes.  We looked at having specialized functions for
## each of the possible length pairings, but that didn't seem to improve things
## too much (at least single core without any contention).

bin_op_vec_rec <- paste0('
static void %1$s(%2$s) {
  int di0 = di[0];
  int di1 = di[1];
  int dires = di[2];
  double * e1 = data[di0];
  double * e2 = data[di1];
  R_xlen_t len1 = lens[di0];
  R_xlen_t len2 = lens[di1];
  double * res = data[dires];

  if(len1 == 0 || len2 == 0) { // empty recycle is zero
    lens[dires] = 0;
    return;
  }
  // Not all "bin" operators are commutative
  // so we cannot play tricks with switching parameter order
  R_xlen_t i, j;
  if(len1 == len2) {
    // special casing the len1 == len2 == 1 case doesnt seem to improve timings
    LOOP_W_INTERRUPT1(len1, {res[i] = %3$s(e1[i] %4$s e2[i]);});
    lens[dires] = len1;
  } else if (len2 == 1) {
    LOOP_W_INTERRUPT1(len1, {res[i] = %3$s(e1[i] %4$s *e2);});
    lens[dires] = len1;
  } else if (len1 == 1) {
    LOOP_W_INTERRUPT1(len2, {res[i] = %3$s(*e1 %4$s e2[i]);});
    lens[dires] = len2;
  } else if (len1 > len2) {
    LOOP_W_INTERRUPT2(len1, len2, res[i] = %3$s(e1[i] %4$s e2[j]););
    if(j != len2) data[%5$s][%6$s] = 1.;   // bad recycle
    lens[dires] = len1;
  } else if (len2 > len1) {
    LOOP_W_INTERRUPT2(len2, len1, res[i] = %3$s(e1[j] %4$s e2[i]););
    if(j != len1) data[%5$s][%6$s] = 1.;   // bad recycle
    lens[dires] = len2;
  }
}')
code_gen_bin <- function(fun, pars, par.types) {
  vetr(
    CHR.1 && . %in% setdiff(names(OP.OP), names(OP.DEFN)),
    pars=list(NULL, NULL),
    par.types=character() && all(. %in% PAR.INT)
  )
  name <- FUN.NAMES[fun]
  op <- OP.OP[fun]      # needed for modulo
  defn <- sprintf(
    bin_op_vec_rec, name, toString(F.ARGS.BASE), "", op,
    IX[['I.STAT']], IX[['STAT.RECYCLE']] # these are now available as defines
  )
  code_res(defn=defn, name=name, headers=character())
}
# For the ones that need the defined macro
code_gen_bin2 <- function(fun, pars, par.types) {
  vetr(
    CHR.1 && . %in% names(OP.DEFN),
    pars=list(),
    par.types=character() && all(. %in% PAR.INT)
  )
  name <- FUN.NAMES[fun]
  op <- OP.OP[fun]      # needed for modulo
  defn <- sprintf(
    bin_op_vec_rec, name, toString(F.ARGS.BASE), op, ",",
    IX[['I.STAT']], IX[['STAT.RECYCLE']]
  )
  code_res(
    defn=defn, name=name, headers=character(), defines=OP.DEFN[fun]
  )
}
