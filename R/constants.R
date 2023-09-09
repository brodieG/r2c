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

# - C Fun Parameters -----------------------------------------------------------

ARGS.NM.BASE <- c('data', 'lens', 'di')
ARGS.NM.VAR <- 'narg'
ARGS.NM.EXTERN <- 'extn'
ARGS.NM.ALL <- c(ARGS.NM.BASE, ARGS.NM.VAR, ARGS.NM.EXTERN)

ARGS.TYPE.F <- c('double **', 'R_xlen_t *', 'int *')
ARGS.TYPE.R <- c('double **', 'R_xlen_t *', 'int **')

## F for function, R for overall runner
F.ARGS.BASE <- paste(ARGS.TYPE.F, ARGS.NM.BASE)
R.ARGS.BASE <- paste(ARGS.TYPE.R, ARGS.NM.BASE)

F.ARGS.VAR <- paste('int', ARGS.NM.VAR)
R.ARGS.VAR <- paste('int *', ARGS.NM.VAR)

F.ARGS.EXTERN <- R.ARGS.EXTERN <- 'SEXP extn'

F.ARGS.ALL <- c(F.ARGS.BASE, F.ARGS.VAR, F.ARGS.EXTERN)
R.ARGS.ALL <- c(R.ARGS.BASE, R.ARGS.VAR, R.ARGS.EXTERN)

CALL.BASE <- c(ARGS.NM.BASE[1L:2L], paste0(ARGS.NM.BASE[3L], "[%1$d]"))
CALL.VAR <- paste0(ARGS.NM.VAR, "[%1$d]")
# this should be length 1 (see checks)
CALL.EXTERN <- paste0("VECTOR_ELT(", ARGS.NM.EXTERN, ", %1$d)")
CALL.ALL <- c(CALL.BASE, CALL.VAR, CALL.EXTERN)

## Sanity checks
pat <- "\\[%1\\$d\\]|\\bSEXP\\b|\\bdouble\\b|\\bint\\b|\\bR_xlen_t\\b|[ +*]"
stopifnot(
  identical(gsub(pat, "", F.ARGS.ALL), ARGS.NM.ALL),
  identical(gsub(pat, "", R.ARGS.ALL), ARGS.NM.ALL),
  identical(
    gsub(pat, "", c(CALL.BASE, CALL.VAR)),
    ARGS.NM.ALL[-length(ARGS.NM.ALL)] # CALL.EXTERN hard to compare
  )
)

# - Constants ------------------------------------------------------------------

# Filled in via onLoad from values stored in C (so C and R stay in sync)

IX <- list()
QDOTS <- quote(...)
QBRACE <- as.name("{")
QR2C.FOR <- quote(r2c::r2c_for)
QFOR.ITER <- quote(r2c::for_iter)
QFOR.INIT <- quote(r2c::for_init)
# need to wrap in list because can't be a top level for R CMD check
MISSING <- list(formals(base::identical)[[1L]])

# `for` assigns to the counter variable.  `->` becomes `<-` on parsing.

R2C.FOR <- "r2c_for"
FOR.INIT <- "for_init"
FOR.ITER <- "for_iter"
FOR.N <- "for_n"
FOR.0 <- "for_0"
ASSIGN.SYM.BASE <- c("<-", "=")
ASSIGN.SYM <- c(ASSIGN.SYM.BASE, "for", FOR.ITER)
MODIFY.SYM <- c(ASSIGN.SYM, "subassign")
LOOP.SYM <- c("for", "while", "repeat")
FOR.SYM.ALL <- c(R2C.FOR, FOR.ITER, FOR.N, FOR.0)
LOOP.SUB.SYM <- c(FOR.0, FOR.N)
IF.SUB.SYM <- c("if_true", "if_false")
CTRL.SYM <- c("if", LOOP.SYM)
CTRL.SUB.SYM <- c(IF.SUB.SYM, LOOP.SUB.SYM)
# `for_iter` and `if_test` are not quite the same because for_iter is nested
# inside `r2c_for`.  Works out the same though in terms of sandwiching the
# branches.
BRANCH.START.SYM <- c("if_test", FOR.ITER)
BRANCH.MID.SYM <- c("if_true", FOR.N)
BRANCH.END.SYM <- c("if_false", FOR.0)
BRANCH.EXEC.SYM <- c("r2c_if", R2C.FOR)
REC.FUNS <- c('vcopy', 'rec')
L.USE <- 'luse'
L.SET <- 'lset'
L.REC <- 'lrec'
LREC.FUNS <- c(L.USE, L.SET, L.REC)

INTERNAL.FUNS <- c(
  IF.SUB.SYM, BRANCH.START.SYM, BRANCH.EXEC.SYM, REC.FUNS, FOR.INIT, LREC.FUNS
)

NUM.TYPES <- c('logical', 'integer', 'double')

# Calls that don't actually do any computing themselves, rather rely on
# computations that happen in their arguments `for` is a bit tricky as it does
# "compute" the counter value, and `r2c_if` also because it doesn't per-se
# compute, but if the return value is used, it ensures that both of it's
# branches either compute or `vcopy` that..
#
# Additionaly complexity is some of these are removed (e.g. `if` is expanded
# into `r2c_if`) so some question if they should even be in here.
PASSIVE.SYM <- unique(
  c(
    MODIFY.SYM, CTRL.SYM, "{", "uplus",
    CTRL.SUB.SYM, BRANCH.EXEC.SYM,
    'rec',     # vcopy not passive
    LREC.FUNS,
    FOR.INIT   # so it allows assignments inside
  )
)
# In branches, some symbols are not considered passive even though they don't
# strictly compute.
PASSIVE.BRANCH.SYM <- setdiff(PASSIVE.SYM, BRANCH.EXEC.SYM)

# For `record_call_dat` and `alloc_dat`, fields that are supposed to be scalar
# for each allocation/call
CALL.DAT.VEC <- c(
  'argn', 'depth', 'par.type', 'assign', 'indent', 'rec', 'par.validate',
  'linfo'
)
ALLOC.DAT.VEC <- c(
  'ids0', 'alloc', 'size.coefs', 'depth', 'type', 'typeof'
)

# Parameter types
PAR.EXT.ANY <- "ext.any"
PAR.EXT.NUM <- "ext.num"
PAR.EXT <- c(num=PAR.EXT.NUM, any=PAR.EXT.ANY)
PAR.INT.LEAF <- "int.leaf"
PAR.INT.CALL <- "int.call"
PAR.INT <- c(PAR.INT.LEAF, PAR.INT.CALL)
PAR.TYPES <- c(PAR.INT, PAR.EXT)

# Packages allowable in `::`
VALID.PKG <- c('base', 'r2c')

# names(FUN.NAMES) are R names, values are C names.  R names are after
# preprocessing so will include r2c internal names.
FUN.NAMES <- c(
  "+"="add", "-"="subtract", "*"="multiply", "/"="divide",

  # "%%"="modulo",

  "^"="power",

  ">"="gt", ">="="gte", "<"="lt", "<="="lte", "=="="eq", "!="="neq",

  "&"="vand", "|"="vor", "&&"="and2", "||"="or2",

  all="all", any="any", ifelse="ifelse",

  "!"="negate", uplus="uplus", uminus="uminus",

  "<-"="assign", "="="assign", "{"="braces",

  sum="sum", mean="mean", length="r2c_length", # conflicts with Rf_length

  mean1="mean1", square="square",

  vcopy="vcopy", rec="rec",

  # "while"="while", "repeat"="repeat", "if"="if"

  if_test="if_test", if_true="if_true", if_false="if_false", r2c_if="r2c_if",
  "if"="if",

  for_init=FOR.INIT, for_iter=FOR.ITER, for_n=FOR.N, for_0=FOR.0,
  r2c_for=R2C.FOR, "for"="for", lrec="lrec", luse="luse", lset="lset",

  seq_along="seq_along",

  "["="subset",
  subassign="subassign",   # this is [<-

  numeric="numeric", numeric_along="numeric_along",
  numeric_alongn="numeric_alongn"
)
# C Generator Output Types
CGEN.OUT.CALL <- 1L   # output call to C fun e.g. `mean(data, lens, di[5])`
CGEN.OUT.MUTE <- 2L   # prepend '// NOOP: ' to call, thus disabling it
CGEN.OUT.RDEP <- 4L   # add the corresponding deparsed R call above C call
CGEN.OUT.DEFN <- 8L   # output the c function definition
CGEN.OUT.DFLT <- CGEN.OUT.CALL + CGEN.OUT.RDEP + CGEN.OUT.DEFN
CGEN.OUT.NOOP <- CGEN.OUT.CALL + CGEN.OUT.MUTE + CGEN.OUT.RDEP
CGEN.OUT.NONE <- 0L

# - Internal Symbols -----------------------------------------------------------

R2C.PRIV.BASE <- ".R2C"
R2C.PRIV.RX <- sprintf("^%s", R2C.PRIV.BASE)

# For arguments that show up as `..1`, `..2`, replaced to match pattern below
DOT.ARG.BASE <- sprintf("%s_DOT_", R2C.PRIV.BASE)
DOT.ARG.RX <- sprintf("^\\%s\\d+$", DOT.ARG.BASE)
DOT.ARG.TPL <- sprintf("%s%%d", DOT.ARG.BASE)

# For renames symbols
RENAME.ARG.BASE <- sprintf("%s_RN_", R2C.PRIV.BASE)
RENAME.ARG.RX <- sprintf("^\\%s\\d+$", RENAME.ARG.BASE)
RENAME.ARG.TPL <- sprintf("%s%%s_%%d", RENAME.ARG.BASE)

# For substitution (re-use)
REUSE.ARG.BASE <- sprintf("%s_SUB_", R2C.PRIV.BASE)
REUSE.ARG.RX <- sprintf("^\\%s\\d+$", RENAME.ARG.BASE)
REUSE.ARG.TPL <- sprintf("%s%%d", REUSE.ARG.BASE)


