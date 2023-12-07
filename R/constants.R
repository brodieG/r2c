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

# Generally we're trying to bind constant character tokens to constant variable
# names to reduce the chance that we introduce typo-bugs.  We're not 100%
# consistent about this.

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
QREC <- quote(r2c::rec)
QVCOPY <- quote(r2c::vcopy)
QNULL <- quote(numeric(length=0L))
QNULL.REC <- en_rec(QNULL)
# need to wrap in list because can't be a top level for R CMD check
MISSING <- list(formals(base::identical)[[1L]])

# `->` becomes `<-` on parsing. While `for` assigns to the counter variable, we
# handle that by decomposing the for call and having assignments in `for_init`.
R2C.FOR <- "r2c_for"
FOR.INIT <- "for_init"
FOR.ITER <- "for_iter"
FOR.N <- "for_n"
FOR.0 <- "for_0"
ASSIGN.SYM.BASE <- c("<-", "=")
# 'for' is considered assignment symbol, used in `reuse_calls`, but once it gets
# decomposed into FOR.ITER, FOR.N, etc., it no longer is as we add an explicit
# assignment to the iteration variable.
ASSIGN.SYM <- c(ASSIGN.SYM.BASE, 'for')
MODIFY.SYM <- c(ASSIGN.SYM, "subassign")
LOOP.SYM <- c("for", "while", "repeat")
FOR.SYM.ALL <- c(R2C.FOR, FOR.ITER, FOR.N, FOR.0)
LOOP.SUB.SYM <- c(FOR.0, FOR.N)
R2C.IF <- "r2c_if"
IF.TEST <- "if_test"
IF.SUB.SYM <- c("if_true", "if_false")
CTRL.SYM <- c("if", LOOP.SYM)
CTRL.SUB.SYM <- c(IF.SUB.SYM, LOOP.SUB.SYM)

# Markers to detect what part of a branch we're on when processing the
# linearized call list.  `for_iter` and `if_test` are not quite the same wrt to
# locaton in the linearized call list because for_iter is nested inside
# `r2c_for`.  Works out the same though in terms of sandwiching the branches.
BRANCH.START.SYM <- c(IF.TEST, FOR.ITER)
BRANCH.MID.SYM <- c("if_true", FOR.N)
BRANCH.END.SYM <- c("if_false", FOR.0)
BRANCH.EXEC.SYM <- c(R2C.IF, R2C.FOR)

# For recomposing control we match to different things than we do in alloc
CTRL.LEAD <- c(IF.TEST, FOR.INIT)
CTRL.MAIN <- BRANCH.EXEC.SYM

REC.FUNS <- c('vcopy', 'rec')
L.SET <- 'lset'
L.COPY <- 'lcopy'
L.COPY.FUNS <- c(L.SET, L.COPY)

INTERNAL.FUNS <- c(
  IF.SUB.SYM, BRANCH.START.SYM, BRANCH.EXEC.SYM, REC.FUNS, FOR.INIT, L.COPY.FUNS
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
    'rec',         # vcopy not passive
    L.SET,         # lcopy (unlike rec) copies thus not included here
    FOR.INIT       # to allow assignments inside
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
  'ids0', 'alloc', 'size.coefs', 'depth', 'type', 'typeof', 'iter.var'
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

  if_test=IF.TEST, if_true="if_true", if_false="if_false", r2c_if=R2C.IF,
  "if"="if",

  for_init=FOR.INIT, for_iter=FOR.ITER, for_n=FOR.N, for_0=FOR.0,
  r2c_for=R2C.FOR, "for"="for", lcopy="lcopy", lset=L.SET,

  seq_along="seq_along",

  "["="subset",
  subassign="subassign",   # this is [<-

  numeric="numeric", numeric_along="numeric_along",
  numeric_alongn="numeric_alongn",

  rep="rep",

  c="concat"
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
R2C.DOTS <- ".R2C.DOTS"
QR2C.DOTS <- as.name(R2C.DOTS)

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

# For unsupported function subs
UNSUP.CALL.BASE <- sprintf("%s_UNSUP_", R2C.PRIV.BASE)
UNSUP.CALL.RX <- sprintf("^\\%s\\d+$", UNSUP.CALL.BASE)
UNSUP.CALL.TPL <- sprintf("%s%%d", UNSUP.CALL.BASE)




