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
ARGS.NM.CTRL <- 'ctrl'
ARGS.NM.FLAG <- 'flag'
ARGS.NM.ALL <- c(ARGS.NM.BASE, ARGS.NM.VAR, ARGS.NM.FLAG, ARGS.NM.CTRL)

ARGS.TYPE.F <- c('double **', 'R_xlen_t *', 'int *')
ARGS.TYPE.R <- c('double **', 'R_xlen_t *', 'int **')

## F for function, R for overall runner
F.ARGS.BASE <- paste(ARGS.TYPE.F, ARGS.NM.BASE)
R.ARGS.BASE <- paste(ARGS.TYPE.R, ARGS.NM.BASE)

F.ARGS.VAR <- paste('int', ARGS.NM.VAR)
R.ARGS.VAR <- paste('int *', ARGS.NM.VAR)

F.ARGS.FLAG <- paste('int', ARGS.NM.FLAG)
R.ARGS.FLAG <- paste('int *', ARGS.NM.FLAG)

F.ARGS.CTRL <- R.ARGS.CTRL <- 'SEXP ctrl'

F.ARGS.ALL <- c(F.ARGS.BASE, F.ARGS.VAR, F.ARGS.FLAG, F.ARGS.CTRL)
R.ARGS.ALL <- c(R.ARGS.BASE, R.ARGS.VAR, R.ARGS.FLAG, R.ARGS.CTRL)

INC.VAR <- paste0("++", ARGS.NM.VAR);
INC.FLAG <- paste0("++", ARGS.NM.FLAG);
INC.CTRL <- "++v";
INC.DAT <- paste0("++", ARGS.NM.BASE[3L]) # also used by CALL.BASE

CALL.BASE <- c(ARGS.NM.BASE[1L:2L], paste0("*", ARGS.NM.BASE[3L], "++"))
CALL.VAR <- "*narg"
CALL.CTRL <- "VECTOR_ELT(ctrl, v)"  # this should be length 1 (see checks)
CALL.FLAG <- "*flag";
CALL.ALL <- c(CALL.BASE, CALL.VAR, CALL.FLAG, CALL.CTRL)


## Sanity checks
pat <- "\\bSEXP\\b|\\bdouble\\b|\\bint\\b|\\bR_xlen_t\\b|[ +*]"
stopifnot(
  identical(gsub(pat, "", F.ARGS.ALL), ARGS.NM.ALL),
  identical(gsub(pat, "", R.ARGS.ALL), ARGS.NM.ALL),
  identical(
    gsub(pat, "", c(CALL.BASE, CALL.VAR, CALL.FLAG)),
    ARGS.NM.ALL[-length(ARGS.NM.ALL)] # CALL.CTRL hard to compare
  )
)

# - Constants ------------------------------------------------------------------

# Filled in via onLoad from values stored in C (so C and R stay in sync)

IX <- list()
QDOTS <- quote(...)
MISSING <- formals(base::identical)[[1L]]

# `for` assigns to the counter variable.  `->` becomes `<-` on parsing.
ASSIGN.SYM <- c("<-", "=", "for")
LOOP.SYM <- c("for", "while", "repeat")

# Calls that don't actually do any computing themselves, rather rely on
# computations that happen in their arguments `for` is a bit tricky as it does
# "compute" the counter value.
PASSIVE.SYM <- unique(c(ASSIGN.SYM, LOOP.SYM, "if", "{"))

# For `record_call_dat`.
CALL.DAT.VEC <- c('argn', 'depth', 'type', 'assign')

# To avoid typos
CTRL.FLAG <- c("control", "flag")

# Packages allowable in `::`
VALID.PKG <- c('base', 'r2c')

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

