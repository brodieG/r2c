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

ARGS.NM.BASE <- c('data', 'datai', 'off', 'len')
ARGS.NM.VAR <- 'narg'
ARGS.NM.CTRL <- 'ctrl'
ARGS.NM.ALL <- c(ARGS.NM.BASE, ARGS.NM.VAR, ARGS.NM.CTRL)

ARGS.TYPE.F <- c('double **', 'int *', 'R_xlen_t *', 'R_xlen_t *')
ARGS.TYPE.R <- c('double **', 'int **', 'R_xlen_t **', 'R_xlen_t **')

## F for function, R for overall runner
F.ARGS.BASE <- paste(ARGS.TYPE.F, ARGS.NM.BASE)
R.ARGS.BASE <- paste(ARGS.TYPE.R, ARGS.NM.BASE)

F.ARGS.VAR <- paste('int', ARGS.NM.VAR)
R.ARGS.VAR <- paste('int *', ARGS.NM.VAR)

F.ARGS.CTRL <- R.ARGS.CTRL <- 'SEXP ctrl'

F.ARGS.ALL <- c(F.ARGS.BASE, F.ARGS.VAR, F.ARGS.CTRL)
R.ARGS.ALL <- c(R.ARGS.BASE, R.ARGS.VAR, R.ARGS.CTRL)

CALL.BASE <- c(ARGS.NM.BASE[1L], paste0("*", ARGS.NM.BASE[-1L], "++"))
CALL.VAR <- "*narg++"
CALL.CTRL <- "VECTOR_ELT(ctrl, v++)"

## Sanity checks
pat <- "\\bSEXP\\b|\\bdouble\\b|\\bint\\b|\\bR_xlen_t\\b|[ +*]"
stopifnot(
  identical(gsub(pat, "", F.ARGS.ALL), ARGS.NM.ALL),
  identical(gsub(pat, "", R.ARGS.ALL), ARGS.NM.ALL),
  identical(
    gsub(pat, "", c(CALL.BASE, CALL.VAR)),
    ARGS.NM.ALL[-length(ARGS.NM.ALL)]
) )
# external is unknown at compile time, external or group is also unknown, but we
# need to keep track of the possibility that it could be either external or
# group for final size computation during allocation stage.
SIZE.TYPES <- c("scalar", "constant", "group", "external", "external_or_group")

