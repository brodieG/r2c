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

ARGS.NM.DT <- c('data')
ARGS.NM.BASE.2 <- c('datai', 'off', 'len')
ARGS.NM.BASE <- c(ARGS.NM.DT, ARGS.NM.BASE.2)
ARGS.NM.VAR <- c('narg')
ARGS.NM.CTRL <- 'ctrl'
ARGS.NM.ALL <- c(ARGS.NM.BASE, ARGS.NM.VAR, ARGS.NM.CTRL)

## F for function, R for overall runner
F.ARGS.DT <- R.ARGS.DT <- paste('double **', ARGS.NM.DT)

F.ARGS.BASE <- c(F.ARGS.DT, paste('double *', ARGS.NM.BASE.2))
R.ARGS.BASE <- c(R.ARGS.DT, paste('double **', ARGS.NM.BASE.2))

F.ARGS.VAR <- paste('double *', ARGS.NM.VAR)
R.ARGS.VAR <- paste('double **', ARGS.NM.VAR)

F.ARGS.CTRL <- R.ARGS.CTRL <- 'SEXP ctrl'

F.ARGS.ALL <- c(F.ARGS.BASE, F.ARGS.VAR, F.ARGS.CTRL)
R.ARGS.ALL <- c(R.ARGS.BASE, R.ARGS.VAR, R.ARGS.CTRL)

CALL.BASE <- c(ARGS.NM.DT, paste0("*", ARGS.NM.BASE.2, "++"))
CALL.VAR <- "*narg++"
CALL.CTRL <- "VECTOR_ELT(ctrl, v++)"

## Sanity checks
stopifnot(
  identical(gsub("SEXP|double|[ +*]", "", F.ARGS.ALL), ARGS.NM.ALL),
  identical(gsub("SEXP|double|[ +*]", "", R.ARGS.ALL), ARGS.NM.ALL),
  identical(
    gsub("double|[ +*]", "", c(CALL.BASE, CALL.VAR)),
    ARGS.NM.ALL[-length(ARGS.NM.ALL)]
) )
# external is unknown at compile time, external or group is also unknown, but we
# need to keep track of the possibility that it could be either external or
# group for final size computation during allocation stage.
SIZE.TYPES <- c("scalar", "constant", "group", "external", "external_or_group")

