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

ARG.DATA <- 'double ** data'
# Used both to generate the call to individual functions, but also to the
# overall code runner, and each needs a different level of dereferencing, hence
# the %s (and we need to match the former to the latter).
ARGS.BASE <- c('int %sdatai', 'R_xlen_t %soff', 'R_xlen_t %slen')
ARGS.VAR <- "int %snarg"
ARGS.CTRL <- "SEXP ctrl"
# This is ued only for matching order
ARGS.ALL <- c(ARGS.BASE, ARGS.VAR, ARGS.CTRL)

CALL.BASE <- c("data", "*datai++", "*off++", "*len++")
CALL.VAR <- "*narg++"
CALL.CTRL <- "VECTOR_ELT(ctrl, v++)"

