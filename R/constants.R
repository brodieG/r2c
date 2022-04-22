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

ARGS.BASE <- "double ** data, int * datai, R_xlen_t * off, R_xlen_t * len"
ARGS.VAR <- "int narg"
ARGS.CTRL <- "SEXP ctrl"

CALL.BASE <- "data, *datai++, *off++, *len++"
CALL.VAR <- "*narg++"
CALL.CTRL <- "VECTOR_ELT(ctrl, v++)"

