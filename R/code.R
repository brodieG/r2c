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

#' Code Generation Functions
#'
#' * Must accept one of four possible signatures.
#' * Must set result by reference.
#' * Must set result size by reference.

code_gen <- function(dat) {

}
code_blank <- function() list(
  defn="", name="", call="", args=character(), headers=character()
)
code_valid <- function(code, call) {
  isTRUE(check <- vet(CHR.1, code$defn)) &&
    isTRUE(check <- vet(CHR.1, code$name)) &&
    isTRUE(check <- vet(CHR.1, code$call)) &&
    isTRUE(check <- vet(CHR, code$args)) &&
    isTRUE(check <- vet(CHR, code$headers))
  if(!isTRUE(check))
    stop("Generated code format invalid for `", deparse1(call), "`:\n", check)

  TRUE
}
