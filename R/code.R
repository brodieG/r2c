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

#' Translate Pre-process Data Into Code
#'
#' Needs to:
#' 1. Deduplicate and write definitions.
#' 2. Generate the ** of data pointer indices.
#' 3. Generate the ** of data pointer offsets.
#' 4. Generate the ** of data pointer lengths.
#'
#' Number 2 just needs to be done once.  Numbers 3 and 4 need to be updated for
#' each group.  Realistically, for number 3 and 4 there will be one set that is
#' updating with the groups, where the length is the group size and the offset
#' is the cumsum of the prior groups, and another where these numbers are
#' constant.  So in between calling groups.  But the complexity is that since we
#' update the data structure line by line, each time we advance what is in each
#' group might change.
#'
#' We need to clearly segregate in the data the immutable data and the mutable
#' data?  Or just those entries that might or might not require offsets to save
#' the offset calc?
#'
#' If we require each function update the length offset of the written result,
#' then we don't have to maintain this ourselves.  This simplifies things quite
#' a bit.
#'
#' For each call, we need the indices of its inputs, and that of it's output.
#'
#' * Output: get from `alloc_dat` call
#'
#' Between each group, update the offset and len vectors for the data.
#'
#' To conclude thought process, we do need to keep appending all the numeric
#' data to one big vector, including both group data, temp data, and argument
#' data, indicating the type of each.  Then, for each call we can generate an
#' index into this large vector.
#'
#' The vector is never re-ordered, we only do the reordering to assess whether
#' we can re-use one of the temporary data structures or not.

code_gen <- function(dat) {

}

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
