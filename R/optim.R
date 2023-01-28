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

dep1 <- function(call)
  paste(deparse(call, control="all", width.cutoff=500L), collapse=" ")

#' "Compiler" Optimizations
#'
#' See if there are opportunities for optimization.  Currently we look for
#' repeated expressions and replace them with the first instance assigning it's
#' result, and subsequent instances using the assigned variable.
#'
#' @noRd
#' @param a call dat object as produced by preprocess

optim <- function(x) {
  which.calls <- which(vapply(x[['call.rename']], typeof, "") == "language")
  calls.dep <- vapply(x[['call.rename']][which.calls], dep1, "")

  # Find repeated calls
  calls.match <- match(calls.dep, calls.dep)
  calls.rep <- calls.match != seq_along(calls.match)

  # For the repeated call, we need to:
  # 1. generate an assignment to a new variable, we can use any name since it
  #    will be renamed.
  # 2. Replace all the duplicate calls with that variable
  #
  # This is really complicated by the linearized data since adding a call
  # requires hoisting every child call up.  And excising a call is not trivial.
  # All the different linearized tracking variables need to be cut exactly.
  # The alternative is we don't linearize at first.
  #
  # The flipside is if the thing is still a tree, matching like the above is a
  # real PITA.  So maybe the first step is to make the call dat object a 2D list
  # so we can easily slice and dice it.
  #
  # To remove children we need to find the items that are next to each of the
  # `call.rep` with increasing depth?
  #
  # So, for each item in calls.rep, get a sequence from that item up until depth
  # starts decreasing.
  #
  # To add an assignment we need to insert a "row" and raise all the children.
  # Each time we make a change we need to update all our ids so the next
  # iteration maps correctly.

  for(call.id in call.rep) {
    # We assume here we can't have an adjacent 
    seq_along(calls.match) <= call.id & diff(depth) >= 0

  }

}
#' Flatten Calls Preserving Indices Into Recursive Structure
#'
#' @noRd
#' @param x renamed call tree
#' @return call tree

flatten_call <- function(x) {
  calls <- list()
  indices <- integer()
  flatten_call_rec(x, calls, indices)
}
flatten_call_rec <- function(x, calls, indices) {
  calls.res <- indices.res <- list()
  if(is.call(x) && length(x) > 1) {
    for(i in seq(2L, length(x), 1)) {
      dat <- flatten_call_rec(x[[i]], calls, c(indices, i))
      calls.res <- c(calls.res, dat[['calls']])
      indices.res <- c(indices.res, dat[['indices']])
    }
  }
  calls.res <- c(calls.res, list(x))
  calls.i.res <- c(indices.res, list(indices))
  list(calls=calls.res, indices=calls.i.res)
}

#' @noRd
#' @param rename.i current rename index

re_use_calls <- function(x, rename.i=1L) {
  flat <- flatten_call(x)
  calls.flat <- flat[['calls']]
  calls.indices <- flat[['indices']]

  # Deparse in order to match calls
  is.call <- vapply(calls.flat, typeof, "") == "language"
  calls.dep <- character(length(calls.flat))
  calls.dep[is.call] <- vapply(calls.flat[is.call], dep1, "")

  # Find repeated calls
  calls.match <- match(calls.dep, calls.dep)
  calls.rep <- calls.match != seq_along(calls.match) & is.call
  # For each repeated, call, find the source call
  calls.source <- unique(match(calls.dep[calls.rep], calls.dep))

  # Source calls need to be augmented with an assignment to a renamed variable,
  # repeated calls need to be replaced by that variable.
  for(i in calls.source) {
    call.source <- calls.dep[i]
    call.source.i <- calls.indices[[i]]
    rename.arg <- as.name(sprintf(RENAME.ARG.TPL, rename.i))
    rename.i <- rename.i + 1L
    x[[call.source.i]] <- call("<-", rename.arg, x[[call.source.i]])
    call.rep <- match(call.source, calls.dep)[-1L]
    for(j in call.rep) x[[calls.indices[[j]]]] <- rename.arg
  }
  x
}

