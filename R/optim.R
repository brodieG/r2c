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

#' @include rename.R

# - Sub-call Reuse -------------------------------------------------------------

dep1 <- function(call)
  paste(deparse(call, width.cutoff=500L), collapse=" ")

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

#' Identify Repeated Calls and Reuse First Instance
#'
#' Complex statistics often re-use a simpler statistic multiple times, providing
#' the opportunity to optimize them by storing the result of the simple
#' statistic instead of recomputing it.  This function detects reuses of
#' sub-calls and modifies the call tree to implement the store-and-reuse
#' optimization.
#'
#' It is not possible to reuse calls that first occur in loops that reference
#' names (directly or indirectly) that are assigned to in a loop.  This is
#' because we cannot know ex-ante k
#' ```
#' i <- 3
#' mean(i)
#' for(i in 1:2) {
#'   mean(i)
#'   if(i > 1) break
#'   mean(i)
#' }
#' mean(i)
#'
#'
#' @param x a call, ideally first renamed with [`rename_calls`].
#' @param rename.i index to used for additional "renamed" variables generated

reuse_calls <- function(x) {
  # rename assigned-to symbols, etc
  rename.dat <- rename_call(x)
  x <- rename.dat[['x']]

  # Flatten the calls so we can quickly compare and find matches.  Retain tree
  # structure in 'indices' so we can retrieve the original calls and map new
  # modified calls to the original ones
  flat <- flatten_call(x)
  calls.flat <- flat[['calls']]
  calls.indices <- flat[['indices']]
  index.lookup <- vapply(calls.indices, toString, "")

  # Deparse in order to match calls
  is.call <- vapply(calls.flat, typeof, "") == "language"
  calls.dep <- character(length(calls.flat))
  calls.dep[is.call] <- vapply(calls.flat[is.call], dep1, "")

  # Find repeated calls, and track the first instance for each.  The first
  # instance will be the one that gets assigned to a variable, subsequent ones
  # will then use that variable.
  calls.match <- match(calls.dep, calls.dep)
  calls.rep <- calls.match != seq_along(calls.match) & is.call
  calls.first <- unique(match(calls.dep[calls.rep], calls.dep))

  # Eliminate Redundant repeated calls caused by a repeated call with subcalls
  # as obviously the subcalls will be repeated too
  rep.counts <- table(calls.match[calls.match %in% which(is.call)])
  index.lookup.parent <-
    vapply(calls.indices[calls.first], function(x) toString(x[-length(x)]), "")
  index.parent <- match(index.lookup.parent, index.lookup)
  rep.count.first <- rep.counts[as.character(calls.first)]
  rep.count.parent <- rep.counts[as.character(index.parent)]
  calls.first.keep <- calls.first[rep.count.first > rep.count.parent]
  if(any(rep.count.first < rep.count.parent) || anyNA(calls.first.keep))
    stop("Internal Error: implied nonsensical call hierarchy.")

  # Modify calls so first instance of repeats becomes an assignment, subsequent
  # ones reuse this symbol.  For each modified subcall, record a reference to
  # the original call (useful for annotating the modified call)
  ru.i <- 1L
  for(i in calls.first.keep) {
    index.i <- calls.indices[[i]]
    ru.arg <- as.name(sprintf(REUSE.ARG.TPL, ru.i))
    ru.i <- ru.i + 1L
    x[[index.i]] <- call("<-", ru.arg, x[[index.i]])
    call.rep <- seq_along(calls.dep)[-calls.first][
      match(calls.dep[i], calls.dep[-calls.first])
    ]
    for(j in call.rep) x[[calls.indices[[j]]]] <- ru.arg
  }
  # Undo the renames we used to ensure call reuse did not incorrectly reuse
  # things that are different due to e.g. assignment
  unrename_call(x, rename.dat[['rn']])

  # Return the reuse-substituted call along with the mapping of each subcall to
  # the subcall it originated from (for useful deparsing of subbed calls)


}

