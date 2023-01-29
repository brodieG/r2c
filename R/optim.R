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
#' Rename Symbols in Calls
#'
#' The purpose of the renaming is to distinguish calls that are identical except
#' that the content of the calls they reference has changed due to an assignment
#' overwriting or masking an existing variable.
#'
#' We have a 1-1 mapping to the original call since we're not adding/removing
#' calls.
#'
#' Assignments in loop need to be renamed one additional time on loop exit
#' because there is no guarantee that every expression in some way involving the
#' counter variable (or any other loop generated variable) will be in a specific
#' state.  For example, if we have `x <- i` in a loop, but we break out of the
#' loop before the loop is complete, `i` and `x` will not be the same at the end
#' of the loop.  Or even simpler:
#' ```
#' for(i in 1) {
#'   break
#'   x <- y
#' }
#' mean(x)
#' ```
#' ```
#' mean(x)
#' for(i in 1:2) {
#'   mean(x)
#'   if(i > 1) break
#'   x <- i
#'   mean(x)
#'   z <- c(z, y)
#' }
#' mean(x)
#' ```
#' So find all the names written in the loop (**including the loop counter**),
#' ideally we would identify the set of those (recursively) that are loop
#' varying vs loop constant.  Loop constant ones can get the same treatment, but
#' loop varying ones are not eligible.
#'
#' What do we consider loop varying?  Any expression involving any variables
#' created in the loop.  So we need to do a pass to identify all the calls that
#' are loop varying and annotate them some how.
#'
#' How do nested loops work?  Maybe doesn't matter, once you're in a loop, we
#' can apply the same algorithm to absolutely everything in the loop.
#'
#' @noRd
#' @param x a call
#' @param renames a named list of symbols, in which the names are the original
#'   symbol name, and the values are the symbol to rename the original symbol
#'   to.  This list is updated as `rename_call` invokes itself recursively.
#' @param i the index that the next rename will adopt.

rename_call <- function(x, renames=list(), i=1L) {
  renames <- list()
  rename.i <- i
  if(
    is.call(x) && length(x) > 1L &&
    (is.symbol(x[[1L]]) || is.character(x[[1L]]))
  ) {
    fun.name <- as.character(x[[1L]])
    # For does an assignment to the counter variable
    if(fun.name %in% c("<-", "-", "for")) {
      target.symbol <- x[[2L]]
      target.type <- typeof(target.symbol)
      if(target.symbol != 'symbol') {
        msg <-
          if(fun.name == "for")
            paste("expected symbol for loop variable but got", target.type)
          else "invalid left-hand side to assignment."
        stop(simpleError(msg, x))
      }
      target.char <- as.character(target.symbol)
      target.rename <- sprintf(RENAME.ARG.TPL, rename.i)
      target.rename.symbol <- as.symbol(target.rename)
      rename.i <- rename.i + 1L
      renames[[target.char]] <- x[[2L]] <- target.rename.symbol
    }
    for(j in seq(2L, length(x), 1L)) {
      rdat <- rename_call(x[[j]], renames=renames, i=i)
      x[[j]] <- rdat[['x']]
      renames <- rdat[['renames']]
      renames.i <- rdat[['i']]
    }
  } else if (is.symbol(x)) {
    symbol.char <- as.character(x)
    if(symbol.char %in% names(renames)) x <- x[[symbol.char]]
  }
  list(x=x, renames=renames, i=rename.i)
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

#' Identify Repeated Calls and Reuse First Instance
#'
#' Complex statistics often re-use a simpler statistic multiple times, providing
#' the opportunity to optimize them by storing the result of the simple
#' statistic instead of recomputing it.  This function detects reuses of
#' sub-calls and modifies the call tree to implement the store-and-reuse
#' optimization.
#'
#' Store-and-reuse only works if there are no side-effects in the evaluation
#' of the original call being optimized, including assignments that overwrite
#' pre-existing variables.  It is still possible to optimize such calls if
#' the call is modified to use new variable names instead of overwriting
#' existing ones, as can be done with [`rename_calls`].
#'
#' @param x a call, ideally first renamed with [`rename_calls`].
#' @param rename.i index to used for additional "renamed" variables generated

reuse_calls <- function(x) {
  in.loop <- FALSE
  rename.dat <- rename_call(x)
  x <- rename.arg[['x']]

  flat <- flatten_call(x)
  calls.flat <- flat[['calls']]
  calls.indices <- flat[['indices']]
  index.lookup <- vapply(calls.indices, toString, "")

  # Deparse in order to match calls
  is.call <- vapply(calls.flat, typeof, "") == "language"
  calls.dep <- character(length(calls.flat))
  calls.dep[is.call] <- vapply(calls.flat[is.call], dep1, "")

  # Find repeated calls
  calls.match <- match(calls.dep, calls.dep)
  calls.rep <- calls.match != seq_along(calls.match) & is.call
  # For each repeated, call, find the first instance
  calls.first <- unique(match(calls.dep[calls.rep], calls.dep))

  warning("check we're in loop")

  # Eliminate Redundant repeated calls caused by a repeated call with subcalls
  # (which will obviously be repeated too).
  # For each repeated call, find the repeat count of its parent
  rep.counts <- table(calls.match[calls.match %in% which(is.call)])
  index.lookup.parent <-
    vapply(calls.indices[calls.first], function(x) toString(x[-length(x)]), "")
  index.parent <- match(index.lookup.parent, index.lookup)
  rep.count.first <- rep.counts[as.character(calls.first)]
  rep.count.parent <- rep.counts[as.character(index.parent)]
  calls.first.keep <- calls.first[rep.count.first > rep.count.parent]
  if(any(rep.count.first < rep.count.parent) || anyNA(calls.first.keep))
    stop("Internal Error: implies nonsensical call hierarchy.")

  # Source calls need to be augmented with an assignment to a renamed variable,
  # repeated calls need to be replaced by that variable.
  for(i in calls.first.keep) {
    index.i <- calls.indices[[i]]
    rename.arg <- as.name(sprintf(RENAME.ARG.TPL, rename.n))
    rename.n <- rename.n + 1L
    x[[index.i]] <- call("<-", rename.arg, x[[index.i]])
    call.rep <- seq_along(calls.dep)[-calls.first][
      match(calls.dep[i], calls.dep[-calls.first])
    ]
    for(j in call.rep) x[[calls.indices[[j]]]] <- rename.arg
  }
  x
}

