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
#' While this function is intended primarily for use as a pre-processing
#' optimization for `r2c`, it can be applied to R expressions generally with
#' some important caveats.  `r2c` should work correctly with all `r2c`
#' compatible R expressions, but is not guaranteed to do so with all R
#' expressions.  In particular, any expressions with side effects except for
#' simple assignments with `<-` or `=` are likely to cause problems.
#' Additionally, `r2c` processes function parameters in function order with
#' depth first recursion, whereas R makes no guarantees about the order in which
#' parameters are evaluated.  This could cause a normal R evaluation to evaluate
#' temporary assignments out of intended order with bad consequences.
#'
#' This function avoids potentially ambiguous substitutions, e.g. when component
#' variables might be set to different values by different branches.  This might
#' void substitutions that at run-time would have turned out to be valid, and
#' even some that could have been known to be valid with more sophisticated
#' static analysis (e.g. `if(TRUE) ... else ...` will be treated as a branch
#' even though it is not really).  Substitutions involving loop modified
#' variables are also limited due to the possibility of their value changing
#' during e.g. the first and nth loop iteration, or the possibility of the loop
#' not running at all.
#'
#' @export
#' @param x a call
#' @return a list with elements:
#'
#' * x: the call with the re-used expressions substituted
#' * reuse: named list with names the variables that reference the expressions
#'   that are substituted, and values those expressions.
#'
#' @examples
#' x <- runif(100)
#' y <- runif(100)
#' slope <- quote(((x - mean(x)) * (y - mean(y))) / (x - mean(x))^2)
#' (slope.r <- reuse_calls(slope))
#' identical(eval(slope), eval(slope.r))
#'
#' intercept <- quote(
#'   mean(y) - mean(x) * ((x - mean(x)) * (y - mean(y))) / (x - mean(x))^2
#' )
#' (intercept.r <- reuse_calls(intercept))
#' identical(eval(intercept), eval(intercept.r))

reuse_calls <- function(x) reuse_calls_int(x)[['x']]

## Internal version that additional returns a map of the temporary values to the
## expressions they replace.

reuse_calls_int <- function(x) {
  # rename assigned-to symbols, etc
  x.orig <- x
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
  reuse <- list()
  for(i in calls.first.keep) {
    index.i <- calls.indices[[i]]
    ru.char <- sprintf(REUSE.ARG.TPL, ru.i)
    ru.arg <- as.name(ru.char)
    ru.i <- ru.i + 1L
    reuse[[ru.char]] <- x.orig[[index.i]]  # keep a copy of the reuse subs
    x[[index.i]] <- call("<-", ru.arg, x[[index.i]])
    call.rep <- seq_along(calls.dep)[-calls.first][
      match(calls.dep[i], calls.dep[-calls.first])
    ]
    for(j in call.rep) x[[calls.indices[[j]]]] <- ru.arg
  }
  # Undo the renames we used to ensure call reuse did not incorrectly reuse
  # things that are different due to e.g. assignment
  x <- unrename_call(x, rename.dat[['rn']])

  # Return the reuse-substituted call along with the mapping of each reuse
  # variable to the expression it stands in for.
  list(x=x, reuse=reuse)
}

