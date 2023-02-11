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
    for(i in seq(2L, length(x), 1L)) {
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
  index.max <- max(lengths(calls.indices))
  calls.indices.mx <- vapply(
    calls.indices, function(x) {length(x) <- index.max; x}, integer(index.max)
  )

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

  # Find earliest index of each symbol.
  sym.assign <- vapply(calls.flat, is.assign_call, TRUE)
  syms <- character(length(calls.flat))
  syms[sym.assign] <- vapply(calls.flat[sym.assign], get_target_symbol, "")
  if(anyDuplicated(syms[nzchar(syms)]))
    stop("Internal Error: duplicated assigned symbols, rename should prevent.")

  # For each repeated call, identify how far out it can be hoisted (we want
  # generated assignments to occur outside of parameters, i.e. avoid things
  # like `f(tmp1 <- x, tmp2 <- y)` as param evaluation order is tricky).
  #
  # An expression should not be hoisted no earlier than a sub-expression it
  # contains.  So process these in reverse depth order (which is the order the
  # flattened indices are in).  Brace limit is first expression at level of
  # closeset enclosing brace.  Enclosing calls are listed after their leaves,
  # but at any given call depth the arguments are in the order they are in the
  # call.
  hoists <- vector("list", length(calls.first))
  hoist.last <- 0L
  braces <- which(vapply(calls.flat, is.brace_call, TRUE))
  for(j in seq_along(calls.first)) {
    i <- calls.first[j]
    enc.brace <- min(c(length(calls.flat), braces[braces > i]))
    call.syms <- collect_call_symbols(calls.flat[[i]])
    sym.hoist.lim <- max(match(call.syms, sym.assign, nomatch=0))
    if(sym.hoist.lim >= enc.brace) {
      # can't hoist b/c component symbol defined too late; void the replacement
      calls.first <- calls.first[!calls.first == i]
      calls.rep <- calls.rep[
        !calls.rep %in% match(calls.dep[[i]], calls.dep, nomatch=0)
      ]
    } else {
      # ok to hoist, find index to hoist into (i.e. insert just before).  The
      # idea is to find the enclosing brace, and then get the next level index
      # that our call is in relative to the brace, and insert just before that.
      if(!enc.brace) {
        brace.root <- integer()
        brace.i.len <- 0L
      }
      else {
        brace.i.len <- length(calls.indices[[enc.brace]])
        brace.root <- calls.indices.mx[seq_len(brace.i.len), , drop=FALSE]
      }
      brace.match <-
        colSums(brace.root == calls.indices[[enc.brace]], na.rm=TRUE) ==
        brace.i.len
      brace.content.lvl <- lengths(calls.indices) ==  brace.i.len + 1L
      brace.contents <- brace.content.lvl & brace.match
      # Get our call's next level index
      call.root.i <- seq_len(brace.i.len + 1L)
      call.root <- calls.indices.mx[call.root.i, , drop=FALSE]
      call.root.match <- call.root == calls.indices[[i]][call.root.i]
      call.match <- colSums(call.root.match, na.rm=TRUE) == brace.i.len + 1L
      hoist.point <-
        which(call.match & lengths(calls.indices) == brace.i.len + 1L)
      if(length(hoist.point) != 1L)
        stop("Internal Error: failed to find unique hoist point.")
      if(hoist.point < hoist.last)
        stop("Internal Error: non-sequential hoist points?")
      hoists[[j]] <- list(
        i=i, hoist=calls.indices[[hoist.point]], expr=calls.flat[[i]],
        expr.sub=NULL
      )
      hoist.last <- hoist.point
  } }
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

  # Now also eliminate redundant from hoists; we do this after because hoisting
  # itself can eliminate calls from eligibility differently than redundant
  # elimination, so this reduces chance of eliminating something made
  # non-redundant by hoisting.
  hoist.i <- vapply(hoists, "[[", 0L, "i")
  hoists <- hoists[hoist.i %in% calls.first.keep]

  # Modify calls so each expression becomes a reference to the re-use symbol.
  ru.i <- 1L
  reuse <- list()
  for(i in seq_along(hoists)) {
    to.sub <- which(calls.dep[hoists[[i]][['i']]] == calls.dep)
    ru.char <- sprintf(REUSE.ARG.TPL, ru.i)
    ru.arg <- as.name(ru.char)
    ru.i <- ru.i + 1L
    hoists[[i]][['expr.sub']] <- call("<-", ru.arg, hoists[[i]][['expr']])
    for(j in to.sub) x[[calls.indices[[j]]]] <- ru.arg
  }
  # Merge all the hoists that happen at the same point
  hoist.indices <- vapply(hoists, function(x) toString(x[['hoist']]), "")
  hoist.split <- split(hoists, hoist.indices)  # ugh, order not guaranteed?
  hoists.merged <- lapply(
    hoist.split,
    function(x) {
      list(hoist=x[[1L]][['hoist']], exprs=lapply(x, "[[", "expr.sub"))
  } )
  # Insert assignment calls at the hoist points.
  extra.i <- integer()
  if(length(hoists.merged) && !length(braces)) {
    # Need to add an outermost brace
    x <- call("{", x)
    extra.i <- 2L
  }
  for(h.i in seq_along(hoists.merged)) {
    hoist <- hoists.merged[[h.i]]
    hoist.point <- c(extra.i, hoist[['hoist']])
    hoist.parent <- hoist.point[-length(hoist.point)]
    hoist.index <- hoist.point[length(hoist.point)]
    expr.tmp <- x[[hoist.parent]]
    if(!is.brace_call(expr.tmp) || hoist.index < 2L)
      stop("Internal Error: bad hoist data.")
    new.len <- length(hoist[['exprs']])
    expr.len <- length(expr.tmp) + new.len
    expr.target <- as.call(vector("list", expr.len))
    left <- seq_len(hoist.index - 1L)
    right <- seq(hoist.index, length(expr.tmp), 1L)
    middle <- hoist.index + seq_along(hoist[['exprs']]) - 1L
    expr.target[left] <- expr.tmp[left]
    expr.target[middle] <- hoist[['exprs']]
    expr.target[right + new.len] <- expr.tmp[right]
    x[[hoist.parent]] <- expr.target
  }
  # Undo the renames we used to ensure call reuse did not incorrectly reuse
  # things that are different due to e.g. assignment
  x <- unrename_call(x, rename.dat[['rn']])

  # Return the reuse-substituted call along with the mapping of each reuse
  # variable to the expression it stands in for (the latter would be `reuse`,
  # but we dropped that for now).
  list(x=x, reuse=NULL)
}

