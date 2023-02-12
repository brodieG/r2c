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
  x.orig <- x
  # Add braces if there are none
  no.braces <- FALSE
  if(!is.brace_call(x)) {
    x <- call("{", x)
    no.braces <- TRUE
  }
  # rename assigned-to symbols, etc
  rename.dat <- rename_call(x)
  x <- rename.dat[['x']]

  # Flatten the calls so we can quickly compare and find matches.  Retain tree
  # structure in 'indices' so we can retrieve the original calls and map new
  # modified calls to the original ones
  flat <- flatten_call(x)
  calls.flat <- flat[['calls']]
  calls.ix <- flat[['indices']]
  calls.ix.len <- lengths(calls.ix)
  index.lookup <- vapply(calls.ix, toString, "")
  depth.max <- max(calls.ix.len)
  calls.ix.mx <- vapply(
    calls.ix, function(x) {length(x) <- depth.max; x}, integer(depth.max)
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
  hoists <- list()
  hoist.last <- 0L
  braces <- which(vapply(calls.flat, is.brace_call, TRUE))
  hoist.candidates <- calls.first
  while(length(hoist.candidates)) {
    i <- hoist.candidates[1L]

    # Find index to hoist into (i.e. insert just before).  The idea is to find
    # the enclosing brace, and then get the next level index that our call is in
    # relative to the brace, and insert just before that.
    enc.brace <- min(c(length(calls.flat), braces[braces > i]))
    if(!enc.brace) {
      brace.root <- integer()
      brace.i.len <- 0L
    }
    else {
      brace.i.len <- length(calls.ix[[enc.brace]])
      brace.root <- calls.ix.mx[seq_len(brace.i.len), , drop=FALSE]
    }
    # Get our call's next level index.  calls.ix contains the tree index of
    # every sub-call, lengths(calls.ix) thus gives the depth of each sub-call
    # (and is stored in calls.ix.len).  calls.ix.mx is a matrix version of
    # calls.ix made regular by NA padding.
    call.root.i <- seq_len(brace.i.len + 1L)
    call.root <- calls.ix.mx[call.root.i, , drop=FALSE]
    call.root.match <- call.root == calls.ix[[i]][call.root.i]
    call.match <- colSums(call.root.match, na.rm=TRUE) == brace.i.len + 1L
    hoist.point <-
      which(call.match & calls.ix.len == brace.i.len + 1L)

    # Check whether the hoist point is valid, and if not remove expression from
    # consideration as a substitutable one.  hoist.point is enclosing call, find
    # all sub-calls too
    hoist.call <- colSums(
      calls.ix.mx[length(hoist.point), ,drop=FALSE] == calls.ix[[hoist.point]],
      na.rm=TRUE
    ) == length(hoist.point)
    hoist.start <- min(which(hoist.call))
    call.syms <- collect_call_symbols(calls.flat[[i]])
    sym.hoist.lim <- max(match(call.syms, syms, nomatch=0))
    if(hoist.start <= sym.hoist.lim) {
      # can't hoist b/c component symbol defined too late; void the replacement
      # check how many repeated calls for this one are after the limit, if more
      # than two update the listing to try substituting those.
      calls.rep.i <- match(call.dep[i], call.dep, 0L)
      calls.rep.i <- calls.rep.i[calls.rep.i > sym.hoist.lim]
      if(length(calls.rep.i) > 1L)
        hoist.candidates <- c(
          hoist.candidates[hoist.candidates < call.rep.i[1L]],
          call.rep.i[1L],
          hoist.candidates[hoist.candidates > call.rep.i[1L]]
        )
    } else {
      # can hoist it, so record the hoist
      if(length(hoist.point) != 1L)
        stop("Internal Error: failed to find unique hoist point.")
      if(hoist.point < hoist.last)
        stop("Internal Error: non-sequential hoist points?")
      hoists <- c(
        hoists,
        list(
          list(
            i=i, hoist=calls.ix[[hoist.point]], expr=calls.flat[[i]],
            expr.sub=NULL
      ) ) )
      hoist.last <- hoist.point
    }
    hoist.candidates <- hoist.candidates[-1L]
  }
  # Eliminate Redundant repeated calls caused by a repeated call with subcalls
  # as obviously the subcalls will be repeated too.  Redundant calls are those
  # that have for every instance the same hoisted call as a parent.
  calls.i.hoisted <- vapply(hoists, "[[", 0L, "i")
  redundant <- logical(length(hoists))
  for(i in calls.i.hoisted) {
    call.depth <- length(calls.ix[[i]])
    call.match <- which(calls.dep == calls.dep[[i]])
    call.match <- call.match[call.match >= i]
    parents <- integer(length(call.match))
    for(j in seq_along(call.match)) {
      k <- call.match[j]
      depth.par <- calls.ix.len[k] - 1L
      par.i <- seq_len(depth.par)
      call.ix.mx <- calls.ix.mx[par.i,,drop=FALSE]
      call.par.ix <- calls.ix[[k]][par.i]
      parent <- which(
        colSums(call.ix.mx == call.par.ix, na.rm=TRUE) == depth.par &
        calls.ix.len == depth.par
      )
      if(length(parent) != 1L) stop("Internal Error: bad parent seek.")
      parents[j] <- parent
    }
    if(length(unique(calls.dep[parents])) == 1L)
      redundant[match(i, calls.i.hoisted)] <- TRUE
  }
  hoists <- hoists[!redundant]

  # Modify calls so each expression becomes a reference to the re-use symbol.
  ru.i <- 1L
  reuse <- list()
  for(i in seq_along(hoists)) {
    to.sub <- which(calls.dep[hoists[[i]][['i']]] == calls.dep)
    ru.char <- sprintf(REUSE.ARG.TPL, ru.i)
    ru.arg <- as.name(ru.char)
    ru.i <- ru.i + 1L
    hoists[[i]][['expr.sub']] <- call("<-", ru.arg, hoists[[i]][['expr']])
    for(j in to.sub) x[[calls.ix[[j]]]] <- ru.arg
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
  x <- list(x)              # so there is always a parent
  extra.i <- 1L
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
  # Drop fake layers
  x <- x[[1L]]
  if(no.braces && length(x) == 2L) x <- x[[2L]]

  # Undo the renames we used to ensure call reuse did not incorrectly reuse
  # things that are different due to e.g. assignment
  x <- unrename_call(x, rename.dat[['rn']])

  # Return the reuse-substituted call along with the mapping of each reuse
  # variable to the expression it stands in for (the latter would be `reuse`,
  # but we dropped that for now).
  list(x=x, reuse=NULL)
}

