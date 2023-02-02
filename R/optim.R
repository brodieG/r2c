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
#' A Call To Recurse Into
#' @noRd

is.call_w_args <- function(x)
  is.call(x) && length(x) > 1L && (is.symbol(x[[1L]]) || is.character(x[[1L]]))

get_target_symbol <- function(x, fun.name) {
  target.symbol <- x[[2L]]
  target.type <- typeof(target.symbol)
  if(target.type != 'symbol') {
    msg <-
      if(fun.name == "for")
        paste("expected symbol for loop variable but got", target.type)
      else "invalid left-hand side to assignment."
    stop(simpleError(msg, x))
  }
  as.character(target.symbol)
}
generate_rename <- function(rn, name) {
  rn.root <- gsub("(^[^.[:alpha:]]|[^[:alnum:]])", ".", substr(name, 1, 8))
  rn.i <- rn[['i']][rn.root]
  rn.i <- if(is.na(rn.i)) 1L else rn.i + 1L
  target.rename <- sprintf(RENAME.ARG.TPL, rn.root, rn.i)
  rn[['i']][rn.root] <- rn.i
  rn[['renames']][[rn.root]] <- as.symbol(target.rename)
  rn
}
apply_rename <- function(rn, symbol) {
  name <- as.character(symbol)
  if(name %in% names(rn[['renames']])) rn[['renames']][[name]]
  else symbol
}
#' Rename Symbols in Calls
#'
#' Any symbols that are assigned to are given new unique names.  This
#' distinguishes calls that are identical except that the content of the calls
#' they reference has changed due to an assignment overwriting or masking an
#' existing variable.  Without doing this, we would not be able to look up the
#' correct memory slot at runtime since `r2c` does not explicitly bind data to
#' symbols.
#'
#' Only assignments mechanisms that are currently allowed or likely to be
#' allowed are recognized.  For example, assignments with `assign` or by editing
#' environments will not be recognized.
#'
#' Due to the special run-time semantics of control structures we need to do
#' additional renaming for them to avoid incorrect code reuse substitutions or
#' memory allocations.  Loops can change a variable they read from the first
#' iteration to the second, and might run zero iterations.  If statements can
#' cause variables to be different depending on what branch (including none) is
#' taken.  To avoid making any assumptions about the value of a symbol, we
#' generate renames.  This resolves the reuse substitution, but not memory
#' allocations for the variables which have additional constraints.  E.g. an
#' `if` statement could assign to a variable as a scalar in one branch and as a
#' vector in another.  This is resolved in the allocation step (later).
#'
#' In addition to renaming, we need to add statements that cause the renames to
#' happen (assignments).  We do not do that yet because we want to be able to
#' keep a 1-1 map of the renamed call to the original call.  Instead, we return
#' the indices where such statements occur, along with the set of symbols that
#' need renaming assignments added to them.
#'
#' What should these renames be though?  I.e. what do we assign to the rename
#' symbol.  The allocator step will be comparing all the live rename symbols for
#' each original symbol to check for sizes.  If we throw in a dummy one for the
#' substitutions we won't be able to collapse it.  Maybe it's a call to
#' `r2c_collapse` with all the potential branch symbols to resolve at that
#' point?  This seems like a good idea for `if` where we can check at the end.
#' What about for `for`?
#'
#'
#' @noRd
#' @param x a call
#' @param renames a list with two elements:
#'
#' * i: named integer, for each variable root (first 8 characters), how many
#'   instances there have been of it so far (the root is the name).  This is
#'   designed to give some sense of what the original variable name was for
#'   debugging purposes, but is not collision proof.  We don't allow full
#'   variable names as if we did we would have to enforce a restriction on the
#'   allowed number of characters in symbols.
#' * renames: a named list in which the names are the original symbol name, and
#'   the values are the symbol to rename the original symbol to.  This list is
#'   updated as `rename_call` invokes itself recursively.
#'
#' @param ri the index that the next rename will adopt.
#' @param index the index of `x` in its containing call, if any.  Any empty
#'   index means `x` is the outermost call.
#' @param ctrl.dat a list of the location and renames required for each by any
#'   control structure.
#' @return a list with components:
#'
#' * x: the renamed call
#' * renames: the final list of renames (see `renames`)
#' * ri: see `ri`.

rename_call <- function(
  x, rn=list(i=integer(), renames=list()), index=integer(), ctrl.dat=list()
) {
  rename.loop <- c("for", "while", "repeat")
  rename.ctrls <- c("if", rename.loop)
  if(is.call_w_args(x)) {
    fun.name <- as.character(x[[1L]])
    # Rename caused by assignment.  `for` assigns to the counter variable.
    # `->` becomes `<-` on parsing.
    if(fun.name %in% c("<-", "=", "for")) {
      tar.char <- get_target_symbol(x, fun.name)
      rn <- generate_rename(rn, tar.char)
    }
    # rn required by control structures (also applied going through params)
    ctrl.symbols <- character()
    if(fun.name %in% rename.ctrls) {
      # Record the control data
      ctrl.dat <- c(ctrl.dat, list(list(index=index, name=fun.name)))
      ctrl.dat.i <- length(ctrl.dat)
      ctrl.symbols <- assigned_symbols(x)
      if(fun.name %in% rename.loop) {
        # loops need to generate rn on entry
        for(sym.char in ctrl.symbols) rn <- generate_rename(rn, sym.char)
        ctrl.dat[[ctrl.dat.i]][['rn.entry']] <- rn[['renames']][ctrl.symbols]
      }
    }
    # Recurse into the paramaters of the call (techincally for assignments we
    # shouldn't do 2L, but it should be harmless).
    for(j in seq(2L, length(x), 1L)) {
      rdat <- rename_call(
        x[[j]], rn=rn, index=c(index, j), ctrl.dat=ctrl.dat
      )
      x[[j]] <- rdat[['x']]
      rn <- rdat[['rn']]
      ctrl.dat <- rdat[['ctrl.dat']]
    }
    # Apply and record ex-post control rn
    if(length(ctrl.symbols)) {
      for(sym.char in ctrl.symbols) rn <- generate_rename(rn, sym.char)
      ctrl.dat[[ctrl.dat.i]][['rn.exit']] <- rn[['renames']][ctrl.symbols]
    }
  } else if (is.symbol(x)) {
    # Perform the rename if the symbol is one of those that is assigned to
    x <- apply_rename(rn, x)
  }
  list(x=x, rn=rn, ctrl.dat=ctrl.dat)
}
#' Identify Symbols Assigned
#'
#' Return names of all symbols assigned to within a call.  This is not super
#' efficient because we recurse into every subcall, but then later as
#' `rename_call` reaches deeper, it will recurse over the subcalls again.  In
#' theory, we could do a 1 pass version of it that can then be subset into in
#' some way if this ever became a bottleneck.

assigned_symbols <- function(x, symbols=character()) {
  if(is.call_w_args(x)) {
    fun.name <- as.character(x[[1L]])
    # `->` becomes `<-` on parsing.
    if(fun.name %in% c("<-", "=")) {
      symbols <- c(symbols, get_target_symbol(x, fun.name))
    }
    # Recurse into the paramaters of the call (techincally for assignments we
    # shouldn't do 2L, but it should be harmless).
    for(j in seq(2L, length(x), 1L)) {
      symbols <- assigned_symbols(x[[j]], symbols=symbols)
    }
  }
  unique(symbols)
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
#' ```
#'
#'
#' @param x a call, ideally first renamed with [`rename_calls`].
#' @param rename.i index to used for additional "renamed" variables generated

reuse_calls <- function(x) {
  in.loop <- FALSE
  rename.dat <- rename_call(x)
  x <- rename.dat[['x']]

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

  warning("handle loop and if variables")

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
    rename.arg <- as.name(sprintf(REUSE.ARG.TPL, rename.n))
    rename.n <- rename.n + 1L
    x[[index.i]] <- call("<-", rename.arg, x[[index.i]])
    call.rep <- seq_along(calls.dep)[-calls.first][
      match(calls.dep[i], calls.dep[-calls.first])
    ]
    for(j in call.rep) x[[calls.indices[[j]]]] <- rename.arg
  }
  x
}

