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

# - Util Funs ------------------------------------------------------------------

is.call_w_args <- function(x)
  is.call(x) && length(x) > 1L && (is.symbol(x[[1L]]) || is.character(x[[1L]]))

## See `rename_call` for spec this is checking.  We don't get into checking the
## symbols to try to reduce the cost of the check.

is.renames <- function(x)
  stopifnot(
    is.list(x), identical(names(x), c("i", "renames", "map")),
    is.list(x[['renames']]), is.integer(x[['i']]), is.character(x[['map']]),
    all(names(x[['renames']]) %in% x[['map']]),
    all(x[['map']] %in% names(x[['renames']])),
    all(names(x[['i']]) %in% x[['map']]),
    all(x[['map']] %in% names(x[['i']]))
  )

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
  rn[['map']][target.rename] <- name
  rn
}
apply_rename <- function(rn, symbol) {
  name <- as.character(symbol)
  if(name %in% names(rn[['renames']])) rn[['renames']][[name]]
  else symbol
}
init_rename <- function() list(i=integer(), renames=list(), map=character())

#' Identify Symbols Assigned
#'
#' Return names of all symbols assigned to within a call.  This is not super
#' efficient because we recurse into every subcall, but then later as
#' `rename_call` reaches deeper, it will recurse over the subcalls again.  In
#' theory, we could do a 1 pass version of it that can then be subset into in
#' some way if this ever became a bottleneck.
#'
#' @noRd

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
# - Main Funs ------------------------------------------------------------------

#' Rename Symbols in Calls
#'
#' Any symbols that are assigned to are given new unique names.  This
#' distinguishes calls that are identical except that the content of the symbols
#' they reference has changed due to an assignment overwriting or masking
#' an existing variable.
#'
#' Due to the special run-time semantics of control structures we need to do
#' additional renaming for them to avoid incorrect code reuse substitutions.
#' Loops can change a variable they read from the first iteration to the second,
#' and might run zero iterations.  If statements can cause variables to be
#' different depending on what branch (including none) is taken.  To prevent the
#' reused expression substitution from making bad assumptions about symbol
#' values, we generate renames for any symbols assigned to in control
#' structures.
#'
#' Once re-use substitution is completed, we restore the original symbol names
#' with `unrename_call`.
#'
#' This resolves the reuse substitution, but not memory allocations for the
#' variables which have additional constraints.  E.g. an `if` statement could
#' assign to a variable as a scalar in one branch and as length 100 vector in
#' another.  This is resolved in the allocation step (later) by disallowing
#' variables from having ambiguous sizes.
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
#' * map: named character vector 
#'
#' @return a list with elements:
#'
#' * x: the renamed call
#' * rn: the updated `renames` list (see `renames` above).

rename_call <- function(x, rn=init_rename()) {
  is.renames(rn)
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
    # Renames required by control structures, `for` needs them on entry and
    # exit, `if` on entry (actual rename applied at leaves during recursion)
    ctrl.symbols <- character()
    if(fun.name %in% rename.ctrls) {
      ctrl.symbols <- assigned_symbols(x)
      if(fun.name %in% rename.loop) {
        # for loops need to generate rn on entry
        for(sym.char in ctrl.symbols) rn <- generate_rename(rn, sym.char)
    } }
    # Recurse into the paramaters of the call
    for(j in seq(2L, length(x), 1L)) {
      rdat <- rename_call(x[[j]], rn=rn)
      x[[j]] <- rdat[['x']]
      rn <- rdat[['rn']]
    }
    # Exit renames for control structures
    if(length(ctrl.symbols)) {
      for(sym.char in ctrl.symbols) rn <- generate_rename(rn, sym.char)
    }
  } else if (is.symbol(x)) {
    # Perform the rename if the symbol had one generated
    x <- apply_rename(rn, x)
  }
  list(x=x, rn=rn)
}
unrename_call <- function(x, rn) {
  is.renames(rn)
  if(is.call_w_args(x)) {
    for(j in seq(2L, length(x), 1L)) x[[j]] <- unrename_call(x[[j]], rn=rn)
  } else if (is.symbol(x)) {
    # Hmm, maybe 'map' should be list of symbols like 'renames'
    x.chr <- as.character(x)
    if(x.chr %in% names(rn[['map']])) x <- as.name(rn[['map']][x.chr])
  }
  x
}
