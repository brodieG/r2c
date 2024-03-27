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

## See `rename_call` for spec this is checking.  We don't get into checking the
## symbols to try to reduce the cost of the check.

is.renames <- function(x) {
  map.root <- generate_root(x[['map']])
  stopifnot(
    is.list(x), identical(names(x), c("i", "map")),
    all(names(x[['i']]) %in% map.root),
    all(map.root %in% names(x[['i']]))
  )
}

generate_root <- function(name)
  gsub("(^[^.[:alpha:]]|[^[:alnum:]_])", ".", substr(name, 1, 8))

# Renamed names will start with `.R2C_RN_` followed by first 8 characters of the
# variable name (with non-standard characters replaced with ".") along with a
# incrementing counter for cases where there are multiple names with ~same
# initial 8 characters (see rn[['i']] parameter to `rename_calls`).

generate_rename <- function(rn, name) {
  rn.root <- generate_root(name)
  rn.i <- rn[['i']][rn.root]
  rn.i <- if(is.na(rn.i)) 1L else rn.i + 1L
  target.rename <- sprintf(RENAME.ARG.TPL, rn.root, rn.i)
  rn[['i']][rn.root] <- rn.i
  rn[['map']][target.rename] <- name
  rn
}
apply_rename <- function(rn, symbol) {
  name <- as.character(symbol)
  # Same symbol could be renamed multiple times; take the latest rename
  if(length(matches <- names(rn[['map']])[name == rn[['map']]]))
    as.symbol(matches[length(matches)])
  else symbol
}
init_rename <- function() list(i=integer(), map=character())

# - Main Funs ------------------------------------------------------------------

## Rename Symbols in Calls
##
## Any symbols that are assigned to are given new unique names.  This
## distinguishes calls that are identical except that the content of the symbols
## they reference has changed due to an assignment overwriting or masking
## an existing variable.
##
## Due to the special run-time semantics of control structures we need to do
## additional renaming for them to avoid incorrect code reuse substitutions.
## Loops can change a variable they read from the first iteration to the second,
## and might run zero iterations.  If statements can cause variables to be
## different depending on what branch (including none) is taken.  To prevent the
## reused expression substitution from making bad assumptions about symbol
## values, we generate renames for any symbols assigned to in control
## structures.
##
## The objective of the renaming is to ensure that any same-name symbols that
## might possibly contain different values end up with different names.  It
## is not intended that the renamed call will have the same semantics as the
## original call (and it definitely won't with loops that do assignments), only
## that there is no chance of the same symbol referring to two different values
## in the course of "execution".
##
## This resolves the reuse substitution, but not memory allocations for the
## variables which have additional constraints.  E.g. an `if` statement could
## assign to a variable as a scalar in one branch and as length 100 vector in
## another.  This is resolved in the allocation step (later) by disallowing
## variables from having ambiguous sizes, and requiring that they all resolve to
## the same memory allocation..
##
## Once re-use substitution is completed, we restore the original symbol names
## with `unrename_call`.
##
## @noRd
## @param x a call
## @param rn a list with two elements:
##
## * i: named integer, for each variable root (first 8 characters), how many
##   instances there have been of it so far (the root is the name).  This is
##   designed to give some sense of what the original variable name was for
##   debugging purposes, but is not collision proof.  We don't allow full
##   variable names as if we did we would have to enforce a restriction on the
##   allowed number of characters in symbols.
## * map: named character vector where the names are the new names and the
##   values the original..
##
## @return a list with elements:
##
## * x: the renamed call
## * rn: the updated `renames` list (see `renames` above).

rename_call <- function(x, rn=init_rename()) {
  # To generate a rename is to increment an index associated with a variable.
  # To apply the rename is to replace an instance of that variable with a name
  # mangled using the index.  Strategy is to detect points at which a variable
  # name is invalidated (e.g. overwritten), and generate new renames then for
  # application at appropriate time.
  is.renames(rn)
  rename.ctrls <- CTRL.SYM
  if(is.call_w_args(x)) {
    fun.name <- get_lang_name(x[[1L]])
    rec.ids <- seq(2L, length(x), 1L)

    # Assignments both generate renames, but are special cases where we need to
    # recurse in a specific order: first the source expression for the
    # assignment, then the assignment symbol, finally the rest (for `for`)
    if(fun.name %in% ASSIGN.SYM) {
      if(length(x) != (3L + (fun.name == 'for')))
        stop("Assignment / `for` with incorrect token count (", dep1(x), ")")
      # Rename the expression that is assigned
      rdat <- rename_call(x[[rec.ids[2L]]], rn=rn)
      x[[rec.ids[2L]]] <- rdat[['x']]
      rn <- rdat[['rn']]
      # Rename the assigned-to symbol (only non-leaf rename)
      tar.char <- get_target_symbol(x, fun.name)
      rn <- generate_rename(rn, tar.char)
      x[[2L]] <- apply_rename(rn, x[[2L]])
      # Don't need to recurse into these anymore as we just dealt with them
      rec.ids <- rec.ids[-(1:2)]
    }
    # Extra renames required by control structs: loops on entry and exit, `if`
    # on exit (actual renames applied at leaves during recursion).  This means
    # that `else if` generates potentially unnecessary but harmless renames.
    ctrl.symbols <- character()
    if(fun.name %in% rename.ctrls) {
      # Exclude portions of the control exprs that don't need renames.  This is
      # ugly with possible "OOB" reads (that resolve to NULL), and the resulting
      # `x.ctrl` is garbage, but works within `assigned_symbols`.  `while`'s
      # condition is part of the loop (though guaranteed to run once).
      x.ctrl <-
        if(fun.name == 'for') x[[4L]]
        else if(fun.name == 'if') x[c(1L, 3:4)]
        else x
      ctrl.symbols <- assigned_symbols(x.ctrl, fun.name=fun.name)
      if(fun.name %in% LOOP.SYM) {
        # for loops need to generate rn on entry
        for(sym.char in ctrl.symbols) rn <- generate_rename(rn, sym.char)
    } }
    # Recurse into the paramaters of the call and apply rename
    for(j in rec.ids) {
      rdat <- rename_call(x[[j]], rn=rn)
      x[[j]] <- rdat[['x']]
      rn <- rdat[['rn']]
    }
    # Exit renames for control structures
    if(length(ctrl.symbols)) {
      for(sym.char in ctrl.symbols) rn <- generate_rename(rn, sym.char)
    }
  } else if (is.symbol(x)) {
    # Perform the rename if the symbol had one generated, this is where most of
    # the actual renaming is done, except for the `for` loop counter variable
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
