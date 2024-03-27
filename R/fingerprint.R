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

## Replace Symbols in Code With Name Based on Appearance order
##
## We normalize calls before compiling them so that we can check against our
## library of previously compiled calls to see if we already have a compiled
## call that matches what we're looking for.  The purpose of the normalization
## is so that calls such as `mean(x) + mean(y)` and `mean(foo) + mean(bar)` can
## re-use the same compiled code since it will be identical.
##
## If formals are specified, then we only want to normalize symbols matching the
## formals, otherwise we normalize all the symbols (free symbols are made into
## parameters, and bound ones are defined in our function anyway so it doesn't
## really matter what name they use so long as its consistent).
##
## We probably should include a character version of the mapping so we're not
## always converting to/from character/name.
##
## @param call a call to normalize (matchcalled)
## @param map named list where names are original names and values
##   are the normalized symbols, can be seeded with e.g. formals to the function
##   `call` is the body of.
## @param unsupported return value from `sub_unsupported` (see norm_unsupported)
## @param add whether new symbols should be added to the symbol mapping (set to
##   FALSE if formals already specified).

normalize_calls <- function(call, formals, unsupported) {
  sym.map <- norm_formals(formals)
  tmp <- norm_symbols_rec(call, sym.map, unsupported, add=is.null(formals))
  call <- tmp[['call']]
  tmp <-
    norm_unsupported(unsupported, tmp[['map']], add=is.null(formals))
  list(call=call, map=tmp[['map']], unsupported=tmp[['unsupported']])
}

norm_formals <- function(formals) {
  if(!is.null(formals)) {
    formals <- formals_to_chr(formals)
    dots <- formals == "..."
    formals.no.dot <- formals[!dots]
    map <- vector("list", length(formals.no.dot))
    names(map) <- formals.no.dot
    map[!dots] <- lapply(formals.no.dot, as.name)
  } else structure(list(), names=character())
}
norm_symbols_rec <- function(call, map, unsupported, add) {
  if(is.call_w_args(call)) {
    for(i in seq_along(call)[-1L]) {
      tmp <- norm_symbols_rec(call[[i]], map, unsupported, add)
      call[[i]] <- tmp[['call']]
      map <- tmp[['map']]
    }
  } else if (is.symbol(call)) {
    sym.name <- as.character(call)
    # dots should already be R2C.DOTS by the time we get here
    if(!sym.name %in% names(map) && add && !grepl(R2C.PRIV.RX, sym.name)) {
      map[[sym.name]] <- as.name(paste0(NORM.ARG.BASE, length(map) + 1L))
    } else if (grepl(UNSUP.CALL.RX, sym.name) && add) {
      # Handle symbols embedded in unsupported calls
      sym.names <- unsupported[[sym.name]][['syms']]
      sym.names <- sym.names[!sym.names %in% names(map)]
      map[sym.names] <- lapply(
        seq_along(sym.names) + length(map),
        function(i) as.name(paste0(NORM.ARG.BASE, i))
      )
    }
    if (sym.name %in% names(map))  call <- map[[sym.name]]
  }
  list(call=call, map=map)
}
## Separate normalization for unsupported b/c of the awkwardness of needing to
## do the match-calling after unsupported sub, but the normalization after match
## calling.  This can probably be done more cleanly.

norm_unsupported <- function(unsupported, map, add) {
  for(i in seq_along(unsupported)) {
    ui <- unsupported[[i]]
    tmp <- norm_symbols_rec(ui[['call']], map, unsupported, add)
    ui[['call']] <- tmp[['call']]
    map <- tmp[['map']]
    ui[['syms']] <-
      as.vector(vapply(ui[['syms']], function(x) as.character(map[[x]]), ""))
    unsupported[[i]] <- ui
  }
  list(unsupported=unsupported, map=map)
}

# Reverse norm_symbol

denorm_symbol <- function(call, map) {
  map <- lapply(denorm_map(map), as.symbol)
  denorm_symbol_rec(call, map)
}
denorm_symbol_rec <- function(call, map) {
  if(is.call_w_args(call)) {
    for(i in seq_along(call)[-1L]) {
      call[[i]] <- denorm_symbol_rec(call[[i]], map)
    }
  } else if (is.symbol(call)) {
    sym.chr <- as.character(call)
    if(sym.chr %in% names(map)) call <- map[[sym.chr]]
  }
  call
}
# Reverse mapping, outputs as named character.
denorm_map <- function(map) {
  map.names <- vapply(map, as.character, "")
  map.vals <- names(map)
  names(map.vals) <- map.names
  map.vals
}

