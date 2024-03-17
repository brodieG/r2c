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

## Replace All Symbols in Code With Name Based on Appearance order
##
## We normalize calls before compiling them so that we can check against our
## library of previously compiled calls to see if we already have a compiled
## call that matches what we're looking for.  The purpose of the normalization
## is so that calls such as `mean(x) + mean(y)` and `mean(foo) + mean(bar)` can
## re-use the same compiled code since it will be identical.
##
## @param call a call to normalize (matchcalled)
## @param map named list where names are original names and values
##   are the normalized symbols, can be seeded with e.g. formals to the function
##   `call` is the body of.

norm_symbols <- function(call, formals) {
  vetr(formals=CHR && !anyDuplicated(.))
  map <- vector("list", length(formals))
  names(map) <- formals
  map[] <- lapply(seq_along(map), function(x) as.name(paste0(NORM.ARG.BASE, x)))
  norm_symbols_rec(call, map)
}
norm_symbols_rec <- function(call, map) {
  if(is.call_w_args(call)) {
    for(i in seq_along(call)[-1L]) {
      tmp <- norm_symbols_rec(call[[i]], map)
      call[[i]] <- tmp[['call']]
      map <- tmp[['map']]
    }
  } else if (is.symbol(call)) {
    sym.name <- as.character(call)
    if(!sym.name %in% names(map)) {
      map[[sym.name]] <- as.name(paste0(NORM.ARG.BASE, length(map) + 1L))
    }
    call <- map[[sym.name]]
  }
  list(call=call, map=map)
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

