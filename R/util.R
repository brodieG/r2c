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

#' Basic Split Apply Combine
#'
#' Evaluates quoted expressions in the context of data split by group.  Intended
#' purely for testing against r2c calculations.
#'
#' @export
#' @inheritParams group_exec
#' @param call quoted R call to apply to each group.
#' @param enclos environment to use as the enclosure to the data in the
#'   evaluation call (see [`eval`]).
#' @return numeric vector

bsac <- function(call, data, groups, MoreArgs=list(), enclos=parent.frame()) {
  data <- if(!is.data.frame(data)) as.data.frame(data) else data
  d.s <- split(data, groups)

  more <- list2env(MoreArgs, parent=enclos)
  res <- lapply(
    d.s,
    function(x, call) eval(call, envir=x, enclos=more), call=call
  )
  unlist(res)
}
#' Retrieve First Vector from Data
#'
#' Designed to handle the case where `data` can be either a numeric vector or a
#' list of numeric vectors.
#'
#' @export
#' @param x a numeric vector, or a (possibly empty) list of numeric vectors.
#' @return if `data` is a list, the first element if it is a numeric vector, or
#'   an empty numeric vector if the list is empty.  If `data` is a numeric
#'   vector, then `data`.  Otherwise an error is thrown.
#' @examples
#' first_vec(1:5)
#' first_vec(runif(5))
#' first_vec(mtcars)
#' first_vec(matrix(1:4, 2))  # matrices treated as vectors

first_vec <- function(x) {
  vetr(is.list(.) || is.numeric(.))
  if(is.list(x) && !length(x)) x <- numeric()
  if(is.list(x)) x <- x[[1L]]
  if(!is.numeric(x))
    stop("Argument `data` must be numeric, or a list containing numeric vectors.")
  x
}

is.r2c_fun <- function(x)
  is.function(x) && inherits(x, "r2c_fun")

is.num_naked <- function(x)
  vapply(x, is.vector, TRUE, "numeric") |
  vapply(x, is.vector, TRUE, "integer") |
  vapply(x, is.vector, TRUE, "logical")

not_num_naked_err <- function(name, val) {
  has.class <- length(class(val)) && !identical(class(val), typeof(val))
  sprintf(
    "(type: %s%s%s)",
    typeof(val),
    if(has.class) ", class: " else "",
    if(has.class) toString(class(val)) else ""
) }

## Run an Expression with Random Seed Unset
##
## In cases where we would really like something to be random and not dictated
## by a set seed.

without_seed <- function(expr, env=parent.frame()) {
  prev.seed <- mget(
    ".Random.seed", envir=.GlobalEnv, ifnotfound=list(NULL)
  )[[1L]]
  on.exit({
    if(is.null(prev.seed) && exists(".Random.seed", envir=.GlobalEnv))
      rm(".Random.seed", envir=.GlobalEnv)
    else
      assign(".Random.seed", prev.seed, envir=.GlobalEnv)
  })
  set.seed(NULL)
  expr
}

#' Pre-Set Function Parameters
#'
#' Create a new function from an existing function, but with parameters pre-set.
#' This is a function intended for testing to simplify complex expressions
#' involving the `_exec` functions.  It merely stores the function expression to
#' execute in the lexical environment it was created in.  All symbols will be
#' resolved at evaluation time, not at creation time.
#'
#' This is inspired by a function originally from Byron Ellis, adapted by Jamie
#' F Olson, and discovered by me via Peter Danenberg's `{functional}` (see
#' packages `?functional::Curry` and `functional::CurryL`).  The implementation
#' here is different, in particular it makes it easy to see what the intended
#' call is by displaying the function contents (see examples).
#'
#' @export
#' @param FUN the function to pre-set parameters for
#' @param ... parameters to pre-set
#' @return `FUN` wrapped with pre-set parameters
#' @examples
#' sum_nona <- lcurry(sum, na.rm=TRUE)
#' sum_nona(c(1, NA, 2))
#' sum_nona

lcurry <- function (FUN, ...) {
  call <- match.call()
  call[[1]] <- call[[2]]
  call[[2]] <- NULL
  call[[length(call) + 1]] <- quote(...)
  f <- function(...) NULL
  body(f) <- call
  environment(f) <- parent.frame()
  f
}

# - Internal Utility Tools -----------------------------------------------------

is.chr_or_sym <- function(x) is.symbol(x) || is.character(x) && length(x) == 1L

is.call_w_args <- function(x) is.call(x) && length(x) > 1L

is.assign_call <- function(x)
  is.call(x) && isTRUE(get_lang_name(x) %in% ASSIGN.SYM) &&
  length(x) %in% 3:4 # (`for` has four parameters, and it assigns to its second)
is.brace_call <- function(x)
  is.call(x) && identical(x[[1L]], QBRACE)

## Specifically tests for pkg::name, not pkg::name(...) (for the latter you can
## use e.g. `nzchar(get_lang_info(x)[['pkg']])`
is.dbl_colon_call <- function(x)
  is.call(x) && length(x) == 3L &&
  is.chr_or_sym(x[[1L]]) && is.chr_or_sym(x[[2L]]) && is.chr_or_sym(x[[3L]]) &&
  as.character(x[[1L]]) == "::"

is.passive_call <- function(x)
  is.call(x) && get_lang_name(x) %in% c(PASSIVE.SYM, ASSIGN.SYM)

# For `<symbol> <- y`, retrieve the symbol.  Obviously assumes `x` has been
# checked previously to be an assignment.  Recall `for` includes an assignment.
get_target_symbol <- function(x, fun.name) {
  if(!fun.name %in% MODIFY.SYM)
    stop("Internal Error: ", fun.name, " is not an assignment function.")
  target.symbol <- x[[2L]]
  target.type <- typeof(target.symbol)
  if(target.type != 'symbol') {
    msg <-
      if(fun.name == "for") # FOR.ITER not needed (see constants.R)
        paste("expected symbol for loop variable but got", target.type)
      else "invalid left-hand side to assignment."
    stop(simpleError(msg, x))
  }
  as.character(target.symbol)
}
# Which parameteres in formals have default values
default_params <- function(formals)
  vapply(formals, function(x) !identical(x, MISSING[[1L]]), TRUE)

#' Identify Symbols Assigned
#'
#' Return names of all symbols assigned to within a call.  This is not super
#' efficient because we recurse into every subcall, but then later as
#' `rename_call` reaches deeper, it will recurse over the subcalls again.  In
#' theory, we could do a 1 pass version of it that can then be subset into in
#' some way if this ever became a bottleneck.
#'
#' @param x a call
#' @param fun.name could be deducible from `x`, but we've already computed it
#'   wherever we call this from so we just pass it in.
#'
#' @noRd

assigned_symbols <- function(x, symbols=character(), fun.name) {
  if(is.assign_call(x)) {
    symbols <- c(symbols, get_target_symbol(x, fun.name))
  } else if (is.call_w_args(x)) {
    for(j in seq(2L, length(x), 1L)) {
      if (is.call_w_args(x[[j]])) {
        symbols <-
          assigned_symbols(x[[j]], symbols=symbols, get_lang_name(x[[j]]))
  } } }
  unique(symbols)
}

collect_call_symbols <- function(x) {
  syms <- character()
  if(is.call(x) && length(x) > 1) {
    for(i in seq(2L, length(x), 1L))
      syms <- union(syms, collect_call_symbols(x[[i]]))
  } else if (is.symbol(x)) {
    syms <- as.character(x)
  }
  syms
}
collect_loop_call_symbols <- function(x) {
  syms <- character()
  if(is.call(x) && length(x) > 1) {
    name <- get_lang_name(x)
    syms <-
      if(name == FOR.N && length(x) == 2L) collect_call_symbols(x[[2L]])
      # else if(name == "while" && length(x) == 3L) collect_call_symbols(x[[3L]])
      # else if(name == "repeat" && length(x) == 2L) collect_call_symbols(x[[2L]])
      else character()
  }
  syms
}

## Extract Function Name and Package From Call
##
## Designed to handle the `pkg::fun(...)` case.  In general the "pkg" part is
## mostly decoration as we assume that all the allowable "name" values are
## unique (i.e. no two supported packages both implement a function with a name
## corresponding to an r2c supported function).
##
## Also handles standalone symbols, in which case "name" is that symbol coerced
## to character.
##
## @return list with members "name" (function/symbol name) and "pkg" (package
##   name, "" if no package specified) for `get_lang_info`, or just the
##   function/symbol name for `get_lang_name`.

get_lang_info <- function(call) {
  pkg <- ""
  name <-
    if (is.chr_or_sym(call)) as.character(call)
    else if (is.call(call)) {
      if(is.dbl_colon_call(call[[1L]])) {
        pkg <- as.character(call[[1L]][[2L]])
        as.character(call[[1L]][[3L]])
      }
      else if (is.chr_or_sym(call[[1L]])) as.character(call[[1L]])
      else stop(
        "Only calls in form `fun(...)` and `pkg::fun(...)` where `fun` is a ",
        "symbol or character are supported, i.e. not:\n", deparseLines(call)
      )
    }
    else ""
  list(name=name, pkg=pkg)
}
get_lang_name <- function(call) {
  if(is.language(call)) get_lang_info(call)[['name']]
  else ""
}
blank_lang_info <- function() list(name="", pkg="")

pkg_fun <- function(fun, pkg='r2c')
  call("::", pkg=as.name(pkg), name=as.name(fun))

en_assign <- function(x, value) call("<-", x=x, value=value)

# deparse but concatenate multi-element results with newlines

deparseLines <- function(x, ...) paste0(deparse(x, ...), collapse="\n")

# helper fun for tests looking at the cleaned up call resulting from
# preprocessing
pp_clean <- function(x, optimize=TRUE) {
  clean_call(preprocess(x, optimize=TRUE)[['call.processed']])
}

convolve <- function(a, b) {
  stopifnot(is.numeric(a), is.numeric(b))
  .Call(R2C_convolve, a, b)
}
# Helper to display the call data stored by `alloc`.

disp_call_dat <- function(call.dat) {
  lang <- vapply(
    call.dat,
    function(x) gsub("\br2c::\b", "", deparse(x[['call']])[[1L]]),
    ""
  )
  i <- format(seq_along(call.dat))
  ids <- format(
    vapply(call.dat, function(x) paste0(x[['ids']], collapse=" "), "")
  )
  dat <- paste0(i, ": ", ids, " | ")
  lang <- substr(lang, 1, max(c(10, 80 - max(nchar(dat)))))
  writeLines(paste0(dat, lang))
}
