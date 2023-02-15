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
  vapply(x, is.vector, TRUE, "integer")

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
is.assign_call <- function(x)
  is.call(x) && length(x) > 2 &&
  (is.name(x[[1L]]) || is.character(x[[1L]])) &&
  isTRUE(as.character(x[[1L]]) %in% ASSIGN.SYM)
is.brace_call <- function(x)
  is.call(x) &&
  (is.name(x[[1L]]) || is.character(x[[1L]])) &&
  as.character(x[[1L]]) == "{"

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
  if(is.assign_call(x)) {
    symbols <- c(symbols, get_target_symbol(x, fun.name))
  } else if (is.call_w_args(x)) {
    for(j in seq(2L, length(x), 1L))
      symbols <- assigned_symbols(x[[j]], symbols=symbols)
  }
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
    name <- as.character(x[[1L]])
    syms <-
      if(name == "for" && length(x) == 4L) collect_call_symbols(x[[4L]])
      else if(name == "while" && length(x) == 3L) collect_call_symbols(x[[3L]])
      else if(name == "repeat" && length(x) == 2L) collect_call_symbols(x[[2L]])
      else character()
  }
  syms
}




