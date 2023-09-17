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
# Collapse Braces
#
# Removes redundant braces such as those containing only one call or braces
# nested immediately inside each other.

collapse_braces <- function(x, par.assign=FALSE) {
  if(is.brace_call(x)) {
    if(par.assign) stop("could not find function \"{<-\"")
    if(length(x) == 2L) x <- collapse_braces(x[[2L]])
    else if(length(x) > 2L) {
      i <- 2L
      while(i <= length(x)) {
        if(is.brace_call(x[[i]])) {
          # fold child call into parent if it is a nested brace
          call.list <- as.list(x)
          x <- dot_names(
            as.call(
              c(
                call.list[seq(1L, i - 1L, 1L)],
                # Empty braces change semantics of call (`{5;{}}` is not equal
                # to `{5}`), use numeric(0) like we do with missing branches.
                if(length(x[[i]]) == 1L) list(numeric(0))
                else as.list(x[[i]])[-1L],
                call.list[seq(i + 1L, length.out=length(x) - i, by=1L)]
          ) ) )
        } else {
          x[[i]] <- collapse_braces(x[[i]])
          i <- i + 1L
        }
      }
    }
  } else if(is.call_w_args(x)) {
    call.assign <- is.assign_call(x)
    for(i in seq(2L, length(x), 1L))
      x[[i]] <- collapse_braces(x[[i]], call.assign)
  }
  x
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
#' expressions.  In particular, any expressions with side effects are likely to
#' cause problems.  Simple assignments like `<-` or `=` are safe in cases where
#' there is no ambiguity about the order in which they would be made (e.g.
#' `{a <- b; b <- a}` is safe, but `fun(a <- b, b <- a)` might not be).  `r2c`
#' allows only the unambiguous cases for the "r2c_fun" functions, but that is
#' enforced by the [compilation functions][r2c-compile], not by `reuse_calls`.
#' Nested scopes will also trick `reuse_calls` (e.g. use of `local`, in-lined
#' functions).
#'
#' Sub-calls are compared by equality of their deparsed forms after symbols are
#' disambiguated to account for potential re-assignment.  `reuse_calls` is
#' conservative about branches, assuming that variables might be set to
#' different values by different branches.  This might void substitutions that
#' at run-time would have turned out to be valid, and even some that could have
#' been known to be valid with more sophisticated static analysis (e.g.
#' `if(TRUE) ... else ...` will be treated as a branch even though it is not
#' really).  Substitutions involving loop modified variables are also limited
#' due to the possibility of their value changing during e.g. the first and
#' nth loop iteration, or the possibility of the loop not running at all.
#'
#' `reuse_calls` relies on functions being bound to their original symbols, so
#' do not expect it to work correctly if e.g. you rebind `<-` to some other
#' function.  `r2c` checks this in its own use, but if you use `reuse_calls`
#' directly you are responsible for this.
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
  # Remove redundant braces so the hoisting isn't confused.
  x <- collapse_braces(x)

  # Add braces if there are none
  no.braces <- FALSE
  if(!is.brace_call(x)) {
    x <- call("{", x)
    no.braces <- TRUE
  }
  # rename assigned-to symbols, etc.  We rely completly on this to handle the
  # semantics of branches and loops.
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
  if(depth.max == 1L) dim(calls.ix.mx) <- c(1L, length(calls.ix.mx))
  # Find all calls with same call tree root
  root_calls <- function(ix)
    colSums(calls.ix.mx[seq_along(ix), ,drop=FALSE] == ix, na.rm=TRUE) ==
    length(ix)
  # Given an index from calls.flat/calls.ix, return all sibling indices
  sib_calls <- function(i) which(root_calls(calls.ix[[i]]))
  # Given an index from calls.flat/calls.ix, return the ancestor index
  ancestor_call <- function(i, n) {
    ix <- calls.ix[[i]]
    ixp <- ix[-seq(length(ix), length.out=n, by=-1L)]
    par <- which(root_calls(ixp) & calls.ix.len == length(ixp))
    if(length(par) != 1L) stop("Internal Error: multiple ancestors.")
    par
  }
  par_call <- function(i) ancestor_call(i, 1L)

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
  calls.name <- vapply(calls.flat, get_lang_name, "")
  sym.assign <- is.call & calls.name %in% ASSIGN.SYM
  syms <- character(length(calls.flat))
  syms[sym.assign] <- vapply(
    which(sym.assign),
    function(i) get_target_symbol(calls.flat[[i]], calls.name[[i]]), ""
  )
  if(anyDuplicated(syms[nzchar(syms)]))
    stop("Internal Error: duplicated assigned symbols, rename should prevent.")

  # For each repeated call, identify how far out it can be hoisted (we want
  # generated assignments to occur outside of parameters, i.e. avoid things
  # like `f(tmp1 <- x, tmp2 <- y)` as param evaluation order is tricky).
  #
  # An expression should not be hoisted earlier than a sub-expression it
  # contains.  So process these in reverse depth order (which is the order the
  # flattened indices are in).  Brace limit is first expression at level of
  # closeset enclosing brace.  Enclosing calls are listed after their leaves,
  # but at any given call depth the arguments are in the order they are in the
  # call.
  #
  # A brace immediately after a control structure does not stop hoisting as we
  # want hoisting to affect all subsequent branches.  Primary reason for this is
  # that it is easier to do this than to prevent substitutions across branches.
  # This is only for the special case of a computation referencing symbols
  # consistent within branches (as renames will change assigned symbols across
  # branches).
  hoists <- list()
  hoist.last <- 0L
  braces <- which(is.call & calls.name == "{")
  braces.par <- vapply(braces, par_call, 1L)
  braces <- braces[!calls.name[braces.par] %in% CTRL.SYM]
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
    } else {
      brace.i.len <- length(calls.ix[[enc.brace]])
      brace.root <- calls.ix.mx[seq_len(brace.i.len), , drop=FALSE]
    }
    # Get our call's ancestor at the brace level, unless already at brace level.
    hoist.point <-
      if(length(calls.ix[[i]]) > brace.i.len + 1L)
        ancestor_call(i, length(calls.ix[[i]]) - brace.i.len - 1L)
      else i

    # Check whether the hoist point is valid, and if not remove expression from
    # consideration as a substitutable one.  hoist.point is enclosing call, find
    # all sub-calls too
    hoist.start <- min(sib_calls(hoist.point))
    call.syms <- collect_call_symbols(calls.flat[[i]])
    sym.hoist.lim <- max(match(call.syms, syms, nomatch=0))
    if(hoist.start <= sym.hoist.lim) {
      # can't hoist b/c component symbol defined too late; void the replacement
      # check how many repeated calls for this one are after the limit, if more
      # than two update the listing to try substituting those.
      calls.rep.i <- which(calls.dep[i] == calls.dep)
      calls.rep.i <- calls.rep.i[calls.rep.i > i]
      if(length(calls.rep.i) > 1L)
        hoist.candidates <- c(
          hoist.candidates[hoist.candidates < calls.rep.i[1L]],
          calls.rep.i[1L],
          hoist.candidates[hoist.candidates > calls.rep.i[1L]]
        )
    } else {
      # can hoist it, so record the hoist
      if(length(hoist.point) != 1L)
        stop("Internal Error: failed to find unique hoist point.")
      if(hoist.point < hoist.last)
        stop("Internal Error: non-sequential hoist points?")
      hoists <- c(
        hoists, list(list(i=i, hoist=calls.ix[[hoist.point]], sub.sym=NULL))
      )
      hoist.last <- hoist.point
    }
    hoist.candidates <- hoist.candidates[-1L]
  }
  # Eliminate Redundant repeated calls caused by a repeated call with subcalls
  # as obviously the subcalls will be repeated too.  Redundant calls are those
  # that have for every instance the same hoisted call as a parent.  This is not
  # critical to do, but otherwise we end up strictly unncessary re-assignments.
  calls.i.hoisted <- vapply(hoists, "[[", 0L, "i")
  redundant <- logical(length(hoists))
  for(i in calls.i.hoisted) {
    call.depth <- length(calls.ix[[i]])
    call.match <- which(calls.dep == calls.dep[[i]])
    call.match <- call.match[call.match >= i]
    parents <- integer(length(call.match))
    for(j in seq_along(call.match)) parents[j] <- par_call(call.match[j])
    if(
      length(unique(calls.dep[parents])) == 1L &&
      parents[1L] %in% calls.i.hoisted
    )
      redundant[match(i, calls.i.hoisted)] <- TRUE
  }
  hoists <- hoists[!redundant]

  # Modify calls so each expression becomes a reference to the re-use symbol.
  ru.i <- 1L
  reuse <- list()
  for(i in seq_along(hoists)) {
    to.sub <- which(calls.dep[hoists[[i]][['i']]] == calls.dep)
    to.sub <- to.sub[to.sub >= hoists[[i]][['i']]]
    ru.char <- sprintf(REUSE.ARG.TPL, ru.i)
    ru.arg <- as.name(ru.char)
    ru.i <- ru.i + 1L
    hoists[[i]][['sub.sym']] <- ru.arg
    if(length(to.sub))  # Generate the replacement expression
      hoists[[i]][['expr']] <- call("<-", ru.arg, x[[calls.ix[[to.sub[[1L]]]]]])
    for(j in to.sub) {
      x[[calls.ix[[j]]]] <- ru.arg
    }
  }
  # Merge all the hoists that happen at the same point
  hoist.indices <- vapply(hoists, function(x) toString(x[['hoist']]), "")
  hoist.split <- split(hoists, hoist.indices)  # ugh, order not guaranteed?
  hoists.merged <- lapply(
    hoist.split,
    function(y) {
      list(
        hoist=y[[1L]][['hoist']],
        exprs=lapply(y, "[[", "expr")
  ) } )
  # Insert assignment calls at the hoist points, in reverse order as otherwise
  # each insertion affects the subsequent insertion points.
  x <- list(x)              # so there is always a parent
  extra.i <- 1L
  for(h.i in rev(seq_along(hoists.merged))) {
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

  # Drop fake layers, part 1
  x <- dot_names(x[[1L]])
  # Drop fake layers, part 2
  if(no.braces && length(x) == 2L) x <- x[[2L]]

  # Undo the renames we used to ensure call reuse did not incorrectly reuse
  # things that are different due to e.g. assignment
  x <- unrename_call(x, rename.dat[['rn']])

  # Return the reuse-substituted call along with the mapping of each reuse
  # variable to the expression it stands in for (the latter would be `reuse`,
  # but we dropped that for now).
  list(x=x, reuse=NULL)
}

