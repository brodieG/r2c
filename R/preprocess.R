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

#' @include code.R
#' @include optim.R

NULL

## `match.call` but with default arguments
##
## Also, result is a list with just the arguments, not the original call.

match_call <- function(definition, call, name) {
  frm <- formals(definition)
  frm.req <-
    vapply(frm, function(x) is.name(x) && !nzchar(as.character(x)), TRUE)
  frm.bad <- vapply(frm[!frm.req], function(x) is.language(x), TRUE)
  if(any(frm.bad))
    stop("Functions with non-constant defaults are unsupported.")

  # Match, check call
  call[[1L]] <- as.character(call[[1L]]) # hack for e.g. `+`
  # Replace dots (note these are dots as an argument, as opposed to dots in the
  # formals of the function we'll generate); we do not want these dots to be
  # matched against anything since that will be done at run-time, not now (i.e.
  # dots might not even exist at the moment).
  any.dots <- vapply(call[-1L], identical, TRUE, quote(...))
  any.r2cdots <- vapply(call[-1L], identical, TRUE, quote(.R2C.DOTS))
  if(any(any.r2cdots)) stop("Symbol `.R2C.DOTS` is disallowed.")
  if(any(any.dots)) call[-1L][which(any.dots)] <- list(quote(.R2C.DOTS))

  mcall <- match.call(definition=definition, call=call, expand.dots=FALSE)
  mcall.nm <- names(mcall)[-1L]

  frms.req <- names(frm)[frm.req]
  # recall: these dots (from formals) are not the ones replaced by .R2C.DOTS
  # (forwarded as an argument)!
  frms.req <- frms.req[frms.req != "..."]
  if(!all(frms.req.have <- frms.req %in% mcall.nm))
    stop(
      "Missing required formals ",
      toString(sprintf("`%s`", frms.req[!frms.req.have])),
      " for `", name, "`."
    )
  # Add defaults, expand formals dots, order matters
  mcall.nm.missing <- names(frm)[!names(frm) %in% c(mcall.nm, "...")]
  mcall.dots <- mcall[['...']]
  if(is.null(mcall.dots)) mcall.dots <- list()
  args.nm <- c(
    names(mcall[seq_along(mcall) != 1L & names(mcall) != "..."]),
    "...",
    names(frm[mcall.nm.missing])
  )
  args.dummy <- c(
    as.list(mcall[seq_along(mcall) != 1L & names(mcall) != "..."]),
    list(NULL),     # placeholder for dots
    frm[mcall.nm.missing]
  )
  args.pos <- match(args.nm, names(frm))
  args.ord <- order(args.pos)
  args <- args.dummy[args.ord]
  args.dot.pos <- if(length(mcall.dots)) {
    names(mcall.dots) <- rep('...', length(mcall.dots))
    match("...", args.nm[args.ord])
  } else length(args)
  # expand the dots
  c(
    args[seq_len(args.dot.pos - 1L)],
    mcall.dots,
    if(length(args.dummy) > args.dot.pos)
      args[seq(args.dot.pos + 1L, length(args), by=1L)]
  )
}

#' Generate C Code for Compilation
#'
#' Match each call and its parameters, identifying which parameters are control
#' vs. flag, and associating the C call to each R call.  The call tree is
#' linearized depth first, so the parameters are recorded before the call they
#' belong to.  The depth of the parameters allows us to distinguish what call
#' they belong to (note a parameter can be a call too).  The order of the
#' elements in the linearized call implicitly contains all parameter matching
#' information (i.e. everything has been `match.call`ed already).
#'
#' See `alloc` and `init_call_dat` for more details.
#'
#' @noRd
#' @param call an unevaluated R call
#' @return a call dat list as described in `init_call_dat`.

preprocess <- function(call, formals) {
  # All the data generated goes into x
  x <- init_call_dat(formals)
  # We use this for match.call, but very questionable given the env might be
  # different when we actually run the code
  x <- pp_internal(call=call, depth=0L, x=x)

  # Apply "compiler" optimizations
  x <- compile_optim(x)

  # Deduplicate the code and generate the final C file (need headers).
  headers <- unique(unlist(lapply(x[['code']], "[[", "headers")))
  codes <- vapply(x[['code']], "[[", "", "defn")
  names <- vapply(x[['code']], "[[", "", "name")
  codes.m <- match(codes, unique(codes))
  names.m <- match(names, unique(names))
  if(!identical(codes.m, names.m))
    stop("Internal error: functions redefined with changing definitions.")
  codes.u <- paste0(
    gsub("^(\\s|\\n)+|(\\s|\\n)+$", "", unique(codes[nzchar(codes)])),
    "\n"
  )
  # Generate the C equivalent code calls and annotate them with the R ones
  c.calls <- vapply(x[['code']], "[[", "", "call")
  r.calls.dep <- vapply(
    x[['call']], function(x) paste0(deparse(x), collapse="\n"), ""
  )
  calls.keep <- nzchar(c.calls)
  c.calls.keep <- c.calls[calls.keep]
  r.calls.dep.keep <- r.calls.dep[calls.keep]
  r.calls.dep.m <- grepl("\n", r.calls.dep.keep, fixed=TRUE)
  c.calls.fmt <- format(c.calls.keep)

  # Do we need extra parameters that need to be incremented explicitly?
  c.narg <- vapply(x[['code']][calls.keep], "[[", TRUE, "narg")
  c.flag <- vapply(x[['code']][calls.keep], "[[", TRUE, "flag")
  c.ctrl <- vapply(x[['code']][calls.keep], "[[", TRUE, "ctrl")
  any.inc <- c.narg | c.flag | c.ctrl

  calls.fin <- lapply(
    seq_along(which(calls.keep)),
    function(i)
      c(
        if(i > 1) "",
        paste("//", unlist(strsplit(r.calls.dep.keep[i], "\n"))),
        c.calls.keep[i],
        if(any(any.inc))
          sprintf(
            "%s;",
            paste(
              c(
                if(any(c.narg)) INC.VAR, if(any(c.flag)) INC.FLAG,
                if(any(c.ctrl)) INC.CTRL
              ),
              collapse="; "
      )   ) )
  )
  code.txt <- c(
    # Headers, system headers first (are these going to go in right order?)
    paste("#include", c(headers, "<R.h>", "<Rinternals.h>")),
    "",
    # Function Definitions
    codes.u,
    # Calls
    sprintf("void run(\n  %s\n) {", toString(R.ARGS.ALL)),
    paste0(
      "  ",
      c(
        if(any(c.ctrl)) "int v=0;",
        if(!any(c.narg)) "(void) narg; // unused",
        if(!any(c.flag)) "(void) flag; // unused",
        if(!any(c.ctrl)) "(void) ctrl; // unused",
        "",
        unlist(calls.fin)
      )
    ),
    "}"
  )
  x[['code']] <- structure(code.txt, class="code_text")
  x
}
# Recursion portion of preprocess

pp_internal <- function(call, depth, x, argn="") {
  if(depth == .Machine$integer.max)
    stop("Expression max depth exceeded.") # exceedingly unlikely

  # - Prep ---------------------------------------------------------------------

  # Remove any parentheses calls
  while(is.call(call) && identical(as.character(call[[1L]]), "(")) {
    if(length(call) != 2L)
      stop("Internal Error: call to `(` with wrong parameter count.")  # nocov
    call <- call[[2L]]
  }
  # Transform call (e.g. x^2 becomes x * x); ideally the transformation function
  # would have the matched call, but since the only use we have for it is a
  # primitive that doesn't match arguments we don't worry about that right now.

  while(is.call(call)) { # equivalent to if(...) repeat
    func <- call_valid(call)
    call.transform <- VALID_FUNS[[c(func, "transform")]](call)
    if(identical(call.transform, call)) break
    call <- call.transform
  }
  rename <- character()
  if(is.call(call)) {
    # - Recursion on Params ----------------------------------------------------
    func <- call_valid(call)
    # Classify Params
    args <- if(!is.null(defn <- VALID_FUNS[[c(func, "defn")]])) {
      match_call(definition=defn, call=call, name=func)
    } else {
      as.list(call[-1L])
    }
    args.types <- rep("other", length(args))
    args.types[names(args) %in% VALID_FUNS[[c(func, "ctrl")]]] <- "control"
    args.types[names(args) %in% VALID_FUNS[[c(func, "flag")]]] <- "flag"

    for(i in seq_along(args)) {
      if(args.types[i] %in% c('control', 'flag')) {
        x <- record_call_dat(
          x, call=args[[i]], depth=depth + 1L, argn=names(args)[i],
          type=args.types[i], code=code_blank(),
          sym.free=sym_free(x, args[[i]])
        )
      } else {
        x <- pp_internal(
          call=args[[i]], depth=depth + 1L, x=x, argn=names(args)[i]
    ) } }
    type <- "call"
    # Generate Code
    warning("Don't generate code, instead record data for code gen")
    code <- VALID_FUNS[[c(func, "code.gen")]](
      func,
      args[args.types == "other"],
      args[args.types == "control"],
      args[args.types == "flag"]
    )
    code_valid(code, call)
    # In case of assignment, we generate a rename.  Rename is not necessary if
    # the symbol already exists, but it doesn't harm and is needed for when the
    # symbol already exists
    if(func %in% c("=", "<-")) {
      target.symbol <- args[[1L]]
      if(!is.name(target.symbol))
        stop(simpleError("invalid left-hand side to assignment.", call))
      rename <- as.character(target.symbol)
    }
  } else {
    # - Symbol or Constant -----------------------------------------------------
    type <- "leaf"
    args <- list()
    code <- code_blank()
    # Deal with `..1`, etc, that may be generated by dots forwarding.
    # We could check for user defined .ARG[0-9] and then start incrementing
    # after max, but too complicated (need to handle case where someone e.g.
    # defines .ARG9999999999 or whatever to try to overflow us).
    if(is.name(call)) {
      name <- as.character(call)
      if(grepl(DOT.ARG.RX, name)) {
        stop(
          "Symbols matching regex \"", DOT.ARG.RX, "\" are disallowed (",
          name, ")."
        )
      } else if(grepl("^\\.\\.[0-9]+$", name)) {
        call <- as.name(sprintf(DOT.ARG.TPL, x[['dot.arg.i']]))
        x[['dot.arg.i']] <- x[['dot.arg.i']] + 1L
  } } }
  record_call_dat(
    x, call=call, depth=depth, argn=argn, type=type, code=code,
    rename=rename
  )
}
# See preprocess for some discussion of what the elements are
#'
#' $call: linearized call tree with parameters preceeding calls (recall that a
#'   call can itself be a parameter to another call nearer the root).
#' $depth: tree depth of each call or parameter
#' $args: unused leftover from prior bad implementation?
#' $args.type: unused leftover from prior bad implementation?
#' $argn: parameter name argument is bound to.
#' $code: the generated C code
#' $sym.free/bound: ? Not sure this is still correct since we add the `formals`
#'   parameter.
#' $dot.arg.i: counter used when generating the symbols that replace
#'   the `..1`, `..2`, ... symbols.
#' $rename.arg.i: counter used when generating the symbols that replace
#'   symbols that have been re-bound via assignment or are stand-ins for
#'   repeated calculations.  Note that the difference between the dot version
#'   and this one is that the dot numbering is "0###" whereas this one is
#'   "1###".  We use such close name just to avoid having to add more
#'   categories of disallowed variable names.
#' $rename: named character vector with the symbols as they appear in the call
#'   as the names, and their current renamed version as the value.  This is a
#'   point in time snapshot and cannot be used to reconstruct the history of the
#'   renames.
#' $last.read: named integer of indeces in the linearized call tree of the last
#'   _call_ that read a particular symbol.  This allow us to know when we can
#'   free an allocation otherwise used by that symbol.  The names of this vector
#'   correspond to the **renamed** variables.
#' $call.rename: version of `call` with symbols renamed using `rename`.
#' $type: argument type
#'
#' @noRd

init_call_dat <- function(formals)
  list(
    call=list(),
    depth=integer(),
    args=list(),
    args.type=list(),
    code=list(),
    sym.free=formals,
    sym.bound=character(),
    dot.arg.i=1L,
    rename.arg.i=1L,
    last.read=integer(),
    protected=logical(),
    rename=character(),
    call.rename=character()
  )

## Record Expression Data
##
## This is what linearizes the call tree.

record_call_dat <- function(
  x, call, depth, argn, type, code, sym.free=sym_free(x, call),
  rename=character()
) {
  # Undo the dots replacement
  call <- if(identical(call, quote(.R2C.DOTS))) QDOTS else call

  # list data
  x[['call']] <- c(x[['call']], list(call))
  x[['code']] <- c(x[['code']], list(code))

  # vec data
  x[['argn']] <- c(x[['argn']], argn)
  x[['depth']] <- c(x[['depth']], depth)
  x[['type']] <- c(x[['type']], type)
  x[['sym.free']] <- union(x[['sym.free']], sym.free)

  # Assignment calls should submit non empty rename, which then will lead to any
  # subsequent calls having matching symbols renamed.  See `rename_call`.
  x[['rename']] <- append_rename(x, rename)
  x[['call.rename']] <- c(x[['call.rename']], list(rename_call(x, call)))

  if(is.symbol(x[['call.rename']])) {
    x[['last.read']][as.character(x[['call.rename']])] <- length(x[['call']])
  }
  x
}
# ## Rename Symbols in Calls
# ##
# ## The purpose of the renaming is to distinguish calls that are identical except
# ## that the content of the calls they reference has changed due to an assignment
# ## overwriting or masking an existing variable.
# ##
# ## @param x a call dat object
# ## @param call language a call/symbol to rename
# 
# rename_call <- function(x, call) {
#   call <- if (is.name(call)) {
#     call.chr <- as.character(call)
#     call <-
#       if(call.chr %in% names(x[['rename']])) as.name(x[['rename']][call.chr])
#       else call
#   } else if (is.call(call) && length(call) > 1L) {
#     # Re-assemble call from the prior arguments that have already been renamed.
#     # For a call with n arguments, the prior n elements in the list at depth + 1
#     # will be the arguments.
#     renamed.at.depth <- x[['call.rename']][
#       x[['depth']] == x[['depth']][length(x[['depth']])] + 1L
#     ]
#     call[-1L] <- renamed.at.depth[
#       seq(length(renamed.at.depth), length.out=length(call) - 1L, by=-1L)
#     ]
#   }
#   call
# }
# ## Apply Renames to Character Representation of Symbol
# ##
# ## A new instance of a symbol to rename is added to the list.  An existing
# ## instance is further renamed, using the `.ARG1###` syntax.
# ##
# ## @param x a call dat object
# ## @param symbol a symbol to rename
# 
# append_rename <- function(x, symbol) {
#   stopifnot(is.symbol(symbol))
#   sym.char <- as.character(symbol)
#   rename.i <- x[['rename.arg.i']]
#   x[['rename']][sym.char] <- as.name(sprintf(RENAME.ARG.TPL, rename.i))
#   x[['rename.arg.i']] <- rename.i + 1L
#   x
# }

sym_free <- function(x, sym) {
  if(is.symbol(sym)) {
    sym.chr <- as.character(sym)
    if(identical(sym.chr, ".R2C.DOTS")) sym.chr <- "..."
    if(!sym.chr %in% x[['sym.free']]) sym.chr
  }
}

## Expand Dots
##
## Once we have an actual param match at runtime, we need to expand out the call
## data matched to dots to the number of dots.

expand_dots <- function(x, arg.names) {
  exp.fields <- c('argn', 'type', 'depth')
  is.dots <- vapply(x[['call']], identical, TRUE, QDOTS)
  is.dots.m <- grepl(DOT.ARG.RX, arg.names)
  if(any(is.dots)) {
    dots.m.names <- lapply(arg.names[is.dots.m], as.name)
    # Could have multiple sets of dots
    for(i in which(is.dots)) {
      x[['call']][[i]] <- NULL
      x[['call']] <- append(x[['call']], dots.m.names, after=i - 1L)
      for(j in exp.fields) {
        exp.val <- x[[j]][i]
        x[[j]] <- c(
          x[[j]][seq_len(i - 1L)],
          rep(exp.val, sum(is.dots.m)),
          x[[j]][seq_len(length(x[[j]]) - i) + i]
  ) } } }
  x
}



