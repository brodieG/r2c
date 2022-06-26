## Copyright (C) 2022 Brodie Gaslam
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

NULL

## `match.call` but with default arguments
##
## Also, result is a list with just the arguments, not the original call.

match_call <- function(definition, call, envir, name) {
  frm <- formals(definition)
  frm.req <-
    vapply(frm, function(x) is.name(x) && !nzchar(as.character(x)), TRUE)
  frm.bad <- vapply(frm[!frm.req], function(x) is.language(x), TRUE)
  if(any(frm.bad))
    stop("Functions with non-constant defaults are unsupported.")

  # Match, check call
  call[[1L]] <- as.character(call[[1L]]) # hack for e.g. `+`
  mcall <-
    match.call(definition=definition, call=call, envir=envir, expand.dots=FALSE)
  mcall.nm <- names(mcall)[-1L]

  frms.req <- names(frm)[frm.req]
  frms.req <- frms.req[frms.req != "..."]
  if(!all(frms.req.have <- frms.req %in% mcall.nm))
    stop(
      "Missing required formals ",
      toString(sprintf("`%s`", frms.req[!frms.req.have])),
      " for `", name, "`."
    )
  # Add defaults, expand dots, order matters
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
#' Match each R symbol to C code, possibly parameterized by some simple
#' parameters such as which parameters are specified, but not much else as
#' we need to know the groupwise data to make more decisions.
#'
#' @noRd
#' @param call an unevaluated R call
#' @return a list containing (update):
#'   * Accrued temporary storage requirements.
#'   * Size of the current call being assessed.
#'   * A linear list of generated code and associated calls.

preprocess <- function(call, env) {
  # All the data generated goes into x
  x <- list(
    call=list(), depth=integer(), args=list(), args.type=list(), code=list(),
    sym.free=character(), sym.bound=character(), dot.arg.i=1L
  )
  # We use this for match.call, but very questionable given the env might be
  # different when we actually run the code
  x <- pp_internal(call=call, depth=0L, x=x, env=env)

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
        if(i > 1) "", paste("//", r.calls.dep.keep[i]), c.calls.keep[i],
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

pp_internal <- function(call, depth, x, argn="", env) {
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
  if(is.call(call)) {
    # - Recursion on Params ----------------------------------------------------
    func <- call_valid(call)
    # Classify Params
    args <- if(!is.null(defn <- VALID_FUNS[[c(func, "defn")]])) {
      match_call(definition=defn, call=call, envir=env, name=func)
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
          call=args[[i]], depth=depth + 1L, x=x, argn=names(args)[i], env=env
    ) } }
    type <- "call"
    # Generate Code
    code <- VALID_FUNS[[c(func, "code.gen")]](
      func,
      args[args.types == "other"],
      args[args.types == "control"],
      args[args.types == "flag"]
    )
    code_valid(code, call)
  } else {
    # - Symbol or Constant -----------------------------------------------------
    type <- "leaf"
    args <- list()
    code <- code_blank()
    # Deal with `..1`, etc, that may be generated by dots forwarding.
    # We could check for users defined. .ARG[0-9] and then start incrementing
    # after max, but too complicated (need to handle case where someone e.g.
    # defines .ARG9999999999 or whatever to try to overflow us).
    if(is.name(call)) {
      name <- as.character(call)
      if(grepl("^\\.ARG[0-9]+$", name)) {
        stop(
          "Symbols matching regex \"^\\.ARG[0-9]+$\" are disallowed (",
          name, ")."
        )
      } else if(grepl("^\\.\\.[0-9]+$", name)) {
        call <- as.name(paste0(".ARG", x[['dot.arg.i']]))
        x[['dot.arg.i']] <- x[['dot.arg.i']] + 1L
  } } }
  record_call_dat(x, call=call, depth=depth, argn=argn, type=type, code=code)
}
## Record Expression Data

record_call_dat <- function(
  x, call, depth, argn, type, code, sym.free=sym_free(x, call)
) {
  # list dataj
  x[['call']] <- c(x[['call']], list(call))
  x[['code']] <- c(x[['code']], list(code))

  # vec data
  x[['argn']] <- c(x[['argn']], argn)
  x[['depth']] <- c(x[['depth']], depth)
  x[['type']] <- c(x[['type']], type)
  x[['sym.free']] <- union(x[['sym.free']], sym.free)
  x
}
sym_free <- function(x, sym) {
  if(is.symbol(sym)) {
    sym.chr <- as.character(sym)
    if(!sym.chr %in% x[['sym.free']]) sym.chr
  }
}
