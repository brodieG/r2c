## Copyright (C) 2022 Brodie Gaslam
##
## This file is part of "fapply - DSL For Fast Groupwise Numeric Calculations"
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
  # Add defaults, collapse dot, don't worry about order (should be okay)
  mcall.nm.missing <- names(frm)[!names(frm) %in% c(mcall.nm, "...")]
  mcall.dots <- mcall[['...']]
  if(length(mcall.dots)) names(mcall.dots) <- rep('...', length(mcall.dots))
  c(
    as.list(mcall[seq_along(mcall) != 1L & names(mcall) != "..."]),
    mcall.dots,
    frm[mcall.nm.missing]
  )
}

is.num_naked <- function(x) vapply(x, is.vector, TRUE, "numeric")

not_num_naked_err <- function(name, val) {
  sprintf(
    "(type: %s %s%s)",
    typeof(val),
    if(length(class(val))) "class: " else "",
    if(length(class(val))) toString(class(arg.bad.val)) else ""
) }

#' Generate C Code for Compilation
#'
#' Match each R symbol to C code, possibly parameterized by some simple
#' parameters such as which parameters are specified, but not much else as
#' we need to know the groupwise data to make more decisions.
#'
#' @param call an unevaluated R call
#' @return a list containing (update):
#'   * Accrued temporary storage requirements.
#'   * Size of the current call being assessed.
#'   * A linear list of generated code and associated calls.

preprocess <- function(call) {
  # All the data generated goes into x
  x <- list(
    call=list(), depth=integer(), args=list(), args.type=list(), code=list()
  )
  # We use this for match.call, but very questionable given the env might be
  # different when we actually run the code
  env <- new.env(parent=parent.frame())
  x <- pp_internal(call=call, depth=0L, x=x, env=env)

  # Deduplicate the code and generate the final C file (need headers).
  headers <- unique(unlist(lapply(x[['code']], "[[", "headers")))
  codes <- vapply(x[['code']], "[[", "", "defn")
  names <- vapply(x[['code']], "[[", "", "name")
  codes.m <- match(codes, unique(codes))
  names.m <- match(names, unique(names))
  if(!identical(codes.m, names.m))
    stop("Internal error: functions redefind with changing definitions.")

  args.u <- unique(unlist(lapply(x[['code']], "[[", "args")))
  args <- args.u[order(match(args.u, c(ARGS.NM.ALL)))]

  code.calls <- vapply(x[['code']], "[[", "", "call")
  code.txt <- c(
    # Headers, system headers first (are these going to go in right order?)
    paste("#include", c(headers, "<R.h>", "<Rinternals.h>")),
    # Function Definitions
    unique(codes),
    # Calls
    "",
    sprintf("void run(%s) {", toString(R.ARGS.ALL[match(args, ARGS.NM.ALL)])),
    paste0(
      "  ",
      c(
        if(any(ARGS.NM.CTRL %in% args)) "int v=0;",
        code.calls[nzchar(code.calls)]
      )
    ),
    "}"
  )
  x[['code-text']] <- structure(code.txt, class="code_text")
  x[['interface']] <- interface_type(args)
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
          type=args.types[i], code=code_blank()
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
  }
  record_call_dat(x, call=call, depth=depth, argn=argn, type=type, code=code)
}
## Record Expression Data

record_call_dat <- function(x, call, depth, argn, type, code) {
  # list dataj
  x[['call']] <- c(x[['call']], list(call))
  x[['code']] <- c(x[['code']], list(code))

  # vec data
  x[['argn']] <- c(x[['argn']], argn)
  x[['depth']] <- c(x[['depth']], depth)
  x[['type']] <- c(x[['type']], type)
  x
}
# These are the possible interfaces (this got out of hand), need better solution
# 1. data, datai, len
# 2. data, datai, len, narg
# 3. data, datai, len, flag
# 4. data, datai, len, ctrl
# 5. data, datai, len, flag, ctrl
# 6. data, datai, len, narg, flag
# 7. data, datai, len, narg, ctrl
# 8. data, datai, len, narg, flag, ctrl

interface_type <- function(x) {
  ids <- toString(match(x, ARGS.NM.ALL))
  valid <- vapply(
    list(
      match(ARGS.NM.BASE, ARGS.NM.ALL),
      match(c(ARGS.NM.BASE, ARGS.NM.VAR), ARGS.NM.ALL),
      match(c(ARGS.NM.BASE, ARGS.NM.FLAG), ARGS.NM.ALL),
      match(c(ARGS.NM.BASE, ARGS.NM.CTRL), ARGS.NM.ALL),
      match(c(ARGS.NM.BASE, ARGS.NM.FLAG, ARGS.NM.CTRL), ARGS.NM.ALL),
      match(c(ARGS.NM.BASE, ARGS.NM.VAR, ARGS.NM.FLAG), ARGS.NM.ALL),
      match(c(ARGS.NM.BASE, ARGS.NM.VAR, ARGS.NM.CTRL), ARGS.NM.ALL),
      seq_along(ARGS.NM.ALL)
    ),
    toString,
    ""
  )
  if(!ids %in% valid)
    stop("Internal Error: invalid generated paramters")

  match(ids, valid)
}
