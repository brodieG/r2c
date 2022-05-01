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

#' @include code-arith.R
#' @include code-summary.R

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
  c(
    as.list(mcall[seq_along(mcall) != 1L & names(mcall) != "..."]),
    mcall[['...']],
    frm[mcall.nm.missing]
  )
}

#' Initializer for Function Registration Entries
#'
#' @section Code Generation:
#'
#' Code generation is handled by the function provided in `code.gen`.  The
#' function should return a list with three character vectors, each
#' representing:
#'
#' * Definition of the C function.
#' * Name of the C function.
#' * Call to use to invoke the function.
#'
#' Function parameters are specifically defined.
#'
#' @param name character(1L) symbol that will reference the function
#' @param fun the function we're trying to emulate
#' @param defn NULL if fun is a closure, otherwise a function template to use
#'   for [`match.call`]'s `definition` parameter.
#' @param ctrl.params character names of all the formal parameters that are
#'   to be evaluated once up front and not for each group in the data.  If any
#'   data columns are referenced by these parameters, the entire data column
#'   will be used for them, not the group varying subsets of them.
#' @param type list(2L) containing the type of function in "constant", "arglen",
#'   or "vecrec" at position one, and additional meta data at position two
#'   that can be depending on the value in position one:
#'
#'   * constant: a positive non-NA integer indicating the constant result size
#'     (e.g. 1L for `mean`)
#'   * arglen: character(1L) the name of the argument to use the length of as
#'     the result size (e.g. `probs` for [`quantile`].
#'   * vecrec: character(n) the names of the arguments to use to compute result
#'     size under assumption of recycling to longest, or zero if any argument is
#'     zero length.
#'
#' @param code.gen a function that generates the C code corresponding to an R
#'   function, which accepts three parameters (see details for the expected
#'   function semantics):
#'
#'   * Name of the R function.
#'   * A numeric vector of argument sizes of non-control parameters,
#'     where arguments of group size are given NA size.
#'   * A list of the evaluated control parameters.
#'
#' @return a list containing the above information after validating it.

fap_fun <- function(
  name, fun, defn, ctrl.params=character(), type,
  code.gen, ctrl.validate=function(x) TRUE
) {
  vetr(
    name=CHR.1,
    fun=is.function(.),
    defn=typeof(fun) == "closure" && NULL || is.function(.),
    ctrl.params=
      character() && !anyNA(.) && all(. %in% names(formals(defn))) &&
      !"..." %in% .,
    type=list(NULL, NULL),
    code.gen=is.function(.),
    ctrl.validate=is.function(.)
  )
  # Bug in vetr prevents this being done directly above
  stopifnot(
    is.character(type[[1L]]) && length(type[[1L]]) == 1L && !is.na(type[[1L]]),
    type[[1L]] %in% c("constant", "arglen", "vecrec"),
    (
      (
        type[[1L]] == "constant" &&
        is.integer(type[[2L]]) &&
        length(type[[2L]]) == 1L &&
        !is.na(type[[2L]]) &&
        type[[2L]] >= 0L
      ) ||
      (
        type[[1L]] == "arglen" &&
        is.character(type[[2L]]) &&
        length(type[[2L]]) == 1L &&
        !is.na(type[[2L]])
      ) ||
      (
        type[[1L]] == "vecrec" &&
        is.character(type[[2L]]) &&
        !anyNA(type[[2L]])
      )
  ) )
  list(
    name=name, fun=fun, defn=defn, ctrl=ctrl.params, type=type,
    code.gen=code.gen
  )
}
VALID_FUNS <- list(
  fap_fun(
    "sum", base::sum, function(..., na.rm=FALSE) NULL,
    "na.rm", list("constant", 1L),
    code.gen=code_gen_summary,
    ctrl.validate=ctrl_val_summary
  ),
  fap_fun(
    "mean", fun=base::mean, defn=base::mean.default,
    c("trim", "na.rm"), list("constant", 1L),
    code.gen=code_gen_summary,
    ctrl.validate=ctrl_val_summary
  ),
  fap_fun(
    "+", base::`+`, defn=function(e1, e2) NULL,
    type=list("vecrec", c("e1", "e2")), code.gen=code_gen_arith
  ),
  fap_fun(
    "-", base::`-`, defn=function(e1, e2) NULL,
    type=list("vecrec", c("e1", "e2")), code.gen=code_gen_arith
  ),
  fap_fun(
    "*", base::`*`, defn=function(e1, e2) NULL,
    type=list("vecrec", c("e1", "e2")), code.gen=code_gen_arith
  ),
  fap_fun(
    "/", base::`/`, defn=function(e1, e2) NULL,
    type=list("vecrec", c("e1", "e2")), code.gen=code_gen_arith
  ),
  # this is not "really" a fun, but we shoehorn it into our model here, we could
  # also add code to skip it
  fap_fun(
    "(", base::`(`, defn=function(x) NULL,
    type=list("arglen", c("x")), code.gen=function(...) NULL
  )
)
names(VALID_FUNS) <- vapply(VALID_FUNS, "[[", "", "name")

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
    sprintf("void run(%s) {", toString(R.ARGS.ALL[args])),
    paste0("  ", code.calls[nzchar(code.calls)]),
    "}"
  )
  x[['code-text']] <- structure(code.txt, class="code_text")
  x
}

pp_internal <- function(call, depth, x, argn="", env) {
  writeLines(sprintf("Depth %d: %s", depth, deparse1(call)))
  if(is.call(call) && identical(as.character(call[[1L]]), "(")) {
    # Remove any parentheses calls
    if(length(call) != 2L)
      stop("Internal Error: call to `(` with wrong parameter count.")  # nocov
    call <- call[[2L]]
  }
  if(is.call(call)) {
    # - Recursion on Params ----------------------------------------------------
    fun <- call[[1L]]
    if(!is.name(fun) && !is.character(fun))
      stop(
        "Only calls in form `symbol(<parameters>)` are supported (i.e. not ",
        deparse1(fun), ")."
      )
    func <- as.character(fun)
    if(!func %in% names(VALID_FUNS))
      stop("`", func, "` is not a supported function.")

    # Classify Params
    args <- if(!is.null(defn <- VALID_FUNS[[c(func, "defn")]])) {
      match_call(definition=defn, call=call, envir=env, name=func)
    } else {
      as.list(callm[-1L])
    }
    args.ctrl <- names(args) %in% VALID_FUNS[[c(func, "ctrl")]]

    for(i in seq_along(args)) {
      if(args.ctrl[i]) {
        x <- record_call_dat(
          x, call=call, depth=depth, argn=names(args)[i],
          type="control", code=code_blank()
        )
      } else {
        x <- pp_internal(
          call=args[[i]], depth=depth + 1L, x=x, argn=names(args)[i], env=env
    ) } }
    type <- "call"
    # Generate Code
    code <- VALID_FUNS[[c(func, "code.gen")]](
      func, args[!args.ctrl], args[args.ctrl]
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

