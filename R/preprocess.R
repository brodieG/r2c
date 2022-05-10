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
  mcall.dots <- mcall[['...']]
  if(length(mcall.dots)) names(mcall.dots) <- rep('...', length(mcall.dots))
  c(
    as.list(mcall[seq_along(mcall) != 1L & names(mcall) != "..."]),
    mcall.dots,
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
#'   will be used for them, not the group varying subsets of them.  Any
#'   parameters here are exclusive of those listed in `flag.params`.
#' @param flag.params character names as for `ctrl.params`, except this is
#'   specifically for parameters that evaluated to TRUE or FALSE, so that they
#'   may be conveyed to the function without the need to use `VECTOR_ELT`, etc.,
#'   to access the specific control parameter.  Any parameters here are
#'   exclusive of those listed in `ctrl.params`.
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
#' @param ctrl.validate a function to validate both control and flag parameters,
#'   should `stop`, or return the flag parameters encoded into an integer.
#'
#' @return a list containing the above information after validating it.

fap_fun <- function(
  name, fun, defn, ctrl.params=character(), flag.params=character(),
  type, code.gen, ctrl.validate=function(...) 0L
) {
  vetr(
    name=CHR.1,
    fun=is.function(.),
    defn=typeof(fun) == "closure" && NULL || is.function(.),
    ctrl.params=
      character() && !anyNA(.) && all(. %in% names(formals(defn))) &&
      !"..." %in% .,
    flag.params=
      character() && !anyNA(.) && all(. %in% names(formals(defn))) &&
      !"..." %in% . && length(.) < 32L,
    type=list(NULL, NULL),
    code.gen=is.function(.),
    ctrl.validate=is.function(.)
  )
  if(length(intersect(ctrl.params, flag.params)))
    stop("Control and Flag parameters may not overlap.")
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
    name=name, fun=fun, defn=defn, ctrl=ctrl.params, flag=flag.params,
    type=type, code.gen=code.gen, ctrl.validate=ctrl.validate
  )
}
VALID_FUNS <- list(
  fap_fun(
    "sum", base::sum, function(..., na.rm=FALSE) NULL,
    flag.params="na.rm",
    type=list("constant", 1L),
    code.gen=code_gen_summary,
    ctrl.validate=ctrl_val_summary
  ),
  fap_fun(
    "mean", fun=base::mean, defn=base::mean.default,
    flag.params="na.rm",
    ctrl.params="trim",
    type=list("constant", 1L),
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
    sprintf("void run(%s) {", toString(R.ARGS.ALL[match(args, ARGS.NM.ALL)])),
    paste0(
      "  ",
      c("int v=0;", code.calls[nzchar(code.calls)])
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
