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

#' @include code-assign-control.R
#' @include code-summary.R
#' @include code-arith.R
#' @include code-pow.R

NULL


is.valid_arglen <- function(type)
  is.character(type[[2L]]) &&
  length(type[[2L]]) == 1L &&
  !is.na(type[[2L]]) &&
  (length(type) <= 2L || is.function(type[[3L]]))

is.valid_vecrec <- function(type)
  type[[1L]] == "vecrec" &&
  is.character(type[[2L]]) &&
  !anyNA(type[[2L]])

is.valid_constant <- function(type)
  is.integer(type[[2L]]) &&
  length(type[[2L]]) == 1L &&
  !is.na(type[[2L]]) &&
  type[[2L]] >= 0L

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
#' Right now one must manually verify that the definition is consistent with the
#' call format.  The call format is generated with `code_res`, but unfortunately
#' the actual function definition needs to be manually coded to match what
#' `code_res` does.  `code_gen_summary` is a good one to look at for
#' inspiration.
#'
#' Default parameters that every C function should have are in `ARGS.NM.BASE`,
#' and include:
#'
#' * data: an array of pointers to double, which includes every every allocation
#'   that exists at any point in time in the process, including those required
#'   to support the inputs and the output of the C function.
#' * lens: an array of `R_xlen_t` values, each one representing how many items
#'   the corresponding array of doubles in `data` has.
#' * di: an array of integers that represents, in order, the indices of the data
#'   parameters (i.e. not control or flag) of the function, in `data`.  So for
#'   example, `data[di[0]]` returns a pointer to the data backing the first data
#'   parameter for the function.  If a function takes `n` args, then
#'   `data[di[n]]` points to where the result of the function should be written
#'   to.
#'
#' Generally a C function with `n` arguments is supposed to compute on the data
#' in `data[di[0:(n-1)]]` and record the result into `data[di[n]]` and the
#' length of the result into `lens[di[n]]` (although the latter in theory should
#' be known ahead of time - maybe this allows a check?).
#'
#' For functions with variable arguments (e.g. because they have `...` in their
#' signature), be sure to include `F.ARGS.VAR` in the definition, and to use
#' `narg=TRUE` for `code_res` (see "code-summary.R" which handles both the case
#' with a single parameter, and many parameters).
#'
#' @noRd
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
#' @param type list(2:3) containing the type of function in "constant", "arglen",
#'   or "vecrec" at position one, and additional meta data at position two or
#'   three that can be depending on the value in position one:
#'
#'   * constant: a positive non-NA integer indicating the constant result size
#'     (e.g. 1L for `mean`)
#'   * arglen: character(1L) the name of the argument to use the length of as
#'     the result size (e.g. `probs` for [`quantile`]), also allows specifying a
#'     function at position 3 to e.g. pick which of multiple arguments matching
#'     `...` to use for the length.
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
  type, code.gen, ctrl.validate=function(...) 0L, transform=identity,
  preserve.int=FALSE
) {
  vetr(
    name=CHR.1,
    fun=is.function(.),
    # really should have put some parens to resolve ambiguity below
    defn=typeof(fun) == "closure" && NULL || is.function(.),
    ctrl.params=
      character() && !anyNA(.) && all(. %in% names(formals(defn))) &&
      !"..." %in% .,
    flag.params=
      character() && !anyNA(.) && all(. %in% names(formals(defn))) &&
      !"..." %in% . && length(.) < 32L,
    type=list() && length(.) %in% 2:3,
    code.gen=is.function(.),
    ctrl.validate=is.function(.),
    transform=is.function(.),
    preserve.int=LGL.1
  )
  if(length(intersect(ctrl.params, flag.params)))
    stop("Control and Flag parameters may not overlap.")
  # Limitation in vetr prevents this being done directly above
  stopifnot(
    is.character(type[[1L]]) && length(type[[1L]]) == 1L && !is.na(type[[1L]]),
    type[[1L]] %in% c("constant", "arglen", "vecrec"),
    (
      (type[[1L]] == "constant" && is.valid_constant(type)) ||
      (type[[1L]] == "arglen" && is.valid_arglen(type)) ||
      (type[[1L]] == "vecrec" && is.valid_vecrec(type))
  ) )
  list(
    name=name, fun=fun, defn=defn, ctrl=ctrl.params, flag=flag.params,
    type=type, code.gen=code.gen, ctrl.validate=ctrl.validate,
    transform=transform, preserve.int=preserve.int
  )
}
# Make sure "(" is not added to this list as it's pre-processed away.
VALID_FUNS <- list(
  # - Base Funs ----------------------------------------------------------------
  fap_fun(
    "sum", base::sum, function(..., na.rm=FALSE) NULL,
    flag.params="na.rm",
    type=list("constant", 1L),
    code.gen=code_gen_summary,
    ctrl.validate=ctrl_val_summary,
    preserve.int=TRUE
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
    "length", base::length, defn=function(x) NULL,
    type=list("constant", 1L), code.gen=code_gen_length
  ),
  fap_fun(
    "+", base::`+`, defn=function(e1, e2) NULL,
    type=list("vecrec", c("e1", "e2")), code.gen=code_gen_arith,
    preserve.int=TRUE
  ),
  fap_fun(
    "-", base::`-`, defn=function(e1, e2) NULL,
    type=list("vecrec", c("e1", "e2")), code.gen=code_gen_arith,
    preserve.int=TRUE
  ),
  fap_fun(
    "*", base::`*`, defn=function(e1, e2) NULL,
    type=list("vecrec", c("e1", "e2")), code.gen=code_gen_arith,
    preserve.int=TRUE
  ),
  fap_fun(
    "/", base::`/`, defn=function(e1, e2) NULL,
    type=list("vecrec", c("e1", "e2")), code.gen=code_gen_arith
  ),
  ## # Not implemented for now given not just a simple counterpart, but
  ## # could add a function like square to deal with it..
  ## fap_fun(
  ##   "%%", base::`%%`, defn=function(e1, e2) NULL,
  ##   type=list("vecrec", c("e1", "e2")), code.gen=code_gen_arith,
  ##   preserve.int=TRUE
  ## ),
  fap_fun(
    "^", base::`^`, defn=function(e1, e2) NULL,
    type=list("vecrec", c("e1", "e2")), code.gen=code_gen_pow,
    transform=pow_transform
  ),
  # - Assign / Control----------------------------------------------------------

  fap_fun(
    "<-", fun=base::`<-`, defn=function(x, value) NULL,
    type=list("arglen", "value"),
    code.gen=code_gen_assign
  ),
  fap_fun(
    "=", fun=base::`=`, defn=function(x, value) NULL,
    type=list("arglen", "value"),
    code.gen=code_gen_assign
  ),
  fap_fun(
    "{", fun=base::"{", defn=function(...) NULL,
    # arglen of last argument matching dots
    type=list("arglen", "...", function(x) x[length(x)]),
    code.gen=code_gen_braces
  ),

  # - r2c funs -----------------------------------------------------------------
  fap_fun(
    "mean1", fun=mean1, defn=mean1,
    flag.params="na.rm",
    type=list("constant", 1L),
    code.gen=code_gen_summary,
    ctrl.validate=ctrl_val_summary
  ),
  fap_fun(
    "square", fun=square, defn=square,
    type=list("arglen", "x"),
    code.gen=code_gen_square
  ),
  fap_fun(
    "r2c_copy", fun=r2c_copy, defn=r2c_copy,
    type=list("arglen", "x"),
    code.gen=code_gen_copy
  )
)
names(VALID_FUNS) <- vapply(VALID_FUNS, "[[", "", "name")

code_blank <- function()
  list(
    defn="", name="", call="", narg=FALSE, flag=FALSE, ctrl=FALSE,
    headers=character()
  )
code_valid <- function(code, call) {
  isTRUE(check <- vet(CHR.1, code[['defn']])) &&
    isTRUE(check <- vet(CHR.1, code[['name']])) &&
    isTRUE(check <- vet(CHR.1, code[['call']])) &&
    isTRUE(check <- vet(CHR, code[['headers']]))
  if(!isTRUE(check))
    stop("Generated code format invalid for `", deparse1(call), "`:\n", check)

  TRUE
}
call_valid <- function(call) {
  fun <- call[[1L]]
  if(!is.name(fun) && !is.character(fun))
    stop(
      "only calls in form `symbol(<parameters>)` are supported (i.e. not ",
      deparse1(call), ")."
    )
  func <- as.character(fun)
  if(!func %in% names(VALID_FUNS))
    stop("`", func, "` is not a supported function.")
  func
}
#' Organize Code Generation Output
#'
#' Additionally, generates the call to the function.
#'
#' @noRd
#' @param narg TRUE if function has variable number of arguments
#' @param flag TRUE if function has flag parameters
#' @param ctrl TRUE if function has control parameters

code_res <- function(
  defn, name, narg=FALSE, flag=FALSE, ctrl=FALSE, headers=character()
) {
  call <- sprintf(
    "%s(%s%s%s%s);",
    name,
    toString(CALL.BASE),
    if(narg) paste0(", ", CALL.VAR) else "",
    if(flag) paste0(", ", CALL.FLAG) else "",
    if(ctrl) paste0(", ", CALL.CTRL) else ""
  )
  list(
    defn=defn, name=name, call=call, headers=headers,
    narg=narg, flag=flag, ctrl=ctrl
  )
}
