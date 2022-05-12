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

#' @include code-summary.R
#' @include code-arith.R
#' @include code-pow.R

NULL

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
  type, code.gen, ctrl.validate=function(...) 0L, transform=identity
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
    ctrl.validate=is.function(.),
    transform=is.function(.)
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
    type=type, code.gen=code.gen, ctrl.validate=ctrl.validate,
    transform=transform
  )
}
# Make sure "(" is not added to this list.
VALID_FUNS <- list(
  fap_fun(
    "sum", base::sum, function(..., na.rm=FALSE) NULL,
    flag.params="na.rm",
    type=list("constant", 1L),
    code.gen=code_gen_summary,
    ctrl.validate=ctrl_val_summary
  ),
  fap_fun(
    "mean0", fun=mean0, defn=mean0,
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
  fap_fun(
    "%%", base::`%%`, defn=function(e1, e2) NULL,
    type=list("vecrec", c("e1", "e2")), code.gen=code_gen_arith
  ),
  fap_fun(
    "^", base::`^`, defn=function(e1, e2) NULL,
    type=list("vecrec", c("e1", "e2")), code.gen=code_gen_pow,
    transform=pow_transform
  )
)
names(VALID_FUNS) <- vapply(VALID_FUNS, "[[", "", "name")

#' Code Generation Functions
#'
#' * Must accept one of four possible signatures.
#' * Must set result by reference.
#' * Must set result size by reference.

code_gen <- function(dat) {

}
code_blank <- function() list(
  defn="", name="", call="", args=character(), headers=character()
)
code_valid <- function(code, call) {
  isTRUE(check <- vet(CHR.1, code$defn)) &&
    isTRUE(check <- vet(CHR.1, code$name)) &&
    isTRUE(check <- vet(CHR.1, code$call)) &&
    isTRUE(check <- vet(CHR, code$args)) &&
    isTRUE(check <- vet(CHR, code$headers))
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


# To make sure we use the same structure everywhere.

code_res <- function(defn, name, args, headers=character()) {
  call <- sprintf("%s(%s);", name, toString(CALL.ALL[match(args, ARGS.NM.ALL)]))
  list(defn=defn, name=name, call=call, args=args, headers=headers)
}
