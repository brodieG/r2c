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
#' @include code-bin.R
#' @include code-pow.R
#' @include code-logical.R
#' @include code-unary.R

NULL

is.valid_arglen <- function(type)
  (is.character(type[[2L]]) || is.integer(type[[2L]])) &&
  length(type[[2L]]) == 1L &&
  !is.na(type[[2L]]) &&
  (length(type) <= 2L || is.function(type[[3L]]))

is.valid_n_arglen <- function(type)
  length(type) == 2L &&
  (is.character(type[[2L]]) || is.integer(type[[2L]])) &&
  !anyNA(type[[2L]])

is.valid_vecrec <- function(type)
  is.valid_n_arglen(type) && type[[1L]] == "vecrec"

is.valid_eqlen <- function(type)
  is.valid_n_arglen(type) && type[[1L]] == "eqlen"

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
#'   for [`match.call`]'s `definition` parameter.  Also can be NULL for
#'   primitives that only do positional matching.
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
#'   * vecrec: character(n) (or integer(n)) the names (or indices in the matched
#'     call) of the arguments to use to compute result size under assumption of
#'     recycling to longest, or zero if any argument is zero length.
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
#' @param res.type one of "double", "integer", "logical", or "preserve.int", the
#'   latter equivalent to integer if all stack inputs are "integer" or
#'   "logical", double otherwise.
#'
#' @return a list containing the above information after validating it.

cgen <- function(
  name, fun=get(name, baseenv(), mode="function"),
  defn=if(typeof(fun) == 'closure') fun,
  ctrl.params=character(), flag.params=character(),
  type, code.gen, ctrl.validate=function(...) 0L, transform=identity,
  res.type="double"
) {
  vetr(
    name=CHR.1,
    fun=is.function(.),
    # really should have put some parens to resolve ambiguity below
    defn=typeof(.) == "closure" || NULL,
    ctrl.params=identity(
      is.null(defn) || all(. %in% names(formals(defn))) && !"..." %in% .
    ),
    flag.params=identity(
      is.null(defn) ||
      all(. %in% names(formals(defn))) && !"..." %in% . && length(.) < 32L
    ),
    type=list() && length(.) %in% 1:3,
    code.gen=is.function(.),
    ctrl.validate=is.function(.),
    transform=is.function(.),
    res.type=CHR.1 && . %in% c('logical', 'double', 'numeric', 'preserve.int')
  )
  if(length(intersect(ctrl.params, flag.params)))
    stop("Control and Flag parameters may not overlap.")
  # Limitation in vetr prevents this being done directly above
  stopifnot(
    is.character(type[[1L]]) && length(type[[1L]]) == 1L && !is.na(type[[1L]]),
    type[[1L]] %in% c("constant", "arglen", "vecrec", "eqlen"),
    (
      (type[[1L]] == "constant" && is.valid_constant(type)) ||
      (type[[1L]] == "arglen" && is.valid_arglen(type)) ||
      (type[[1L]] == "vecrec" && is.valid_vecrec(type)) ||
      (type[[1L]] == "eqlen" && is.valid_eqlen(type))
    ),
    # positional matching or match.call?
    type[[1L]] %in% c("constant") ||
    (
      type[[1L]] %in% c('arglen', 'vecrec', 'eqlen') && (
        (is.null(defn) && is.integer(type[[2L]])) ||
        (!is.null(defn) && is.character(type[[2L]]))
      )
    )
  )
  list(
    name=name, fun=fun, defn=defn, ctrl=ctrl.params, flag=flag.params,
    type=type, code.gen=code.gen, ctrl.validate=ctrl.validate,
    transform=transform, res.type=res.type
  )
}
## Specialized for binops
cgen_bin <- function(name, res.type="preserve.int") {
  cgen(
    name, defn=NULL,
    type=list("vecrec", 1:2), code.gen=code_gen_bin, res.type=res.type,
    transform=unary_transform
  )
}
## Specialized for binops that require the macros
cgen_bin2 <- function(name, res.type="preserve.int") {
  cgen(
    name, defn=NULL,
    type=list("vecrec", 1:2), code.gen=code_gen_bin2, res.type=res.type
  )
}

# Make sure "(" is not added to this list as it's pre-processed away.
VALID_FUNS <- c(
  # - Base Stats ---------------------------------------------------------------
  list(
    cgen(
      "sum", defn=function(..., na.rm=FALSE) NULL,
      flag.params="na.rm",
      type=list("constant", 1L),
      code.gen=code_gen_summary,
      ctrl.validate=ctrl_val_summary,
      res.type='preserve.int'
    ),
    cgen(
      "mean", defn=base::mean.default,
      flag.params="na.rm", ctrl.params="trim",
      type=list("constant", 1L),
      code.gen=code_gen_summary,
      ctrl.validate=ctrl_val_summary
    ),
    cgen("length", type=list("constant", 1L), code.gen=code_gen_length),
    cgen(
      "all", defn=function(..., na.rm=FALSE) NULL,
      flag.params="na.rm",
      type=list("constant", 1L),
      code.gen=code_gen_summary,
      ctrl.validate=ctrl_val_summary,
      res.type='logical'
    ),
    cgen(
      "any", defn=function(..., na.rm=FALSE) NULL,
      flag.params="na.rm",
      type=list("constant", 1L),
      code.gen=code_gen_summary,
      ctrl.validate=ctrl_val_summary,
      res.type='logical'
    )
  ),
  # - Vec Binops ---------------------------------------------------------------

  lapply(c("+", "-", "*", "/"), cgen_bin),
  list(
    cgen(
       # needs transform, could be folded into cgen_bin like e.g. uminus
       "^", type=list("vecrec", 1:2), code.gen=code_gen_pow,
       transform=pow_transform
  ) ),
  lapply(c(">", ">=", "<", "<=", "==", "!="), cgen_bin2, res.type="logical"),
  lapply(c("|", "&"), cgen_bin2, res.type='logical'),
  ## # Not implemented for now given not just a simple counterpart, but
  ## # could add a function like square to deal with it..  See myfmod in
  ## src/arithmetic.c in R sources
  ## cgen(
  ##   "%%", base::`%%`, defn=function(e1, e2) NULL,
  ##   type=list("vecrec", c("e1", "e2")), code.gen=code_gen_arith,
  ##   res.type='preserve.int'
  ## ),
  # - Unary Ops ----------------------------------------------------------------

  ## + and - are remapped to uplus and uminus via a transform by cgen_bin as
  ## we don't allow the same function name to have different sizing methods
  list(
    cgen(
      "uplus", fun=uplus, type=list("arglen", "x"), code.gen=code_gen_unary,
      res.type="preserve.int"
    ),
    cgen(
      "uminus", fun=uminus, type=list("arglen", "x"), code.gen=code_gen_unary,
      res.type="preserve.int"
    ),
    cgen(
      "!", type=list("arglen", 1L), code.gen=code_gen_unary,
      res.type="logical"
    )
  ),

  # - Other Logical ------------------------------------------------------------

  list(
    cgen(
      "&&", type=list("constant", 1L), code.gen=code_gen_lgl2,
      res.type="logical"
    ),
    cgen(
      "||", type=list("constant", 1L), code.gen=code_gen_lgl2,
      res.type="logical"
    ),
    cgen(
      "ifelse", type=list("arglen", "test"), code.gen=code_gen_ifelse,
      res.type="preserve.int" # not faithful to what R does
    )
  ),
  # - Assign / Control----------------------------------------------------------

  list(
    cgen("<-", type=list("arglen", 2L), code.gen=code_gen_assign),
    cgen("=", type=list("arglen", 2L), code.gen=code_gen_assign),
    cgen(
      "{", defn=function(...) NULL,
      # arglen of last argument matching dots
      type=list("arglen", "...", function(x) x[length(x)]),
      code.gen=code_gen_braces
    ),
    # result of this one is not used outside of the C code
    cgen(
      "if_test", type=list("constant", 1L), code.gen=code_gen_if_test,
      res.type="logical", fun=if_test
    ),
    cgen(
      "r2c_if", type=list("eqlen", c("true", "false")),
      code.gen=code_gen_r2c_if, fun=r2c_if
    ),
    cgen(
      "if_true", type=list("arglen", "expr"), code.gen=code_gen_if_true,
      fun=if_true
    ),
    cgen(
      "if_false", type=list("arglen", "expr"), code.gen=code_gen_if_false,
      fun=if_false
    ),
    # This one can't actually generate code and needs to be first decomposed
    # into the above if_test/r2c_if/if_true/if_false, but we need it here so the
    # early parsing passes recognize it as an allowed function.
    cgen("if", type=list("eqlen", 2:3), code.gen=code_gen_if)
  ),
  # - r2c funs -----------------------------------------------------------------
  list(
    cgen(
      "mean1", fun=mean1,
      flag.params="na.rm",
      type=list("constant", 1L),
      code.gen=code_gen_summary,
      ctrl.validate=ctrl_val_summary
    ),
    cgen(
      "square", fun=square, defn=square,
      type=list("arglen", "x"),
      code.gen=code_gen_square
    ),
    cgen(
      "vcopy", fun=vcopy, defn=NULL,
      type=list("arglen", 1L),
      code.gen=code_gen_copy,
      res.type="preserve.int"   # for uplus
    )
  )
)
names(VALID_FUNS) <- vapply(VALID_FUNS, "[[", "", "name")
# even though we allow ::, we don't allow duplicate function names for
# simplicity.
stopifnot(
  !anyDuplicated(names(VALID_FUNS)),
  identical(sort(names(VALID_FUNS)), sort(names(FUN.NAMES)))
)

code_blank <- function()
  list(
    defn="", name="", call="", narg=FALSE, flag=FALSE, ctrl=FALSE,
    headers=character(), defines=character(), out.ctrl=CGEN.OUT.NONE
  )
code_valid <- function(code, call) {
  isTRUE(check <- vet(CHR.1, code[['defn']])) &&
    isTRUE(check <- vet(CHR.1, code[['name']])) &&
    isTRUE(check <- vet(CHR.1, code[['call']])) &&
    isTRUE(check <- vet(CHR || NULL, code[['headers']])) &&
    isTRUE(check <- vet(CHR || NULL, code[['defines']])) &&
    isTRUE(
      check <- vet(INT.1 && all_bw(., 0, CGEN.OUT.DFLT), code[['out.ctrl']])
    )
  if(!isTRUE(check))
    stop("Generated code format invalid for `", deparse1(call), "`:\n", check)

  TRUE
}
# Check whether a call is in valid format
#
# We allow pkg::fun for a select set of packages that r2c implements functions
# from.  Duplicate `fun` across packages is assumed impossible (just b/c we have
# not implemented such, and the assumption simplifies things).

call_valid <- function(call) {
  fun <- call[[1L]]
  if(is.call(fun)) {
    if(is.dbl_colon_call(fun) && as.character(fun[[2L]]) %in% VALID.PKG) {
      fun <- as.character(fun[[3L]])
    } else {
      stop("`", deparse1(call[[1L]]), "` is not a supported function.")
    }
  }
  if(!is.chr_or_sym(fun))
    stop(
      "only calls in form `symbol(<parameters>)` are supported (i.e. not ",
      deparse1(call), ")."
    )
  func <- as.character(fun)
  if(!func %in% names(VALID_FUNS))
    stop("`", deparse1(call[[1L]]), "` is not a supported function.")
  func
}
#' Default C Call Generation
#'
#' Calls outside of controls follow a clear pattern that this function
#' implements.  See `code_res` for parameter description.
#'
#' @noRd

c_call_gen <- function(name, narg, flag, ctrl) {
  sprintf(
    "%s(%s%s%s%s);",
    name,
    toString(CALL.BASE),
    if(narg) paste0(", ", CALL.VAR) else "",
    if(flag) paste0(", ", CALL.FLAG) else "",
    if(ctrl) paste0(", ", CALL.CTRL) else ""
  )
}
#' Organize C Code Generation Output
#'
#' There are three types of C output:
#'
#' * Call to the C function
#' * Definition of C function
#' * Deparsing of R function as comment for context
#'
#' Different R level calls require outputting different mixes of the above,
#' where the default standard call like `mean(x)` will output all three.
#' Exceptions include so called NO-OP calls that don't actually require a C
#' function call such as assignments or braces.  These don't compute anything
#' directly.  Additionally, control functions don't map R calls directly to C
#' calls, but still use proxy R calls as stand-ins for e.g. the braces, the
#' `else`, etc.  So those have an associated output that is "executed" at
#' run-time, but e.g. don't have corresponding definitions or deparsed R
#' commentary.
#'
#' Every R call, including proxy R calls, maintains a spot in the data
#' indexing, flag, and control arrays, even if they are not run at the C level.
#' This simplifies the logic of the allocation code.  Additionally, every R call
#' must generate a C function definition and a call, irrespective of whether the
#' definition and/or call are emitted to the final C output file.  This
#' requirement is due to some sanity checks that preceded the possibility of
#' calls/definitions that might not get emitted.
#'
#' @noRd
#' @seealso `cgen` for the actual C code generation, `preprocess` for the
#'   assembly into the final C file.
#' @param narg TRUE if function has variable number of arguments
#' @param flag TRUE if function has flag parameters
#' @param ctrl TRUE if function has control parameters
#' @param headers character vector with header names that need to be #included
#' @param defines character with #define directives
#' @param out.ctrl scalar integer sum of various `CGEN.OUT.*` constants (see
#'   constants.R) that control which parts of the C output are generated and
#'   how.  The actual final C file is produced by `preprocess` (consulting these
#'   values).
#' @param c.call.gen function to generate the C call.  Primarily used to allow
#'   generation of control flow code that follows different patterns than all
#'   the others, including cases where there is no function call at all, only
#'   e.g. braces or an else statement.

code_res <- function(
  defn, name, narg=FALSE, flag=FALSE, ctrl=FALSE,
  headers=character(), defines=character(), out.ctrl=CGEN.OUT.DFLT,
  c.call.gen=c_call_gen
) {
  if(is.na(name)) stop("Internal Error: mismapped function name.")
  if(
    bitwAnd(out.ctrl, CGEN.OUT.MUTE) && !name %in% FUN.NAMES[PASSIVE.SYM]
  )
    stop("Internal Error: cannot mute ", name, ", as not in PASSIVE.SYM")

  c_call <- paste0(
    c.call.gen(name, narg=narg, flag=flag, ctrl=ctrl), collapse="\n"
  )
  list(
    defn=defn, name=name,
    call=c_call,
    headers=if(is.null(headers)) character() else headers,
    defines=if(is.null(defines)) character() else defines,
    narg=narg, flag=flag, ctrl=ctrl, out.ctrl=out.ctrl
  )
}
