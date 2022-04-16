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

## `match.call` but with default arguments
##
## Also, result is just the arguments, not the original call.

match_call <- function(definition, call, envir, name) {
  frm <- formals(definition)
  frm.req <-
    vapply(frm, function(x) is.name(x) && !nzchar(as.character(x)), TRUE)
  frm.bad <- vapply(frm[!frm.req], function(x) is.language(x), TRUE)
  if(any(frm.bad))
    stop("Functions with non-constant defaults are unsupported.")

  # Match, check call
  call[[1L]] <- as.character(call[[1L]]) # hack for e.g. `+`
  mcall <- match.call(definition=definition, call=call, envir=envir)
  mcall.nm <- names(mcall)[-1L]

  frms.req <- names(frm)[frm.req]
  frms.req <- frms.req[frms.req != "..."]
  if(!all(frms.req.have <- frms.req %in% mcall.nm))
    stop(
      "Missing required formals ",
      toString(sprintf("`%s`", frms.req[!frms.req.have])),
      " for `", name, "`."
    )
  # Add defaults, not worrying about order (should be okay)
  mcall.nm.missing <- names(frm)[!names(frm) %in% c(mcall.nm, "...")]
  c(as.list(mcall[-1L]), frm[mcall.nm.missing])
}

#' Initializer for Function Registration Entries
#'
#' @param name character(1L) symbol that will reference the function
#' @param fun the function we're trying to emulate
#' @param defn NULL if fun is a closure, otherwise a function template to use
#'   for [`match.call`]'s `definition` paramter.
#' @param ctrl.params character names of all the formal parameters that are
#'   to be evaluated once up front and not for each group in the data.  If any
#'   data columns are matched to these parameters, the entire data column will
#'   be used for them.
#' @param type list(2L) containing the type of function in "constant", "arglen",
#'   or "vecrec" at position one, and additional meta data at position two
#'   described next:
#'   * constant: a positive non-NA integer indicating the constant result size
#'     (e.g. 1L for `mean`)
#'   * arglen: character(1L) the name of the argument to use the length of as
#'     the result size (e.g. `probs` for [`quantile`].
#'   * vecrec: character(n) the names of the arguments to use to compute result
#'     size under assumption of recycling to longest, or zero if any argument is
#'     zero length.
#' @return a list containing the above information after validating it.

fap_fun <- function(name, fun, defn, ctrl.params=character(), type) {
  vetr(
    name=CHR.1,
    fun=is.function(.),
    defn=typeof(fun) == "closure" && NULL || is.function(.),
    ctrl.params=
      character() && !anyNA(.) && all(. %in% names(formals(defn))) &&
      !"..." %in% .,
    type=list(NULL, NULL)
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
  list(name=name, defn=defn, ctrl=ctrl.params, type=type)
}
VALID_FUNS <- list(
  fap_fun(
    "sum", base::sum, function(..., na.rm=FALSE) NULL,
    "na.rm", list("constant", 1L)
  ),
  fap_fun(
    "mean", fun=base::sum, defn=base::mean.default,
    "na.rm", list("constant", 1L)
  ),
  fap_fun(
    "+", base::`+`, defn=function(e1, e2) NULL,
    type=list("vecrec", c("e1", "e2"))
  ),
  fap_fun(
    "-", base::`-`, defn=function(e1, e2) NULL,
    type=list("vecrec", c("e1", "e2"))
  ),
  fap_fun(
    "*", base::`*`, defn=function(e1, e2) NULL,
    type=list("vecrec", c("e1", "e2"))
  ),
  fap_fun(
    "/", base::`/`, defn=function(e1, e2) NULL,
    type=list("vecrec", c("e1", "e2"))
  ),
  # this is not "really" a fun, but we shoehorn it into our model here, we could
  # also add code to skip it
  fap_fun(
    "(", base::`(`, defn=function(x) NULL,
    type=list("arglen", c("x"))
  )
)
names(VALID_FUNS) <- vapply(VALID_FUNS, "[[", "", "name")

#' Prepare Calls for Evaluation
#'
#' Retrieve appropriate entry points based on configuration parameters, and
#' compute required temporary storage sizes.
#'
#' @return a list containing the accrued temporary storage requirements, as well
#'   as a recursive list of entry point names along with the parameters to call
#'   them with and the expected result size.
#' the entry point name, the parameters to call it
#'   with (mixture of eval and uneval)
#' @param call an unevaluated R call
#' @param data character the names of the data columns
#' @param g max group size
#' @param env calling frame to evaluate expression in
#' @param depth how deep into the expression we've recursed
#' @param temp structure that tracks the required size

prepare <- function(call, data, g, env, depth, temp) {
  fun <- call[[1L]]
  if(!is.name(fun))
    stop(
      "Only calls in form `symbol(<parameters>)` are supported (i.e. not ",
      deparse1(fun), ")."
    )

  func <- as.character(fun)
  if(!func %in% names(VALID_FUNS))
    stop("`", as.character(call[[1L]]), "` is not a supported function.")

  args <- if(!is.null(defn <- VALID_FUNS[[c(func, "defn")]])) {
    match_call(definition=defn, call=call, envir=env, name=func)
  } else {
    as.list(callm[-1L])
  }
  # Identify arguments that must be evaluated, which are the control parameters
  # as well as any naked symbols (i.e. not expressions) in the parameter list
  # that reference objects not in the data.
  args.ctrl <- names(args) %in% VALID_FUNS[[c(func, "ctrl")]]
  args.other <- !names(args) %in% VALID_FUNS[[c(func, "ctrl")]]
  if(!all(nzchar(names(data)))) stop("All data must be named.")
  args.sym <- vapply(args, is.symbol, TRUE)
  args.sym.dat <- logical(length(args))
  args.sym.dat[
    seq_along(args.sym)[args.sym][as.character(args[args.sym]) %in% names(data)]
  ] <- TRUE
  args.ctrl.eval <- lapply(args[args.ctrl], eval, envir=data, enclos=env)
  args.not.dat <- args.sym & !args.ctrl & !args.sym.dat
  args.not.dat.eval <- lapply(args[args.not.dat], eval, envir=data, enclos=env)

  # Compute size of non-evaluated arguments. We do not need to worry
  # about the evaluated ones as those are in the R heap.
  args.comp <- !args.not.dat & !args.ctrl
  args.comp.w <- which(args.comp)

  # max size possible, needs to hold R_xlen_t, assume numeric does this.  First
  # row of the matrix designates the minimum length of the result, second
  # whether the group size affects the size of the result.
  args.sizes <- matrix(numeric(length(args) * 2), nrow=2)
  colnames(args.sizes) <- names(args)
  args.comp.sizes <- matrix(numeric(sum(args.comp) * 2), 2)
  for(i in seq_along(args.comp.w)) {
    ii <- args.comp.w[i]
    # Symbol bound to group data, known size, no need to allocate
    args.comp.sizes[,i] <- if(ii %in% which(args.sym.dat)) {
      # Means no constant minimum size, derive size from group
      c(NA_real_, 1)
    } else if(!is.symbol(args[[ii]]) && is.language(args[[ii]])) {
      # Recurse
      prep.sub <- prepare(
        call=args[[ii]], data=data, g=g, env=env, depth=depth + 1L, temp=temp
      )
      temp <- prep.sub[['temp']]
      prep.sub[['size']]
    } else if (is.symbol(args[[ii]])) {
      # Should either be a data symbol or should have been evaluated
      stop("Internal Error: Unexpected un-evaluated token.")
    } else {
      c(length(args[[ii]]), 0)
    }
  }
  # Record the lengths of all the different types of argument
  args.sizes[,args.comp.w] <- args.comp.sizes
  args.sizes[,args.ctrl] <- rbind(as.numeric(lengths(args.ctrl.eval)), 0)
  args.sizes[,args.not.dat] <- rbind(as.numeric(lengths(args.not.dat.eval)), 0)

  # Based on the function type and recorded lengths, compute the result length.
  # We distinguish size used for allocation (maximum possible size), vs size in
  # `size` (accounting for actual group size)
  ftype <- VALID_FUNS[[c(func, "type")]]
  if(ftype[[1L]] == "constant") {
    # Always constant size, e.g. 1 for `sum`
    size <- c(ftype[[2L]], 0)
    temp <- alloc_temp(temp, depth, size=size[1])
  } else if(ftype[[1L]] %in% c("arglen", "vecrec")) {
    # Length of a specific argument, like `probs` for `quantile`
    if(!all(ftype[[2L]] %in% colnames(args.sizes)))
      stop(
        "Parameter(s) ",
        deparse1(ftype[[2L]][!ftype[[2L]]%in% colnames(args.sizes)]),
        " missing but required for sizing."
      )
    sizes.tmp <- args.sizes[,ftype[[2L]], drop=FALSE]
    temp <- alloc_temp(temp, depth, size=max_size(sizes.tmp, g))
    size <- c(known_size(sizes.tmp[1L,]), max(sizes.tmp[2L,]))
  } else stop("Internal Error: unknown function type.")

  # Return temp allocation plus current size
  list(temp=temp, size=size)
}
## Compute Max Possible Size
##
## This is affected by maximum group size as well as any non-group parameters.

known_size <- function(x) {
  tmp <- x[!is.na(x)]
  if(!length(tmp)) NA_real
  else if(any(tmp == 0)) 0
  else max(tmp)
}
max_size <- function(x, g) {
  if(
    !is.numeric(x) || !is.matrix(x) || ncol(x) != 2 ||
    any(is.na(x[1L,] & !x[2L,]))
  )
    stop("Internal error, malformed size data.")

  size <- x[1L,]
  size[is.na(size)] <- g
  if(any(size == 0)) 0 else max(size)
}


#' Track Required Allocations for Intermediate vectors
#'
#' List of all allocated temporary vector sizes, and which of those are free.

alloc_temp <- function(temp, depth, size) {
  if(depth == .Machine$integer.max)
    stop("Expression max depth exceeded for alloc.") # exceedingly unlikely
  temp[['alloc']][temp[['depth']] > depth + 1L] <- NA_real_
  fit <- which(is.na(temp[['alloc']]))
  temp <- if(!length(fit)) {
    temp[['alloc']] <- c(temp[['alloc']], size)
    temp[['depth']] <- c(temp[['depth']], depth)
    o <- order(temp[['alloc']], decreasing=TRUE)
    list(alloc=temp[['alloc']][o], depth=temp[['depth']][o])
  } else {
    temp[['depth']][max(fit)] <- depth
    temp
  }
  temp
}
init_temp <- function() list(alloc=numeric(), depth=integer())

