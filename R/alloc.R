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
  code.gen
) {
  vetr(
    name=CHR.1,
    fun=is.function(.),
    defn=typeof(fun) == "closure" && NULL || is.function(.),
    ctrl.params=
      character() && !anyNA(.) && all(. %in% names(formals(defn))) &&
      !"..." %in% .,
    type=list(NULL, NULL),
    code.gen=is.function(.)
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
    "na.rm", list("constant", 1L), code.gen=code_gen_summary
  ),
  fap_fun(
    "mean", fun=base::mean, defn=base::mean.default,
    c("trim", "na.rm"), list("constant", 1L), code.gen=code_gen_summary
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

#' Prepare Calls for Evaluation
#'
#' Retrieve appropriate entry points based on configuration parameters, and
#' compute required temporary storage sizes.
#'
#' @param call an unevaluated R call
#' @param data character the names of the data columns
#' @param gmax max group size
#' @param env calling frame to evaluate expression in
#' @param depth how deep into the expression we've recursed
#' @param alloc.dat structure that tracks the required size
#' @param code a list of generated code data, one for each sub-call.
#' @param datai a list of integer vectors that represent indices into
#'   `alloc.dat`, one for each sub-call.
#' @param ctrl a list of lists, each sub-list a set of R objects to pass on as
#'   the control parameters, one for each sub-call.
#' @return a list containing (update):
#'   * Accrued temporary storage requirements.
#'   * Size of the current call being assessed.
#'   * A linear list of generated code and associated calls.

preprocess <- function(call, data, gmax, env) {
  if(!all(nzchar(names(data)))) stop("All data must be named.")
  depth <- 0
  force(env)

  # Add group data.
  data.naked <- data[is.num_naked(data)]
  alloc <- append_dat(
    init_dat(), new=data.naked, sizes=rep(gmax, length(data.naked)),
    depth=0L, type="grp"
  )
  # Put in a dummy for the result, to be alloc'ed once we know group sizes.
  alloc <- append_dat(alloc, new=list(numeric()), sizes=0L, depth=0L, type="res")

  # All the data generated goes into x
  x <- list(alloc=alloc, code=list(), argi=list(), ctrl=list(), size=0, id=0)
  x <- pp_internal(call=call, data=data, gmax=gmax, env=env, depth=0L, x=x)

  # Deduplicate the code and generate the final C file (need headers).
  headers <- paste(
    "#include", unique(unlist(lapply(x[['code']], "[[", "headers")))
  )
  codes <- vapply(x[['code']], "[[", "", "defn")
  names <- vapply(x[['code']], "[[", "", "name")
  codes.m <- match(codes, unique(codes))
  names.m <- match(names, unique(names))
  if(!identical(codes.m, names.m))
    stop("Internal error: functions redefind with changing definitions.")

  args.u <- unique(unlist(lapply(x[['code']], "[[", "args")))
  args <- args.u[order(match(args.u, c(ARGS.ALL)))]

  code.txt <- c(
    headers,
    # Function Definitions
    unique(codes),
    # Calls
    "",
    sprintf("void run(%s) {", toString(args)),
    paste0("  ", vapply(x[['code']], "[[", "", "call")),
    "}"
  )
  x[['code-text']] <- code.txt
  x
}

pp_internal <- function(call, data, gmax, env, depth, x) {
  # - Validate -----------------------------------------------------------------

  fun <- call[[1L]]
  if(!is.name(fun) && !is.character(fun))
    stop(
      "Only calls in form `symbol(<parameters>)` are supported (i.e. not ",
      deparse1(fun), ")."
    )
  # Remove parentheses.  These are not needed in an already parsed expression as
  # the precedence is dictated by the call tree.
  func <- as.character(fun)
  check_fun(func, env)
  if(func == "(") {
    if(length(call) != 2L)
      stop("Internal Error: call to `(` with wrong parameter count.")  # nocov
    call <- call[[2L]]
    fun <- call[[1L]]
    func <- as.character(fun)
    check_fun(func, env)
  }
  writeLines(sprintf("Depth %d: %s", depth, deparse1(call)))

  # - Classify/Eval Params -----------------------------------------------------

  args <- if(!is.null(defn <- VALID_FUNS[[c(func, "defn")]])) {
    match_call(definition=defn, call=call, envir=env, name=func)
  } else {
    as.list(callm[-1L])
  }
  args.i <- seq_along(args)

  args.sym <- vapply(args, is.symbol, TRUE)
  args.sym.c <- rep(NA_character_, length(args.sym))
  args.sym.c[args.sym] <- as.character(args[args.sym])

  # Control parameters
  args.ctrl <- names(args) %in% VALID_FUNS[[c(func, "ctrl")]]

  # Group varying parameters (not control, naked numeric)
  args.sym.dat <- logical(length(args))
  args.sym.dat[args.sym] <-
    !args.ctrl[args.sym] & args.sym.c[args.sym] %in% names(data)
  dat.valid <- is.num_naked(data)
  args.sym.dat.bad <- args.sym.c[args.sym.dat] %in% names(data)[!dat.valid]
  if(any(args.sym.dat.bad)) {
    args.sym.bad.c <- args.sym.c[args.sym.dat.bad[1L]]
    stop(
      "Classed or non-double data used for data parameter in `",
      deparse1(call), "` ", not_num_naked_err(data[[args.sym.bad.c]]), "."
  ) }

  # External symbols, not-control or "data" (must be naked numeric)
  args.ext <- args.sym & !args.ctrl & !args.sym.dat

  # Eval those that need to be evaluated
  args.eval <- args
  args.eval[args.ctrl] <- lapply(args[args.ctrl], eval, envir=data, enclos=env)
  args.eval[args.ext] <- lapply(args[args.ext], eval, envir=data, enclos=env)
  args.evaled <- args.ctrl | args.ext

  # Validate external args after eval
  args.ext.good <- is.num_naked(args.eval[args.ext])
  if(!all(args.ext.good)) {
    arg.bad.1 <- which(!args.ext.good)[1L]
    arg.bad.val <- args.eval[arg.ba.d1]
    stop(
      "External Parameter `", args.sym.c[arg.bad.1], "` for `", deparse1(call),
      "` is not unclassed double ", not_num_naked_err(args.sym.c), "."
  ) }
  # - Process Params -----------------------------------------------------------

  # Compute sizes, and register required allocations / parameters.  Unclassified
  # parameters need to be processed recursively.  The later should all be
  # expressions as otherwise would be one of the other three categories.
  args.types <- rep("process", length(args))
  args.types[args.sym.dat] <- "data"
  args.types[args.ctrl] <- "control"
  args.types[args.ext] <- "external"

  # Record sizes
  # * Row 1: minimum length of the result
  # * Row 2: whether the group size affects the size of the result.
  args.sizes <- matrix(numeric(length(args) * 2), nrow=2L) # must hold R_xlen_t
  colnames(args.sizes) <- names(args)

  # To track what data structure in alloc.dat each arg matches, only for
  # non-data args.
  args.ids <- integer(length(args))

  for(i in seq_along(args)) {
    if(args.types[i] == "process") {

      # Unevaluated expression that we need to compute
      if(is.symbol(args[[i]]) || !is.language(args[[i]]))
        stop("Internal error in parameter classification.")

      x <- pp_internal(
        call=args[[i]], data=data, gmax=gmax, env=env, depth=depth + 1L, x=x
      )
      args.ids[i] <- x[['id']]
      args.sizes[,i] <- x[['size']]
    } else if (args.types[i] == "data") {
      # Symbol bound to group data, known size, arleady in data structure
      # (leading part of it, hence we can use `match` for their position)
      args.ids[i] <- match(args.sym.c[i], names(data)[dat.valid])
      args.sizes[,i] <- c(NA_real_, 1)  # NA means group size
    } else if (args.types[i] == "external") {
      # No need to allocate external, but we do want to track them
      x[['alloc']] <- append_dat(
        x[['alloc']], args.eval[i], length(args.eval[i]), depth, "ext"
      )
      args.ids[i] <- x[['alloc']][['i']]
      args.sizes[,i] <- c(length(args.eval[i]), 1)
    } else if (args.types[i] == "control") {
      # Control params unregistered, but their size could affect result
      args.sizes[,i] <- c(length(args.eval[i]), 1)
    } else stop("Internal Error, unexpected arg type ", args.types[i])
  }
  # - Compute Result Size ------------------------------------------------------

  # Based on the function type and recorded lengths, compute the result length.
  # We distinguish size used for allocation (maximum possible size), vs size in
  # `size` (accounting for actual group size)
  ftype <- VALID_FUNS[[c(func, "type")]]
  if(ftype[[1L]] == "constant") {
    # Always constant size, e.g. 1 for `sum`
    size <- c(ftype[[2L]], 0)
    x[['alloc']] <- alloc_dat(x[['alloc']], depth, size=size[1], call)
  } else if(ftype[[1L]] %in% c("arglen", "vecrec")) {
    # Length of a specific argument, like `probs` for `quantile`
    if(!all(ftype[[2L]] %in% colnames(args.sizes)))
      stop(
        "Parameter(s) ",
        deparse1(ftype[[2L]][!ftype[[2L]]%in% colnames(args.sizes)]),
        " missing but required for sizing."
      )
    sizes.tmp <- args.sizes[, ftype[[2L]], drop=FALSE]
    if(depth > 0L) {  # depth == 0 is the result vector, alloc'ed later
      x[['alloc']] <- alloc_dat(
        x[['alloc']], depth, size=max_size(sizes.tmp, gmax), call
    ) }
    size <- c(
      known_size(sizes.tmp[1L,]), # knowable sizes
      max(sizes.tmp[2L,])         # any group size in the lot?
    )
  } else stop("Internal Error: unknown function type.")
  res.id <- x[['alloc']][['i']]

  # - Generate Code ------------------------------------------------------------

  # At this point we need a vector with the id in the data structure of every
  # data parameter and the result.

  x[['argi']] <- c(x[['argi']], list(args.ids[!args.ctrl], res.id))

  # Compute the call data
  args.sizes.act <- args.sizes[1L,]
  args.sizes.act[!!args.sizes[2L,]] <- NA_real_
  args.sizes.act[args.sizes[1L,] == 0L] <- 0   # not sure about this one

  code <-
    VALID_FUNS[[c(func, "code.gen")]](func, args.sizes.act, args.eval[args.ctrl])
  code_valid(code, call)
  x[['code']] <- c(x[['code']], list(code))

  # - Finalize -----------------------------------------------------------------

  # Free any unused allocations.  Initially we kept depth + 1 but by this point
  # in actual code execution the value should have been released since we're
  # about to return to depth - 1.
  to.free <-
    x[[c('alloc', 'depth')]] > depth & is.finite(x[[c('alloc', 'depth')]]) &
    x[[c('alloc', 'type')]] == 'tmp'
  writeLines(paste0("    freeing slots ", deparse1(which(to.free))))
  x[[c('alloc', 'depth')]][to.free] <- Inf

  x
}
## Compute Max Possible Size
##
## This is affected by maximum group size as well as any non-group parameters.

known_size <- function(x) {
  tmp <- x[!is.na(x)]
  if(!length(tmp)) NA_real_
  else if(any(tmp == 0)) 0
  else max(tmp)
}
max_size <- function(x, gmax) {
  if(
    !is.numeric(x) || !is.matrix(x) || nrow(x) != 2 ||
    any(is.na(x[1L,] & !x[2L,]))
  )
    stop("Internal error, malformed size data.")

  size <- x[1L,]
  size[!!x[2L,]] <- gmax
  if(any(size == 0)) 0 else max(size)
}
## Track Required Allocations for Intermediate vectors
##
## List of all allocated temporary vector sizes, and which of those are free,
## along with identifiers, and a scalar noting which of the slots the most
## request request was assigned to.
##
## * dat: the actual data, for "tmp" type (i.e. generated by alloc_dat) this
##   will be written to so should not be accessible via R, or at least hidden.
##   Doesn't have to be done this way but it simplifies things.
## * ids: an integer identifier for each item in `dat`.
## * alloc: the "true" size of the vector.
## * type: one of "tmp" (allocated), "grp" (from the data we're generating
##   groups from, "ext" (any other data vector), or "res" (the result)
## * depth: the depth at which allocation occurred, only relevant for
##   `type == "tmp"`
## * i: scalar integer the first index of the appended items.

alloc_dat <- function(dat, depth, size, call) {
  writeLines(sprintf("  d: %d s: %d c: %s", depth, size, deparse1(call)))
  if(depth == .Machine$integer.max)
    stop("Expression max depth exceeded for alloc.") # exceedingly unlikely
  free <- !is.finite(dat[['depth']])
  fit <- free & dat[['type']] == "tmp" & dat[['alloc']] >= size
  if(!any(fit)) {
    # New allocation, then sort by size
    dat[['alloc']] <- c(dat[['alloc']], size)
    dat[['depth']] <- c(dat[['depth']], depth)
    id <- if(length(dat[['ids']])) max(dat[['ids']]) + 1L else 1L
    dat[['ids']] <- c(dat[['ids']], id)
    dat[['type']] <- c(dat[['type']], 'tmp')
    dat[['dat']] <- c(dat[['dat']], list(numeric(size)))
    writeLines(paste0("    alloc new: ", size))
    dat[['i']] <- id
  } else {
    # Allocate to smallest available that will fit
    target <- which.min(dat[['alloc']][fit])
    slot <- seq_along(dat[['alloc']])[fit][target]
    writeLines(
      sprintf("    re-use slot: %d (size %d)", slot, dat[['alloc']][slot])
    )
    dat[['depth']][slot] <- depth
    dat[['i']] <- dat[['ids']][slot]
  }
  dat
}
init_dat <- function() list(
  dat=list(), alloc=numeric(), depth=integer(), id=integer(), type=character(),
  id=0L
)
# Data need to contain:
#
# * List of the actual data, NULLs (or numerics); we could do the latter but a
#   bit dangerous.
# *

append_dat <- function(dat, new, sizes, depth, type) {
  if(!all(is.num_naked(new))) stop("Internal Error: bad data column.")
  if(!type %in% c("res", "grp", "ext")) stop("Internal Error: bad type.")
  id.max <- length(dat[['dat']])
  dat[['dat']] <- c(dat[['dat']], new)
  dat[['id']] <- c(dat[['id']], seq_along(new) + id.max)
  dat[['alloc']] <- c(dat[['alloc']], sizes)
  dat[['depth']] <- c(dat[['depth']], rep(depth, length(new)))
  dat[['type']] <- c(dat[['type']], rep(type, length(new)))
  dat[['i']] <- id.max
  dat
}

## Check function validity
check_fun <- function(x, env) {
  if(!x %in% names(VALID_FUNS))
    stop("`", as.character(call[[1L]]), "` is not a supported function.")
  if(
    !identical(
      try(got.fun <- get(x, envir=env, mode="function"), silent=TRUE),
      VALID_FUNS[[c(x, "fun")]]
  ) ) {
    tar.fun <- VALID_FUNS[[c(x, "fun")]]
    env.fun <-
      if(is.null(environment(tar.fun))) getNamespace("base")
      else environment(tar.fun)
    stop(
      "Symbol `", x, "` does not resolve to the expected function from ",
      capture.output(print(env.fun)),
      " (resolves to one from ",
      capture.output(print(environment(got.fun))), ")"
    )
  }
}

