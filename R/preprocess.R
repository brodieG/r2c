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
#' @include preproc-copy.R

NULL

#' Generate C Code for Compilation
#'
#' Match each call and its parameters, identifying which parameters are
#' iteration constant (and of what type), and associating the C call to each R
#' call.  The call tree is linearized depth first, so the parameters are
#' recorded before the call they belong to.  The depth of the parameters allows
#' us to distinguish what call they belong to (note a parameter can be a call
#' too).  The order of the elements in the linearized call implicitly contains
#' all parameter matching information (i.e. everything has been `match.call`ed
#' already).
#'
#' See `alloc` and `init_call_dat` for more details.
#'
#' @noRd
#' @param call an unevaluated R call
#' @param optimize logical or integer, level of optimization to apply
#' @return a call dat list as described in `init_call_dat`.

preprocess <- function(call, optimize=FALSE) {
  call0 <- call
  # - Call Manipulations -------------------------------------------------------

  # WARNINGS:
  # * all call modifications must be made as if `match.called` (names attached
  #   for closures, in order) (and be in matched order) for closures.
  # * The order these are applied in really matters.

  # Sub in place holders for unsupported funs
  tmp <- sub_unsupported(call)
  call <- tmp[['call']]
  unsupported <- tmp[['unsupported']]

  # Match calls
  call <- match_call_rec(call)

  # Transform call. Should be an "optimization"?  Some transforms definitely are
  # not (e.g. subassign).
  call <- transform_call_rec(call)

  # Apply optimizations
  if(optimize > 0L) {
    callr <- reuse_calls_int(call)
    call <- callr[['x']]  # also contains renames
  }
  # What follows are more aggressive tranformations that are not usually shown
  # to the user because they start to make the call unrecognizable.

  # Restructure if/else and loops.  Not done as a transformation as
  # `reuse_calls_int` needs to be able to recognize the original control flow
  call <- transform_control(call)

  # Prepare the for loops
  tmp <- copy_fordat(call)
  call <- tmp[['call']]

  # Copy "external" data to r2c alloc mem (see fun docs), reconcile branches.
  # Think twice before you add any manipulations after this step.
  tmp <- copy_branchdat(call, unsupported)
  call <- tmp[['call']]
  sym.free <- tmp[['sym.free']]

  # Collapse any added braces
  call <- collapse_braces(call)

  # Reverse unsupported substitution
  call <- unsub_unsupported(call, unsupported)

  # WARNING: Read warning at top of section before making changes.

  # - Code Gen -----------------------------------------------------------------

  # All the data generated goes into x
  x <- init_call_dat()
  x[['sym.free']] <- sym.free

  # Classify parameters and generate code recursively
  x <- pp_internal(
    call=call, depth=0L, x=x, unsupported=unsupported, par.type=PAR.IVARY.NUM
  )
  x[['call.processed']] <- call
  if(!all(x[['par.type']] %in% PAR.TYPES))
    stop("Internal Error: invalid parameter types.")

  # Deduplicate the code and generate the final C file (need headers).
  headers <- unique(unlist(lapply(x[['code']], "[[", "headers")))
  defines <- unique(unlist(lapply(x[['code']], "[[", "defines")))
  codes <- vapply(x[['code']], "[[", "", "defn")
  names <- vapply(x[['code']], "[[", "", "name")
  codes.m <- match(codes, unique(codes))
  names.m <- match(names, unique(names))
  if(!identical(codes.m, names.m))
    stop("Internal error: functions redefined with changing definitions.")

  # noops don't need their C code generated
  out.ctrl <- vapply(x[['code']], "[[", 0L, "out.ctrl")
  codes <- codes[bitwAnd(out.ctrl, CGEN.OUT.DEFN) != 0L]
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
  out.ctrl.keep <- out.ctrl[calls.keep]

  indent <- strrep(" ", x[['indent']])[calls.keep]
  needs.padding <- c(FALSE, indent[-1L] != indent[-length(indent)])

  calls.fin <- lapply(
    seq_along(which(calls.keep)),
    function(i) {
      c(
        if(needs.padding[i]) "",
        if(bitwAnd(out.ctrl.keep[i], CGEN.OUT.RDEP))
          paste0(
            indent[i], "// ", unlist(strsplit(r.calls.dep.keep[i], "\n"))
          ),
        if(bitwAnd(out.ctrl.keep[i], CGEN.OUT.CALL)) {
          paste0(
            indent[i],
            if(bitwAnd(out.ctrl.keep[i], CGEN.OUT.MUTE)) "// NOOP: ",
            # add the i to e.g. di[i]
            if(grepl("%1\\$d", c.calls.keep[i]))
              sprintf(c.calls.keep[i], i - 1L)
            else c.calls.keep[i]
          )
        }
  ) } )
  extra.vars <- c('narg', 'icnst.any')
  extra.vars.tpl <- logical(length(extra.vars))
  names(extra.vars.tpl) <- extra.vars
  pars.used <- vapply(
    x[['code']][calls.keep], function(x) unlist(x[extra.vars]), extra.vars.tpl
  )
  code.txt <- c(
    # Headers, system headers first (are these going to go in right order?)
    paste(
      "#include",
      c(
        headers, "<stdint.h>",
        r2c_local_headers("r2c-const.h"),
        "<R.h>", "<Rinternals.h>",
        "<R_ext/Utils.h>",
        r2c_local_headers("loop-interrupt.h")
      )
    ),
    if(length(defines)) c("", defines),
    "",
    # Function Definitions
    codes.u,

    # C function to be invoked by runners
    sprintf("void run(\n  %s\n) {", toString(CR.ARGS.ALL)),

    paste0(
      "  ",
      c(
        # Some parameters not always used so add dummy uses to suppress warnings
        if(!any(pars.used['narg',]))      "(void) narg;  // unused",
        if(!any(pars.used['icnst.any',])) "(void) extn;  // unused",
        "",
        # C calls.
        unlist(calls.fin)
      )
    ),
    "}"
  )
  x[['code']] <- structure(code.txt, class="code_text")
  x
}
# Recursion portion of preprocess
#
# Classify parameters and generate code.
#
# WARNING: if `call` is not match-called with names, e.g. because it was
# inserted via transform or other manipulation after the `match_call_rec` in
# `preprocess`, this might fail in mysterious ways.
#
# @param call a recursively `match.call`ed call.
# @param assign indicate whether current evaluation is of a symbol being
#   assigned to, to tell the allocator to use a stub for it in the temp
#   allocation list.
# @param call.parent if `call` is being evaluated as an argument to a parent
#   call, `call.parent` is that call.  Used so we can tell if we're e.g. called
#   from braces.
# @param call.parent.name scalar character name of function in `call.parent` to
#   avoid recomputing it.
# @param indent additional indentation to add to code, used to support controls
#   with indented contents.  This is the total required indentation.
# @param passive whether every parent call (in a dynamic, not lexical sense) is
#   passive.  Used to ensure assignments are not made inside active calls.
#   `vcopy` counts as passive for these purposes (even though it's not).
# @param unsupported see `sub_unsupported`.

pp_internal <- function(
  call, depth, x, argn="", assign=FALSE, call.parent=NULL,
  call.parent.name="",
  par.validate=valid_always,
  indent=0L, passive=TRUE, par.type, unsupported
) {
  if(depth == .Machine$integer.max)
    stop("Expression max depth exceeded.") # exceedingly unlikely

  linfo <- get_lang_info(call)
  if (
    is.call(call) &&
    linfo[['name']] %in% names(VALID_FUNS) &&
    !par.type %in% PAR.ICNST
  ) {
    # - r2c Eval Call Recursion on Params --------------------------------------
    # Classify Params
    args <- as.list(call[-1L])
    if(!all(nzchar(names(args))))
      stop("Internal Error: parameters missing names - unmatched call?")

    # Ideally we wouldn't allow missing names - but it happens for primitives?
    if(is.null(names(args))) {
      names(args) <- character(length(args))
    }
    func <- linfo[['name']]

    arg.type <- ARG.INT
    # If we ever add more than one internal type, we need to ensure that's
    # resolved in the subsequent code at some point.
    if(length(arg.type) != 1L)
      stop("Internal Error: resolve multiple internal arg types")
    par.icnst <- VALID_FUNS[[c(func, "icnst")]]
    par.icnst.names <- names(par.icnst)
    par.icnst.types <- vapply(par.icnst, "[[", "", "type")
    par.icnst.validate <- lapply(par.icnst, "[[", "validate")

    if(!all(par.icnst.names %in% names(args)))
      stop(
        "Internal Error: designated iteration-constant parameters missing; is ",
        "`call` not properly match-called?"
      )
    par.icnst.loc <- match(par.icnst.names, names(args), nomatch=0)
    par.types <- rep(PAR.IVARY.NUM, length(args))
    par.types[par.icnst.loc] <- par.icnst.types
    par.validate <- replicate(length(args), valid_always, simplify=FALSE)
    par.validate[par.icnst.loc] <- par.icnst.validate

    passive <- passive && func %in% c(PASSIVE.SYM, 'vcopy')

    # Check if we're in assignment call
    next.assign <- func %in% ASSIGN.SYM  # not MODIFY.SYM
    # Assignments only allowed at brace level or top level because we cannot
    # assure the order of evaluation so safer to just disallow.  We _could_
    # allow it but it just seems dangerous.
    if(next.assign && !passive) {
      call.dep <- deparseLines(clean_call(call, level=2L))
      msg <- sprintf(
        "r2c disallows assignments inside arguments. Found: %s", call.dep
      )
      stop(simpleError(msg, call.parent))
    }
    for(i in seq_along(args)) {
      x <- pp_internal(
        call=args[[i]], depth=depth + 1L, x=x, argn=names(args)[i],
        assign=i == 1L && next.assign,
        call.parent=call, call.parent.name=func,
        par.validate=par.validate[i],
        indent=indent +
          (func %in% c(CTRL.SUB.SYM, FOR.ITER, R2C.FOR)) * 2L,
        passive=passive, unsupported=unsupported,
        par.type=par.types[i]
      )
    }
    # Are we in a rec chain?  Needed for alloc to know which bindings are
    # from rec (see reconcile_control_flow).
    rec <- func == "rec" || (
      func %in% PASSIVE.BRANCH.SYM &&
      length(x[['rec']]) && x[['rec']][length(x[['rec']])]
    )
    # Generate Code
    code <- VALID_FUNS[[c(func, "code.gen")]](func, args, par.types)
    code_valid(code, call)
    par.validate <- par.validate[1L]  # this is never used as its a call
  } else {
    # - Constant Param, Symbol, Literal, or Unsupported Call -------------------
    if(par.type %in% PAR.ICNST) {
      arg.type <- ARG.EXT[match(par.type, PAR.ICNST)]
    } else if(is.call(call) && !linfo[['name']] %in% names(VALID_FUNS)) {
      arg.type <- ARG.EXT.NUM
    } else {
      arg.type <- ARG.INT
    }
    args <- list()
    code <- code_blank()
    rec <- FALSE

    # Deal with `..1`, etc, that may be generated by dots forwarding.
    # We could check for user defined .ARG[0-9] and then start incrementing
    # after max, but too complicated (need to handle case where someone e.g.
    # defines .ARG9999999999 or whatever and it overflow us).
    # PAR.EXT shouldn't end up using this?
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
      }
    }
  }
  record_call_dat(
    x, call=call, depth=depth, linfo=linfo, argn=argn,
    arg.type=arg.type, par.type=par.type, par.validate=par.validate,
    code=code, assign=assign, indent=indent, rec=rec
  )
}

#' See preprocess for some discussion of what the elements are
#'
#' $call: linearized call tree with parameters preceding calls (recall that a
#'   call can itself be a parameter to another call nearer the root).
#' $depth: tree depth of each call or parameter
#' $linfo: result of calling get_lang_info on call, or NULL for terminals.
#' $argn: parameter name argument is bound to.
#' $code: the generated C code
#' $sym.free: symbols that were used without first being defined.
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
#' $call.rename: version of `call` with symbols renamed using `rename`.
#' $par.type: parameter type, one of PAR.TYPES, designates whether a parameter
#'   requires iteration constant (`PAR.ICNST.NUM`, `PAR.ICNST.ANY`) expressions,
#'   or can accept iteration varying data (`PAR.IVARY.NUM`).  The former
#'   requires external expressions (i.e. `eval`ed in R), whereas the latter can
#'   accept either internal (i.e. computed in `r2c` native code) or external
#'   numeric.
#' $arg.type: argument type, one of `ARG.EXT`, or `ARG.INT`. The first
#'   are "external" in that they require evaluation by `base::eval`, and the
#'   last is evaluated by `r2c` native code.  We use `arg.type` and not
#'   `par.type` because parameters that are classified as iteration varying can
#'   accept external expressions that evaluate to numeric.  See
#'   `?r2c-expression-types`.
#' $par.validate: a list of functions, each will validate the result of
#'   evaluating the corresponding iteration-constant parameters at allocation
#'   time.  Entries that belong to internal parameters or the call themselves
#'   are set to NULL.
#' $rec: whether current call is a part of a chain that ends with a `rec`
#' $is.call: call is a call (as opposed to symbol, terminal, whatever).
#'
#' @noRd

init_call_dat <- function()
  list(
    call=list(),
    code=list(),
    linfo=list(),
    par.validate=list(),
    sym.free=character(),
    dot.arg.i=1L,

    argn=character(),
    depth=integer(),
    arg.type=character(),
    par.type=character(),
    assign=logical(),
    indent=integer(),
    rec=logical(),
    is.call=logical()
  )

## Record Expression Data
##
## We record both calls and terminals, but really there is some stuff that only
## matters for calls and we kind of work around it by using things like
## `code_blank` for terminals.  We need the terminals because the allocator
## still needs to know about them to find them in the data array or to evaluate
## them (for external symbols).
##
## See `init_call_dat` for parameter details.

record_call_dat <- function(
  x, call, depth, linfo, argn, arg.type, par.type, par.validate, code,
  assign, indent, rec
) {
  vetr(par.validate=list(NULL))
  # list data
  x[['call']] <- c(
    x[['call']],
    # Undo the dots replacement
    list(if(identical(call, QR2C.DOTS)) QDOTS else call)
  )
  x[['code']] <- c(x[['code']], list(code))
  if(length(unique(lengths(x[c('call', 'code')]))) != 1L)
    stop("Internal Error: list component irregular size.")

  # arg data, if we add any here, be sure to add them to `exp.fields` in
  # `expand_dots`.
  x[['par.validate']] <- c(x[['par.validate']], par.validate)
  x[['linfo']] <- c(x[['linfo']], list(linfo))
  x[['argn']] <- c(x[['argn']], argn)
  x[['depth']] <- c(x[['depth']], depth)
  x[['arg.type']] <- c(x[['arg.type']], arg.type)
  x[['par.type']] <- c(x[['par.type']], par.type)
  x[['assign']] <- c(x[['assign']], assign)
  x[['indent']] <- c(x[['indent']], indent)
  x[['rec']] <- c(x[['rec']], rec)
  x[['is.call']] <- c(x[['is.call']], is.call(call))
  if(length(unique(lengths(x[CALL.DAT.VEC]))) != 1L)
    stop("Internal Error: irregular vector call data.")

  x
}
## Like names, but always return a character vector
##
## Used for calls for which we can't set empty names.

names2 <- function(x)
  if(is.null(names(x))) character(length(x)) else names(x)

#' Recursively match.call Call and Sub-Calls
#'
#' Additionally fills in default values, guarantees (possibly zero length)
#' names, and does basic validation.
#'
#' @noRd

match_call_rec <- function(call) {
  # strip parentheses as they are implicit in call structure
  while(is.call(call) && identical(as.character(call[[1L]]), "(")) {
    if(length(call) != 2L)
      stop("Internal Error: call to `(` with wrong parameter count.")  # nocov
    call <- call[[2L]]
  }
  if(is.call(call)) {
    func <- call_valid(call)
    # Only functions that have a closure definition are match-called.  These
    # are either closures, or primitives that do argument matching that we
    # manually provided a stand-in closure for match.call purposes (e.g. `sum`)
    defn <- VALID_FUNS[[c(func, "defn")]]
    if(!VALID_FUNS[[c(func, "primitive.nomatch")]]) {
      # Replace dots (note these are dots as an argument, as opposed to dots in
      # the formals of the function we'll generate); we do not want these dots
      # to be matched against anything since that will be done at run-time, not
      # now (i.e.  dots might not even exist at the moment).
      any.dots <- vapply(call[-1L], identical, TRUE, QDOTS)
      any.r2cdots <- vapply(call[-1L], identical, TRUE, QR2C.DOTS)
      if(any(any.r2cdots)) stop("Symbol `", R2C.DOTS, "` is disallowed.")
      if(any(any.dots)) call[-1L][which(any.dots)] <- list(quote(.R2C.DOTS))
      # since we don't resolve dots env does not matter.
      call <- match.call(definition=defn, call=call, expand.dots=FALSE)
    } else if (func == "if") {
      # `if` is a primitive but weird because it can have different number of
      # arguments (and even `args(if)` returns empty names).  We don't bother
      # giving it correct names because `if/else` will be decomposed into other
      # functions (otherwise we would need correct names and dealing with
      # variable length).
      names(call) <- character(length(call))
    } else {
      # These are primitives that don't match parameters; we want the return
      # match value to always have parameter names, but we don't want parameter
      # order to be changed if someone happens to do e.g. `"+"(e2=x, e1=y)`
      # since R ignores that.

      # We special case unary +/- b/c they are not transformed yet; this is a
      # smidge dicey because names are not the same...
      if(func %in% c("+", "-") && length(call) == 2L) defn <- uplus

      if(!is.null(names(call))) {
        stop(
          "Supplied primitive parameter names don't match definition in ",
          deparseLines(call)
        )
      } else if (length(call) != length(formals(defn)) + 1L) {
        stop(
          "Incorrect number of parameters for primitive ",
          deparseLines(call)
        )
      }
      if(is.null(names(call))) {
        names(call) <- c("", names(formals(defn)))
      }
    }
    if(length(call) > 1) {
      par.icnst <- VALID_FUNS[[c(func, "icnst")]]
      for(i in seq(2L, length(call), by=1L)) {
        par.name <- names2(call)[i]
        if(!par.name %in% names(par.icnst)) {
          if(par.name == "...") {
            for(j in seq_along(call[[i]]))
              call[[i]][[j]] <- match_call_rec(call[[i]][[j]])
          } else call[[i]] <- match_call_rec(call[[i]])
        }
      }
    }
    if(!is.null(defn) && func != "if") {
      # Fill in defaults
      frm <- formals(defn)
      frm.req <- vapply(frm, missing_sym, TRUE)
      frm.bad <- vapply(frm[!frm.req], function(x) is.language(x), TRUE)
      if(any(frm.bad))
        stop("Functions with non-constant defaults are unsupported.")

      call.nm <- names2(call)[-1L]
      frms.req <- names(frm)[frm.req]
      # recall: these dots (from formals) are not the ones replaced by .R2C.DOTS
      # (forwarded as an argument)!  See match_call_rec.
      frms.req <- frms.req[frms.req != "..."]
      if(!all(frms.req.have <- frms.req %in% call.nm))
        stop(
          "Missing required formals ",
          toString(sprintf("`%s`", frms.req[!frms.req.have])),
          " for `", func, "`."
        )
      # Add defaults, expand formals dots, order matters
      call.nm.missing <- names(frm)[!names(frm) %in% c(call.nm, "...")]
      call.dots <- call[['...']]
      if(is.null(call.dots)) call.dots <- list()
      args.nm <- c(
        names2(call[seq_along(call) != 1L & names2(call) != "..."]),
        "...",
        names(frm[call.nm.missing])
      )
      args.dummy <- c(
        as.list(call[seq_along(call) != 1L & names2(call) != "..."]),
        list(NULL),     # placeholder for dots
        frm[call.nm.missing]
      )
      args.pos <- match(args.nm, names(frm))
      args.ord <- order(args.pos)
      args <- args.dummy[args.ord]
      names(call.dots) <- rep('...', length(call.dots))
      args.dot.pos <- match("...", args.nm[args.ord])

      # expand the dots
      res <- c(
        args[seq_len(args.dot.pos - 1L)],
        call.dots,
        if(length(args.dummy) > args.dot.pos)
          args[seq(args.dot.pos + 1L, length(args), by=1L)]
      )
      # reconstitute the call
      call <- as.call(c(list(call[[1L]]), res))
    }
  }
  call
}
#' Transform a Call Into An Alternate Equivalent Call
#'
#' This could be for optimization purposes (x^2 -> x * x).
#'
#' @noRd

transform_call_rec <- function(call) {
  while(is.call(call)) { # equivalent to if(...) repeat
    func <- get_lang_name(call)
    call.transform <- VALID_FUNS[[c(func, "transform")]](call)
    if(identical(call.transform, call)) break
    call <- call.transform
  }
  if(is.call(call) && length(call) > 1L) {
    par.icnst.names <- names(VALID_FUNS[[c(func, "icnst")]])
    for(i in seq(2L, length(call), 1L)) {
      if(!names2(call)[i] %in% par.icnst.names)
        call[[i]] <- transform_call_rec(call[[i]])
    }
  }
  call
}
# Expand Control Structures Into Expected Format
#
# Modifies control structures and other things like nested braces so they meet
# expectations of subsequent logic.  For example an if/else like:
#
# ```
# if(a) b else c
# ```
#
# Becomes:
#
# ```
# r2c::if_test(a)
# r2c::r2c_if(if_true(b), if_false(c))
# ```
#
# This allows us to generate the control structures in C and line them up with
# calls for required allocations, and also creates several other useful patterns
# to help deal with the limitation of linearized calls.  For example, we know
# that the TRUE branch contents will be between the `if_test` and `if_true`
# call.
#
# Best way to understand what's going on is to look at the generated C code, the
# linearized recorded calls in `preprocess`, and the C generator functions in
# e.g. `code-ifelse.R`.
#
# **CAREFUL**: subsequent code relies on the exact nesting structure and
# parameter order of the code replacement.  Things you'll need to modify if you
# change the replaced call:
#
# * Branch handling logic in `copy_branchdat_rec`.
# * Branch merging logic in `merge_copy_dat`.
# * Code generation logic in e.g. `code-loop.R` or `code-ifelse.R`, as well as
#   formal order, etc.
# * Undoing of this in `recompose_control`.
# * Indentation in `pp_internal`.
#
# @param i scalar integer used to track loop number

transform_control <- function(x, i=0L) {
  if(i > 999L)
    stop("Exceeded maximum allowable control structure count (", i - 1L,")")
  if(is.call(x)) {
    call.sym <- get_lang_name(x)
    par.icnst.names <- names(VALID_FUNS[[c(call.sym, "icnst")]])
    par.icnst <- which(names2(x) %in% par.icnst.names)
    skip <- -c(1L, par.icnst)
    x[skip] <- lapply(x[skip], transform_control, i=i + 1L)
    if(call.sym == "if") {
      if(!length(x) %in% 3:4)
        stop("Invalid if/else call:\n", paste0(deparse(x), collapse="\n"))
      if(length(x) == 3L) x[[4L]] <- QNULL
      # **DANGER**, read docs if you change this
      x <- dot_names(
        bquote(
          {
            r2c::if_test(cond=.(x[[2L]]))
            r2c::r2c_if(
              true=r2c::if_true(expr=.(x[[3L]])),
              false=r2c::if_false(expr=.(x[[4L]]))
            )
        } )
      )
    } else if (call.sym == "for") {
      # Strategy is to compute the iteration vector into a new variable (_seq_)
      # so that it is immune from subsequent modification.  We then track where
      # we are in that vector (_seqi_).  The C code for `for_iter` will update
      # the iteration variable using those, and increment _seq_i_.  The `%d`
      # business is b/c we need different variables for each nested loop.

      # A copy (candidate) of the result of evaluating the exp in for(i in exp)
      seq.name <- as.name(sprintf(".R2C_for_seq_%d", i))
      # The index into `seq.name` that we're at
      seq.i.name <- as.name(sprintf(".R2C_for_seqi_%d", i))
      # **DANGER**, read docs if you change this
      # `for_iter` nested inside `r2c_for` for correct indenting of the C code.
      # `for_iter` is considered an assignment function (b/c it sets the
      # iteration variable).  While `for_iter` is declared as passive, it does
      # change the value of `i` and `seq.i` in-place.
      x <- dot_names(
        bquote(
          { # We rely on these braces in for loop processing
            r2c::for_init(
              seq=.(en_assign(seq.name, x[[3L]])),
              # Need fresh allocs so each of these point to their own memory.
              seq.i=.(en_assign(seq.i.name, en_vcopy(0))),
              # The iteration variable is NA if there are no iterations (this
              # differs from R that does NULL for that case).
              var=.(en_assign( x[[2L]], en_vcopy(NA_real_)))
            )
            r2c::r2c_for(
              iter=r2c::for_iter(
                var=.(x[[2L]]), seq=.(seq.name), seq.i=.(seq.i.name)
              ),
              for.n=r2c::for_n(expr=.(append_null(x[[4L]]))),
              for.0=r2c::for_0(expr=.(QNULL))
            )
          }
      ) )
    } else if (call.sym %in% c('repeat', 'while')) {
      # BE SURE TO UPDATE `collect_loop_call_symbols` IF WE ADD THESE
      stop("`", call.sym, "` unimplemented.")
    }
  }
  x
}
# Undo `transform_control`
#
# This is not a perferct reversal because we don't track if an `else numeric(0)`
# was there originally or added by `transform_if_else`.  Additionally, we always
# remove braces when they only contain one sub-call, and we don't check for
# stray `r2c_if`s not paired with a corresponding `if_test` (which shouldn't
# happen).
#
# This is NOT recursive as `clean_call` does the recursion.

recompose_control <- function(x) {
  if(is.call_w_args(x)) {
    call.sym <- get_lang_name(x)
    if(call.sym == "{") {
      x.call <- vapply(x[-1L], is.call, TRUE)
      x.call.name <- c("", vapply(x[-1L][x.call], get_lang_name, ""))
      ctrl.lead <- which(x.call.name %in% CTRL.LEAD)
      # Control leads cannot be last
      if(any(ctrl.lead == length(x.call.name)))
        stop("Internal Error, bad decomposed if call:\n", deparseLines(x))

      is.rec.v <- logical(length(x.call.name))
      for(i in ctrl.lead) {
        m1 <- match(x.call.name[i], CTRL.LEAD)
        m2 <- match(x.call.name[i + 1L], CTRL.MAIN)
        is.rec <- x.call.name[i + 1L] == 'rec'
        if(!identical(m1, m2) && !(m1 == 1L && is.rec))
          stop("Internal Error, bad decomposed if call 2:\n", deparseLines(x))
        # Temporarily remove the `rec` call
        if(is.rec) {
          is.rec.v[i] <- TRUE
          x[[i + 1L]] <- x[[i + 1L]][[2L]]
        }
      }
      # reconstruct call from end so indices don't change, +1L because
      # `ctrl.lead` is not counting the function slot
      for(i in rev(ctrl.lead)) {
        if(x.call.name[i] == IF.TEST) {
          call.recompose <- call(
            "if",
            x[[i]][[2L]],                 # test
            x[[i + 1L]][[c(2L, 2L)]],     # true
            x[[i + 1L]][[c(3L, 2L)]]      # false
          )
          # Undo empty else (this might undo a legit numeric(0))
          if(identical(call.recompose[[3L]], quote(numeric(length=0))))
            call.recompose[[3L]] <- NULL
          if(is.rec.v[i])
            call.recompose <- en_rec(call.recompose, clean=TRUE)
        } else if (x.call.name[i] == FOR.INIT) {
          # This doesn't remove the use-b4-set business
          call.recompose <- call(
            "for",
            x[[c(i + 1L, 2L, 2L)]],       # var
            x[[c(i, 2L, 3L)]],            # seq
            x[[c(i + 1L, 3L, 2L)]]        # expr
          )
          # Drop trailing numeric(0)
          cr4 <- call.recompose[[4L]]
          if(
            !is.brace_call(cr4) ||
            (
              !identical(cr4[[length(cr4)]], QNULL) &&
              !identical(cr4[[length(cr4)]], QNULL.REC)
            )
          )
            stop(
              "Internal Error: expecting numeric(0) at end of for body, got: ",
              deparseLines(call.recompose)
            )
          call.recompose[[4L]][[length(call.recompose[[4L]])]] <- NULL
        } else stop("Internal Error: recompose corrupt.")

        x <- as.call(
          c(
            if(i - 1L) as.list(x[seq_len(i - 1L)]),
            list(call.recompose),
            if(length(x) > i + 1L)
              as.list(x[seq(i + 2L, by=1L, length.out=length(x) - i - 1L)])
        ) )
      }
    }
    # Remove potentially uncessary braces
    if(call.sym == "{" && length(x) == 2L) x <- x[[2L]]
  }
  x
}

## Expand Dots
##
## Once we have an actual param match at runtime, we need to expand out the call
## data matched to dots to the number of dots.
##
## Not a given that all of `exp.fields` really need to be expanded since some of
## them are only relevant for calls and not terminals, but to avoid thinking
## about it we just do it.

expand_dots <- function(x, arg.names) {
  exp.fields <- CALL.DAT.VEC
  is.dots <- vapply(x[['call']], identical, TRUE, QDOTS)
  is.dots.m <- grepl(DOT.ARG.RX, arg.names)
  if(any(is.dots)) {
    dots.m.chr <- arg.names[is.dots.m]
    dots.m.names <- lapply(dots.m.chr, as.name)
    # Could have multiple sets of dots
    for(i in which(is.dots)) {
      x[['call']][[i]] <- NULL
      x[['call']] <- append(x[['call']], dots.m.names, after=i - 1L)
      for(j in exp.fields) {
        exp.val <- x[[j]][i]
        exp.vals <- if(j == "linfo") {
          if(!identical(exp.val, list(list(name=R2C.DOTS, pkg=""))))
            stop("Internal Error: bad dots lang info.")
          lapply(dots.m.chr, function(x) list(name=x, pkg=""))
        } else {
          rep(exp.val, sum(is.dots.m))
        }
        x[[j]] <- c(
          x[[j]][seq_len(i - 1L)],
          exp.vals,
          x[[j]][seq_len(length(x[[j]]) - i) + i]
  ) } } }
  x
}
## Ensure `x` is a brace expression returning "NULL" where "NULL" is proxied by
## a zero length numeric.

append_null <- function(x) {
  if(!is.brace_call(x)) x <- call("{", x)
  x[[length(x) + 1L]] <- quote(numeric(length=0L))
  dot_names(x)
}

## Substitute Symbols for Unsupported Calls
##
## Calls to unsupported function are replaced by symbols so that no further
## manipulation happens them in preprocessing.  They will be unsubbed after call
## manipulation, and then evaluated as a constant expression at allocation time.
##
## @param unsupported list of already substituted calls, where the names are the
##   substitution symbol based on UNSUP.CALL.TPL, and the values are lists with
##   element call (the original call) and syms (the symbols present in the
##   call, used to trigger candidates, track free symbols, etc).

sub_unsupported <- function(x, unsupported=list()) {
  if(is.call(x) && !get_lang_name(x) %in% c("(", names(VALID_FUNS))) {
    i <- length(unsupported) + 1L
    name <- sprintf(UNSUP.CALL.TPL, i)
    sym <- as.name(name)
    val <- list(list(call=x, syms=collect_call_symbols(x)))
    names(val) <- name
    unsupported <- c(unsupported, val)
    x <- sym
  } else if (is.call_w_args(x)) {
    for(i in seq_along(x)[-1L]) {
      tmp <- sub_unsupported(x[[i]], unsupported)
      x[[i]] <- tmp[['call']]
      unsupported <- tmp[['unsupported']]
    }
  }
  list(call=x, unsupported=unsupported)
}
unsub_unsupported <- function(x, unsupported) {
  if(is.symbol(x)) {
    sym.name <- as.character(x)
    if(sym.name %in% names(unsupported)) x <- unsupported[[sym.name]][['call']]
  } else if (is.call_w_args(x)) {
    for(i in seq_along(x)[-1L]) x[[i]] <- unsub_unsupported(x[[i]], unsupported)
  }
  x
}
