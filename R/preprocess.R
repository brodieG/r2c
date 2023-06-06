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
#' Match each call and its parameters, identifying which parameters are control
#' vs. flag (flag are TRUE/FALSE control parameters), and associating the C call
#' to each R call.  The call tree is linearized depth first, so the parameters
#' are recorded before the call they belong to.  The depth of the parameters
#' allows us to distinguish what call they belong to (note a parameter can be a
#' call too).  The order of the elements in the linearized call implicitly
#' contains all parameter matching information (i.e. everything has been
#' `match.call`ed already).
#'
#' See `alloc` and `init_call_dat` for more details.
#'
#' @noRd
#' @param call an unevaluated R call
#' @param optimize logical or integer, level of optimization to apply
#' @param formals character vector of defined parameter names
#' @return a call dat list as described in `init_call_dat`.

preprocess <- function(call, formals=character(), optimize=FALSE) {
  call0 <- call
  # - Call Manipulations -------------------------------------------------------

  # WARNINGS:
  # * all call modifications must be made as if `match.called` (names attached
  #   for closures, in order) (and be in matched order) for closures.
  # * The order these are applied in really matters.

  # Match calls
  call <- match_call_rec(call)

  # Transform call. Should be an "optimization"?  Some transforms might not be.
  call <- transform_call_rec(call)

  # Apply optimizations
  if(optimize > 0L) {
    callr <- reuse_calls_int(call)
    call <- callr[['x']]  # also contains renames
  }
  # Restructure if/else and loops.  Not done as a transformation as
  # `reuse_calls_int` needs to be able to recognize the original control flow
  call <- transform_ifelse(call)

  # Copy "external" data to r2c alloc mem (see fun docs); must be the last step.
  tmp <- copy_branchdat(call)
  call <- tmp[['call']]
  sym.free <- tmp[['sym.free']]

  # WARNING: Read warning at top of section before making changes.

  # - Code Gen -----------------------------------------------------------------

  # All the data generated goes into x
  x <- init_call_dat()
  x[['sym.free']] <- sym.free

  # Classify parameters and generate code recursively
  x <- pp_internal(call=call, depth=0L, x=x)
  x[['call.processed']] <- call

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
  extra.vars <- c('narg', 'flag', 'ctrl')
  extra.vars.tpl <- logical(length(extra.vars))
  names(extra.vars.tpl) <- extra.vars
  args.used <- vapply(
    x[['code']][calls.keep], function(x) unlist(x[extra.vars]), extra.vars.tpl
  )
  code.txt <- c(
    # Headers, system headers first (are these going to go in right order?)
    paste(
      "#include",
      c(
        headers, '"r2c-const.h"', "<R.h>", "<Rinternals.h>", "<R_ext/Utils.h>",
        '"loop-interrupt.h"'
      )
    ),
    if(length(defines)) c("", defines),
    "",
    # Function Definitions
    codes.u,

    # C function to be invoked by runners
    sprintf("void run(\n  %s\n) {", toString(R.ARGS.ALL)),

    paste0(
      "  ",
      c(
        # Some variables not always used so add dummy uses to suppress warnings
        if(!any(args.used['narg',])) "(void) narg; // unused",
        if(!any(args.used['flag',])) "(void) flag; // unused",
        if(!any(args.used['ctrl',])) "(void) ctrl; // unused",
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
# @param call a recursively `match.call`ed call.
# @param assign indicate whether current evaluation is of a symbol being
#   assigned to to avoid recording that as a free symbol and also to tell the
#   allocator to use a stub for it in the temp allocation list.
# @param call.parent if `call` is being evaluated as an argument to a parent
#   call, `call.parent` is that call.  Used so we can tell if we're e.g. called
#   from braces.
# @param call.parent.name scalar character name of function in `call.parent` to
#   avoid recomputing it.
# @param indent additional indentation to add to code, used to support controls
#   with indented contents.  This is the total required indentation.

pp_internal <- function(
  call, depth, x, argn="", assign=FALSE, call.parent=NULL,
  call.parent.name="", indent=0L
) {
  if(depth == .Machine$integer.max)
    stop("Expression max depth exceeded.") # exceedingly unlikely

  if(is.call(call)) {
    # - Recursion on Params ----------------------------------------------------
    # Classify Params
    args <- as.list(call[-1L])
    if(is.null(names(args))) names(args) <- character(length(args))
    func <- call_valid(call)
    args.types <- rep("other", length(args))
    args.types[names(args) %in% VALID_FUNS[[c(func, "ctrl")]]] <- "control"
    args.types[names(args) %in% VALID_FUNS[[c(func, "flag")]]] <- "flag"

    # Check if we're in assignment call
    name <- get_lang_name(call) # should this just be `func`?
    next.assign <- name %in% ASSIGN.SYM
    # Assignments only allowed at brace level or top level because we cannot
    # assure the order of evaluation so safer to just disallow.  We _could_
    # allow it but it just seems dangerous.
    if(
      next.assign &&
      !call.parent.name %in% c(
        ASSIGN.SYM, "{", IF.SUB.SYM, LOOP.SYM, "vcopy", "rec"
      ) &&
      !is.null(call.parent)
    ) {
      call.dep <- deparse(clean_call(call))
      msg <- sprintf(
        "r2c disallows assignments inside arguments. Found: %s",
        if(length(call.dep) == 1) call.dep
        else paste0(c("", call.dep), collapse="\n")
      )
      stop(simpleError(msg, call.parent))
    }
    for(i in seq_along(args)) {
      if(args.types[i] %in% c('control', 'flag')) { # shouldn't be assign symbol
        if(next.assign) stop("Internal error: controls/flag on assignment.")
        x <- record_call_dat(
          x, call=args[[i]], depth=depth + 1L, argn=names(args)[i],
          type=args.types[i], code=code_blank(), assign=FALSE, indent=indent,
          rec=FALSE
        )
      } else {
        x <- pp_internal(
          call=args[[i]], depth=depth + 1L, x=x, argn=names(args)[i],
          assign=i == 1L && next.assign,
          call.parent=call, call.parent.name=func,
          indent=indent + (func %in% IF.SUB.SYM) * 2L
    ) } }
    # Are we in a rec chain?  Needed for alloc to know which bindings are
    # from rec (see reconcile_control_flow).
    rec <- name == "rec" || (
      name %in% PASSIVE.BRANCH.SYM &&
      length(x[['rec']]) && x[['rec']][length(x[['rec']])]
    )
    # Bind assignments (we do it after processing of the rest of the call)
    if(next.assign) {
      sym.bound <- get_target_symbol(call, name)
      x[['sym.bound']] <- union(sym.bound, x[['sym.bound']])
    }
    # Generate Code
    code <- VALID_FUNS[[c(func, "code.gen")]](
      func,
      args[args.types == "other"],
      args[args.types == "control"],
      args[args.types == "flag"]
    )
    code_valid(code, call)

    # Record linearized call data
    record_call_dat(
      x, call=call, depth=depth, argn=argn, type="call", code=code,
      assign=assign, indent=indent, rec=rec
    )
  } else {
    # - Symbol or Constant -----------------------------------------------------
    type <- "leaf"
    args <- list()
    code <- code_blank()
    # Deal with `..1`, etc, that may be generated by dots forwarding.
    # We could check for user defined .ARG[0-9] and then start incrementing
    # after max, but too complicated (need to handle case where someone e.g.
    # defines .ARG9999999999 or whatever to try to overflow us).
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
    record_call_dat(
      x, call=call, depth=depth, argn=argn, type=type, code=code, assign=assign,
      indent=indent, rec=FALSE
    )
} }

#' See preprocess for some discussion of what the elements are
#'
#' $call: linearized call tree with parameters preceeding calls (recall that a
#'   call can itself be a parameter to another call nearer the root).
#' $depth: tree depth of each call or parameter
#' $args: unused leftover from prior bad implementation?
#' $args.type: unused leftover from prior bad implementation?
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
#' $last.read: named integer of indeces in the linearized call tree of the last
#'   _call_ that read a particular symbol.  This allow us to know when we can
#'   free an allocation otherwise used by that symbol.  The names of this vector
#'   correspond to the **renamed** variables.
#' $call.rename: version of `call` with symbols renamed using `rename`.
#' $type: argument type
#' $rec: whether current call is a part of a chain that ends with a `rec`
#'
#' @noRd

init_call_dat <- function()
  list(
    call=list(),
    depth=integer(),
    args=list(),
    args.type=list(),
    code=list(),
    sym.free=character(),
    dot.arg.i=1L,
    last.read=integer(),
    assign=logical(),
    indent=integer(),
    rec=logical()
  )

## Record Expression Data
##
## We record both calls and terminals, but really there is some stuff that only
## matters for calls and we kind of work around it by using things like
## `code_blank` for terminals.  We need the terminals because the allocator
## still needs to know about them to find them in the data array or to evaluate
## them (for external symbols).

record_call_dat <- function(
  x, call, depth, argn, type, code, assign, indent, rec
) {
  # list data
  x[['call']] <- c(
    x[['call']],
    # Undo the dots replacement
    list(if(identical(call, quote(.R2C.DOTS))) QDOTS else call)
  )
  x[['code']] <- c(x[['code']], list(code))

  # vec data, if we add any here, be sure to add them to `exp.fields` in
  # `expand_dots`.
  x[['argn']] <- c(x[['argn']], argn)
  x[['depth']] <- c(x[['depth']], depth)
  x[['type']] <- c(x[['type']], type)
  x[['assign']] <- c(x[['assign']], assign)
  x[['indent']] <- c(x[['indent']], indent)
  x[['rec']] <- c(x[['rec']], rec)
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
#' Additionally fills in default values, and guarantees (possibly zero length)
#' names.
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
    # Only functions that have a closure definition are matched-called.  These
    # are either closures, or primitives that do argument matching that we
    # manually provided a stand-in closure for match.call purposes (e.g. `sum`)
    if(!is.null(defn <- VALID_FUNS[[c(func, "defn")]])) {
      # Replace dots (note these are dots as an argument, as opposed to dots in
      # the formals of the function we'll generate); we do not want these dots
      # to be matched against anything since that will be done at run-time, not
      # now (i.e.  dots might not even exist at the moment).
      any.dots <- vapply(call[-1L], identical, TRUE, quote(...))
      any.r2cdots <- vapply(call[-1L], identical, TRUE, quote(.R2C.DOTS))
      if(any(any.r2cdots)) stop("Symbol `.R2C.DOTS` is disallowed.")
      if(any(any.dots)) call[-1L][which(any.dots)] <- list(quote(.R2C.DOTS))
      # since we don't resolve dots env does not matter.
      call <- match.call(definition=defn, call=call, expand.dots=FALSE)
    }
    if(length(call) > 1) {
      for(i in seq(2L, length(call), by=1L)) {
        if(names2(call)[i] == "...") {
          for(j in seq_along(call[[i]]))
            call[[i]][[j]] <- match_call_rec(call[[i]][[j]])
        } else call[[i]] <- match_call_rec(call[[i]])
      }
    }
    if(!is.null(defn)) {
      # Fill in defaults
      frm <- formals(defn)
      frm.req <-
        vapply(frm, function(x) is.name(x) && !nzchar(as.character(x)), TRUE)
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
      args.dot.pos <- if(length(call.dots)) {
        # Used when e.g. vecrec is done on dots
        names(call.dots) <- rep('...', length(call.dots))
        match("...", args.nm[args.ord])
      } else length(args)
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
    func <- call_valid(call)
    call.transform <- VALID_FUNS[[c(func, "transform")]](call)
    if(identical(call.transform, call)) break
    call <- call.transform
  }
  if(is.call(call) && length(call) > 1L) {
    for(i in seq(2L, length(call), 1L))
      call[[i]] <- transform_call_rec(call[[i]])
  }
  call
}
# Expand if/else Into Expected Format
#
# An if/else lik:
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
# calls for required allocations.

transform_ifelse <- function(x) {
  if(is.call(x)) {
    x[-1L] <- lapply(x[-1L], transform_ifelse)
    call.sym <- get_lang_name(x)
    if(call.sym == "if") {
      if(!length(x) %in% 3:4)
        stop("Invalid if/else call:\n", paste0(deparse(x), collapse="\n"))
      # Can't use `quote(numeric(0L))` because we don't (and can't? Err we
      # could since it's constant alloc) implement `numeric`.
      if(length(x) == 3L) x[[4L]] <- numeric(0L)
      x <- bquote(
        {
          r2c::if_test(cond=.(x[[2L]]))
          r2c::r2c_if(
            true=r2c::if_true(expr=.(x[[3L]])),
            false=r2c::if_false(expr=.(x[[4L]]))
          )
        }
      )
      names(x)[2:3] <- "..."  # needed for alloc logic
    } else if (call.sym == "{" && length(x) == 2L) {
      # remove redundant nested braces as could be introduced above.  This could
      # remove other redundant nested braces.
      x <- x[[2L]]
    }
  }
  x
}
# Undo `transform_ifelse`
#
# This is not a perferct reversal because we don't track if an `else numeric(0)`
# was there originally or added by `transform_if_else`.  Additionally, we always
# remove braces when they only contain one sub-call, and we don't check for
# stray `r2c_if`s not paired with a corresponding `if_test` (which shouldn't
# happen).
#
# This is NOT recursive as `clean_call` does the recursion.

recompose_ifelse <- function(x) {
  if(is.call_w_args(x)) {
    call.sym <- get_lang_name(x)
    if(call.sym == "{") {
      x.call <- vapply(x[-1L], is.call, TRUE)
      x.call.name <- c("", vapply(x[-1L][x.call], get_lang_name, ""))
      if.test <- which(x.call.name == "if_test")
      # Every if.test needs to be followed either by and `r2c_if`, or by an
      # `rec(r2c_if)`
      if(any(if.test == length(x.call.name)))
        stop("Internal Error, bad decomposed if call:\n", deparseLines(x))

      is.rec <- FALSE
      for(i in if.test) {
        if(
          x.call.name[i + 1L] != "r2c_if" && x.call.name[i + 1L] != "rec" &&
          !is.call(x[[i + 1L]]) &&
          (call.sym.2 <- get_lang_name(x[[i + 1L]])) != "r2c_if"
        )
          stop("Internal Error, bad decomposed if call 2:\n", deparseLines(x))
        if(x.call.name[i + 1L] == "rec") {
          is.rec <- TRUE
          x[[i + 1L]] <- x[[i + 1L]][[2L]]
        }
      }
      # reconstruct call from end so indices don't change, +1L because `if.test`
      # is not counting the function slot
      for(i in rev(if.test)) {
        if.call <- call(
          "if",
          x[[i]][[2L]],                 # test
          x[[i + 1L]][[c(2L, 2L)]],     # true
          x[[i + 1L]][[c(3L, 2L)]]      # false
        )
        # Undo empty else (this might undo a legit numeric(0L))
        if(identical(if.call[[3L]], numeric(0L))) if.call[[3L]] <- NULL

        # Add back the `rec` around the entire if
        if(is.rec) if.call <- en_rec(if.call, clean=TRUE)
        x <- as.call(
          c(
            if(i - 1L) as.list(x[seq_len(i - 1L)]),
            list(if.call),
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

sym_free <- function(x, sym) {
  if(is.symbol(sym)) {
    sym.chr <- as.character(sym)
    if(identical(sym.chr, ".R2C.DOTS")) sym.chr <- "..."
    if(!sym.chr %in% x[['sym.free']]) sym.chr
  }
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
    dots.m.names <- lapply(arg.names[is.dots.m], as.name)
    # Could have multiple sets of dots
    for(i in which(is.dots)) {
      x[['call']][[i]] <- NULL
      x[['call']] <- append(x[['call']], dots.m.names, after=i - 1L)
      for(j in exp.fields) {
        exp.val <- x[[j]][i]
        x[[j]] <- c(
          x[[j]][seq_len(i - 1L)],
          rep(exp.val, sum(is.dots.m)),
          x[[j]][seq_len(length(x[[j]]) - i) + i]
  ) } } }
  x
}
