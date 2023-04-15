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
#' @param logical or integer, level of optimization to apply
#' @return a call dat list as described in `init_call_dat`.

preprocess <- function(call, formals, optimize=FALSE) {
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
  # `reuse_calls_int` needs to be able to recognize the original constrol flow
  call <- transform_ifelse(call)

  # Copy "external" data into internal alloctions (see fun docs).  This must be
  # the very last step
  call <- copy_encsym(call)

  # WARNING: Read section warning above before changing section.

  # - Code Gen -----------------------------------------------------------------

  # All the data generated goes into x
  x <- init_call_dat(formals)
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
            if(grepl("%1\\$d", c.calls.keep[i])) sprintf(c.calls.keep[i], i)
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
# @param indent additional indentation to add to code, used to support controls
#   with indented contents.  This is the total required indentation.

pp_internal <- function(
  call, depth, x, argn="", assign=FALSE, call.parent=NULL, indent=0L
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
    name <- get_lang_name(call[[1L]]) # should this just be `func`?
    next.assign <- name %in% ASSIGN.SYM
    # Assignments only allowed at brace level or top level because we cannot
    # assure the order of evaluation so safer to just disallow.  We _could_
    # allow it but it just seems dangerous.
    if(
      next.assign &&
      !is.brace_or_assign_call(call.parent) && !is.null(call.parent)
    ) {
      call.dep <- deparse(call)
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
          type=args.types[i], code=code_blank(),
          sym.free=sym_free(x, args[[i]]), assign=FALSE, indent=indent
        )
      } else {
        x <- pp_internal(
          call=args[[i]], depth=depth + 1L, x=x, argn=names(args)[i],
          assign=i == 1L && next.assign, call.parent=call,
          indent=indent + (func %in% IF.SUB.SYM) * 2L
    ) } }
    # Bind assignments (we do it after processing of the rest of the call)
    if(next.assign) {
      sym.bound <- get_target_symbol(call, name)
      x[['sym.bound']] <- union(sym.bound, x[['sym.bound']])
    }
    type <- "call"
    # Generate Code
    code <- VALID_FUNS[[c(func, "code.gen")]](
      func,
      args[args.types == "other"],
      args[args.types == "control"],
      args[args.types == "flag"]
    )
    code_valid(code, call)
    record_call_dat(
      x, call=call, depth=depth, argn=argn, type=type, code=code, assign=assign,
      indent=indent
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
      indent=indent
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
#' $sym.free/bound: ? Not sure this is still correct since we add the `formals`
#'   parameter.
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
#'
#' @noRd

init_call_dat <- function(formals)
  list(
    call=list(),
    depth=integer(),
    args=list(),
    args.type=list(),
    code=list(),
    sym.free=formals,
    sym.bound=character(),
    dot.arg.i=1L,
    last.read=integer(),
    assign=logical(),
    indent=integer()
  )

## Record Expression Data
##
## We record both calls and terminals, but really there is some stuff that only
## matters for calls and we kind of work around it by using things like
## `code_blank` for terminals.  We need the terminals because the allocator
## still needs to know about them to find them in the data array or to evaluate
## them (for external symbols).

record_call_dat <- function(
  x, call, depth, argn, type, code, assign, sym.free=sym_free(x, call), indent
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
  if(length(unique(lengths(x[CALL.DAT.VEC]))) != 1L)
    stop("Internal Error: irregular vector call data.")

  # symbols only bound after first instance of being bound, i.e. can start off
  # as free until actually gets assigned to.
  if(!assign) {
    x[['sym.free']] <-
      union(x[['sym.free']], setdiff(sym.free, x[['sym.bound']]))
  }
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
# Copy Out-of-Scope Symbols
#
# Most r2c functions will write data to allocations because the data is new,
# produced by computations.  Some don't actually generate data (e.g. `a <- b`),
# we call these functions passive.  There are two situations where this ends up
# mattering:
#
# 1. Final return value references external memory.  Normally we just redirect
#    the write to the final allocation to be in the result vector, but we can
#    only do that if the data in question would have been written by the r2c
#    function.  This saves a copy in most cases, but does mean we need to check
#    for the need for a copy for external references.
# 2. We require that multiple calls write to the same memory location, as we do
#    e.g. with assignments to the same symbol in different branches of an
#    if/else control structure.
#
# For both of these cases we need to reference a copy of the original data,
# otherwise the manipulations don't work.  We do this by inserting a call to
# `vcopy` so e.g. `a <- b` becomes `a <- vcopy(b)` which generates an actual
# C-level `memcpy`.
#
# This could end up generating wastefull copies if the symbols end up being
# unused, but that's just means there is some dead code we should detect and
# remove as part of other optimizations.
#
# Note: this will need to stop at function call boundaries.
#
# @param x call to process
# @param in.scope vector of symbol names that reference data computed by `r2c`
#   (i.e. not external).
# @param parent.passive TRUE or FALSE, whether the parent.call is one of the
#   passive calls.

copy_ooss <- function(
  x, in.scope=character(), parent.passive=TRUE
) {
  if(
    is.symbol(x) && parent.passive &&
    (!length(in.scope) || !as.character(x) %in% in.scope)
  ) {
    x <- en_vcopy(x)
  } else if (is.call(x)) {
    call.sym <- get_lang_name(x)
    if(is.assign_call(x)) in.scope <- c(in.scope, get_target_symbol(x, call.sym))
    else if (call.sym %in% IF.SUB.SYM) in.scope <- character()

    # Assume arguments are evaluated in order; this should only matters wrt to
    # `in.scope` when the arguments include assignments, which we only allow at
    # the top level of braces, so in-order eval is a fair assumption.
    passive <- call.sym %in% PASSIVE.SYM
    for(i in seq_along(x)[-1L]) {
      tmp <- copy_ooss(x[[i]], in.scope=in.scope, parent.passive=passive)
      x[[i]] <- tmp[['call']]
      in.scope <- union(in.scope, tmp[['in.scope']])
    }
  }
  list(call=x, in.scope=in.scope)
}
# Copy Encumbered Symbols
#
# Encumbered symbols are those that point to either external (i.e. non `r2c`
# allocated, typically this is references to group or other pre-existing data)
# memory or memory bound to multiple symbols.  This is memory we cannot
# arbitrarily change, and there are some contexts where we do want to
# arbitrarily change memory.  To allow this, we can copy the memory bound to
# these symbols to a new `r2c` memory location thus unencumbering them.
#
# There are two situations where we want to mess with memory associated with
# possibly encumbered symbols:
#
# 1. Final return value: normally we just redirect the write of the final
#    expression to to the result vector, but for that to happen the final
#    expression must be one that computes and generates new data. If the final
#    result is just a symbol the value of which was not produced via
#    computation, we need to explicitly copy that.
# 2. Branches: to implement branches we require that every symbol written to in
#    one branch is bound to the same memory as what that same symbol is in the
#    other branch.  This allows subsequent references to that symbol to work
#    irrespective of branch taken at run time.
#
# For both of these cases we need to reference a *copy* of the original data,
# otherwise the manipulations that produce the desired outcomes. We do this by
# inserting a call to `vcopy` so e.g. `a <- b` becomes `a <- vcopy(b)` which
# generates an actual C-level `memcpy`.
#
# This could end up generating wastefull copies if the symbols end up being
# unused, but that's just means there is some dead code we should detect and
# remove as part of other optimizations.
#
# Note: this will need to stop at function call boundaries.
#
# @param x call to process
# @param in.scope vector of symbol names that reference data computed by `r2c`
#   (i.e. not external).
# @param parent.passive TRUE or FALSE, whether the parent.call is one of the
#   passive calls.

copy_encsym <- function(x) {
  x <- copy_encsym_revpass(x, assign=FALSE)[['x']]
  copy_encsym_cleanpass(x)[['x']]
}
# Processing call tree in reverse allows us to know if a symbol is used after
# any call as we'll have seen it.
#
# Need to know all surviving symbols written in each branch.  Opposite branch
# will gain `x <- vcopy(x)` at beginning for missing symbols, but if `x` doesn't
# exist there will be an error.
#
# We assume that arguments are evaluated in order, which might not be true in
# reality, but shouldn't matter because we don't allow assignments in arguments
# with indeterminate evaluation order.
#
# See `copy_encsym`.
#
# @param live.sym character vector symbols that will be used later in the full
#   original call than the current sub-call being processed.

copy_encsym_revpass <- function(
  x, live.sym=character(), assign=FALSE, last=TRUE
) {
  if(is.call(x)) {
    call.sym <- get_lang_name(x)
    call.passive <- call.sym %in% PASSIVE.SYM
    if(call.sym == 'r2c_if') {
      x.true <-
        copy_encsym_revpass(x[[c(2L,2L)]], live.sym=live.sym, last=FALSE)
      x.false <-
        copy_encsym_revpass(x[[c(3L,2L)]], live.sym=live.sym, last=last)
      live.sym.merge <- union(x.true[['live.sym']], x.false[['live.sym']])
      x[[c(2L,2L)]] <- add_missing_symbols(x.true, live.sym.merge)
      x[[c(3L,2L)]] <- add_missing_symbols(x.false, live.sym.merge)
    } else if (call.sym %in% IF.SUB.SYM) {
      stop("Internal Error: if/else branch in unexpected location.")
    } else {
      # Passive calls forward source to assignment
      assign <-
        call.sym %in% ASSIGN.SYM ||
        assign && call.sym %in% PASSIVE.SYM

      rec.skip <- if(call.sym %in% ASSIGN.SYM) -(1:2) else -1L
      for(i in rev(seq_along(x)[rec.skip])) {
        tmp <- copy_encsym_revpass(
          x[[i]], live.sym=live.sym, assign=assign, last=last && i == length(x)
        )
        x[[i]] <- tmp[['x']]
        live.sym <- tmp[['live.sym']]
      }
      if(call.sym %in% ASSIGN.SYM) {
        # In reverse traversal a symbol assigned to is dead until the next time
        # it is used (where next is earlier in the tree)
        live.sym <- live.sym[live.sym != get_target_symbol(x)]
    } }
  } else if (is.symbol(x) && (assign || last)) {
    # vcopy if part of an assignment chain or return value, later we will undo
    # the vcopy that turn out not to be necessary.
    x <- en_vcopy(x)

    # Update the live symbol
    if(assign) live.sym <- union(live.sym, as.character(x))
  }
  list(x=x, live.sym=live.sym)
}
# Remove Redundant `vcopy` Calls
#
# Redundant vcopy are dumped.  These are the ones that were added to symbols
# defined in the compiled expression that would point to memory shared with no
# other symbols.  We can't know that in the backward pass ahead of time, so that
# one takes a conservative view and `vcopy`s everything, and this one undoes the
# ones we can tell are unnecessary in the forward pass.
#
# See `copy_encsym`
#
# @param bindings a named list where each named element shares its name with a
#   symbol that would be bound in the evaluation of x, and the element itself is
#   a character vector of symbols that would be bound to the same memory,
#   including itself (so elements are never length 0).

copy_encsym_cleanpass <- function(x, bindings=list(), assign.to=character()) {
  if(is.call(x)) {
    call.sym <- get_lang_name(x)
    if(call.sym == "vcopy") {
      # remove vcopy that reference a symbol not sharing referees
      vc.sym <- as.character(x[[2L]])
      if(vc.sym %in% names(bindings) && length(bindings[[vc.sym]]) == 1L) {
        x <- x[[2L]]
      }
    } else {
      rec.skip <- -1L
      if(call.sym %in% ASSIGN.SYM) {
        assign.to <- c(assign.to, get_target_symbol(call))
        rec.skip <- -(1:2)
      }
      for(i in seq_along(x)[rec.skip]) {
        # passive calls return memory bound to last arg result for assignment
        at.tmp <-
          if(call.sym %in% PASSIVE.SYM && i == length(x)) assign.to
          else character()
        tmp <-
          copy_encsym_cleanpass(x[[i]], bindings=bindings, assign.to=at.tmp)
        x[[i]] <- tmp[['x']]
        bindings <- tmp[['bindings']]
      }
    }
  } else if (is.symbol(x) && length(assign.to)) {
    # Update shared bindings
    sym.name <- as.character(x)
    bindings[assign.to] <-
      lapply(bindings[assign.to], union, c(assign.to, sym.name))
  }
  list(x=x, bindings=bindings)
}
# Inject Missing Symbols as `x <- vcopy(x)`
#
# Used as part of the process to balance balanced symbols assigned across
# branches.  The symbols are added before the existing expressions in the call.

add_missing_symbols <- function(x, missing) {
  if(length(missing)) {
    call <- x[['x']]
    call.sym <- get_lang_name(x)
    if(!is.call(call) || call.sym != "{") call <- call("{", call)

    # generate e.g. `x <- vcopy(x)`
    sym.miss <- lapply(setdiff(missing, x[['live.sym']]), as.symbol)
    sym.miss.vcopy <- lapply(sym.miss, en_vcopy)
    add.missing <- lapply(
      sym.miss,
      function(x) call("<-", sym.miss, en_vcopy(sym.miss))
    )
    as.call(list(quote("{"), add.missing, if(call.sym == "{") x[-1L] else x))
  } else x
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
    } else if (call.sym == "{" && length(x) == 2L) {
      # remove redundant nested braces as could be introduced above.  This could
      # remove other redundant nested braces.
      x <- x[[2L]]
    }
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
