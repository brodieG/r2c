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
#' @param optimize logical or integer, level of optimization to apply
#' @param formals character vector of defined parameter names
#' @return a call dat list as described in `init_call_dat`.

preprocess <- function(call, formals=character(), optimize=FALSE) {
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

  # Copy "external" data to r2c alloc mem (see fun docs); must be the last step.
  call <- copy_symdat(call)

  # WARNING: Read warning at top of section before making changes.

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
    name <- get_lang_name(call[[1L]]) # should this just be `func`?
    next.assign <- name %in% ASSIGN.SYM
    # Assignments only allowed at brace level or top level because we cannot
    # assure the order of evaluation so safer to just disallow.  We _could_
    # allow it but it just seems dangerous.
    if(
      next.assign &&
      !call.parent.name %in% c(ASSIGN.SYM, "{", IF.SUB.SYM, LOOP.SYM) &&
      !is.null(call.parent)
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
          assign=i == 1L && next.assign,
          call.parent=call, call.parent.name=func,
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
# Copy Symbol Data
#
# r2c allows assignments by binding a symbol to the memory associated with the
# RHS of the assignment.  In many cases, that RHS is a computation, e.g. as in
# `y <- mean(x)`, but in others it might just be a rebinding as in `x <- y`.
# This becomes an issue in two contexts:
#
# 1. Final return value: normally we just redirect the write of the final
#    expression to to the result vector, but for that to happen the final
#    expression must be one that computes and generates new data. If the final
#    result is just a symbol referencing iteration or external data, that needs
#    to be explicitly copied to the result vector as there is no possibility of
#    redirection of a prior computation result (since there is no computation).
# 2. Branches: to implement branches we require that every symbol written to in
#    one branch that is used subsequent to the branch be bound to the same
#    memory in both branches.  This allows subsequent references to that symbol
#    to work irrespective of branch taken at run time.  This is accomplished
#    at allocation time by remapping the different allocations in each branch to
#    a new shared allocation.  The allocation must be the same size across
#    branches.  Subsequent accesses will then read from the shared location.
#
# For 1 we need to make sure that a returned value is either a computation, or
# is explicitly copied by injection of `vcopy`, i.e `x <- y` becomes
# `x <- vcopy(y)`.
#
# The second requires that every symbol bound in a branch (either `if/else`, or
# body of a loop (NB: a loop body may be taken or not)) point to memory newly
# allocated in that branch, which may require `vcopy`ing symbols *re*bound in
# that branch.  While it might be tempting, It is unsafe to try to remap
# `r2c` allocated memory that was in use prior to the branches because that
# memory could be used in different conflicting ways across the branches:
#
#    x <- mean(y)    # original `x` alloc
#    if(a) {
#      x <- x + 1    # new `x` alloc, frees original `x` alloc
#      z <- mean(x)  # potentially uses the original `x` alloc
#    } else {
#      z <- x
#    }
#    x + z
#
# We cannot try to use the original `x` allocation for `z` even though it would
# work for the `else` branch.  In the special case where in both branches we
# have e.g. `z <- x`  and `x` isn't touched we could avoid the `vcopy`, but we
# don't worry about that special case.
#
#
# @param x call to process

CAND <- c('copy', 'cand')
ACT <- c('copy', 'act')
B.LOC <- c('bind', 'loc')
B.ALL <- c('bind', 'all')

copy_symdat <- function(x) {
  if(is.symbol(x)) {
    # Special case: a single symbol.  For logic to work cleanly every symbol
    # needs to be part of a call
    x <- en_vcopy(x)
  } else {
    # Compute locations requring vcopy
    promoted <- unique(copy_symdat_rec(x)[[ACT]])

    # We want to apply the changes in reverse order so that indices are not made
    # invalid by tree modifications ahead of them.
    if(length(promoted)) {
      indices <- lapply(promoted, "[[", 2L)
      indices.eq <- lapply(indices, `length<-`, max(lengths(indices)))
      indices.mx <- do.call(cbind, indices.eq)
      indices.order <- do.call(order, split(indices.mx, row(indices.mx)))

      # Inject the vcopies
      for(i in promoted[rev(indices.order)]) {
        if(i[[2L]][length(i[[2L]])]) {
          # Trailing index not zero, so wrap a symbol in vcopy
          if(is.symbol(x[[i[[2L]]]]) && identical(i[[2L]], 0L)) {
            # standalone symbol
            x <- en_vcopy(x)
          } else if (is.call(x)) {
            # symbol in top level (passive) call e.g. `{x}`
            if(!is.symbol(x[[i[[2L]]]]))
              stop("Internal Error: attempting to vcopy non symbol.")
            x[[i[[2L]]]] <- en_vcopy(x[[i[[2L]]]])
          } else stop("Internal Error: bad context for symbol vcopy.")
        } else {
          # Insert an e.g. `x <- vcopy(x)` when trailing index is zero
          par.idx <- i[[2L]][-length(i[[2L]])]
          call <- x[[c(par.idx, 2L)]]  # par.idx should point to if_true/false
          call.sym <- get_lang_name(call)
          if(!is.call(call) || call.sym != "{") call <- call("{", call)

          # generate e.g. `x <- vcopy(x)`
          sym.miss <- as.symbol(i[[1L]])
          sym.vcopy <- call("<-", sym.miss, en_vcopy(sym.miss))
          call.list <- as.list(call)
          call <- as.call(c(call.list[1L], list(sym.vcopy), call.list[-1L]))
          x[[c(par.idx, 2L)]] <- call
    } } }
    x
  }
}
# Compute Locations of Required `vcopy`s
#
# Because we don't now know whether we need to `vcopy` an unbound symbol or a
# symbol bound in an if/else branch until after the point where the `vcopy`
# needs to happen, we record the coordinates of all such symbols in the call
# tree as "candidates" for `vcopy`.  Once we can determine whether they should
# be `vcopy`ed, we promote the candidates to an "actual" `vcopy` list.  When
# the full recursive traversal of the input call is complete, we can use the
# returned "actual" list and modify calls to add `vcopy` (this happens outside
# of this function).
#
# In addition to tracking candidates and actual vcopy promotions, we also track
# active bindings (i.e. symbols that have been assigned to in the call being
# processed).  We only consider bindings to be active when they are known to
# reference r2c allocated memory.  This means we do not include bindings to
# `vcopy` candidates until such a time as the candidates are promoted.  So e.g.
# in:
#
#    x <- y
#
# If `y` is an external symbol, then `x` is not considered an active binding
# until **after** `x` is used and triggers the promotion of `y` into an actual
# `vcopy`ed symbol.
#
# Symbols bound in branches are only required to reference memory allocated in
# the branch if they are used outside of the branch.  Other branch-bound symbols
# are branch-local and thus don't require `vcopy`  Because of this distinction
# we track local bindings and global bindings separately in the `data` object.
#
# Unfortunately the code is hideously complicated.  There should be a simpler
# solution that doesn't require handling so many corner cases, but the
# interactions between `vcopy` for last vs `vcopy` for accessing branch-assigned
# symbols complicate things.
#
# @seealso `copy_symdat` for context.
# @param index integer vector that can be used to pull up a sub-call within a
#   call tree e.g. with `x[[index]]` where `index` might be `c(2,3,1)`.
# @param assign.to character vector of symbol names that are part of current
#   binding call chain (e.g. in `a <- b <- {d <- a+b; c}` once we get to `c`
#   this will be `c("a", "b")`.
# @param last whether the current call being processed was the last sub-call of
#   the parent call.  This is only ultimately relevant for passive calls like
#   assignment and braces because for normal calls like `mean(x)` we know the
#   computation result will be an r2c allocation..
# @param data list of realized bindings "bind", and candidate/actual `vcopy`
#   symbols.  See details.

copy_symdat_rec <- function(
  x, index=integer(), assign.to=character(), last=TRUE,
  data=list(
    bind=list(loc=character(), all=character()),
    copy=list(cand=list(), act=list())
  )
) {
  if(is.call(x)) {
    call.sym <- get_lang_name(x)
    if(call.sym == 'r2c_if') {
      # New if/else context resets all local bindings
      data[[B.LOC]] <- character()
      # Recurse through each branch independently since they are "simultaneous"
      # with respect to call order (either could be last too).
      data.T <- copy_symdat_rec(
        x[[c(2L,2L)]], index=c(index, c(2L,2L)), data=data, last=last
      )
      data.F <- copy_symdat_rec(
        x[[c(3L,2L)]], index=c(index, c(3L,2L)), data=data, last=last
      )
      # Combine the copy data.
      data <- merge_copy_dat(data, data.T, data.F, index)
    } else {
      call.assign <- call.sym %in% ASSIGN.SYM
      call.passive <- call.sym %in% PASSIVE.SYM

      rec.skip <- 1L
      if(call.assign) {
        tar.sym <- get_target_symbol(x, call.sym)
        assign.to <- union(assign.to, tar.sym)
        rec.skip <- 1:2
      }
      # Recurse on language subcomponents
      for(i in seq_along(x)[-rec.skip]) {
        if(is.language(x[[i]])) {
          # assign.to is forwarded by the last expression in passive calls
          assign.to.next <-
            if(!call.passive || i < length(x)) character() else assign.to
          data <- copy_symdat_rec(
            x[[i]], index=c(index, i), assign.to=assign.to.next, data=data,
            last=last && i == length(x) && call.passive
          )
      } }
      if(call.assign) {
        # Need to clear candidates bound to this symbol, except that:
        # * We don't want to clear the candidate we just created.
        # * We need to allow candidates that will be cleared to be used during
        #   the assignment before they are cleared (so we can't do this before
        #   recursion).
        # * We should not clear any candidates on the last call (odd case where
        #   the return value assigns itself `x <- x`).

        if(!last) {
          #  Start by finding the first candidate earlier than this assignment
          indices <- lapply(data[[CAND]], "[[", 2L)
          indices.gt <- vapply(indices, index_greater, TRUE, index)
          # Clear those candidates
          data[[CAND]] <-
            data[[CAND]][names(data[[CAND]]) != tar.sym | indices.gt]
        }
        # Record local and global bindings.
        data[[B.LOC]] <- union(data[[B.LOC]], tar.sym)
        data[[B.ALL]] <- union(data[[B.ALL]], tar.sym)
      }
    }
  } else if (is.symbol(x)) {
    sym.name <- as.character(x)
    sym.local <- sym.name %in% data[[B.LOC]]
    sym.global <- sym.name %in% data[[B.ALL]]
    # If any existing candidates match this symbol, promote them unless they
    # are local, or it is the last expression and it points to a global var.
    cand <- data[[CAND]]
    cand.global <- vapply(cand, "[[", TRUE, "global")
    cand.prom.i <-
      which(names(cand) == sym.name & (!sym.local | (last & !cand.global)))
    cand.prom <- cand[cand.prom.i]

    # If last, turn off the "candidate" flag (later used in merging).
    # Last candidates are dont require `x <- vcopy(x)`.
    if(last) cand.prom <- lapply(cand.prom, "[[<-", "candidate", FALSE)

    data[[ACT]] <- c(data[[ACT]], cand.prom)
    data[[CAND]][cand.prom.i] <- NULL

    # Once a symbol is promoted, it can be added to the bindings.
    bind.prom <- names(cand.prom)
    data[[B.ALL]] <- union(data[[B.ALL]], bind.prom)
    data[[B.LOC]] <- union(data[[B.LOC]], bind.prom)
    sym.local <- sym.name %in% data[[B.LOC]]
    sym.global <- sym.name %in% data[[B.ALL]]

    # Generate new candidates/promotes if warranted.  Name is included so that
    # `union` etc. on lists can distinguish b/w same name but different index
    if (last && !sym.name %in% data[[B.ALL]]) {
      # last symbol in r2c exp referencing ext symbol automatically promoted
      # trailing FALSE denotes this was not an explicit promotion
      new.act <- list(cpyptr(sym.name, index, FALSE))
      data[[ACT]] <- c(data[[ACT]], new.act)
    } else if(length(assign.to) && !sym.local) {
      # non-local symbols being bound to other symbols are candidates
      new.cand <- sapply(
        assign.to, cpyptr, index, candidate=TRUE, global=sym.global,
        simplify=FALSE
      )
      data[[CAND]] <- c(data[[CAND]], new.cand)
    }
  }
  data
}
# A "Pointer" To Symbol To `vcopy`
#
# Allows us to track potential candidates for `vcopy` promotion along with meta
# data to discern if promotion is truly needed..
#
# @param name scalar character the symbol that triggers promotion of a
#   candidate, which is typically not the name of the symbol being promoted
#   (e.g. in `x <- y; x` the trigger is `x`, but the symbol that will be
#   `vcopy`ed is `y`.
# @param index the location of the symbol to vcopy; if the index ends in 0L then
#   a `x <- vcopy(x)` is added as this represents an external symbol missing
#   from one branch but used in the other.
# @param candidate TRUE if element is a candidate that requires explicit
#   promotion via a later reference to its name, as opposed to this
#   automatically became an actual vcopy because e.g. it was last.
# @param global whether the symbol had a potential valid global binding, for the
#   special case where we generate a candidate due to assignment in if/else, but
#   then just return the symbol as the last expression.

cpyptr <- function(name, index, candidate=TRUE, global=FALSE) {
  list(
    name=name, index=index, candidate=candidate, global=global
  )
}

# TRUE if the index a points to `a` position later in the tree than `b` in a
# depth-first traversal order.
index_greater <- function(a, b) {
  if(length(a) > length(b)) length(b) <- length(a)
  else length(a) <- length(b)
  a[is.na(a)] <- 0L
  b[is.na(b)] <- 0L
  all(a >= b) & any(a > b)
}

# Merge Candidates between Branches
#
# Both `a` and `b` contain lists at index CAND and ACT.  These are named lists
# with the names being those of symbols in the expression, and the elements a
# list of that same name as a scalar character, and the index to the symbol to
# vcopy, (and some other info not shown, see `cpyptr`) e.g:
#
#     list(a=list("a", c(1L, 4L, 2L)), b=list("b", c(1L, 4L, 4L)))
#
# The thing being `vcopy`ed need not be the same as often we see `x <- y` where
# the name is "x" but `y` is getting vcopied).  We include the symbol name in
# the data even though it's present in the names so `unique` and similar
# recognize that indices with different names are different as e.g. multiple
# symbols may reference the same symbol.
#
# @param a list should be `data` from `copy_symdat_rec` (see details).
# @param index integer vector the index into the call returning the expression
#   setting the current if/else context.

merge_copy_dat <- function(old, a, b, index) {
  # Removed shared history from both branches.  This is for the case where a
  # pre-existing candidate is cleared in one branch, but not the other.  If we
  # didn't remove shared history, the old candidate would be seen as new.
  a.cand <- setdiff(a[[CAND]], old[[CAND]])
  b.cand <- setdiff(b[[CAND]], old[[CAND]])
  # Track shared history to add back later.
  old.cand <- intersect(a[[CAND]], b[[CAND]])

  # Find candidates added in one branch missing in the other to inject e.g.
  # `x <- vcopy(x)` at start (hence 0L; so branch return value unchanged).
  a.miss <- setdiff(b.cand, a.cand)
  b.miss <- setdiff(a.cand, b.cand)
  a.miss.list <- lapply(a.miss, "[[<-", "index", c(index, 2L, 0L))
  b.miss.list <- lapply(b.miss, "[[<-", "index", c(index, 3L, 0L))

  # Non-candidate local bindings also need to be balanced in the opposing branch
  a.nc.miss <- setdiff(setdiff(b[[B.LOC]], a[[B.LOC]]), names(a.miss))
  b.nc.miss <- setdiff(setdiff(a[[B.LOC]], b[[B.LOC]]), names(b.miss))
  a.nc.miss.list <- lapply(a.nc.miss, cpyptr, index=c(index, 2L, 0L))
  b.nc.miss.list <- lapply(b.nc.miss, cpyptr, index=c(index, 3L, 0L))

  # Recombine all the pieces into the new set of candidates
  copy.cand <- unique(
    c(
      old.cand, a.cand, b.cand, a.miss.list, b.miss.list,
      a.nc.miss.list, b.nc.miss.list
  ) )
  # Same with actual inserts, but only include explicit promotions, as opposed
  # to those that happen because an expression is last.  The latter does not
  # require generating a `x <- vcopy(x)`, which is what we're doing here.
  # Promotions are never demoted so we don't worry about `old`.
  a.prom <- a[[ACT]][vapply(a[[ACT]], "[[", TRUE, 'candidate')]
  b.prom <- b[[ACT]][vapply(b[[ACT]], "[[", TRUE, 'candidate')]
  a.missing <- setdiff(b.prom, a.prom)
  b.missing <- setdiff(a.prom, b.prom)
  a.miss.list <- lapply(a.missing, "[[<-", "index", c(index, 2L, 0L))
  b.miss.list <- lapply(b.missing, "[[<-", "index", c(index, 3L, 0L))
  copy.act <- unique(c(a[[ACT]], b[[ACT]], a.miss.list, b.miss.list))

  # regen names lost in unique/union
  names(copy.cand) <- vapply(copy.cand, "[[", "", 1L)
  names(copy.act) <- vapply(copy.act, "[[", "", 1L)

  list(
    copy=list(cand=copy.cand, act=copy.act),
    # Bindings added in branches could be to candidates, so they are unresolved
    # and cannot be relied to point to r2c allocated memory yet.
    bind=old[['bind']]
  )
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
# Undo `transform_ifelse`
#
# This is not a perferct reversal because we don't track if an `else numeric(0)`
# was there originally or added by `transform_if_else`.  Additionally, we always
# remove braces when they only contain one sub-call.
#
# This is NOT recursive as `clean_call` does the recursion.

recompose_ifelse <- function(x) {
  if(is.call_w_args(x)) {
    call.sym <- get_lang_name(x)
    if(call.sym == "{") {
      x.call <- vapply(x[-1L], is.call, TRUE)
      x.call.name <- vapply(x[-1L][x.call], get_lang_name, "")
      if.test <- which(x.call.name == "if_test")
      if.branch <- which(x.call.name == "r2c_if")
      if(length(if.test) != length(if.branch) || !all(if.test == if.branch - 1))
        stop("Internal Error, bad decomposed if call:\n", deparseLines(x))

      # reconstruct call from end so indices don't change, +1L because `if.test`
      # is not counting the function slot
      for(i in rev(if.test) + 1L) {
        x <- as.call(
          c(
            if(i - 1L) as.list(x[seq_len(i - 1L)]),
            list(
              call(
                "if",
                x[[i]][[2L]],                 # test
                x[[i + 1L]][[c(2L, 2L)]],     # true
                x[[i + 1L]][[c(3L, 2L)]]      # false
            ) ),
            if(length(x) > i + 1L)
              as.list(x[seq(i + 2L, by=1L, length.out=length(x) - i - 1L)])
        ) )
        # Undo empty else (this might undo a legit numeric(0L))
        if(identical(x[[i]][[3L]], numeric(0L))) x[[i]][[3L]] <- NULL
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
