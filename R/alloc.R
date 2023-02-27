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

#' @include preprocess.R

NULL

#' Allocate Required Storage
#'
#' This is internal documentation.  Please be sure to read `?r2c-compile` and
#' the documentation for the `preprocess` internal function before reading this.
#'
#' We iterate through the linearized call list (see `preprocess` for details) to
#' determine the memory needs to support the evaluation of the function, and
#' pre-allocate this memory.  The allocations are sized to fit the largest
#' iteration (and thus should fit all iterations).  The allocations are appended
#' to the storage list that is part of the `alloc` object (`alloc[['dat']]).
#' See docs for `append_dat`.
#'
#' Every leaf expression that might affect allocation requirements (i.e.
#' excluding control and flag values) is brought into the storage list if
#' it is not there already.  External vectors (i.e. not part of the group data)
#' are shallow copied into the storage list (i.e. just referenced, not actually
#' copied, per standard R semantics).  The semantics of lookup should mimic
#' those of e.g. `with`, where we first look through any symbols previously
#' bound in the evaluation environment, then in the data in `with`, and finally
#' in the calling environment.
#'
#' The sizes of leaf expressions are tracked in the `stack` array, along with
#' their depth, and where in the storage list they reside.  When we reach a
#' sub-call all its inputs will be in the `stack` with a depth equal to 1 + its
#' own depth.  We can use the input size information along with meta data about
#' the function in the call to compute the output size.  The inputs are then
#' removed from the stack and replaced by the output size from the sub call, and
#' we allocate (or re-use a no-longer needed allocation) to/from the storage
#' list.  For outputs that have iteration-dependent size, we record the size
#' as NA (this will need to change in the future to accommodate more complex
#' functions like `c`), and allocate a vector large enough for the largest
#' group.  Recording the size as NA allows us to propagate that the output is
#' of group-size.
#'
#' This continues until the stack contains the output size and location of the
#' final call, and the allocation memory list contains all the vectors required
#' to hold intermediate calculations.  We record the state of the stack at each
#' sub-call along with other other useful data in `call.dat`.
#'
#' Control and flags are tracked in separate stacks that are also merged into
#' `call.dat`.
#'
#' @section Depth:
#'
#' An allocation is considered "freed" as soon as we emerge above the call
#' level (depth) for which it was originally made, unless it is also referenced
#' by a symbol that is still in use (designated `alloc[['names']]`).  The
#' `depth` variable semantics are strongly shaped by how it is generated (i.e.
#' by incrementing it with each level of recursion in the call tree it
#' originates from).  For example, for each call, it is a given that all of
#' it's parameters will have a `depth` of `depth+1`.  We use `stack` as a
#' mechanism for tracking the current call's parameters.
#'
#' @section Special Sub-Calls:
#'
#' Assignments and braces are considered special because neither of them do any
#' computations.  Assignments change the lifetime of the expression assigned to
#' the symbol to last until the last use of that symbol-instance (a
#' symbol-instance lifetime ends if it is overwritten).  A storage list element
#' containing the value bound to the symbol cannot be re-used until after the
#' lifetime ends.  Braces just "output" the last expression they contain.  Both
#' braces and assignments register on the stack by having their last input also
#' be the output.  This works fine because both these functions are no-op at the
#' C level so there is no risk of corrupting the input.
#'
#' @section Result Vector:
#'
#' All the allocations in the storage list are sized to contain a single
#' iteration (group, window, etc.), but the result vector will hold the
#' concatenation of all of them.  Thus, the final call is intercepted and
#' redirected to the special result vector in the storage list.
#'
#' This is trivial to do so long as what is written to the result is a sub-call
#' (e.g. something like `sum(x)` or `x + y`).  If instead it is just a symbol
#' referencing a previous sub-call or external vector (e.g. just `x`), we would
#' have to back-trace through the history of all the assignments to figure out
#' what sub-call was effectively responsible for producing that value, and
#' redirect that one to write to the result vector.  To simplify `r2c`
#' modifies the last expression to be `r2c_copy(expression)` if it turns out
#' that `expression` is a symbol.  See `copy_last` in preprocess.R
#'
#' @param x preprocess data as produced by `preprocess`
#' @return an alloc object:
#'
#' alloc
#'   $alloc: see `append_dat`.
#'   $call.dat: each actual call with a C counterpart
#'     $call: the R call
#'     $ids: ids in `alloc$dat` for parameters, and then result
#'     $ctrl: evaluated control parameters
#'     $flag: computed flag parameter value
#'   $stack:
#'     matrix used to track parameter sizes, but the time it's returned it
#'     should just have the size of the final return
#'
#' @noRd
#' @param x the result of preprocessing an expression

alloc <- function(x, data, gmax, par.env, MoreArgs, .CALL) {
  call.len <- length(x[['call']])
  call.outer <- x[['call']][[call.len]]

  # - Initialize ---------------------------------------------------------------
  env <- list2env(MoreArgs, parent=par.env)

  # Call indices where various interesting things happen
  meta <- list(
    i.call.max=length(x[['call']]),                # call count
    i.sym.max=latest_symbol_instance(x[['call']])  # last symbol maybe touched
  )
  i.last.action <- latest_action_call(x[['call']]) # last computing call

  alloc <- init_dat(x[['call']], meta=meta, scope=0L)
  # Status control placeholder.
  sts.vec <- numeric(IX[['STAT.N']])
  vdat <- vec_dat(sts.vec, "sts", typeof='double', group=0, size=IX[['STAT.N']])
  alloc <- append_dat(alloc, vdat=vdat, depth=0L)

  # Result placeholder, to be alloc'ed once we know group sizes.
  vdat <- vec_dat(numeric(), size=0L, type="res", typeof='double', group=0)
  alloc <- append_dat(alloc, vdat=vdat, depth=0L)

  # Add group data.
  if(!all(nzchar(names(data)))) stop("All data must be named.")
  data.naked <- data[is.num_naked(data)]
  data.used <- integer()
  for(i in seq_along(data.naked)) {
    datum <- data.naked[[i]]
    dname <- names(data.naked)[i]
    typeof <- typeof(datum)
    vdat <- vec_dat(datum, type="grp", typeof=typeof, group=1, size=NA_real_)
    alloc <- append_dat(alloc, vdat=vdat, depth=0L, name=dname)
  }
  # Bump scope so the data cannot be overwritten
  alloc[['scope']] <- alloc[['scope']] + 1L

  # - Process ------------------------------------------------------------------

  # Compute how much temporary storage we need to track all the intermediate
  # calculations in the call tree, and to allocate all the vectors into a data
  # structure.  This data structure will also include references to external
  # vectors (if there are such references).  The result of evaluating
  # control/flag parameters is recorded separately in `call.dat`.

  stack <- init_stack()
  stack.ctrl <- stack.flag <- list()
  call.dat <- list()

  for(i in seq_along(x[['call']])) {
    type <- x[['type']][[i]]
    call <- x[['call']][[i]]
    depth <- x[['depth']][[i]]
    argn <- x[['argn']][[i]]
    name <-if(!type %in% CTRL.FLAG) {
      if(length(call) < 2L) as.character(call)
      else as.character(call[[1L]])
    } else ""
    vec.dat <- init_vec_dat()

    # - Process Call -----------------------------------------------------------
    if(type == "call") {
      # Based on previously accrued stack and function type, compute call result
      # size, allocate for it, etc.  Stack reduction happens later.
      check_fun(name, env)
      ftype <- VALID_FUNS[[c(name, "type")]]

      # If data inputs are know to be integer, and function returns integer for
      # those, make it known the result should be integer.
      res.typeof <- if(
        VALID_FUNS[[c(name, "preserve.int")]] &&
        all(alloc[['typeof']][stack['id', ]] == "integer")
      ) "integer" else "double"

      # Compute result size
      if(ftype[[1L]] == "constant") {
        # Always constant size, e.g. 1 for `sum`
        asize <- size <- ftype[[2L]]
        group <- 0
      } else if(ftype[[1L]] %in% c("arglen", "vecrec")) {
        # Length of a specific argument, like `probs` for `quantile`
        # `depth + 1L` should be params to current call (this is only true for
        # the stack, not necessarily for other things in `x`)
        sizes.tmp <- compute_call_res_size(stack, depth, ftype)

        # asize uses max group size instead of NA so we can allocate for it
        asize  <- vec_rec_max_size(sizes.tmp, gmax)
        size <- vec_rec_known_size(sizes.tmp[1L,])  # knowable sizes could be NA
        group <- max(sizes.tmp[2L,])                # any group size in the lot?
      } else stop("Internal Error: unknown function type.")

      # Cleanup expired symbols, and bind new ones
      alloc <- names_update(alloc, i, call, call.name=name)

      # Prepare new vec data (if any), and tweak objet depending on situation.
      # Alloc is made later, but only if vec.dat[['new']] is not null.
      vec.dat <- vec_dat(NULL, "tmp", typeof=res.typeof, group=group, size=size)
      if(i == i.last.action) {
        # Last computing call should be written to result slot, vec.dat still
        # used to populate the stack.
        vec.dat[['type']] <- "res"
        alloc <- alloc_result(alloc, vec.dat)
      } else if(!name %in% c(PASSIVE.SYM, ASSIGN.SYM)) {
        # We have a computing expression in need of a free slots.
        # (NB: PASSIVE includes ASSIGN, but use both in case that changes).
        free <-
          !is.finite(alloc[['depth']]) &
          !alloc[['ids']] %in% alloc[['names']]['ids',]
        fit <- free & alloc[['type']] == "tmp" & alloc[['alloc']] >= asize
        # If none fit prep for new allocation, otherwise reuse free alloc
        if(!any(fit)) vec.dat[['new']] <- numeric(asize)
        else alloc <- reuse_dat(alloc, fit, vec.dat, depth=depth)
      } else if (name %in% PASSIVE.SYM) {
        # Don't do anything for these, effectively causing `dat[[i]]` to remain
        # unchanged for use by the next call.
      } else stop("Internal Error: unexpected call allocation state.")

    # - Assigned-To Symbol -----------------------------------------------------
    } else if(x[['assign']][i]) {
      # in e.g. `x <- y`, this is the `x`, which isn't actually data,
      # We don't need to record it but we do for consistency.
      vec.dat <- vec_dat(numeric(), "tmp", typeof="double", group=0, size=0)
    # - Control Parameter / External -------------------------------------------
    } else if (
      type %in% CTRL.FLAG || !name %in% colnames(alloc[['names']])
    ) {
      # Need to eval parameter
      tryCatch(
        arg.e <- eval(call, envir=data, enclos=env),
        error=function(e) stop(simpleError(conditionMessage(e), call.outer))
      )
      if(type == "control") {
        if(!nzchar(argn)) stop("Internal Error: missing arg name for control.")
        ctrl <- list(arg.e)
        names(ctrl) <- argn
        stack.ctrl <- c(stack.ctrl, ctrl)
      } else if (type == "flag") {
        if(!nzchar(argn)) stop("Internal Error: missing arg name for flag")
        flag <- list(arg.e)
        names(flag) <- argn
        stack.flag <- c(stack.flag, flag)
      } else {
        # Validate non-control external args after eval, and add to vec.dat
        validate_ext(x, i, type, arg.e, name, call, .CALL)
        typeof <- typeof(arg.e)
        size <- length(arg.e)
        vec.dat <- vec_dat(arg.e, "ext", typeof=typeof, group=0, size=size)
      }

    # - Match a Symbol In Data -------------------------------------------------
    } else if (id <- name_to_id(alloc, name)) {
      alloc[['i']] <- id
      # Update symbol depth (needed for assigned-to symbols)
      if(alloc[['depth']][id] > depth) alloc[['depth']][id] <- depth
      data.used <- union(data.used, id)
    } else stop("Internal Error: unexpected token.")

    # - Update Stack / Data ----------------------------------------------------

    # Append new data to our data array.  Not all branches produce data; those
    # that do have non-NULL vec.dat[['new']].
    if(!is.null(vec.dat[['new']]))
      alloc <- append_dat(alloc, vec.dat, depth=depth)

    # Call actions that need to happen after allocation data updated
    if(type == "call") {
      # Release allocation after call parameters incorporated
      alloc <- alloc_free(alloc, depth)
      # Append call data (different than append_dat)
      call.dat <- append_call_dat(
        call.dat, call=call, call.name=name, stack=stack, depth=depth,
        alloc=alloc, ctrl=stack.ctrl, flag=stack.flag
      )
      # Reduce stack
      stack <- stack[,stack['depth',] <= depth, drop=FALSE]
      stack.ctrl <- stack.flag <- list()
    }
    # Append new data/computation result to stack
    if(!type %in% CTRL.FLAG)
      stack <- append_stack(stack, alloc=alloc, depth=depth, argn=argn)
  }
  # - Finalize -----------------------------------------------------------------

  # Remove unused data, and re-index to account for that
  ids.all <- seq_along(alloc[['dat']])
  ids.keep <- ids.all[
    alloc[['type']] %in% c("res", "ext", "tmp", "sts") |
    ids.all %in% data.used
  ]
  call.dat <- lapply(
    call.dat, function(x) {
      x[['ids']] <- match(x[['ids']][x[['ids']] %in% ids.keep], ids.keep)
      x
  } )
  stack['id',] <- match(stack['id',], ids.keep)

  alloc.fin <- lapply(
    alloc[c('dat', 'alloc', 'depth', 'type', 'typeof')], "[", ids.keep
  )
  alloc.fin[['i']] <- match(alloc[['i']], ids.keep)
  list(alloc=alloc.fin, call.dat=call.dat, stack=stack)
}

# - Data Structures ------------------------------------------------------------

## Track Required Allocations for Intermediate vectors
##
## r2c keeps a list of every vector that it uses in computations, including the
## original data vectors, any referenced external vectors, and any temporary
## allocated vectors.  The temporary allocated vectors usage is tracked so that
## they may be re-used if their previously values are no longer needed.  The
## tracking is done via their depth in the call tree, and also whether any
## symbols were bound to them (using the `names` matrix).  Once allocated, a
## vector is never truly freed until the whole function execution ends.  It will
## however be re-used within a group if it becomes available, and all allocated
## vectors will be re-used for each group.
##
## This is a description of the input `dat` (which is also the returned value
## after update).
##
## * dat: (this is `dat[['dat']]`) the actual data, for "tmp" type (i.e.
##   generated by computation of a sub-call) this will be written to so should
##   not be accessible via R.
## * ids: an integer identifier for each item in `dat` (I think this is just
##   `seq_along(dat)`, but that seems silly and not sure now).
## * ids0: a unique identifier for each allocation, differs from `ids`
##   as soon as there is a re-use of a previous allocation.  This is to detect
##   deviations between the stack and allocations.
## * alloc: the true size of the vector (should be equivalent to
##   `lengths(dat[['dat']])`?).
## * size: the size of the vector in use (can be less than 'alloc' if a smaller
##   vector is assigned to a freed bigger slot), can be NA for vectors that
##   are group size.
## * type: one of "tmp" (allocated), "grp" (from the data we're generating
##   groups from, "ext" (any other data vector), "res" (the result), "sts"
##   (status flags, e.g. recycle warning).
## * typeof: the intended data format at the end of the computation.  Used to
##   try to track integer-ness (if any).
## * depth: the depth at which allocation occurred, only relevant for
##   `type == "tmp"`
## * i: scalar integer the index of the most recently allocated/appended
##   item(s).  This will point to the result of the most recent calculation.
## * names: a matrix where the (possibly duplicated) column names are symbols,
##   row `ids` are the ids in `dat` each symbol is bound to, `scope` is the
##   scope level the symbol was created in, and `i.max` is the largest index in
##   the linearized call list that the symbol exists in as a leaf (indicating
##   that beyond that the name binding need not prevent release of memory).
## * group: whether the allocation is of group size.  This is distinct to
##   `type=="grp"`, which refers to data from the original group varying data.
##   This designation is needed so that if a group sized allocation is bound to
##   a variable, that that allocation is group sized is still available when the
##   variable is retrieved later (it's possible this is a bit duplicative of NA
##   size).
##
## We're mixing return value elements and params, a bit, but there are some
## differences, e.g.:
##
## @param dat the allocation data structure
## @param vdat see vec_dat
## @param name group data comes with names, or things that are assigned to
##   symbols
## @param size could differ from `length(vdat[['new']])` when new is a special
##   size vector like a group dependent one.
## @return dat, updated

append_dat <- function(dat, vdat, name=NULL, depth) {
  if(is.null(name)) name <- ""
  if(!is.vec_dat(vdat)) stop("Internal Error: bad vec_dat.")
  type <- vdat[['type']]
  typeof <- vdat[['typeof']]
  new <- vdat[['new']]
  size <- vdat[['size']]
  group <- vdat[['group']]
  if(is.null(new)) stop("Internal Error: cannot append null data.")

  if(!is.num_naked(list(new))) stop("Internal Error: bad data column.")
  if(!type %in% c("res", "grp", "ext", "tmp", "sts"))
    stop("Internal Error: bad type.")
  if(is.na(size) && !type %in% c("grp", "tmp"))
    stop("Internal Eror: NA sizes only for temporary allocs or group.")

  new.num <- if(is.integer(new)) as.numeric(new) else new
  dat[['dat']] <- c(dat[['dat']], list(new.num))

  dat[['i']] <- length(dat[['dat']])
  id0.new <- dat[['id0']] <- dat[['id0']] + 1L

  # need to test whether data.frame would slow things down too much
  dat[['ids']] <- c(dat[['ids']], dat[['i']])
  dat[['ids0']] <- c(dat[['ids0']], id0.new)
  dat[['alloc']] <- c(dat[['alloc']], length(new)) # true size
  dat[['size']] <- c(dat[['size']], size)          # could be NA
  dat[['depth']] <- c(dat[['depth']], depth)
  dat[['type']] <- c(dat[['type']], type)
  dat[['typeof']] <- c(dat[['typeof']], typeof)
  dat[['group']] <- c(dat[['group']], group)

  vec.el <- c(
    'ids', 'ids0', 'alloc', 'size', 'depth', 'type', 'typeof', 'group'
  )
  if(length(unique(lengths(dat[vec.el]))) != 1L)
    stop("Internal Error: irregular vector alloc data.")

  # Append dat should never overwrite names in the existing scope
  names <- dat[['names']]
  if(
    name %in% colnames(names[,names['scope',] == dat[['scope']], drop=FALSE])
  )
    stop("Internal error: cannot append names existing in current scope.")
  if(nzchar(name)) dat <- names_bind(dat, name)

  dat
}
## We Have Unused Allocations to Reuse
reuse_dat <- function(alloc, fit, vec.dat, depth) {
  stopifnot(is.vec_dat(vec.dat))
  target <- which.min(fit)
  slot <- seq_along(alloc[['dat']])[fit][target]

  alloc[['depth']][slot] <- depth
  alloc[['size']][slot] <- vec.dat[['size']]
  alloc[['typeof']][slot] <- vec.dat[['typeof']]
  alloc[['ids0']][slot] <- alloc[['id0']] <- alloc[['id0']] + 1L
  alloc[['group']][slot] <- vec.dat[['group']]
  alloc[['i']] <- alloc[['ids']][slot]
  alloc
}
init_dat <- function(call, meta, scope) {
  list(
    # Allocation data
    dat=list(),
    names=rbind(ids=integer(), scope=integer(), i.max=integer()),

    # Equal length vector data
    alloc=numeric(),
    depth=integer(),
    ids=integer(),
    ids0=integer(),
    type=character(),
    typeof=character(),
    group=numeric(),     # because it is used in a numeric matrix too

    # Other data
    i=0L,
    id0=0L,
    scope=scope,
    meta=meta
  )
}
init_vec_dat <- function() {
  list(
    new=NULL, type=NA_character_, typeof=NA_character_, group=NA_real_,
    size=NA_real_
  )
}
vec_dat <- function(
  new=NULL, type=NA_character_, typeof=NA_character_, group=NA_real_,
  size=NA_real_
) {
  vec.dat <- list(new=new, type=type, typeof=typeof, group=group, size=size)
  stopifnot(is.vec_dat(vec.dat))
  vec.dat
}
is.vec_dat <- function(x)
  is.list(x) && all(c('new', 'type', 'group', 'size') %in% names(x)) &&
  is.character(x[['type']]) &&
  isTRUE(x[['type']] %in% c("res", "grp", "ext", "tmp", "sts")) &&
  (is.numeric(x[['new']]) || is.integer(x[['new']]) || is.null(x[['new']])) &&
  is.numeric(x[['group']]) && length(x[['group']]) == 1L &&
  !is.na(x[['group']]) &&
  is.numeric(x[['size']]) && length(x[['size']]) == 1L &&
  is.character(x[['typeof']]) && length(x[['typeof']]) == 1L &&
  isTRUE(x[['typeof']] %in% c("double", "integer"))

## Stack used to track parameters ahead of reduction when processing call.
init_stack <- function() {
  matrix(
    numeric(), nrow=5L,
    dimnames=list(
      c(
        'id',      # id in our allocated data structure
        'id0',     # unique id for integrity checks
        'depth',
        'size',    # size, possibly NA if unknown
        'group'    # size affected by group
      ),
      NULL
) ) }
append_stack <- function(stack, alloc, id=alloc[['i']], depth, argn) {
  if(!id) stop("Internal Error: an alloc id must be specified.")
  id0 <- if(!id) id else alloc[['ids0']][id]

  size <- alloc[['size']][id]
  group <- alloc[['group']][id]

  stack <- cbind(stack, c(id, id0, depth, size, group))
  colnames(stack)[ncol(stack)] <- argn
  stack
}
## Record Call Data
##
## Associates each call with the allocation data used, along with control and
## flag data.
##
## @return list containing:
##
## * call: the call
## * ctrl, flag: evaluated control and flag data

append_call_dat <- function(
  call.dat, call, call.name, stack, alloc, depth, ctrl, flag
) {
  if(!is.call(call))
    stop("Internal Error: only calls should be appended to call dat.")
  ctrl_val_f <- VALID_FUNS[[c(call.name, "ctrl.validate")]]
  flag <- ctrl_val_f(ctrl, flag, call)

  param.ids <- stack['id', stack['depth',] > depth]
  param.ids0 <- stack['id0', stack['depth',] > depth]
  # Make sure that parameter ids tie out
  param.check <- alloc[['ids0']][param.ids] == param.ids0
  # For braces only the last parameter matters
  if(is.brace_call(call)) param.check <- param.check[length(param.check)]
  if(!all(param.check))
    stop(
      "Internal Error: alloc/stack mismatch for ",
      toString(names(which(!param.check)))
    )
  # alloc[['i']] is the last made allocation, which will be the result
  ids <- c(param.ids, alloc[['i']])
  c(call.dat, list(list(call=call, ids=ids, ctrl=ctrl, flag=flag)))
}
# "free" the data produced by arguments.  Not in the malloc sense, just an
# indication that next time this function is called it can re-use the
# slot, provided the slot is not referenced in 'names'.

alloc_free <- function(alloc, depth) {
  alloc[['depth']][alloc[['depth']] > depth & alloc[['type']] == 'tmp'] <- Inf
  alloc
}
# Set allocation to result slot
#
# This does not actually allocate as that's done at runtime once we know the
# group size.

alloc_result <- function(alloc, vdat){
  # Last call that computes anything should be written to result.  In
  # cases where the last expression is a symbol it is turned into a call
  # w/ r2c_copy
  slot <- which(alloc[['type']] == "res")
  # 'depth', 'id0', are not relevant anymore as the stack is done.
  alloc[['i']] <- slot
  alloc[['typeof']][slot] <- vdat[['typeof']]
  alloc[['group']][slot] <- vdat[['group']]
  alloc[['size']][slot] <- vdat[['size']]
  alloc
}

# - Other Helper Functions -----------------------------------------------------

#' Find Latest Symbol Instance
#'
#' This currently allows control/flag parameter symbols to count, even though
#' we're settling on them having different lookup semantics.  So this will be
#' cause some inefficiency in some cases where a symbol will not be released as
#' early as it should just because it's used as a control/flag.
#'
#' @noRd
#' @param x object as produced by preproc

latest_symbol_instance <- function(x) {
  call <- x
  # Compute symbol lifetime.  We only need to find the latest time it exists as
  # a leaf because it will be protected by its expression level in expressions.
  # This is to support protecting assignments across sub-expressions.
  sym.steps <- vapply(
    call, function(x) if(is.symbol(x)) as.character(x) else "", ""
  )
  sym.max <- tapply(seq_along(sym.steps), sym.steps, max)

  # Unfortunately loops complicate matters as an early symbol can be re-used
  # after first loop iteration, so get all the loops, and extract all the leaf
  # symbols used by each loop.
  loop.syms <- lapply(call, collect_loop_call_symbols)
  loop.reps <- rep(seq_along(loop.syms), lengths(loop.syms))
  loop.max <- tapply(loop.reps, unlist(loop.syms), max)

  # Return max instance
  max.names <- union(names(loop.max), names(sym.max))
  max.instance <- integer(length(max.names))
  names(max.instance) <- max.names
  max.instance[names(sym.max)] <- sym.max
  max.instance[names(loop.max)] <- pmax(max.instance[names(loop.max)], loop.max)
  max.instance[nzchar(names(max.instance))]
}
#' Find Last Call That Computes
#'
#' Braces just return the last value (as do assignments and control structures).
#' This is required to ensure the last computing call is written to the result
#' vector and not to a temporary allocation.
#'
#' @noRd
#' @param x linearized call list

latest_action_call <- function(x) {
  calls <- vapply(x, is.call, TRUE)
  call.sym <- vapply(
    x[calls],
    function(x) length(x) && (is.name(x[[1L]]) || is.character(x[[1L]])),
    TRUE
  )
  call.active <- vapply(
    x[calls][call.sym],
    function(x) !as.character(x[[1L]]) %in% PASSIVE.SYM,
    TRUE
  )
  call.active.i <- seq_along(x)[calls][call.sym][call.active]
  if(!length(call.active.i))
    stop("Internal Error: no active return call found.")
  max(call.active.i)
}
## Identify Missing Parameters on Stack
##
## Ends execution with Error

stack_param_missing <- function(params, stack.avail, call, .CALL) {
  stop(
    simpleError(
      paste0(
        "Parameter(s) ",
        deparse1(params[!params %in% stack.avail]),
        " missing but required for sizing in ", deparse1(call)
      ),
      .CALL
  ) )
}

## Compute Max Possible Size
##
## This is affected by maximum group size as well as any non-group parameters.
## This allows us to keep track of what the most significant size resulting from
## prior calls is in the presence of some calls affected by group sizes.  That
## way, if e.g. at some point we have a small group, but the limiting size is
## from the non-group data, that info isn't lost.
##
## IMPORTANT: the result of this alone _must_ be combined with preserving
## whether the data was group data or not.

vec_rec_known_size <- function(x) {
  tmp <- x[!is.na(x)]             # non-group sizes
  if(!length(tmp)) NA_real_
  else if(any(tmp == 0)) 0
  else max(tmp)
}
# only difference with above is we use the max group size for alloc

vec_rec_max_size <- function(x, gmax) {
  if(
    !is.numeric(x) || !is.matrix(x) || nrow(x) != 2 ||
    any(is.na(x[1L,] & !x[2L,]))
  )
    stop("Internal error, malformed size data.")

  size <- x[1L,]
  group <- as.logical(x[2L,])
  size[is.na(size) | group] <- gmax
  if(any(size == 0)) 0 else max(size)
}

compute_call_res_size <- function(stack, depth, ftype) {
  stack.cand <- stack['depth',] == depth + 1L
  if(is.character(ftype[[2L]])) {
    param.cand.tmp <- colnames(stack)[stack.cand]
    if(!all(param.cand.match <- ftype[[2L]] %in% param.cand.tmp))
      stack_param_missing(
        ftype[[2L]][!param.cand.match], param.cand.tmp,
        call, .CALL
      )
    param.cand <-
      seq_len(ncol(stack))[colnames(stack) %in% ftype[[2L]] & stack.cand]
  } else if(is.integer(ftype[[2L]])) {
    param.cand <- seq_along(stack)[stack.cand][ftype[[2L]]]
    if(anyNA(param.cand))
      stack_param_missing(
        ftype[[2L]][is.na(param.cand)], seq_along(stack)[stack.cand],
        call, .CALL
      )
  } else stop("Internal Error: unexpected arg index type.")

  # Arglen needs to disambiguate multiple params (e.g. `...` may show up
  # multiple times in `colnames(stack)` for any given depth).
  if(length(ftype) > 2L && is.function(ftype[[3L]])) {
    param.cand.tmp <- ftype[[3L]](param.cand)
    if(
      !is.integer(param.cand.tmp) ||
      !all(param.cand.tmp %in% param.cand)
    )
      stop("Internal Error: parameter disambiguation for sizing failed.")
    param.cand <- param.cand.tmp
  }
  stack[c('size', 'group'), param.cand, drop=FALSE]
}

## Check function validity
check_fun <- function(x, env) {
  if(!x %in% names(VALID_FUNS))
    stop("`", as.character(call[[1L]]), "` is not a supported function.")
  if(identical(x, 'r2c_copy')) {
    # waive check for `r2c_copy` so that `r2c` does not need to be attached
    # to the search path given user doesn't control when this gets injected.
  } else if(
    !identical(
      got.fun <- try(get(x, envir=env, mode="function"), silent=TRUE),
      VALID_FUNS[[c(x, "fun")]]
  ) ) {
    tar.fun <- VALID_FUNS[[c(x, "fun")]]
    env.fun <-
      if(is.null(environment(tar.fun))) getNamespace("base")
      else environment(tar.fun)
    stop(
      "Symbol `", x, "` does not resolve to the expected function from ",
      format(env.fun),
      if(is.function(got.fun) && !identical(env.fun,  environment(got.fun)))
        paste0(" (resolves to one from ", format(environment(got.fun)), ")")
) } }

#' Helper Functions for Symbol Management
#'
#' * `names_clean` drops name that have no future references to them, which
#'   makes it safe to release any memory that they've been bound to.
#' * `names_free` frees any allocation previously bound to a symbol, use this
#'   when you give a symbol a new binding in the same scope.
#' * `names_bind` record new symbols and their binding to the data allocation.
#'
#' @noRd

names_clean <- function(alloc, i.call) {
  names <- alloc[['names']]
  # Drop out ouf scope names
  names <- names[, names['scope',] <= alloc[['scope']], drop=FALSE]
  # Drop expired names
  alloc[['names']] <- names[, names['i.max',] >= i.call, drop=FALSE]
  alloc
}
names_free <- function(alloc, new.names) {
  names <- alloc[['names']]
  to.free <-
    names['scope', ] == alloc[['scope']] &
    colnames(names) %in% new.names
  alloc[['names']] <- names[, !to.free, drop=FALSE]
  alloc
}
names_bind <- function(alloc, new.name) {
  new.name.dat <- c(
    ids=alloc[['i']],
    scope=alloc[['scope']],
    i.max=unname(alloc[['meta']][['i.sym.max']][new.name])
  )
  if(is.na(new.name.dat['i.max'])) new.name.dat['i.max'] <- 0L
  alloc[['names']] <- cbind(alloc[['names']], new.name.dat)
  colnames(alloc[['names']])[ncol(alloc[['names']])] <- new.name
  alloc
}
names_update <- function(alloc, i, call, call.name) {
  alloc <- names_clean(alloc, i)
  if(call.name %in% ASSIGN.SYM) {
    # Remove protection from prev assignment to same name, and bind previous
    # computation (`alloc[[i]]`) to it.
    sym <- get_target_symbol(call)
    alloc <- names_free(alloc, sym)
    alloc <- names_bind(alloc, sym)
  }
  alloc
}
## Lookup a Name In Storage List
##
## Will find data vectors as well as any live assigned vectors that were
## generated.

name_to_id <- function(alloc, name) {
  if (id <- match(name, rev(colnames(alloc[['names']])), nomatch=0)) {
    # Reconvert the name id into data id (`rev` simulates masking)
    id <- ncol(alloc[['names']]) - id + 1L
    alloc[['names']]['ids', id]
  }
}
## Check That External Vectors are OK

validate_ext <- function(x, i, type, arg.e, name, call, .CALL) {
  if(type == "leaf" && !is.num_naked(list(arg.e))) {
    # Next call, if any
    next.call.v <- which(seq_along(x[['call']]) > i & x[['type']] == 'call')
    err.call <-
      if(length(next.call.v)) x[['call']][[next.call.v[1L]]]
      else call
    stop(
      simpleError(
        paste0(
          "External argument `", name, "` for `", deparse1(err.call),
          "` is not unclassed numeric ", not_num_naked_err(name, arg.e), "."
        ),
        .CALL
  ) ) }
}



