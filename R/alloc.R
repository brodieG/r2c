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
#' We iterate through the linearized call list (see "Linearization"` for
#' details) to determine the memory needs to support the evaluation of the
#' function, and pre-allocate this memory.  The allocations are sized to fit
#' the largest iteration (and thus should fit all iterations).  The
#' allocations are appended to the storage list that is part of the `alloc`
#' object (`alloc[['dat']]).  See docs for `append_dat`.
#'
#' Every leaf expression that might affect allocation requirements  is brought
#' into the storage list if it is not there already.  Leaf expressions include
#' those labeled PAR.INT.LEAF and those labeled PAR.EXT, but note that
#' PAR.EXT.ANY are kept on their own stack and thus don't affect allocation
#' requirements.  External vectors (i.e. not part of the group data) are shallow
#' copied into the storage list (i.e. just referenced, not actually copied, per
#' standard R semantics), except that if they are integer/logical they are
#' coerced and thus copied.  The semantics of lookup should mimic those of e.g.
#' `with`, where we first look through any symbols previously bound in the `r2c`
#' evaluation environment, then in the data in `with`, and finally in the
#' calling environment.
#'
#' The sizes of leaf expressions are tracked in the `stack` array, along with
#' their depth, and where in the storage list they reside.  When we reach a
#' sub-call all its inputs will be in the `stack` with a depth equal to 1 + its
#' own depth.  We can use the input size information along with meta data about
#' the function in the call to compute the output size.  The inputs are then
#' removed from the stack and replaced by the output size from the sub call, and
#' we allocate (or re-use a no-longer needed allocation) to/from the storage
#' list.  Ex-ante calculation of output size is complex (see `compute_size`).
#'
#' This continues until the stack contains the output size and location of the
#' final call, and the allocation memory list contains all the vectors required
#' to hold intermediate calculations.  We record the state of the stack at each
#' sub-call along with other other useful data in `call.dat`.
#'
#' Control and flags are tracked in separate stacks that are also merged into
#' `call.dat`.
#'
#' @section Linearization:
#'
#' Linearization is very convenient when we have a sequence of calls that reduce
#' their parameters into a single result.  This is because we can keep all sorts
#' of meta data in parallel lists that map 1-1 with each call component.
#' Otherwise we need a complex recursive structure that matches the call
#' and also carries the meta data, and it is more awkward to deal with
#' that.  Linearization is done by `preprocess`.  An example:
#'
#'     x + y * z
#'
#' When linearized becomes:
#'
#'     expression          depth
#'
#'     x               2-------+
#'                             |
#'     y                  3--+ |
#'     z                  3--+ |
#'                           | |
#'     y * z           2-----+-+
#'                             |
#'     x + y * z     1---------+
#'
#' Processing the expressions sequentially can be done straightforwardly with a
#' simple stack.
#'
#' Unfortunately much after the initial implementation we decided it
#' would be a fun challenge to add branches, and the linearized format is not
#' well suited for them for the purpose of ex-ante size calculation.  Rather
#' than try to transition everything to a recursive structure we resorted to
#' some convolutions to deal with the branches
#'
#' @section Depth:
#'
#' An allocation is considered "freed" as soon as we emerge above the call
#' level (depth) for which it was originally made, unless it is also referenced
#' by a symbol that is still in use (designated `alloc[['names']]`).  The
#' `depth` variable semantics are strongly shaped by how it is generated (i.e.
#' by incrementing it with each level of recursion in the call tree it
#' originates from).  For example, for a call at depth `depth`, it is a given
#' that all of it's parameters will have a `depth` of `depth+1`.  We use `stack`
#' as a mechanism for tracking the current call's parameters.
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
#' redirected to the special result vector in the storage list.  Special
#' handling is required when the result is just a symbol, or branches are
#' involved.  See `copy_branchdat` in preprocess.R.
#'
#' @param x preprocessed data as produced by `preprocess`
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
#'     matrix used to track call-parameter relationships; when returned it
#'     should just have one column representing the return value of the r2c
#'     expression.
#'
#' @noRd
#' @param x the result of preprocessing an expression

alloc <- function(x, data, gmax, gmin, par.env, MoreArgs, .CALL) {
  call.len <- length(x[['call']])
  call.outer <- x[['call']][[call.len]]

  # - Initialize ---------------------------------------------------------------
  env <- list2env(MoreArgs, parent=par.env)
  env.ext.0 <- env.ext <- new.env(parent=env)

  # Call indices where various interesting things happen
  meta <- list(
    i.call.max=length(x[['call']]),                # call count
    i.sym.max=latest_symbol_instance(x[['call']])  # last symbol maybe touched
  )
  alloc <- init_dat(x[['call']], meta=meta, scope=0L)
  # Status control placeholder.
  sts.vec <- numeric(IX[['STAT.N']])
  vdat <- vec_dat(sts.vec, "sts", typeof='double')
  alloc <- append_dat(
    alloc, vdat=vdat, depth=0L, call.i=0L, rec=0L, branch.lvl=0L
  )
  # Result placeholder, to be alloc'ed once we know group sizes.
  vdat <- vec_dat(numeric(), type="res", typeof='double')
  alloc <- append_dat(
    alloc, vdat=vdat, depth=0L, call.i=0L, rec=0L, branch.lvl=0L
  )

  # Add group data.
  if(!all(nzchar(names(data)))) stop("All data must be named.")
  data.naked <- data[is.num_naked(data)]
  data.used <- integer()
  for(i in seq_along(data.naked)) {
    datum <- data.naked[[i]]
    dname <- names(data.naked)[i]
    typeof <- typeof(datum)
    vdat <- vec_dat(datum, type="grp", typeof=typeof, size.coef=list(0:1))
    alloc <-
      append_dat(alloc, vdat=vdat, depth=0L, name=dname, call.i=0L, rec=0L)
    guard_symbol(dname, env.ext)
  }
  # Bump scope so the data cannot be overwritten
  alloc[['scope']] <- alloc[['scope']] + 1L

  # Stacks to track name state and call index before/after control flow branches
  binding.stack <- list()         # track after first branch
  branch.start.stack <- integer()

  # - Process ------------------------------------------------------------------

  # Compute how much temporary storage we need to track all the intermediate
  # calculations in the call tree, and to allocate all the vectors into a data
  # structure.  This data structure will also include references to external
  # vectors (if there are such references).  The result of evaluating
  # control/flag parameters is recorded separately in `call.dat`.
  stack <- init_stack()
  stack.ext.any <- list()
  stack.sizes <- list()   # argument sizes (see size.R)
  call.dat <- list()
  branch.lvl <- 0L
  external.evals <- list()
  call.names <- vapply(x[['linfo']], "[[", "", "name")
  call.pkgs <- vapply(x[['linfo']], "[[", "", "pkg")

  for(i in seq_along(x[['call']])) {
    ext.id <- ""
    par.type <- x[['par.type']][[i]]
    par.ext <- par.type %in% PAR.EXT

    call <- x[['call']][[i]]
    depth <- x[['depth']][[i]]
    argn <- x[['argn']][[i]]
    pkg <- name <- ""
    if(par.type != PAR.EXT.ANY) {
      name <- call.names[i]
      pkg <- call.pkgs[i]
    }
    id <- if(par.type != PAR.INT.CALL) name_to_id(alloc, name) else 0L
    vec.dat <- init_vec_dat()

    # reconciliation level
    rec <- x[['rec']][[i]]
    if(rec && !branch.lvl) stop("Internal Error: rec found out of branch.")
    rec <- rec * branch.lvl

    # - Process Call -----------------------------------------------------------
    if(par.type == PAR.INT.CALL) {
      # Internal call to evaluate: use accrued stack and function type to
      # compute result size.  Allocation and stack reduction happen later.
      check_fun(name, pkg, env)
      ftype <- VALID_FUNS[[c(name, "type")]] # see cgen() docs

      # Check inputs are valid types (subset/subassign with logical not valid)
      stack.input <- stack_inputs(stack, depth)
      input.type <- alloc[['typeof']][stack.input['id', ]]
      names(input.type) <- colnames(stack.input)
      VALID_FUNS[[c(name, "in.type.validate")]](input.type)

      # Preserve input type of logical/integer if inputs and function allow it
      res.type.mode <- VALID_FUNS[[c(name, "res.type")]]
      res.typeof <- if(grepl('^preserve', res.type.mode)) {
        if(res.type.mode == "preserve.last") {
          input.type[length(input.type)]
        } else if (res.type.mode == "preserve.which") {
          type.which <- VALID_FUNS[[c(name, "res.type.which")]]
          NUM.TYPES[max(match(input.type[type.which], NUM.TYPES))]
        } else {
          min.type <- if(res.type.mode == 'preserve') 1L else 2L
          NUM.TYPES[max(c(match(input.type, NUM.TYPES), min.type))]
        }
      } else VALID_FUNS[[c(name, "res.type")]]

      # For control structures, determine if result is used so that we know
      # whether to enforce eqlen or not.  Alternatively we could try to mark
      # this at the `copy_branchdat` level to keep this branch result use
      # detection in one place, but that requires marking the call tree.

      waive.eqlen <-
        if(name %in% BRANCH.EXEC.SYM) !branch_used(i, call.names, x[['depth']])
        else FALSE

      # Compute expression result size
      size.tmp <- compute_size(
        alloc, stack, depth, gmax=gmax, gmin=gmin, ftype=ftype,
        waive.eqlen=waive.eqlen, call=call, .CALL=.CALL
      )
      size.coef <- size.tmp[['size.coef']] # iteration/group dependant size
      asize <- size.tmp[['asize']]         # required allocation size

      # Cleanup expired symbols, and bind new ones
      alloc <- names_update(
        alloc, call, call.name=name, call.i=i, rec=rec, env.ext=env.ext
      )
      # Prepare new vec data (if any), and tweak objet depending on situation.
      # Alloc is made later, but only if vec.dat[['new']] is not null.
      vec.dat <- vec_dat(NULL, "tmp", typeof=res.typeof, size.coef=size.coef)
      if(!name %in% c(PASSIVE.SYM, MODIFY.SYM)) {
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
        # unchanged for use by the next call, except we do update the `typeof`
        # for e.g. when logical gets turned to numeric by uplus
        alloc[['typeof']][alloc[['i']]] <- res.typeof
      } else stop("Internal Error: unexpected call allocation state.")

    # - Assigned-To Symbol -----------------------------------------------------
    } else if(x[['assign']][i]) {
      # in e.g. `x <- y`, this is the `x`, which isn't actually data,
      # We don't need to record it but we do for consistency.  The values here
      # shouldn't really get used anywhere where they have an impact.
      vec.dat <-
        vec_dat(numeric(), "tmp", typeof="logical", size.coef=list(integer()))
    # - Control Parameter / External -------------------------------------------
    } else if (par.ext || !id) {
      # ext.any evals should not mix with internal values.
      if(id && par.type == PAR.EXT.ANY)
        stop("Internal Error: name-external exp conflict.")
      if(par.ext && id) {
        # An id should never resolve to 'ext' anyway...
        if(!alloc[['type']][id] %in% c('ext')) {
          stop(
            "Parameter `", argn ,"` must resolve to iteration invariant ",
            "external data."
      ) } }
      if(is.language(call)) {
        # Cache the call so we don't re-evaluate if re-encounter.  Particularly
        # important if we reference same external symbol multiple times and it
        # is e.g. integer, which would require coercion to numeric each time.
        # Drawback is expressions with side-effects will not work correctly.
        if(par.type %in% PAR.EXT) {
          ext.id <- paste0(deparse(call, control="all"), collapse="\n")
          if(!nzchar(ext.id))
            stop("Symbol with zero length name disallowed.")
          if(!is.null(external.evals[[ext.id]])) {
            id <- external.evals[[ext.id]]
            if(!id) stop("Internal Error: external.evals cache corrupted.")
            # Similar logic "Symbol in Data" Section
            alloc[['i']] <- id
            # Update symbol depth (from Symbol in Data, maybe N/A here?)
            if(alloc[['depth']][id] > depth) alloc[['depth']][id] <- depth
          }
      } }
      # Need to eval parameter if not cached; `env` contains MoreArgs.
      # Here we need to intercept any attemps to use internal parameters. At the
      # most basic this means create a fake `data` environment that has the
      # special active bindings with the `env` environment as the parent.  Do we
      # add a child environment?  What about branches and nested functions?
      if(!id) {
        tryCatch(
          arg.e <- eval(call, envir=data, enclos=env.ext),
          error=function(e) stop(simpleError(conditionMessage(e), call.outer)),
          internalSymbolAccess=function(e) {
            call.dep <- deparseLines(call)
            stop(
              simpleError(
                paste0(
                  "External parameter expression",
                  if(grepl("\n", call.dep)) paste0("\n:", call.dep, "\n")
                  else paste0(" `", call.dep, "` "),
                  "attempted to access internal symbol `",
                  conditionMessage(e), "`."
                ),
                call.outer
          ) ) }
        )
        # External params need to be validated
        if(
          par.type %in% PAR.EXT &&
          !isTRUE(err.msg <- x[['par.validate']][[i]](arg.e))
        ) {
          stop(
            simpleError(
              paste0("Invalid external parameter: ", err.msg), call.outer
          ) )
        }
        # Other post processing/recording.
        if(par.type == PAR.EXT.ANY) {
          if(!nzchar(argn))
            stop("Internal Error: missing arg name for non-num external")
          ext.any <- list(arg.e)
          names(ext.any) <- argn
          stack.ext.any <- c(stack.ext.any, ext.any)
        } else {
          # These must all be naked numeric; it does result in double validation
          # for external evals that correspond to external parameters.
          if(!par.type %in% PAR.INT && !nzchar(argn))
            stop("Internal Error: missing arg name for external param")
          validate_ext(x, i, par.type, arg.e, name, call, .CALL)
          typeof <- typeof(arg.e)
          size.coef <- list(length(arg.e))
          vec.dat <- vec_dat(arg.e, "ext", typeof=typeof, size.coef=size.coef)
        }
      }
    # - Match a Symbol In Data -------------------------------------------------
    } else if (id) {  # see prior else if for `id`
      # Similar logic in Control/ExternalSection
      alloc[['i']] <- id
      # Update symbol depth (needed for assigned-to symbols)
      if(alloc[['depth']][id] > depth) alloc[['depth']][id] <- depth
      data.used <- union(data.used, id)
    } else stop("Internal Error: unexpected token.")

    # - Update Stack / Data ----------------------------------------------------

    # Append new data to our data array.  Not all calls produce data; those
    # that do have non-NULL vec.dat[['new']].
    if(!is.null(vec.dat[['new']])) {
      alloc <- append_dat(
        alloc, vec.dat, depth=depth, call.i=i, rec=rec, branch.lvl=0L
      )
      # Cache if external and generated a ext.id
      if(nzchar(ext.id)) external.evals[[ext.id]] <- alloc[[i]]
    }
    # Call actions that need to happen after allocation data updated
    if(par.type == PAR.INT.CALL) {
      # Release allocation after call parameters incorporated
      alloc <- alloc_free(alloc, depth)
      # Append call data (different than append_dat)
      call.dat <- append_call_dat(
        call.dat, call=call, call.name=name,
        stack=stack, stack.ext.any=stack.ext.any, depth=depth,
        alloc=alloc, call.i=i
      )
      # Handle name reconciliation for control flow calls.  Recall that call
      # linearization causes paramters to be seen before the call that computes
      # on them.  Also, because we make the control flow test a separate
      # expression (e.g `if()` becomes `if_test(); r2c_if()`) the test and
      # execution calls sandwich all the branch calls.
      if (name %in% BRANCH.START.SYM) {
        # First in-branch expression after this one
        branch.lvl <- branch.lvl + 1L
        branch.start.stack <- c(branch.start.stack, i)

        # Generate a new external tracking env for the TRUE branch
        env.ext <- env.ext.T <- new.env(parent=env.ext)
      } else if (name %in% BRANCH.EXEC.SYM) {
        # Last branch expression before this one
        branch.lvl <- branch.lvl - 1L
      } else if(name %in%  BRANCH.MID.SYM) {
        # Just completed TRUE branch
        # Needs to be stack to handle , e.g. in `if(a) b else {if(c) d}`
        binding.stack <- c(
          binding.stack, list(list(names=alloc[['names']], i.call=i))
        )
        # Hide all the names bound between here and the matching 'if_test'
        # Unhiding done by `reconcile_control_flow`.  This prevents TRUE branch
        # assignments from being seen in FALSE branch.
        names.assign <- alloc[['names']]['i.assign',]
        names.assign.in.br <-
          names.assign > branch.start.stack[length(branch.start.stack)]
        alloc[['names']]['br.hide', names.assign.in.br] <- branch.lvl

        # External env for FALSE branch
        env.ext <- parent.env(env.ext.T)
        env.ext <- env.ext.F <- new.env(parent=env.ext)
      } else if (name %in% BRANCH.END.SYM) {
        # Just completed FALSE branch, reconcile allocations, symbols, etc.
        if(length(x[['call']]) < i + 1L)
          stop("Internal Error: missing outer control after control components.")
        rcf.dat <- reconcile_control_flow(
          alloc, call.dat, stack, binding.stack, i.call=i,
          depth=depth, gmax=gmax, gmin=gmin, branch.lvl=branch.lvl,
          # send full control call for error message
          call=x[['call']][c(tail(branch.start.stack, 1L), i + 1L)]
        )
        alloc <- rcf.dat[['alloc']]
        call.dat <- rcf.dat[['call.dat']]
        stack <- rcf.dat[['stack']]
        binding.stack <- binding.stack[-length(binding.stack)]
        branch.start.stack <- branch.start.stack[-length(branch.start.stack)]
        env.ext <- reconcile_env_ext(env.ext.T, env.ext.F)
      }
      # Reduce stack
      stack <- stack[,stack['depth',] <= depth, drop=FALSE]
      stack.ext.any <- list()
    }
    # Append new data/computation result to stack
    if(par.type != PAR.EXT.ANY)
      stack <- append_stack(stack, alloc=alloc, depth=depth, argn=argn)
  }
  # - Finalize -----------------------------------------------------------------

  # Return value should be redirected to result
  last.call <- call.dat[[length(call.dat)]]
  last.alloc <- last.call[['ids']][length(last.call[['ids']])]
  res.alloc <- which(alloc[['type']] == "res")
  for(i in seq_along(call.dat)) {
    id.replace <- call.dat[[i]][['ids']] == last.alloc
    call.dat[[i]][['ids']][id.replace] <- res.alloc
  }
  alloc[['i']] <- res.alloc
  alloc[['typeof']][res.alloc] <- alloc[['typeof']][last.alloc]

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
    alloc[c('dat', 'alloc', 'depth', 'type', 'typeof', 'size.coefs')],
    "[", ids.keep
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
## however be re-used within a group if the originally allocated vector is no
## longer needed, and each new group will re-use the allocations from the prior
## group.  Allocations are sized to accommodate the largest group.
##
## This is a description of the input `dat` (which is also the returned value
## after update).
##
## * dat: (this is `dat[['dat']]`) the actual data, for "tmp" type (i.e.
##   generated by computation of a sub-call) this will be written to so should
##   not be accessible via R.
## * ids0: a unique identifier for each allocation, differs from column rank in
##   `dat` as soon as there is a re-use of a previous allocation.  This is a
##   sentinel to detect deviations between the stack and allocations.
## * type: one of "tmp" (allocated), "grp" (from the iteration/group variant
##   data, "ext" (any other data vector), "res" (the result), "sts"
##   (status flags, e.g. recycle warning).
## * typeof: the intended data format at the end of the computation.  Used to
##   try to track whether a vector could be interpreted as logical or integer.
## * alloc: the true size of the vector (should be equivalent to
##   `lengths(dat[['dat']])`?).
## * size.coefs: a list of `size.coef` elements as described in `compute_size`.
## * depth: the depth at which allocation occurred, only relevant for
##   `type == "tmp"`
## * i: scalar integer the index in `data` of the most recently
##   allocated/appended/used item(s).  This will point to the result of the most
##   recent calculation.  It does not need to be the last vector as we allow
##   re-use of free vectors.
## * names: a matrix where the (possibly duplicated) column names are symbols,
##   row `ids` are the ids in `dat` each symbol is bound to, `scope` is the
##   scope level the symbol was created in, and `i.max` is the largest index in
##   the linearized call list that the symbol exists in as a leaf (indicating
##   that beyond that the name binding need not prevent release of memory),
##   `i.assign` the index in which the symbol was bound, and `br.hide` is the
##   branch level the symbols assigned in the TRUE branch need to be hidden from
##   (so the FALSE branch can't see them).
## * rec: see `rec` parameter.
##
## We're mixing return value elements and params, a bit, but there are some
## differences, e.g.:
##
## @param dat the allocation data structure
## @param vdat see vec_dat
## @param name group data comes with names, or things that are assigned to
##   symbols
## @param rec whether current call is part of a rec chain
##   (e.g. `x <- z <- rec(vcopy(y))`)
## @return dat, updated

append_dat <- function(
  dat, vdat, name=NULL, depth, call.i, rec, branch.lvl
) {
  if(is.null(name)) name <- ""
  if(!is.vec_dat(vdat)) stop("Internal Error: bad vec_dat.")
  type <- vdat[['type']]
  typeof <- vdat[['typeof']]
  new <- vdat[['new']]
  size.coef <- vdat[['size.coef']]
  if(is.null(new)) stop("Internal Error: cannot append null data.")

  if(!is.num_naked(list(new))) stop("Internal Error: bad data column.")
  if(!type %in% c("res", "grp", "ext", "tmp", "sts"))
    stop("Internal Error: bad type.")
  if(type != "tmp" && (length(size.coef) != 1L || any(lengths(size.coef)) > 2L))
    stop("Internal Eror: complex sizes only for computed allocs.") # and res?

  if(any(lengths(size.coef) > 1L) && !type %in% c("grp", "tmp"))
    stop("Internal Eror: complex sizes only for temporary allocs or group.")

  new.num <- if(is.integer(new) || is.logical(new)) as.numeric(new) else new
  dat[['dat']] <- c(dat[['dat']], list(new.num))

  dat[['i']] <- length(dat[['dat']])
  id0.new <- dat[['id0']] <- dat[['id0']] + 1L

  # need to test whether data.frame would slow things down too much
  dat[['ids0']] <- c(dat[['ids0']], id0.new)
  dat[['alloc']] <- c(dat[['alloc']], length(new))         # true size
  dat[['size.coefs']] <- c(dat[['size.coefs']], list(size.coef)) # list of lists
  dat[['depth']] <- c(dat[['depth']], depth)
  dat[['type']] <- c(dat[['type']], type)
  dat[['typeof']] <- c(dat[['typeof']], typeof)

  if(length(unique(lengths(dat[ALLOC.DAT.VEC]))) != 1L)
    stop("Internal Error: irregular vector alloc data.")

  # Append dat should never overwrite names in the existing scope
  names <- dat[['names']]
  if(
    name %in% colnames(names[,names['scope',] == dat[['scope']], drop=FALSE])
  )
    stop("Internal error: cannot append names existing in current scope.")
  if(nzchar(name)) dat <- names_bind(dat, name, call.i, rec)

  dat
}
## We Have Unused Allocations to Reuse
reuse_dat <- function(alloc, fit, vec.dat, depth) {
  stopifnot(is.vec_dat(vec.dat))
  target <- which.min(fit)
  slot <- seq_along(alloc[['dat']])[fit][target]

  update.vecs <- setdiff(ALLOC.DAT.VEC, c('ids0', 'alloc', 'depth'))
  for(i in update.vecs) alloc[[i]][slot] <- vec.dat[[i]]

  alloc[['depth']][slot] <- depth
  alloc[['ids0']][slot] <- alloc[['id0']] <- alloc[['id0']] + 1L
  alloc[['i']] <- slot
  alloc
}
init_dat <- function(call, meta, scope) {
  data <- list(
    # Allocation data
    dat=list(),
    names=rbind(
      ids=integer(), scope=integer(),
      i.max=integer(), i.assign=integer(), rec=integer(),
      br.hide=integer()
    ),

    # Equal length vector data
    alloc=numeric(),
    size.coefs=list(),
    depth=integer(),
    ids0=integer(),
    type=character(),
    typeof=character(),

    # Other data
    i=0L,
    id0=0L,
    scope=scope,
    meta=meta
  )
  if(
    !all(ALLOC.DAT.VEC %in% names(data)) &&
    !all(lengths(data[ALLOC.DAT.VEC]) == 0L)
  )
    stop("Internal Error: Bad alloc data initialization.")

  colnames(data[['names']]) <- character()
  data
}
init_vec_dat <- function() {
  list(
    new=NULL, type=NA_character_, typeof=NA_character_, group=NA_real_,
    size.coef=list(integer())
  )
}
# See `append_dat` for details on what the parameters are.

vec_dat <- function(
  new=NULL, type=NA_character_, typeof=NA_character_,
  size.coef=list(length(new))
) {
  vec.dat <- list(new=new, type=type, typeof=typeof, size.coef=size.coef)
  stopifnot(is.vec_dat(vec.dat))
  vec.dat
}
is.vec_dat <- function(x)
  is.list(x) &&
  all(c('new', 'type', 'size.coef', 'typeof') %in% names(x)) &&
  is.character(x[['type']]) &&
  isTRUE(x[['type']] %in% c("res", "grp", "ext", "tmp", "sts")) && (
    is.numeric(x[['new']]) || is.integer(x[['new']]) ||
    is.logical(x[['new']]) || is.null(x[['new']])
  ) &&
  # See `append_dat`
  is.size_coef(x[['size.coef']]) && (
    x[['type']] == "tmp" ||
    length(x[['size.coef']]) == 1L && (
      (x[['type']] == "grp" && length(x[['size.coef']][[1L]] == 2L)) ||
      length(x[['size.coef']][[1L]] == 1L)
  ) ) &&
  is.character(x[['typeof']]) && length(x[['typeof']]) == 1L &&
  isTRUE(x[['typeof']] %in% c("double", "integer", "logical"))

## Stack used to track parameters ahead of reduction when processing call.
init_stack <- function() {
  matrix(
    numeric(), nrow=3L,
    dimnames=list(
      c(
        'id',      # index in our allocated data structure
        'id0',     # unique id for integrity checks
        'depth'
      ),
      NULL
) ) }
append_stack <- function(stack, alloc, id=alloc[['i']], depth, argn) {
  if(!id) stop("Internal Error: an alloc id must be specified.")
  id0 <- if(!id) id else alloc[['ids0']][id]
  stack <- cbind(stack, c(id, id0, depth))
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
## * name: the name of the function that's being called (excludes `pkg::` part)
## * stack.ext.any: evaluated external parameters that can be non-numeric.

append_call_dat <- function(
  call.dat, call, call.name, stack, stack.ext.any, alloc, depth, call.i
) {
  if(!is.call(call))
    stop("Internal Error: only calls should be appended to call dat.")

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
  c(
    call.dat,
    list(
      list(
        call=call, name=call.name, ids=ids,
        stack.ext.any=stack.ext.any, call.i=call.i
    ) )
  )
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
  alloc[['size.coefs']][slot] <- vdat[['size.coef']]
  alloc
}
# Reconcile Allocations Across Branches
#
# Based on how the preprocessor works, we know that allocations that are bound
# to the same symbol after a branch, and were `rec`ed, must be redirected to
# the same allocation.  The `rec`ed symbols are those that are used later in
# the call.
#
# We compare the snapshot of bindings at the end of the "TRUE" branch (recall
# for loops this is the body of the loop) to that at the end of the "FALSE"
# branch (for loops this is the r2c generated for0 (i.e. loop not taken/0
# iteration loop) branch).  Every time we encounter the FALSE branch call we can
# look at the end of `binding_stack` as that will be the snapshot from the most
# recent prior "TRUE" branch, and the preprocessor ensures that "TRUE" and
# "FALSE" branches are always paired (injecting "empty" branches as
# necessary).
#
# See "preproc-copy.R" for context.
#
# @param binding.stack list of snapshots of bindings as of the completion of the
#   "TRUE" branches.  See details.

reconcile_control_flow <- function(
  alloc, call.dat, stack, binding.stack, i.call, call, depth, gmax, gmin,
  branch.lvl
) {
  if(!length(binding.stack))
    stop(
      "Internal Error: nothing to pop from `binding.stack`, mismatched if/else?"
    )
  names.F <- alloc[['names']]
  names.T <- binding.stack[[length(binding.stack)]][['names']]
  i.call.F <- i.call
  i.call.T <- binding.stack[[length(binding.stack)]][['i.call']]
  call.dat.i <- vapply(call.dat, '[[', 0L, 'call.i')

  # Identify shared bindings across branches that need to be reconciled.
  names.rc.F <- cand_rec_bind(names.F, branch.lvl)
  names.rc.T <- cand_rec_bind(names.T, branch.lvl)
  if(!identical(colnames(names.rc.F), colnames(names.rc.T)))
    stop("Internal Error: mismatched symbols to reconcile.")

  # Drop those that are the same across the branches (set prior to if/else)
  # Not sure if there should be any of these given rec == branch.lvl
  names.rc.drop <- colSums(names.rc.F == names.rc.T) == nrow(names.rc.F)
  names.rc.F <- names.rc.F[, !names.rc.drop, drop=FALSE]
  names.rc.T <- names.rc.T[, !names.rc.drop, drop=FALSE]

  # Identify return values from both branches and check if they need to
  # be reconciled (if no set to 0)
  id.ret.F <- cand_ret(call.dat, call.dat.i, i.call.F)
  id.ret.T <- cand_ret(call.dat, call.dat.i, i.call.T)
  if(xor(length(id.ret.F), length(id.ret.T)))
    stop("Internal Error: inconsistent reconcile for branch return value.")
  rec.ret <- length(id.ret.F) > 0L

  # All ids that need to be reconciled (tack return at end if needed)
  id.rc.F <- c(names.rc.F['ids',], id.ret.F)
  id.rc.T <- c(names.rc.T['ids',], id.ret.T)
  rc.sym.names <- c(colnames(names.rc.F), if(rec.ret) "<return-value>")

  # Find sizes (they should be the same).  Compare pair wise.
  size.coef.F <- alloc[['size.coefs']][id.rc.F]
  size.coef.T <- alloc[['size.coefs']][id.rc.T]
  size.eq <- vapply(
    seq_along(size.coef.F),
    function(i)
      length(size_eqlen(list(size.coef.F[[i]], size.coef.T[[i]]), gmax, gmin)),
    0L
  )
  if(any(size.neq <- size.eq != 1L)) {
    # Reconstitute the call
    call.rec <- clean_call(call("{", call[[1L]], call[[2L]]), level=2L)
    # this is going to make zero sense to the user since we don't expose the
    # univariate polynomial business in docs anywhere.
    ssT <- paste0(size_coefs_as_string(size.coef.T[size.neq]), collapse="; ")
    ssF <- paste0(size_coefs_as_string(size.coef.F[size.neq]), collapse="; ")
    stop(
      "Assigned variables and return value must be same size across branches; ",
      "potential size discrepancy for ",
      toString(
        sprintf(
          "`%s` (TRUE: %s vs FALSE: %s)", rc.sym.names[size.neq], ssT, ssF
      ) ),
      " in:\n", deparseLines(call.rec)
    )
  }
  # All the targets for reconciliation must be `r2c` allocated.
  if(any(alloc[['type']][c(id.rc.F, id.rc.T)] != 'tmp'))
    stop("Internal Error: reconciliation allocs not r2c generated.")

  # Allocations to remap should all be branch local. To check find matching
  # 'if_test' and confirm all allocs are inside.
  call.i.T <- which(call.dat.i == i.call.T)
  call.names <- vapply(call.dat, "[[", "", "name")
  branch.levels <-
    cumsum(call.names %in% BRANCH.START.SYM) -
    cumsum(call.names %in% BRANCH.END.SYM)
  branch.cur <- branch.levels[call.i.T]
  branch.cur.start <- min(which(branch.levels == branch.cur))
  if(!is.finite(branch.cur.start) || branch.cur == 0L)
    stop("Internal Error: mismatched branch start and end symbols.")
  branch.cur.start.i <- call.dat.i[branch.cur.start]

  alloc.i.F <- c(names.rc.F['i.assign',], i.call.F)
  alloc.i.T <- c(names.rc.T['i.assign',], i.call.T)
  if(any(alloc.i.F <= i.call.T | alloc.i.F > i.call.F))
    stop("Internal Error: reconcile allocation not branch local in FALSE.")
  if(any(alloc.i.T <= branch.cur.start.i | alloc.i.T > i.call.T))
    stop("Internal Error: reconcile allocation not branch local in TRUE.")

  # Reconcile allocations so they point to the same id.  We must use entirely
  # new allocations because we are not tracking how and where the currently
  # available allocations have been used in the different branches.  Step 1 is
  # to create the allocations, Step 2 is to re-point the call data to them.
  typeof.F <- alloc[['typeof']][id.rc.F]
  typeof.T <- alloc[['typeof']][id.rc.T]
  alloc.i.old <- alloc[['i']]

  for(i in seq_along(id.rc.F)) {
    # Size and generate allocation
    size.coef <- size.coef.T[[i]]
    asize <- compute_asize_from_size(size.coef, gmax)
    new <- numeric(asize)
    # Take the most general type from the two branches
    typeof.num <- match(c(typeof.F[i], typeof.T[i]), NUM.TYPES)
    typeof <- NUM.TYPES[max(typeof.num)]
    vec.dat <- vec_dat(new, "tmp", typeof=typeof, size.coef=size.coef)
    # rec=0L b/c reconciliation decisions made on 'rec' data from names matrix,
    # so this value should not be used anymore.
    alloc <- append_dat(
      alloc, vec.dat, depth=depth, rec=0L, branch.lvl=branch.lvl
    )
    # Adjust the call data for the id remapping.  This should be done for all
    # calls within the branch
    new.i <- alloc[['i']]
    new.id0 <- alloc[['ids0']][new.i] # might be the same as new.i always?
    call.dat <- update_cdat_alloc(  # true branch
      call.dat, old=id.rc.T[i], new=new.i,
      start=branch.cur.start.i + 1L, end=i.call.T
    )
    call.dat <- update_cdat_alloc(  # false branch
      call.dat, old=id.rc.F[i], new=new.i,
      start=i.call.T + 1L, end=i.call.F
    )
    # Also update the names matrix.  Just match the FALSE branch location.
    # Last value is the return value, so no names to update for that.
    if(i < length(id.rc.F) || !rec.ret) {
      # Free names at prior level
      alloc <- names_free(alloc, rc.sym.names[i], branch.lvl - 1L)
      # Find the entry in the names matrix we're updated
      names.assign <- alloc[['names']]['i.assign',]
      names.target <- which(
        names.rc.F['i.assign', i] == alloc[['names']]['i.assign',] &
        alloc[['names']]['ids',] == id.rc.F[i]
      )
      if(length(names.target) != 1L)
        stop('Internal Error: failed to find reconciled name in alloc data.')

      # Reset to the reoncile allocation
      alloc[['names']]['ids', names.target] <- new.i
      # Update branch level
      alloc[['names']]['rec', names.target] <- branch.lvl - 1L
    }
    # Finally update stack just in case
    stack.up <- c('id', 'id0')
    stack.up.val <- c(new.i, new.id0)
    stack[stack.up, stack['id',] == id.rc.F[i]] <- stack.up.val
    stack[stack.up, stack['id',] == id.rc.T[i]] <- stack.up.val
  }
  # In the case of no result reconciliation, reset the alloc.id
  if(!rec.ret) alloc[['i']] <- alloc.i.old

  # Undo branch hiding
  hidden <- alloc[['names']]['br.hide', ] == branch.lvl
  alloc[['names']]['br.hide', hidden] <- branch.lvl - 1L

  list(alloc=alloc, call.dat=call.dat, stack=stack)
}
# Names Defined in Both Branches Copied over to Mutual Parent
#
# See `guard_symbol`

reconcile_env_ext <- function(env.ext.T, env.ext.F) {
  par.env <- parent.env(env.ext.T)
  if(!identical(par.env, parent.env(env.ext.F)))
    stop("Internal Error: corrupted external tracking envs.")
  new.names <- intersect(names(env.ext.T), names(env.ext.F))
  for(i in new.names) guard_symbol(i, par.env)
  par.env
}
#
# Identify Bindings that Should Be Reconciled

cand_rec_bind <- function(names, branch.lvl) {
  names.rc <- names[, names['rec',] == branch.lvl, drop=FALSE]
  rc.nm <- sort(colnames(names.rc))
  if(is.null(rc.nm)) rc.nm <- character()
  if(anyDuplicated(rc.nm))
    stop("Internal Error: duplicate symbols to reconcile.")
  # reorder (should be safe since no dup symbols)
  names.rc[, rc.nm, drop=FALSE]
}
# Determine whether branch return values need reconciliation

cand_ret <- function(call.dat, call.dat.i, i.call) {
  # Identify return values from both branches and check if they need to
  # be reconciled (if no set to 0)
  call.i <- which(call.dat.i == i.call)
  call.dat.sub <- call.dat[[call.i]]
  if(is.rec_ret(call.dat.sub[['call']]))
    call.dat.sub[['ids']][length(call.dat.sub[['ids']])]
  else integer()
}
# Determine if Call Return is Rec Tagged

is.rec_ret <- function(x) {
  if(is.call(x)) {
    call.sym <- get_lang_name(x)
    if(call.sym == "rec") TRUE
    else if(call.sym %in% PASSIVE.SYM && !call.sym %in% MODIFY.SYM)
      is.rec_ret(x[[length(x)]])
    else FALSE
  } else FALSE
}

# Update Call Data Allocations
#
# Find all the in-branch usage of the allocation that needs to be reconciled,
# and point them to the new allocation.
#
# @param old the id of the old allocation (from alloc[['alloc']])
# @param new the id of the new allocation
# @param start the point at which the symbol we're reconciling was assigned
# @parem end the end of the current branch

update_cdat_alloc <- function(call.dat, old, new, start, end) {
  # Call dat does not contain every index in the original call list, so we need
  # to map those indices to those available in call.dat
  c.i <- vapply(call.dat, '[[', 1L, 'call.i')

  # In the eligible range, remap all the ids
  for(i in which(c.i >= start & c.i <= end)) {
    call.dat[[i]][['ids']][call.dat[[i]][['ids']] == old] <- new
  }
  call.dat
}


# - Other Helper Functions -----------------------------------------------------

# Retrieve parent id from linearized call list
#
# @param depths the call depth of every sub-call
# @param i the current call index in the linearized call list

linear_parent <- function(i, depths) {
  depth <- depths[i]
  parent.cand <- which(seq_along(depths) > i & depths < depth)
  if(length(parent.cand)) min(parent.cand) else 0L
}
# Determine if a branch return value is used, can only ever return FALSE if a
# branch is involved.
#
# This should be used conditional on the first call being a BRANCH.EXEC.SYM.

branch_used <- function(i, call.names, depths) {
  continue.syms <- c(BRANCH.EXEC.SYM, BRANCH.MID.SYM, BRANCH.END.SYM, "{")
  name <- call.names[i]
  used <- TRUE
  if(name %in% continue.syms) {
    i.par <- linear_parent(i, depths)
    if(i.par) {
      name.par <- call.names[i.par]
      if(name.par == "{" && i.par > i + 1L) {
        used <- FALSE
      } else if (name.par %in% continue.syms) {
        used <- branch_used(i.par, call.names, depths)
      } else TRUE
    }
  }
  used
}


#' Find Latest Symbol Instance
#'
#' This currently allows control/flag parameter symbols to count, even though
#' we're settling on them having different lookup semantics.  So this will be
#' cause some inefficiency in some cases where a symbol will not be released as
#' early as it should just because it's used as a control/flag.
#'
#' If a symbol is re-assigned, it will not be released until the last use after
#' the final re-assign.  This means earlier assignments could be held onto
#' longer than needed if e.g. there is a gap between last use from previous
#' assignment and re-assignment.
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
  # symbols used by each loop.  We will pretend each of these is potentially
  # used up to the last point of the loop.  DO NOT CHANGE this assumption
  # lightly as we depend on it for bindings that are used before write in loops.
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
  call.active <- vapply(
    x[calls], function(x) !get_lang_name(x) %in% PASSIVE.SYM, TRUE
  )
  call.active.i <- seq_along(x)[calls][call.active]
  if(!length(call.active.i))
    stop("Internal Error: no active return call found.")
  max(call.active.i)
}
# Retrieve Inputs to Current Call From Stack

stack_inputs <- function(stack, depth)
  stack[, stack['depth',] == depth + 1L, drop=FALSE]


## Check function validity
check_fun <- function(name, pkg, env) {
  if(!name %in% names(VALID_FUNS))
    stop("`", name, "` is not a supported function.")
  got.fun <- try(
    if(nzchar(pkg)) eval(call("::", as.name(pkg), as.name(name)), envir=env)
    else get(name, envir=env, mode="function"),
    silent=TRUE
  )
  if(inherits(got.fun, "try-error")) {
    stop(
      "Failed retrieving `", name, "` with error: ",
      conditionMessage(attr(got.fun, 'condition'))
    )
  }
  if(!identical(got.fun, VALID_FUNS[[c(name, "fun")]])) {
    tar.fun <- VALID_FUNS[[c(name, "fun")]]
    env.fun <-
      if(is.null(environment(tar.fun))) getNamespace("base")
      else environment(tar.fun)
    stop(
      "Symbol `", name, "` does not resolve to the expected function from ",
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
names_free <- function(alloc, new.names, rec) {
  names <- alloc[['names']]
  to.free <-
    names['scope', ] == alloc[['scope']] &
    names['rec', ] == rec &
    colnames(names) %in% new.names
  alloc[['names']] <- names[, !to.free, drop=FALSE]
  alloc
}
names_bind <- function(alloc, new.name, call.i, rec) {
  new.name.dat <- c(
    ids=alloc[['i']],
    scope=alloc[['scope']],
    i.max=unname(alloc[['meta']][['i.sym.max']][new.name]),
    i.assign=call.i,
    rec=rec,
    br.hide=0L
  )
  if(is.na(new.name.dat['i.max'])) new.name.dat['i.max'] <- 0L
  alloc[['names']] <- cbind(alloc[['names']], new.name.dat)
  colnames(alloc[['names']])[ncol(alloc[['names']])] <- new.name
  alloc
}
names_update <- function(alloc, call, call.name, call.i, rec, env.ext) {
  alloc <- names_clean(alloc, call.i)
  if(call.name %in% ASSIGN.SYM) { # not MODIFY.SYM
    # Remove protection from prev assignment to same name, and bind previous
    # computation (`alloc[[call.i]]`) to it.
    sym <- get_target_symbol(call, call.name)
    alloc <- names_free(alloc, sym, rec)
    alloc <- names_bind(alloc, sym, call.i, rec)

    # Add a forbidden binding to the external environment
    guard_symbol(sym, env.ext)
  }
  alloc
}
# Prevent External Expression from Accessing Internal Symbols
#
# External expressions are disallowed from accessing iteration varying symbols.
# We implement these by setting for each iteration varying symbol (a data symbol
# or a symbol set in an r2c expression) an active binding that triggers a
# condition if it is used.  This way, anytime we evaluate an external
# expression, we will detect any internal symbols used.
#
# This is not a foolproof implementation as someone can always use expressions
# like `eval` to either corrupt these environments, but it should detect the
# typical accidental case.
#
# Add special symbols to env that trigger conditions when they are used so that
# we may detect use of internal symbols when evaluating external parameters.

guard_symbol <- function(name, env) {
  cond <- simpleCondition(name)
  class(cond) <- c('internalSymbolAccess', class(cond))
  makeActiveBinding(name, function() signalCondition(cond), env)
}
## Lookup a Name In Storage List
##
## Will find data vectors as well as any live assigned vectors that were
## generated.

name_to_id <- function(alloc, name) {
  names <- alloc[['names']]
  res <- 0L
  if(ncol(names)) {
    hide.lvl <- max(names['br.hide',])
    visible <- names['br.hide',] != hide.lvl | hide.lvl == 0L
    # Only look through non hidden names (hidden are those in the "TRUE" branch)
    names.v <- names[, visible, drop=FALSE]
    if (id <- match(name, rev(colnames(names.v)), nomatch=0)) {
      # Reconvert the name id into data id (`rev` simulates masking), but we need
      # to undo it with `ncol(names.v) - id + 1L`
      id <- seq_along(visible)[visible][ncol(names.v) - id + 1L]
      res <- names['ids', id]
  } }
  res
}
## Check That External Vectors are OK

validate_ext <- function(x, i, par.type, arg.e, name, call, .CALL) {
  if(par.type == PAR.INT.LEAF && !is.num_naked(list(arg.e))) {
    # Next call, if any
    next.call.v <- which(
      seq_along(x[['call']]) > i & x[['par.type']] == PAR.INT.CALL
    )
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



