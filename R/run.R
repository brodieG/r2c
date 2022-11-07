## Copyright (C) 2022 Brodie Gaslam
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

## Match Data to r2c Fun Parameters
##
## Data is spread between group-varying (`data`) and non-group varying
## (`MoreArgs`).
##
## @param do data, ordered, group varying
## @param MoreArgs (same as for the user facing funs)
## @param preproc compile-time preprocessed meta data retrieved from the `r2c`
##   function
## @param formals the formals of the r2c fun
## @param enclos environment to use as enclosure for data
## @param gmax scalar largest group size
## @param call original call
## @param dat.args the data represented as symbols if e.g. someone used
##   `list(x=y)` as the `data` parameter, otherwise just the data.  Used for
##   producing better error messages.

match_and_alloc <- function(
  do, MoreArgs, preproc, formals, enclos, gmax, call, dat.args
) {
  # Trick here is data is split across `data` and `MoreArgs` so we have to merge
  # together to match, but then split the data back into the two parameters
  # since they have different usage semantics.
  if(any(grepl(RX.ARG, names(do))))
    stop("`data` names may not match regular expression \"", RX.ARG, "\".")
  if(any(grepl(RX.ARG, names(MoreArgs))))
    stop("`MoreArgs` names may not match regular expression \"", RX.ARG, "\".")
  if(is.null(names(do))) names(do) <- character(length(do))
  if(is.null(names(MoreArgs))) names(MoreArgs) <- character(length(MoreArgs))

  # Generate a function and arg list with the combined params to match; we use
  # for param values the index of each item in the original data, and we'll use
  # those indices to split the data back after matching.
  params <- names(formals)
  args.dummy <- as.list(seq_len(length(do) + length(MoreArgs)))
  f.dummy <- function() NULL
  formals(f.dummy) <- formals
  names(args.dummy) <- c(names(do), names(MoreArgs))
  call.dummy <- as.call(c(list(f.dummy), as.list(args.dummy)))
  call.dummy.m <- tryCatch(
    as.list(
      match.call(f.dummy, call.dummy, envir=enclos, expand.dots=FALSE)
    )[-1L] ,
    error=function(e) {
      # Error produced by this is confusing because we're matching to positions,
      # so instead re-match against the actual data for better error message.
      # For the case where the call to e.g. `group_exec` contains something like
      # list(x = y), it would be better to show the name instead of the object,
      # but in many cases list(x = y) is just a data.frame.
      args <- c(dat.args, MoreArgs)
      call.dummy <- as.call(c(list(f.dummy), args))
      tryCatch(
        match.call(f.dummy, call.dummy, envir=enclos),
        error=function(e) stop(simpleError(conditionMessage(e), call=call))
      )
      # In case the above somehow doesn't produce an error; it always should
      stop("Internal Error: no param match error; contact maintainer.")
  } )
  # Rename the dots and splice back in; there is no dots forwarding once we get
  # to r2c implementations, so the original names are useless, and we need new
  # names so we can recognize which arguments came from dots.
  if(dots.pos <- match("...", names(call.dummy.m), nomatch=0)) {
    dots <- call.dummy.m[[dots.pos]]
    names(dots) <- sprintf(".ARG.%d", seq_along(dots))
    call.dummy.m <- append(call.dummy.m[-dots.pos], dots, after=dots.pos - 1L)
  }
  # Split back into group varying (data) vs not (MoreArgs)
  dat.match <- unlist(call.dummy.m[call.dummy.m <= length(do)])
  names(do)[dat.match] <- names(dat.match)
  more.match <- unlist(call.dummy.m[call.dummy.m > length(do)])
  names(MoreArgs)[more.match - length(do)] <- names(more.match)

  # Expand any dots in the preprocess data to match the dot args we were given
  preproc <- expand_dots(preproc, c(names(do), names(MoreArgs)))

  # Prepare temporary memory allocations
  alloc <- alloc(
    x=preproc, data=do, gmax=gmax, par.env=enclos,
    MoreArgs=MoreArgs, .CALL=call
  )
  alloc
}
## Reorganize the allocation data for running
##
## @param res.size the final result vector size

prep_alloc <- function(alloc, res.size) {
  # Allocate result vector, this will be modified by reference
  if(length(alloc[['alloc']][['dat']][[alloc[['alloc']][['i']]]]))
    stop("Internal Error: result should be zero length when uninitialized.")
  alloc[['alloc']][['dat']][[alloc[['alloc']][['i']]]] <- numeric(res.size)

  # Extract control parameters, and run sanity checks (not fool proof)
  dat <- alloc[['alloc']][['dat']]
  control <- lapply(alloc[['call.dat']], "[[", "ctrl")
  flag <- vapply(alloc[['call.dat']], "[[", 0L, "flag")
  # Ids into call.dat, last one will be the result
  ids <- lapply(alloc[['call.dat']], "[[", "ids")
  if(!all(unlist(ids) %in% seq_along(dat)))
    stop("Internal Error: Invalid data indices.")
  ids <- lapply(ids, "-", 1L) # 0-index for C

  dat_cols <- sum(alloc[['alloc']][['type']] == "grp")
  list(dat=dat, dat_cols=dat_cols, ids=ids, control=control, flag=flag)
}

run_group_int <- function(
  handle, dat, dat_cols, ids, flag, control, group.sizes, group.res.sizes
) {
  .Call(
    R2C_run_group,
    handle,
    dat,
    dat_cols,
    ids,
    flag,
    control,
    group.sizes,
    group.res.sizes,
    group.sizes
  )
}
