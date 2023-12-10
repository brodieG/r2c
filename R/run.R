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

rename_dots <- function(call.matched, pattern) {
  if(dots.pos <- match("...", names(call.matched), nomatch=0)) {
    dots <- call.matched[[dots.pos]]
    names(dots) <- sprintf(pattern, seq_along(dots))
    call.matched <- append(call.matched[-dots.pos], dots, after=dots.pos - 1L)
  }
  call.matched
}

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
## @param gmin scalar smallest group size
## @param call original call
## @param runner function this is being called by, e.g. `group_exec` (maybe
##   could extract from `call`, but don't bother to).

match_and_alloc <- function(
  do, MoreArgs, preproc, formals, enclos, gmax, gmin, call, runner
) {
  # Trick here is data is split across `data` and `MoreArgs` so we have to merge
  # together to match, but then split the data back into the two parameters
  # since they have different usage semantics.
  if(any(grepl(R2C.PRIV.RX, names(do))))
    stop("`data` names may not match this regex: \"", R2C.PRIV.RX, "\".")
  if(any(grepl(R2C.PRIV.RX, names(MoreArgs))))
    stop("`MoreArgs` names may not match this regex: \"", R2C.PRIV.RX, "\".")
  if(is.null(names(do))) names(do) <- character(length(do))
  if(is.null(names(MoreArgs))) names(MoreArgs) <- character(length(MoreArgs))

  # Generate a function and arg list with the combined params to match; we use
  # for param values the index of each item in the original data, and we'll use
  # those indices to split the data back after matching.
  params <- names(formals)
  args.dummy <- as.list(seq_len(length(do) + length(MoreArgs)))
  f.dummy <- function() NULL
  formals(f.dummy) <- formals
  body(f.dummy) <- as.call(c(as.name("list"), lapply(names(formals), as.name)))
  names(args.dummy) <- c(names(do), names(MoreArgs))
  call.dummy <- as.call(c(list(as.name("fun")), as.list(args.dummy)))
  call.dummy.m <- tryCatch(
    as.list(
      match.call(f.dummy, call.dummy, envir=enclos, expand.dots=FALSE)
    )[-1L] ,
    error=function(e) {
      # Error produced by this is confusing because we're matching to positions,
      # so instead re-match for better error message.
      #
      # Additional convolution for the case where the call to e.g. `group_exec`
      # contains something like list(x = y), where we want to report the
      # unevaluated expression an not e.g. the value of `y`.

      call.m <- match.call(runner, call, expand.dots=FALSE, envir=enclos)
      data.2 <-
        if(is.call(call.m[['data']]) && call.m[['data']][[1]] == quote('list')) {
          as.list(call.m[['data']])[-1L]
        }
        else do  # note this is not in original order
      moreargs.2 <-
        if(is.call(call.m[['MoreArgs']])) as.list(call.m[['MoreArgs']])[-1L]
        else MoreArgs

      args <- c(data.2, moreargs.2)
      call.dummy <- as.call(c(list(f.dummy), args))
      tryCatch(
        match.call(f.dummy, call.dummy, envir=enclos),
        error=function(e) stop(simpleError(conditionMessage(e), call=call))
      )
      # In case the above somehow doesn't produce an error, fallback to original
      # The error won't make sense because the values that are being matched
      # are the indices that we generated to track data vs. MoreArgs (see above)
      stop(
        "Internal Error: parameter match error, and unable to generate ",
        "friendly error.  The error is \"", conditionMessage(e), "\", but ",
        "this is from an internal matching attempt.  Contact maintainer."
      )
  } )
  # Rename the dots and splice back in; there is no dots forwarding once we get
  # to r2c implementations, so the original names are useless, and we need new
  # names so we can recognize which arguments came from dots.

  call.dummy.m.old <- call.dummy.m
  call.dummy.m <- rename_dots(call.dummy.m, DOT.ARG.TPL)

  # Test that all required parameters were provided, and provide error message
  # if they weren't.  Ideally we'd give the originall expressions in `data` and
  # `MoreArgs`, but that's too much work.
  call.dummy.2 <- rename_dots(call.dummy.m.old, "..%d")
  call.dummy.2[] <- lapply(names(call.dummy.2), as.name)
  call.dummy.2 <- as.call(c(list(quote(fun)), call.dummy.2))
  tryCatch(
    do.call(f.dummy, call.dummy.m),
    error=function(e) stop(
      simpleError(
        paste0(
          c(sprintf("In `%s`:", deparse1(call.dummy.2)), conditionMessage(e)),
          collapse="\n"
        ),
        call=call
  ) ) )

  # Split back into group varying (data) vs not (MoreArgs)
  dummy.idx <- unlist(call.dummy.m)
  dat.match <- dummy.idx[dummy.idx <= length(do)]
  names(do)[dat.match] <- names(dat.match)
  more.match <- unlist(dummy.idx[dummy.idx > length(do)])
  names(MoreArgs)[more.match - length(do)] <- names(more.match)

  # Add unmatched defaults (we already checked for missing formals above)
  # Note we don't allow dots with defaults, which technically is possible but
  # in definition of a function but not in use of it.  Need to eval defaults.
  missing.formals <-
    which(!names(formals) %in% c(names(call.dummy.m.old), '...'))
  if(!all(missing.formals %in% which(default_params(formals))))
    stop("Internal Error: missing non-default formals")
  defaults <- sapply(
    names(formals[missing.formals]),
      function(miss) {
        tryCatch(
          eval(formals[[miss]], envir=c(do, MoreArgs), enclos=enclos),
          error=function(e) {
            stop(
              simpleCondition(
                paste0(
                  "Error evaluating default parameter ", miss, ": ",
                  conditionMessage(e)
                ),
                call
    ) ) } ) },
    simplify=FALSE
  )
  MoreArgs <- c(MoreArgs, defaults)

  # Expand any dots in the preprocess data to match the dot args we were given
  preproc <- expand_dots(preproc, c(names(do), names(MoreArgs)))

  # Prepare temporary memory allocations
  alloc <- alloc(
    x=preproc, data=do, gmax=gmax, gmin=gmin, par.env=enclos,
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

  dat <- alloc[['alloc']][['dat']]
  # External args that are allowed to evaluate to anything
  ext.any <- lapply(alloc[['call.dat']], "[[", "stack.ext.any")
  # Ids into call.dat, last one will be the result
  ids <- lapply(alloc[['call.dat']], "[[", "ids")
  if(!all(unlist(ids) %in% seq_along(dat)))
    stop("Internal Error: Invalid data indices.")
  ids <- lapply(ids, "-", 1L) # 0-index for C

  dat_cols <- sum(alloc[['alloc']][['type']] == "grp")
  list(dat=dat, dat_cols=dat_cols, ids=ids, ext.any=ext.any, alloc=alloc)
}
# Run Once
#
# See `group_exec_int` for details

one_exec_int <- function(obj, formals, MoreArgs, call) {
  preproc <- obj[['preproc']]
  shlib <- obj[['so']]
  enclos <- obj[['envir']]
  do <- data <- list()  # all data via MoreArgs
  gmax <- gmin <- 0

  alloc <- match_and_alloc(
    do=do, MoreArgs=MoreArgs, preproc=preproc, formals=formals,
    enclos=enclos, gmax=gmax, gmin=gmin, call=call, runner=r2c::group_exec
  )
  res.i <- which(alloc[['alloc']][['type']] == "res")
  res.size.coef <- alloc[['alloc']][['size.coefs']][[res.i]]

  # Should really just be constant since no varying data
  res.size.in <- lapply(res.size.coef, iter_result_sizes, base=0)
  res.size <-
    if(length(res.size.in) > 1) pmax2(res.size.in) else res.size.in[[1L]]

  handle <- load_dynlib(obj)
  alp <- prep_alloc(alloc, res.size)

  status <- run_one_int(
    handle[['name']],
    alp[['dat']],
    alp[['dat_cols']],
    alp[['ids']],
    alp[['ext.any']],
    res.size
  )
  if(status) {
    warning("longer object length is not a multiple of shorter object length.")
  }
  # Result vector is modified by reference
  res <- alp[['dat']][[res.i]]

  if(alloc[['alloc']][['typeof']][res.i] == "integer")
    res <- as.integer(res)
  else if(alloc[['alloc']][['typeof']][res.i] == "logical")
    res <- as.logical(res)

  res
}

run_one_int <- function(handle, dat, dat_cols, ids, extern, res.size) {
  .Call(
    R2C_run_one,
    handle,
    dat,
    dat_cols,
    ids,
    extern,
    res.size
  )
}

