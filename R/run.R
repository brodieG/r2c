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

group_exec_int <- function(
  obj, formals, enclos, groups, data, MoreArgs, call
) {
  preproc <- obj[['preproc']]
  shlib <- obj[['so']]

  # - Handle Groups ------------------------------------------------------------

  if(!is.list(data)) data <- list(data)
  if(length(d.len <- unique(lengths(data))) > 1L)
    stop("All `data` vectors must be the same length.")
  if(!length(d.len)) d.len <- 0L  # No data
  groups <- if(is.null(groups)) {
    r2c_group_obj(
      sizes=list(gsizes=as.numeric(d.len), glabs=0L, gmax=as.numeric(d.len)),
      order=seq_len(d.len),
      group.o=list(rep(1L, d.len)), # Not altrep as of 4.2.1, sadly
      sorted=TRUE, mode="ungrouped"
    )
  } else if(!inherits(groups, "r2c.groups")) {
    process_groups(groups)
  } else groups

  mode <- groups[['mode']]
  if(length(d.len) == 0 || !identical(d.len, length(groups[['group.o']][[1L]])))
    stop("`data` vectors must be the same length as `group` vectors.")

  if (mode != "ungrouped") {
    if(!groups[['sorted']]) do <- lapply(data, "[", groups[['order']])
    else do <- data
  } else {
    do <- data
  }
  group.sizes <- groups[['sizes']]

  # - Match Data to Parameters -------------------------------------------------

  # Trick here is data is split across two parameters so we have to merge
  # together to match, but then split the data back into the two parameters.

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
      # so instead match against the actual data for better error message
      args <- c(data, MoreArgs)
      call.dummy <- as.call(c(list(f.dummy), args))
      match.call(f.dummy, call.dummy, envir=enclos)
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
  # Split back into group varying vs not
  dat.match <- unlist(call.dummy.m[call.dummy.m <= length(do)])
  names(do)[dat.match] <- names(dat.match)
  more.match <- unlist(call.dummy.m[call.dummy.m > length(do)])
  names(MoreArgs)[more.match - length(do)] <- names(more.match)

  # Expand any dots in the preprocess data to match the dot args we were given
  preproc <- expand_dots(preproc, c(names(do), names(MoreArgs)))

  # Prepare temporary memory allocations
  alloc <- alloc(
    x=preproc, data=do, gmax=group.sizes[['gmax']], par.env=enclos,
    MoreArgs=MoreArgs, .CALL=call
  )
  stack <- alloc[['stack']]

  # Compute result size
  if(ncol(stack) != 1L)
    stop("Internal Error: unexpected stack state at exit.")

  empty.res <- FALSE
  gsizes <- group.sizes[['gsizes']]
  res.size.type <- "variable"
  if(!stack['group', 1L]) { # constant group size
    group.res.sizes <- rep(stack['size', 1L], length(gsizes))
    res.size.type <- if(stack['size', 1L] == 1L) "scalar" else "constant"
  } else if (is.na(stack['size', 1L])) {
    group.res.sizes <- gsizes
  } else if (stack['size', 1L]) {
    group.res.sizes <- gsizes
    group.res.sizes[
      group.res.sizes < stack['size', 1L] & group.res.sizes != 0
    ] <- stack['size', 1L]
  } else {
    group.res.sizes <- numeric()
  }
  status <- numeric(1)
  res.i <- which(alloc[['alloc']][['type']] == "res")
  res <- if(length(group.res.sizes)) {
    # Allocate result vector, this will be modified by reference
    if(length(alloc[['alloc']][['dat']][[alloc[['alloc']][['i']]]]))
      stop("Internal Error: result should be zero length when uninitialized.")
    alloc[['alloc']][['dat']][[alloc[['alloc']][['i']]]] <-
      numeric(sum(group.res.sizes))

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
    handle <- obj[['handle']]

    if(!is.na(shlib) && !is.loaded("run", PACKAGE=handle[['name']])) {
      handle <- dyn.load(shlib)
    }
    if(!is.loaded("run", PACKAGE=handle[['name']]))
      stop("Could not load native code.")

    status <- run_group_int(
      handle[['name']],
      dat,
      dat_cols,
      ids,
      flag,
      control,
      gsizes,
      group.res.sizes
    )
    # Result vector is modified by reference
    dat[[res.i]]
  } else {
    numeric()
  }
  if(alloc[['alloc']][['typeof']][res.i] == "integer") res <- as.integer(res)

  # Generate and attach group labels, small optimization for predictable groups
  if(mode != "ungrouped") {
    g.lab.raw <- group.sizes[['glabs']]
    g.lab <-
      if(res.size.type == "scalar") g.lab.raw
      else if(res.size.type == "constant")
        rep(g.lab.raw, each=group.res.sizes[1L])
      else rep(g.lab.raw, group.res.sizes)  # could optimize further
    # Attach group labels
    if(mode == 'list') {
      res <- data.frame(group=g.lab, V1=res)
    } else if (mode == 'vec') {
      names(res) <- g.lab
    } else stop("Unknown return format mode")
    if(status) {
      warning(
        "longer object length is not a multiple of shorter object length ",
        sprintf("(first at group %.0f).", status)
    ) }
  } else if(status) {
    warning("longer object length is not a multiple of shorter object length.")
  }
  res
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
