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


r2c_group_obj <- function(sizes, order, group.o, sorted, mode) {
  structure(
    list(sizes=sizes, order=order, group.o=group.o, sorted=sorted, mode=mode),
    class="r2c.groups"
  )
}
group_sizes <- function(go) {
  res <- .Call(R2C_group_sizes, go)
  names(res) <- c('gsizes', 'glabs', 'gmax')
  res
}
#' Compute Group Meta Data
#'
#' Use by [`group_exec`] to organize group data, and made available as an
#' exported function for the case where multiple calculations use the same group
#' set and thus there is an efficiency benefit in processing it once.
#'
#' @note The structure and content of the return value may change in the future.
#' @inheritParams group_exec
#' @seealso [`group_exec`]
#' @export
#' @param sorted TRUE or FALSE (default), whether the vectors in `groups` are
#'   already sorted.  If set to TRUE, no sorting will be done on the groups, nor
#'   later on the `data` by [`group_exec`]. If the data is truly sorted this
#'   produces the same results while avoiding the cost of sorting.  If the data
#'   is not sorted groups `g` will produce groups corresponding to equal-value
#'   runs it contains, which might be useful in some circumstances.
#' @return an "r2c.groups" object, which is a list containing group sizes,
#'   labels, and group count, along with other meta data such as the group
#'   ordering vector.  The contents and structure of this 
#' @examples
#' ## Use same group data for different but same length data.
#' ## (alternatively, could use two functions on same data).
#' g <- c(1L, 2L, 2L)
#' x <- runif(3)
#' y <- runif(3)
#' g.r2c <- process_groups(g, sorted=TRUE)
#' f <- r2cq(sum(x))
#' group_exec(f, g.r2c, x)
#' group_exec(f, g.r2c, y)

process_groups <- function(groups, sorted=FALSE) {
  vetr(
    integer() || (list() && all(vapply(., is.integer, TRUE))),
    LGL.1
  )
  mode <- "list"
  if(is.integer(groups)) {
    groups <- list(groups)
    mode <- "vec"
  }
  if(length(unique(lengths(groups))) != 1L)
    stop("All `groups` vectors must be the same length.")
  if(length(groups) != 1L)
    stop("Only one grouping variable supported at the moment.")
  if(!sorted) {
    o <- do.call(order, groups)
    go <- lapply(groups, "[", o)
  } else {
    o <- seq_along(groups[[1L]])  # should be altrep
    go <- groups
  }

  r2c_group_obj(
    group_sizes(go[[1L]]), # UPDATE IF ALLOW MORE GROUP VECS
    order=o, group.o=go, sorted=sorted, mode=mode
  )
}
r2c_groups_template <- function() {
  x <- process_groups(1:3)
  x[['sizes']] <- lapply(x[['sizes']], "[", 0L)
  x[['order']] <- x[['order']][0L]
  x[['group.o']] <- x[['group.o']][0L]
  x
}

#' Execute r2c Function Iteratively on Groups in Data
#'
#' Organizes `data` according to `groups`, and calls the native code associated
#' with `fun` iteratively for each group.  Data provided in `data` will be
#' subset provided to the native code subset to the portion corresponding to
#' each group.  Each iteration of the native code is invoked directly from
#' native code and thus avoids R interpreter overhead.
#'
#' @export
#' @seealso [`r2c`] for more details on the behavior and constraints of
#'   "r2c_fun" functions, [`base::eval`] for the semantics of `enclos`.
#' @param fun an "r2c_fun" function as produced by [`r2c`].
#' @param groups an integer vector, or a list of equal-length integer vectors,
#'   the interaction of which defines individual groups to organize the vectors
#'   in `data` into.  NA values are considered one group. If a list, the result
#'   of the calculation will be returned as a "data.frame", otherwise as a named
#'   vector.  Currently only one group vector is allowed, even when using list
#'   mode.  Support for multiple group vectors and non-integer vectors will be
#'   added in the future.
#' @param data a numeric vector, or a list of numeric vectors, each vector the
#'   same length as the vector(s) in `groups`.  If a named list, the vectors
#'   will be matched to `fun` parameters by those names.  Elements without names
#'   are matched positionally.  If a list must contain at least one element.
#' @param MoreArgs a list of R objects to pass on as group-invariant arguments
#'   to `fun`.  Unlike with `data`, each of the objects therein are passed in
#'   full to the native code for each group.  This is useful for arguments that
#'   are intended to remain constant group to group.  Matching of these objects
#'   to `fun` parameters is the same as for `data`, with positional matching
#'   occurring after the elements in `data` are matched.
#' @param enclos environment to use as the `enclos` parameter when evaluating
#'   expressions or matching calls.
#' @return If `groups` is an atomic vectors, a named numeric or
#'   integer vector with the results of executing `fun` on each group and the
#'   names set to the groups.  Otherwise, a "data.frame" with the group vectors
#'   as columns and the result of the computation as the last column.
#' @examples
#' r2c_mean <- r2cq(mean(x))
#' with(
#'   mtcars,
#'   group_exec(r2c_mean, as.integer(cyl), hp)
#' )
#'
#' r2c_slope <- r2cq(
#'   sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2)
#' )
#' with(
#'   mtcars,
#'   group_exec(r2c_slope, as.integer(cyl), list(hp, qsec))
#' )
#' ## Parameters are generated in the order they are encountered
#' str(formals(r2c_slope))
#'
#' ## Data frame output, re-order arguments
#' with(
#'   mtcars,
#'   group_exec(r2c_slope, list(as.integer(cyl)), list(y=hp, x=qsec))
#' )
#'
#' ## We can provide group=invariant parameters:
#' r2c_sum_add_na <- r2cq(sum(x * y, na.rm=na.rm) / sum(y))
#' str(formals(r2c_sum_add_na))
#' a <- runif(10)
#' a[8] <- NA
#' weights <- c(.1, .1, .2, .2, .4)
#' g <- rep(1:2, each=5)
#' group_exec(
#'   r2c_sum_add_na,
#'   g,
#'   a,
#'   list(y=weights, na.rm=TRUE)  ## use MoreArgs for group-invariant
#' )
#'
#' ## Groups known to be sorted can save substantial time
#' n <- 1e7
#' x <- runif(1e7)
#' g <- cumsum(sample(c(TRUE, rep(FALSE, 99)), n, replace=TRUE))
#' identical(g, sort(g))  # sorted already!
#' system.time(res1 <- group_exec(r2c_mean, g, x))
#' system.time(res2 <- group_exec(r2c_mean, g, x, sorted=TRUE))
#' identical(res1, res2)
#'
#' ## We can also group by runs with `sorted`
#' x <- 1:8
#' g <- rep(rep(1:2, each=2), 2)
#' g
#' group_exec(r2c_mean, list(g), x, sorted=TRUE)
#' group_exec(r2c_mean, list(g), x, sorted=FALSE)

group_exec <- function(
  fun, groups, data, MoreArgs=list(), enclos=parent.frame()
) {
  # FIXME: add validation for shlib
  vetr(
    fun=is.function(.) && inherits(., 'r2c_fun'),
    groups=
      # Simple group vector
      integer() ||
      # List of group vectors
      (list() && all(vapply(., is.integer, TRUE))) ||
      # An object produced by process_groups
      r2c_groups_template(),
    data=(
      (numeric() || integer()) ||
      (list() && all(is.num_naked(.)) && length(.) > 0)
    ),
    MoreArgs=list()
  )
  obj <- get_r2c_dat(fun)
  call <- sys.call()
  group_exec_int(
    obj, formals=formals(fun), enclos=enclos, groups=groups, data=data,
    MoreArgs=MoreArgs, call=call
  )
}
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

    status <- run_int(
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

run_int <- function(
  handle, dat, dat_cols, ids, flag, control, group.sizes, group.res.sizes
) {
  .Call(
    R2C_run_internal,
    handle,
    dat,
    dat_cols,
    ids,
    flag,
    control,
    group.sizes,
    group.res.sizes
  )
}
