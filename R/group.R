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

r2c_group_obj <- function(sizes, order, group.o, sorted, mode) {
  structure(
    list(sizes=sizes, order=order, group.o=group.o, sorted=sorted, mode=mode),
    class="r2c.groups"
  )
}
group_sizes <- function(go, levels=vector(mode='list', length(go))) {
  res <- .Call(R2C_group_sizes, go)
  names(res) <- c('gsizes', 'glabs', 'gmax', 'gmin')
  res[['glevels']] <- levels  # won't work for list inputs
  res
}
group_exec_int <- function(
  obj, formals, groups, data, MoreArgs, call
) {
  preproc <- obj[['preproc']]
  shlib <- obj[['so']]
  enclos <- obj[['envir']]

  # - Handle Groups ------------------------------------------------------------

  if(!is.list(data)) data <- list(data)
  if(length(d.len <- unique(lengths(data))) > 1L)
    stop("All `data` vectors must be the same length.")
  if(!length(d.len)) d.len <- 0L  # No data
  groups <-  if(!inherits(groups, "r2c.groups")) {
    process_groups(groups)
  } else groups

  mode <- groups[['mode']]
  if(length(data) > 0 && !identical(d.len, length(groups[['group.o']][[1L]])))
    stop("`data` vectors must be the same length as `group` vectors.")

  if (mode != "ungrouped") {
    if(!groups[['sorted']]) do <- lapply(data, "[", groups[['order']])
    else do <- data
  } else {
    do <- data
  }
  group.sizes <- groups[['sizes']]
  gmax <- group.sizes[['gmax']]
  gmin <- group.sizes[['gmin']]

  # - Match Data to Parameters and Allocate ------------------------------------

  alloc <- match_and_alloc(
    do=do, MoreArgs=MoreArgs, preproc=preproc, formals=formals,
    enclos=enclos, gmax=gmax, gmin=gmin, call=call, runner=r2c::group_exec
  )
  res.id <- which(alloc[['alloc']][['type']] == 'res')
  gsizes <- group.sizes[['gsizes']]
  res.size.coef <- alloc[['alloc']][['size.coefs']][[res.id]]

  # Compute sizes for each size coefs element across all groups; skip
  # pmax for single element case for speed.
  iter.sizes.in <- lapply(res.size.coef, iter_result_sizes, base=gsizes)
  group.res.sizes <-
    if(length(iter.sizes.in) > 1) pmax2(iter.sizes.in)
    else iter.sizes.in[[1L]]

  # Identify obvious cases for optimizing result label generation.  size_vecrec
  # should have collapsed to obvious cases if possible.  We rely on the internal
  # allocator to freak out if we request something larger than R_xlen_t?
  res.size.type <- "variable"
  if(length(res.size.coef) == 1L) {
    rsc1 <- res.size.coef[[1L]]
    lrsc1 <- length(rsc1)
    if(lrsc1 == 1L || lrsc1 > 1L && all(rsc1[-1L] == 0)) {
      # constant size
      res.size.type <-
        if(rsc1[1L] == 1L) "scalar"
        else "constant"
  } }
  # - Run ----------------------------------------------------------------------

  status <- numeric(1)
  res.i <- which(alloc[['alloc']][['type']] == "res")
  res <- if(length(group.res.sizes)) {
    handle <- load_dynlib(obj)

    alp <- prep_alloc(alloc, sum(group.res.sizes))

    status <- run_group_int(
      handle[['name']],
      alp[['dat']],
      alp[['dat_cols']],
      alp[['ids']],
      alp[['ext.any']],
      gsizes,
      group.res.sizes
    )
    # Result vector is modified by reference
    alp[['dat']][[res.i]]
  } else {
    numeric()
  }
  if(alloc[['alloc']][['typeof']][res.i] == "integer")
    res <- as.integer(res)
  else if(alloc[['alloc']][['typeof']][res.i] == "logical")
    res <- as.logical(res)

  # Generate and attach group labels, small optimization for predictable groups
  if(mode != "ungrouped") {
    g.lab.raw <- if(!is.null(group.sizes[['glevels']][[1L]])) {
      group.sizes[['glevels']][[1L]]
    } else group.sizes[['glabs']]
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
# Like pmax, except 0 dominates
pmax2 <- function(x) .Call(R2C_vecrec_pmax, x)

#' Compute Group Meta Data
#'
#' [`group_exec`] sorts data by groups prior to iterating through them.  When
#' running `group_exec` multiple times on the same data, it is better to
#' pre-sort the data and tell `group_exec` as much so it does  not sort the data
#' again.  We can do the latter with `process_groups`, which additionally
#' computes group information we can re-use across calls.
#'
#' @note The structure and content of the return value may change in the future.
#' @inheritParams group_exec
#' @seealso [`group_exec`]
#' @export
#' @param sorted TRUE or FALSE (default), whether the vectors in `groups` are
#'   already sorted.  If set to TRUE, no sorting will be done on the groups, nor
#'   later on the `data` by [`group_exec`]. If the data is truly sorted this
#'   produces the same results while avoiding the cost of sorting.  If the data
#'   is not sorted by groups, `g` will produce groups corresponding to
#'   equal-value runs it contains, which might be useful in some circumstances.
#' @return an "r2c.groups" object, which is a list containing group sizes,
#'   labels, and group count, along with other meta data such as the group
#'   ordering vector.
#' @examples
#' ## Use same group data for different but same length data.
#' ## (alternatively, could use two functions on same data).
#' n <- 10
#' dat <- data.frame(x=runif(n), y=runif(n), g=sample(1:3, n, replace=TRUE))
#'
#' ## Pre-sort by group and compute grouping data
#' dat <- dat[order(dat[['g']]),]
#' g.r2c <- process_groups(dat[['g']], sorted=TRUE) # note sorted=TRUE
#'
#' ## Re-use pre-computed group data
#' f <- r2cq(sum(x))
#' with(dat, group_exec(f, x, groups=g.r2c))
#' with(dat, group_exec(f, y, groups=g.r2c))
#'
#' ## Claim unsorted data is sorted to implement RLE
#' g <- c(1, 2, 2, 1, 1, 1, 2, 2)
#' group_exec(f, rep(1, length(g)), process_groups(g, sorted=TRUE))
#' rle(g)$values
#' rle(g)$lengths

process_groups <- function(groups, sorted=FALSE) {
  vetr(
    integer() || numeric() || list(),
    LGL.1
  )
  mode <- "list"
  if(!is.list(groups)) {
    groups <- list(groups)
    mode <- "vec"
  }
  levels <- vector(mode='list', length(groups))
  if(length(unique(lengths(groups))) != 1L)
    stop("All `groups` vectors must be the same length.")
  if(length(groups) != 1L)
    stop("Only one grouping variable supported at the moment.")

  for(i in seq_along(groups)) {
    g <- groups[[i]]
    if(!is.factor(g) && !is.numeric(g) && !is.integer(g)) {
      stop(
        "`groups[[", i, "]]` is of invalid type \"", class(g)[1L],
        "\", should be integer, numeric, or factor."
      )
    }
    if(is.factor(g)) {
      levels[[i]] <- levels(g)
    }
    if(typeof(g) != "integer") groups[[i]] <- as.integer(g)
  }
  if(!sorted) {
    o <- do.call(order, groups)
    go <- lapply(groups, "[", o)
  } else {
    o <- seq_along(groups[[1L]])  # should be altrep
    go <- groups
  }
  r2c_group_obj(
    group_sizes(go[[1L]], levels=levels), # UPDATE IF ALLOW MORE GROUP VECS
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
#' A [runner][runners] that organizes `data` into groups as defined by `groups`, 
#' and executes the native code associated with `fun` iteratively with each
#' group's portion of `data`.
#'
#' @export
#' @seealso [`r2c`] for more details on the behavior and constraints of
#'   "r2c_fun" functions, [`base::eval`] for the semantics of `enclos`.
#' @family runners
#' @param fun an "r2c_fun" function as produced by [`r2c`].
#' @param groups an integer, numeric, or factor vector.  Alternatively, a list
#'   of equal-length such vectors, the interaction of which defines individual
#'   groups to organize the vectors in `data` into (multiple vectors not
#'   implemented yet).  Numeric vectors are coerced to integer, thus copied.
#'   Vectors of integer type, but with different classes/attributes (other than
#'   factors) will be treated as integer vectors.  The vectors must be the
#'   same length as those in `data`.  NA values are considered one group. If a
#'   list, the result of the calculation will be returned as a "data.frame",
#'   otherwise as a named vector.  Currently only one group vector is allowed,
#'   even when using list mode.  Support for multiple group vectors and other
#'   types of vectors will be added in the future.  Zero length groups are not
#'   computed on at all (e.g. missing factor levels, zero-length group vector).
#' @param data a numeric vector, or a list of equal length numeric
#'   vectors.  If a named list, the vectors will be matched to `fun` parameters
#'   by those names.  Elements without names are matched positionally.  If a
#'   list must contain at least one vector.  Conceptually, this parameter is
#'   used similarly to `envir` parameter to [`base::eval`] when that is a list
#'   (see `enclos`).
#' @param MoreArgs a list of R objects to pass on as iteration-invariant
#'   arguments to `fun`.  Unlike with `data`, each of the objects therein are
#'   passed in full to the native code for each iteration  This is useful for
#'   arguments that are intended to remain constant across iterations.  Matching
#'   of these objects to `fun` parameters is the same as for `data`, with
#'   positional matching occurring after the elements in `data` are matched.
#' @return If `groups` is an atomic vector, a named numeric or integer vector
#'   with the results of executing `fun` on each group and the names set to the
#'   groups.  Otherwise, a "data.frame" with the group vectors as columns and
#'   the result of the computation as the last column.
#' @examples
#' r2c_mean <- r2cq(mean(x))
#' with(mtcars, group_exec(r2c_mean, hp, groups=cyl))
#'
#' r2c_slope <- r2cq(
#'   sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x)) ^ 2)
#' )
#' with(mtcars, group_exec(r2c_slope, list(hp, qsec), groups=cyl))
#'
#' ## Parameters are generated in the order they are encountered
#' str(formals(r2c_slope))
#'
#' ## Data frame output, re-order arguments
#' with(
#'   mtcars,
#'   group_exec(r2c_slope, list(y=hp, x=qsec), groups=list(cyl))
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
#'   r2c_sum_add_na, a, groups=g,
#'   MoreArgs=list(y=weights, na.rm=TRUE)  ## use MoreArgs for group-invariant
#' )
#' group_exec(
#'   r2c_sum_add_na, a, groups=g,
#'   MoreArgs=list(y=-weights, na.rm=FALSE)
#' )
#'
#' ## Groups known to be sorted can save substantial time
#' n <- 1e7
#' x <- runif(1e7)
#' g <- cumsum(sample(c(TRUE, rep(FALSE, 99)), n, replace=TRUE))
#' identical(g, sort(g))  # sorted already!
#' system.time(res1 <- group_exec(r2c_mean, x, g))
#' system.time(res2 <- group_exec(r2c_mean, x, process_groups(g, sorted=TRUE)))
#' identical(res1, res2)
#'
#' ## We can also group by runs by lying about `sorted` status
#' x <- 1:8
#' g <- rep(rep(1:2, each=2), 2)
#' g
#' group_exec(r2c_mean, x, groups=list(g))
#' group_exec(r2c_mean, x, groups=process_groups(list(g), sorted=TRUE))

group_exec <- function(fun, data, groups, MoreArgs=list()) {
  # FIXME: add validation for shlib
  vetr(
    fun=is.function(.) && inherits(., 'r2c_fun'),
    groups=
      # Simple group vector
      integer() || numeric() ||
      # List of group vectors
      (
        list() &&
        all(
          vapply(., \(x) is.numeric(x) || is.integer(x) || is.factor(x), TRUE))
      ) ||
      # An object produced by process_groups
      r2c_groups_template(),
    data=(
      (numeric() || integer()) ||
      (list() && all(is.num_naked(.)))
    ),
    MoreArgs=list()
  )
  obj <- get_r2c_dat(fun)
  call <- sys.call()
  group_exec_int(
    obj, formals=formals(fun), groups=groups, data=data,
    MoreArgs=MoreArgs, call=call
  )
}

run_group_int <- function(
  handle, dat, dat_cols, ids, extern, group.sizes, group.res.sizes
) {
  .Call(
    R2C_run_group,
    handle,
    dat,
    dat_cols,
    ids,
    extern,
    group.sizes,
    group.res.sizes
  )
}

