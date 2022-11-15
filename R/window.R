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

#' Execute r2c Function on Sliding Windows in Data
#'
#' Calls the native code associated with `fun` on sequential windows along the
#' `data` vectors.  The windows are aligned relative to a base index that starts
#' at the first "element" of `data` (where "element" is a row if `data` is
#' "data.frame" or similar, or simply an element if it is a vector) and
#' increments with the stride specified in `by`.  Window widths,
#' alignments, and strides can be specified as varying vectors (CURRENTLY ONLY
#' SCALARS ARE SUPPORTED).
#'
#' There are no special optimizations beyond the use of `{r2c}` functions
#' over regular R functions.  For wide windows there are more efficient
#' solutions depending on the statistic applied.  For example, for rolling means
#' and a few other simple statistics `{data.table}` offers the "on-line"
#' algorithm and `{slider}` the "segment tree" algorithm, each with different
#' performance and precision trade-offs.
#'
#' The semantics of this function are **loosely** modelled on those of
#' `zoo::rollapply`, with additional modifications based on
#' `slider::slide_index`.
#'
#' A visual illustration of the effect of the `align` parameter on the fourth
#' iteration of function application, with `by=1L` and `width=4`:
#'
#' ```
#' ## Window size == 4
#'
#'        +--------- On the 4th iteration, base index is [4]
#'        v
#'  1 2 3 4 5 6 7    seq_along(x)  # `x` represents a vector in `data`
#'        + - - +    align = "left"
#'        + - - +    align = 0L
#'  + - - +          align = "right"
#'  + - - +          align = 3L
#'      + - - +      align = "center"
#'      + - - +      align = 1L
#'    + - - +        align = 2L
#' ```
#'
#' If `align = "left"`, on the 4th iteration elements `[4:7]` will be in the
#' window, which is also true with `align = 0L`.  If `align = "center"` (or
#' `align = 1L`), then elements `[3:6]` will be in the window.
#'
#' @export
#' @inheritParams group_exec
#' @seealso [`r2c`] for more details on the behavior and constraints of
#'   "r2c_fun" functions, [`base::eval`] for the semantics of `enclos`.
#' @param fun an "r2c_fun" function as produced by [`r2c`], except with the
#'   additional restriction that it must be guaranteed to produce scalar
#'   results when accounting for the other parameters used by this function.
#' @param width integer vector of positive, non-NA values interpreted
#'   as window widths of values to supply to `fun` from the `data`
#'   vectors.  Recycled together with `by` to match the length of the `data`
#'   vectors.  WARNING: CURRENTLY ONLY SUPPORTS SCALARS.
#' @param by integer vector of positive, non-NA values interpreted as the stride
#'   to increment the base index after each `fun` application  Recycled together
#'   with `width` to match the length of the `data` vectors.
#' @param partial TRUE or FALSE (default), whether to allow computation on
#'   partial windows. If `FALSE`, incomplete windows will be NA.  If `TRUE`
#'   vectors passed to `fun` may have lengths shorter than the corresponding
#'   window sizes when the windows are partially out of bounds at the end of the
#'   `data` vectors. WARNING: CURRENTLY ONLY SUPPORTS SCALARS.
#' @param align vector of one of "center" (default), "left", "right", or a
#'   positive integer representing what part of the window aligns with the base
#'   index on the vector, where "left" is the part of the window nearest the
#'   first element of the vector.  Integer values represent the left-wards
#'   offset of the "left" end of the window relative to the base index, where
#'   `0L` is equivalent to "left" and `width - 1L` and greater equivalent to
#'   "right".  For even length windows, the window will have one more element to
#'   the right than to the left of the base index.  See details.  WARNING:
#'   CURRENTLY ONLY SUPPORTS SCALARS.
#' @return a numeric vector the same length as the first vector in `data`.
#' @examples
#' r2c_mean <- r2cq(mean(x))
#' with(
#'   mtcars,
#'   window_exec(r2c_mean, hp, width=5)
#' )
#' r2c_len <- r2cq(length(x))
#'
#' window_exec(r2c_len, rep(1, 5), width=5, align='left', partial=TRUE)
#' window_exec(r2c_len, rep(1, 5), width=5, align='center', partial=TRUE)
#' window_exec(r2c_len, rep(1, 5), width=5, align='right', partial=TRUE)

window_exec <- function(
  fun, width, data, MoreArgs=list(), by=1L, partial=FALSE,
  align='center', enclos=parent.frame()
) {
  # FIXME: add validation for shlib
  vetr(
    fun=is.function(.) && inherits(., 'r2c_fun'),
    width=INT.1.POS.STR && . <= .Machine[['integer.max']],
    data=(
      (numeric() || integer()) ||
      (list() && all(is.num_naked(.)) && length(.) > 0)
    ),
    by=INT.1.POS && . <= .Machine[['integer.max']],
    partial=LGL.1,
    align=
      (CHR.1 && . %in% c('center', 'left', 'right')) ||
      (INT.1.POS && . <= .Machine[['integer.max']]),
    MoreArgs=list(),
    enclos=is.environment(.)
  )
  width <- as.integer(width)
  by <- as.integer(by)
  if(is.character(align)) {
    offset <- integer(length(align))
    offset[align == 'center'] <- as.integer((width - 1)/2)
    offset[align == 'right'] <- width - 1L
  } else offset <- align

  if(any(offset >= width)) {
    # Bad check, doesn't account for recycling (ok for scalar)
    stop(
      "All `align` integer values must be less than the corresponding ",
      "width value."
  ) }

  obj <- get_r2c_dat(fun)
  call <- sys.call()
  window_exec_int(
    obj, formals=formals(fun), enclos=enclos,
    width=width, data=data,
    MoreArgs=MoreArgs, by=by,
    offset=offset, partial=partial,
    call=call
  )
}
window_exec_int <- function(
  obj, formals, enclos, width, data, MoreArgs, by, partial, offset, call
) {
  preproc <- obj[['preproc']]
  shlib <- obj[['so']]

  if(!is.list(data)) data <- list(data)
  if(length(d.len <- unique(lengths(data))) > 1L)
    stop("All `data` vectors must be the same length.")
  if(!length(d.len)) stop("`data` may not be empty.")

  # - Match Data to Parameters and Allocate ------------------------------------

  alloc <- match_and_alloc(
    do=data, MoreArgs=MoreArgs, preproc=preproc, formals=formals,
    enclos=enclos, gmax=1L, call=call, fun=r2c::window_exec
  )
  stack <- alloc[['stack']]

  if(ncol(stack) != 1L) stop("Internal Error: unexpected stack state at exit.")
  if(!stack['size', 1L] == 1L) stop("`fun` must return scalar values only.")

  empty.res <- FALSE

  # - Run ----------------------------------------------------------------------

  status <- numeric(1)
  res.i <- which(alloc[['alloc']][['type']] == "res")

  res <- if(d.len) {
    handle <- obj[['handle']]
    if(!is.na(shlib) && !is.loaded("run", PACKAGE=handle[['name']])) {
      handle <- dyn.load(shlib)
    }
    if(!is.loaded("run", PACKAGE=handle[['name']]))
      stop("Could not load native code.")

    # Size calc complicated with variable by
    alp <- prep_alloc(alloc, d.len %/% by)

    status <- run_window_int(
      handle=handle[['name']],
      dat=alp[['dat']],
      dat_cols=alp[['dat_cols']],
      ids=alp[['ids']],
      flag=alp[['flag']],
      control=alp[['control']],
      width=width,
      offset=offset,
      by=by,
      partial=partial
    )
    # Result vector is modified by reference
    alp[['dat']][[res.i]]
  } else {
    numeric()
  }
  if(alloc[['alloc']][['typeof']][res.i] == "integer") res <- as.integer(res)

  if(status) {
    warning("longer object length is not a multiple of shorter object length.")
  }
  res
}



## Should we be able for indices to specify a starting point for the index that
## is independent of the data?  Really it should be a start and end point

window_i_exec <- function(
  fun, width, index, data, MoreArgs=list(), by=1L,
  partial=FALSE, align='center', enclos=parent.frame()
) {
  NULL
}

run_window_int <- function(
  handle, dat, dat_cols, ids, flag, control, width, offset, by, partial
) {
  .Call(
    R2C_run_window,
    handle,
    dat,
    dat_cols,
    ids,
    flag,
    control,
    width,
    offset,
    by,
    partial
  )
}

