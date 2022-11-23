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
#' `data` vectors.  The windows are aligned relative to a base index that is
#' stepped through the data range using a stride specified with `by`.
#'
#' For `window_exec` windows are always sized `width` elements except in the
#' cases where the window overflows on either end of the data and
#' `partial=TRUE`.  The first base index corresponds to the first element in the
#' data.  The `align` parameter can be used to change how the window is shifted
#' from the base index.  A visual illustration of the effect of the `align`
#' parameter on the fourth iteration of function application, with `by=1L` and
#' `width=4L`:
#'
#' ```
#' ## window_exec(..., width=4L)
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
#' For `window_i_exec` the ends of the window are always `width` apart, but the
#' number of elements included in any given window are determined by how many of
#' the values in `index` fall between the ends of each window, where the "left"
#' end is treated as closed, and the "right" as open.  The initial base index is
#' specified with `start`, which defaults to the first value in `index`.  The
#' base index will be incremented in steps of size `by` so long as its value is
#' no greater than `end`, which defaults to the last value in `index`.  There is
#' no requirement that the size of the output be the same as that of `index`,
#' nor is there one that windows contain values.
#'
#' Because `window_exec` windows are defined in terms of a count of elements,
#' but in `window_i_exec` they are defined in terms of position along the real
#' line, the meaning of numeric `align` values is different for each.  In
#' particular, for `window_exec`, `align="right"` is equivalent to
#' `align=width - 1L`, whereas for `window_i_exec` it is equivalent to
#' `align=width`.
#'
#' The semantics of these function are **loosely** modelled on those of
#' `zoo::rollapply`, with additional modifications based on
#' `slider::slide_index`.
#'
#' There are no special optimizations beyond the use of `{r2c}` functions
#' over regular R functions.  For wide windows there are more efficient
#' solutions depending on the statistic applied.  For example, for rolling means
#' and a few other simple statistics `{data.table}` offers the "on-line"
#' algorithm and `{slider}` the "segment tree" algorithm, each with different
#' performance and precision trade-offs.
#'
#' @note For the purposes of this documentation, the first value in a set or the
#'   lowest value in a range are considered to be the "leftmost" values.  Thus
#'   we think of vectors as starting on the "left" and ending on the "right", and of
#'   the real line as having negative infinity to the "left" of positive infinity.
#' @note Window widths, alignments, and strides must be scalars (this may change
#'   in the future).
#' @note The `window_i_exec` algorithm iterates over the values in the `index`
#'   vector until they come in range of the window as determined by LT and LTE
#'   relational operators in C.  As such, out of order or NA indices may cause
#'   values to end up in a window they do not belong to, or values to be
#'   excluded from windows they belong to.  The exact behavior of NAs with
#'   respect to relational operators in C is not strictly defined, so the
#'   results of `index` vectors containing NAs might vary depending on the C
#'   implementation used to compile this package.  Future versions may check for
#'   and disallow disordered or NA values in `index`.
#'
#' @export
#' @inheritParams group_exec
#' @seealso [`r2c`] for more details on the behavior and constraints of
#'   "r2c_fun" functions, [`base::eval`] for the semantics of `enclos`.
#' @param fun an "r2c_fun" function as produced by [`r2c`], except with the
#'   additional restriction that it must be guaranteed to produce scalar
#'   results when accounting for the other parameters used by this function.
#' @param width integer (`window_exec`) or numeric (`window_i_exec`)
#'   positive, finite, non-NA scalar value interpreted as the count of elements
#'   in a window (`window_exec`) or the width of the window (`window_i_exec`).
#'   For the latter values at both ends of the window are considered part of the
#'   window.
#' @param index a numeric vector with as many elements as `data`, containing
#'   finite, non-NA, monotonically increasing values to slide the window
#'   against.  It is the user's responsibility to ensure these requirements are
#'   met (see notes).  Data entries that correspond to index values within a
#'   window will be used in the calculation associated with that window.
#'   Windows are closed at their beginning and open at their end.
#' @param by integer (`window_exec`) or numeric (`window_i_exec`)
#'   positive, finite, non-NA scalar value interpreted as the stride to
#'   increment the base index after each `fun` application.
#' @param partial TRUE or FALSE (default), whether to allow computation on
#'   partial windows. If `FALSE`, incomplete windows will be NA.  If `TRUE`
#'   vectors passed to `fun` may have lengths shorter than the corresponding
#'   window sizes when the windows are partially out of bounds at the end of the
#'   `data` vectors.
#' @param align vector of one of "center" (default), "left", "right", or a
#'   positive finite non-NA integer (`window_exec`) or numeric (`window_i_exec`)
#'   scalar value representing what part of the window aligns with the base
#'   index on the vector.  Numeric values represent the leftwards offset of the
#'   "left" end of the window relative to the base index, where `0` is
#'   equivalent to "left".  See "Details" for some subtleties about the effect
#'   of align for `window_exec` vs `window_i_exec`.
#' @param start numeric(1) first base index for windows for `window_i_exec`.
#'   The range defined by `start` and `end` should intersect with values in
#'   `index` otherwise you will compute on a series of empty windows.  See
#'   "Details".
#' @param end numeric(1) last base index for windows for `window_i_exec`.  See
#'   `start`.
#' @return a numeric vector, the length of which is determined by the length of
#'   the data and the value of `by` (`window_exec`), or by
#'   `floor(end - start) / by + 1` (`window_i_exec`).
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
#'
#' index <- seq(0, 1, length.out=100) ^ (1/3)
#' window_i_exec(r2c_len, width=.2, by=.2, index=index, data=numeric(100))

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
  } else offset <- as.integer(align)

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
    width=width, index=NULL, data=data,
    MoreArgs=MoreArgs, by=by,
    offset=offset, partial=partial,
    call=call
  )
}
#' @rdname window_exec
#' @export

window_i_exec <- function(
  fun, width, index, data, MoreArgs=list(), by,
  align='center', start=index[1L], end=index[length(index)],
  enclos=parent.frame()
) {
  # FIXME: add validation for shlib
  vetr(
    fun=is.function(.) && inherits(., 'r2c_fun'),
    width=NUM.1.POS && . > 0,
    index=NUM || INT,
    data=(
      (numeric() || integer()) ||
      (list() && all(is.num_naked(.)) && length(.) > 0)
    ),
    by=NUM.1.POS,
    align=
      (CHR.1 && . %in% c('center', 'left', 'right')) ||
      (NUM.1.POS),
    MoreArgs=list(),
    enclos=is.environment(.),
    start=NUM.1,
    end=NUM.1 && . >= start
  )
  width <- as.numeric(width)
  by <- as.numeric(by)
  start <- as.numeric(start)  # could be e.g. POSIXct
  end <- as.numeric(end)

  if(is.character(align)) {
    offset <- numeric(length(align))
    offset[align == 'center'] <- width / 2
    offset[align == 'right'] <- width
  } else offset <- as.numeric(align)

  if(any(offset > width)) {
    # Bad check, doesn't account for recycling (ok for scalar)
    stop(
      "All `align` values must be less than or equal" ,
      " the corresponding width value."
  ) }

  obj <- get_r2c_dat(fun)
  call <- sys.call()
  window_exec_int(
    obj, formals=formals(fun), enclos=enclos,
    width=width, index=index, data=data,
    MoreArgs=MoreArgs, by=by,
    offset=offset, partial=FALSE,
    call=call, start=start, end=end
  )
}

window_exec_int <- function(
  obj, formals, enclos, width, index, data, MoreArgs, by, partial, offset,
  start, end, call
) {
  preproc <- obj[['preproc']]
  shlib <- obj[['so']]

  if(!is.list(data)) data <- list(data)
  if(length(d.len <- unique(lengths(data))) > 1L)
    stop("All `data` vectors must be the same length.")
  if(!length(d.len)) stop("`data` may not be empty.")
  if(!is.null(index) && length(index) != d.len)
    stop(
      "`index` must be the same length as data (is ",
      length(index), " instead of ", d.len, ")"
    )
  if(!is.null(index)) index <- as.numeric(index)


  # - Match Data to Parameters and Allocate ------------------------------------

  alloc <- match_and_alloc(
    do=data, MoreArgs=MoreArgs, preproc=preproc, formals=formals,
    enclos=enclos, gmax=1L, call=call, fun=r2c::window_exec
  )
  stack <- alloc[['stack']]

  if(ncol(stack) != 1L) stop("Internal Error: unexpected stack state at exit.")
  if(stack['size', 1L] != 1L || stack['group', 1L] != 0L)
    stop("`fun` must return scalar values only.")

  empty.res <- FALSE

  # - Run ----------------------------------------------------------------------

  status <- numeric(1)
  res.i <- which(alloc[['alloc']][['type']] == "res")

  # Result size must account for the `by` step-size, will be complicated if
  # not scalar is allowed
  r.len <-
    if(is.numeric(index)) floor((end - start) / by) + 1
    else (d.len - 1L) %/% by + 1L
  if(r.len > 2^48)  # See R_ints 12.1
    stop("Result length exceeds allowed 2^48 (would be ", r.len, ")")

  res <- if(r.len) {
    handle <- obj[['handle']]
    if(!is.na(shlib) && !is.loaded("run", PACKAGE=handle[['name']])) {
      handle <- dyn.load(shlib)
    }
    if(!is.loaded("run", PACKAGE=handle[['name']]))
      stop("Could not load native code.")

    alp <- prep_alloc(alloc, r.len)

    if(is.null(index)) {
      status <- .Call(
        R2C_run_window,
        handle[['name']],
        alp[['dat']],
        alp[['dat_cols']],
        alp[['ids']],
        alp[['flag']],
        alp[['control']],
        width,
        offset,
        by,
        partial
      )
    } else {
      status <- .Call(
        R2C_run_window_i,
        handle[['name']],
        alp[['dat']],
        alp[['dat_cols']],
        alp[['ids']],
        alp[['flag']],
        alp[['control']],
        width,
        offset,
        by,
        index,
        start,
        end
      )
    }
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

