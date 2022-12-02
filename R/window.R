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

## Helper Functions

roll_prep <- function(obj, data, r.len, formals, enclos, call, fun) {
  if(!r.len > 0) stop("Internal Error: prep only when there is result length.")
  preproc <- obj[['preproc']]
  shlib <- obj[['so']]

  if(!is.list(data)) data <- list(data)
  if(length(d.len <- unique(lengths(data))) > 1L)
    stop("All `data` vectors must be the same length.")
  if(!length(d.len)) stop("`data` may not be empty.")
  if(r.len > 2^48)  # See R_ints 12.1
    stop("Result length exceeds allowed 2^48 (would be ", r.len, ")")

  # - Match Data to Parameters and Allocate ------------------------------------

  alloc <- match_and_alloc(
    do=data, MoreArgs=MoreArgs, preproc=preproc, formals=formals,
    enclos=enclos, gmax=1L, call=call, fun=fun
  )
  stack <- alloc[['stack']]

  if(ncol(stack) != 1L) stop("Internal Error: unexpected stack state at exit.")
  if(stack['size', 1L] != 1L || stack['group', 1L] != 0L)
    stop("`fun` must return scalar values only.")

  # - Run ----------------------------------------------------------------------

  res <- if(r.len) {
    handle <- obj[['handle']]
    if(!is.na(shlib) && !is.loaded("run", PACKAGE=handle[['name']])) {
      handle <- dyn.load(shlib)
    }
    if(!is.loaded("run", PACKAGE=handle[['name']]))
      stop("Could not load native code.")

    alp <- prep_alloc(alloc, r.len)
  }
  roll_finalize(res, res.i, alloc, status)
}
roll_finalize <- function(res, alloc, status) {
  res.i <- which(alloc[['alloc']][['type']] == "res")
  if(alloc[['alloc']][['typeof']][res.i] == "integer") res <- as.integer(res)
  if(status) {
    warning("longer object length is not a multiple of shorter object length.")
  }
  res
}

#' Execute r2c Function on Rolling Windows of Integer Spaced Data
#'
#' Calls the native code associated with `fun` on sequential regularly spaced
#' windows along the `data` vector(s).  Each window is aligned relative to a
#' specific data "element" (anchor), and the set of window size `n` contiguous
#' elements around and including the "anchor" are computed on.  This is a
#' special case of [`rollby_exec`] intended to mimic the semantics of
#' [zoo::rollapply] where `width` is a scalar integer.
#'
#' @inheritSection rollby_exec Data Elements
#'
#' @section Window Alignment:
#'
#' `align` specifies which end of the window aligns with the anchor.  Here we
#' illustrate on the fourth iteration of a call to `rolli_exec`:
#'
#' ```
#' ## rolli_exec(..., data=1:7,  n=4)
#'        +--------- On the 4th iteration, anchor is 4
#'        v
#'  1 2 3 4 5 6 7    | seq_along(first_vec(data))
#'        |
#'        |            Align     In-Window Elements
#'        * * * *    | "left"    {4, 5, 6, 7}
#'  * * * *          | "right"   {1, 2, 3, 4}
#'      * * * *      | "center"  {3, 4, 5, 6}
#' ```
#'
#' For the case of "center" with even sized windows more elements will be to the
#' right than to the left of the anchor.
#'
#' @section Correspondence to [`rollby_exec`]:
#'
#' `rolli_exec` is a slightly more efficient implementation of:
#'
#' ```
#' function(fun, data, n, align, ...)
#'   roll_by_exec(
#'     fun, data,
#'     width=n - 1,
#'     offset=((match(align, c('left', 'center', 'right')) - 1) / 2) * (n - 1)
#'     bounds="[]",
#'     ...
#'    )
#' ```
#'
#' Window element counts correspond to an interval width as `n - 1`, e.g.:
#'
#' ```
#' 1  2  3     |  n = 3
#' [     ]     |  width = 3 - 1 = 2 = n - 1
#' ```
#'
#' The `align` values translate to a number in `[0,width]` such that "left"
#' corresponds to `0`, "center" to `width/2`, and "right" to `width`.
#'
#' @inherit params rollby_exec
#' @family rolling functions
#' @seealso [`first_vec`].
#' @param n integer number of adjacent data "elements" to compute `fun` on.
#'   This is equivalent to the integer case of `width` for `zoo::rollapply`, but
#'   is called `n` to emphasize it is a discrete count instead of an interval
#'   width as in [`rollby_exec`].
#' @param by strictly positive scalar integer interpreted as the stride to
#'   increment the "anchor" after each `fun` application.
#' @param align scalar character one of "center" (default), "left", or "right",
#'   indicating what part of the window should align to the base index.
#'   Alternatively, a scalar integer where `0` is equivalent to "left", `n - 1`
#'   equivalent to "right", and `(width - 1) %/% 2` is equivalent to "center".
#' @return a numeric vector of length `length(first_vec(data)) %/% by`.
#' @examples
#' r2c_mean <- r2cq(mean(x))
#' with(
#'   mtcars,
#'   rolli_exec(r2c_mean, hp, width=5)
#' )
#' r2c_len <- r2cq(length(x))
#'
#' rolli_exec(r2c_len, rep(1, 5), width=5, align='left', partial=TRUE)
#' rolli_exec(r2c_len, rep(1, 5), width=5, align='center', partial=TRUE)
#' rolli_exec(r2c_len, rep(1, 5), width=5, align='right', partial=TRUE)

rolli_exec <- function(
  fun, data, n, by=1L, align='center', partial=FALSE,
  MoreArgs=list(), enclos=parent.frame()
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
      (INT.1 && . <= .Machine[['integer.max']] && . >= -.Machine[['integer.max']]),
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

  obj <- get_r2c_dat(fun)
  call <- sys.call()
  status <- numeric(1)

  res <- if(r.len) {
    prep <- roll_prep(
      obj, data=data, r.len=r.len, formals=formals(fun), enclos=enclos,
      call=call, fun=rollby_exec
    )
    status <- .Call(
      R2C_run_window,
      handle[['name']],
      prep[['dat']],
      prep[['dat_cols']],
      prep[['ids']],
      prep[['flag']],
      prep[['control']],
      width,
      offset,
      by,
      partial
    )
  } else numeric()

  roll_finalize(res, alloc, status)
}
#' Execute r2c Function on Rolling Windows in Data
#'
#' @description
#'
#' Calls the native code associated with `fun` on sequential windows along the
#' `data` vector(s).  The `roll*_exec` functions provide different mechanism for
#' defining the space covered by each window. All of them will compute `fun` for
#' each iteration with the set of data "elements" that fall in the corresponding
#' window.
#'
#' * `rollby_exec` equal width windows at regularly spaced intervals `by` apart.
#' * `rollat_exec` equal width windows at specific locations given in `at`.
#' * `rollin_exec` windows with ends defined explicitly in `left` and `right`.
#'
#' @section Data Elements:
#'
#' `data` is made up of "elements", where an "element" is a vector element if
#' `data` is an atomic vector, or a "row" if it is a "data.frame" / list of
#' equal-length atomic vectors.  Elements of `data` are arrayed on the real line
#' at positions specified by `x`.  The default is for each element to be
#' located at its integer rank, i.e. the first element is at 1, the second at 2,
#' and so on.  Rank position is the sole and implicit option for [`rolli_exec`],
#' which will be more efficient for that case, slightly so for `by = 1`, and
#' more so for larger values of `by`.
#'
#' @section Intervals:
#'
#' Windows are intervals on the real line aligned relative to each anchor for
#' `rollby_exec` and `rollat_exec`, and specified explicitly in `rollin_exec`.
#' Bounds are closed on the left and open on the right.  Both window alignment
#' are adjustable with `bounds` and `offset` respectively.
#'
#' As an illustration for `rollby_exec` and `rollat_exec`, consider the case of
#' width 3 windows (`width = 3`) at the fourth iteration, with various `offset`
#' The offset is the distance from the left end of the window to the anchor:
#'
#' ```
#'                    +------------- 4th iteration, anchor is 4.0
#'                    V
#' 1.0   2.0   3.0   4.0   5.0   6.0   7.0 | < Real Line
#'  1     2     3     4     5     6     7  | < Element Rank
#'                    |
#'                    |                      Offset    In-window Elements
#'           [-----------------)           | o = w/2   {3, 4, 5}
#'                    [-----------------)  | o =   0   {4, 5, 6}
#'  [-----------------)                    | o =   w   {1, 2, 3}
#' ```
#'
#' In each case we get three elements in the window, although this is only
#' because the positions of the elements are on the integers.  Because the
#' windows are open on the right, elements that align exactly on the right end
#' of the window are excluded.  With irregularly spaced elements, e.g. with
#' `x = c(1, 1.25, 2.5, 5.3, 7, ...)`, we might see (positions approximate):
#'
#' ```
#'                    +------------- 4th iteration, base index is 4.0
#'                    V
#' 1.0   2.0   3.0   4.0   5.0   6.0   7.0 | < Real Line
#'  1 2      3        |       4         5  | < Element Rank
#'                    |
#'                    |                      Offset    In-window
#'           [-----------------)           | o = w/2   {3, 4}
#'                    [-----------------)  | o =   0   {4}
#'  [-----------------)                    | o =   w   {1, 2, 3}
#' ```
#'
#' The range within which values are eligible for inclusion in any window is
#' given by `start` and `end`.  Windows that extend outside of this range are
#' considered incomplete; any values that would fall within the window but
#' outside `start` or `end` are ignored.  Setting `partial=FALSE` will skip
#' calculations of those windows and use NA for their result.
#'
#' `rollin_exec` has neither `width` nor `offset` parameters and instead allows
#' you to specify the ends of each window directly with `left` and `right`.
#'
#' @section Performance:
#'
#' There are no special optimizations beyond the use of `{r2c}` functions
#' over regular R functions.  For wide windows there are more efficient
#' solutions depending on the statistic applied.  For example, for rolling means
#' and a few other simple statistics `{data.table}` offers the "on-line"
#' algorithm and `{slider}` the "segment tree" algorithm, each with different
#' performance and precision trade-offs.  In testing with sums we've found the
#' "segment tree" algorithm to start outperforming `{r2c}` at window size ~100.
#' At that size, the `data.table` "on-line" algorithm is significantly faster.
#'
#' For `by` values much wider than the typical difference between `x` values,
#' implementations that adjust the search stride along `x` taking advantage of
#' its ordered nature will likely be faster.  [`rolli_exec`] does this for the
#' special case of integer `x`.
#'
#' @note For the purposes of this documentation, the first value in a set or the
#'   lowest value in a range are considered to be the "leftmost" values.
#'   We think of vectors as starting on the "left" and ending on the "right",
#'   and of the real line as having negative infinity to the "left" of positive
#'   infinity.
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
#'   results as used with this function.
#' @param width scalar positive numeric giving the width of the window interval.
#' @param x finite, non-NA, monotonically increasing numeric vector with as many
#'   elements as `data`.  Each element is the position on the real line of the
#'   corresponding `data` element.  It is the user's responsibility to ensure
#'   these requirements are met (see notes).
#' @param by numeric(1), strictly positive, finite, non-NA interpreted
#'   as the stride to increment the anchor by after each `fun` application.
#' @param partial TRUE or FALSE (default), whether to allow computation on
#'   partial windows. If `FALSE`, incomplete windows will be NA.  If `TRUE`
#'   vectors passed to `fun` may have lengths shorter than the corresponding
#'   window sizes when the windows are partially out of bounds at either end
#'   of the `data` vector(s).
#' @param offset numeric(1), finite, non-na, representing the leftward offset of
#'   the left end of the window from its "anchor".  See "Intervals".
#' @param start numeric(1) position on real line of first "anchor", in
#'   combination with `end` is equivalent in effect to subsetting `data` to the
#'   elements between `start and `end`, without copying the `data`.
#' @param end numeric(1) position on real line of last "anchor", see `start`.
#' @return a numeric vector of length `(end - start) %/% by + 1`
#' @family rolling functions
#' @seealso [`first_vec`].
#' @examples

rollby_exec <- function(
  fun, data, width, by,
  offset=width/2,
  x=seq_along(first_vec(data)),
  start=x[1L], end=x[length(x)], bounds="[)",
  partial=TRUE, MoreArgs=list(), enclos=parent.frame()
) {
  # FIXME: add validation for shlib
  vetr(
    fun=is.function(.) && inherits(., 'r2c_fun'),
    width=NUM.1.POS && . > 0,
    x=numeric() || integer(),
    data=(
      (numeric() || integer()) ||
      (list() && all(is.num_naked(.)) && length(.) > 0)
    ),
    by=NUM.1.POS,
    offset=NUM.1,
    MoreArgs=list(),
    enclos=is.environment(.),
    start=NUM.1,
    end=NUM.1 && . >= start,
    bounds=CHR.1 && . %in% c("()", "[)", "(]", "[]"),
    partial=LGL.1
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

  obj <- get_r2c_dat(fun)
  call <- sys.call()

  d.len <- length(first_vec(data))
  r.len <- (d.len - 1L) %/% by + 1L
  status <- numeric(1)

  res <- if(r.len) {
    prep <- roll_prep(
      obj, data=data, r.len=r.len, formals=formals(fun), enclos=enclos,
      call=call, fun=rollby_exec
    )
    status <- .Call(
      R2C_run_window_i,
      handle[['name']],
      prep[['dat']],
      prep[['dat_cols']],
      prep[['ids']],
      prep[['flag']],
      prep[['control']],
      width,
      offset,
      by,
      x,
      start,
      end,
      match(bounds, c("()", "[)", "(]", "[]")) - 1L,
      partial
    )
    # Result vector is modified by reference
    alp[['dat']][[res.i]]
  } else numeric()

  roll_finalize(res, alloc, status)
}

rollat_exec <- function(
  fun, data, width, at=x,
  offset=width/2,
  x=seq_along(first_vec(data)),
  start=x[1L], end=x[length(x)], bounds="[)",
  partial=TRUE, MoreArgs=list(), enclos=parent.frame()
) {

}

rollin_exec <- function(
  fun, data, width, left, right,
  offset=width/2,
  start=x[1L], end=x[length(x)], bounds="[)",
  partial=TRUE, MoreArgs=list(), enclos=parent.frame()
) {

}

