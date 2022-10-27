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
#' at the first row (or first vector element when `data` is a numeric vector)
#' of `data` and increments with the stride specified in `by`.  Window widths,
#' alignments, and strides can be specified as varying vectors.
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
#'        +--------- Base index = 4
#'        v
#'  1 2 3 4 5 6 7    seq_along(x)  # `x` represents 1st vector in `data`
#'        + - - +    align = "left"
#'        + - - +    align = 0L
#'  + - - +          align = "right"
#'  + - - +          align = 3L
#'      + - - +      align = "center"
#'      + - - +      align = 1L
#'    + - - +        align = 2L
#' ```
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
#'   vectors.
#' @param by integer vector of positive, non-NA values interpreted as the stride
#'   to increment the base index after each `fun` application  Recycled together
#'   with `width` to match the length of the `data` vectors.
#' @param partial TRUE or FALSE (default), whether to allow computation on
#'   partial windows. If `FALSE`, incomplete windows will be NA.  If `TRUE`
#'   vectors passed to `fun` may have lengths shorter than the corresponding
#'   window sizes when the windows are partially out of bounds at the end of the
#'   `data` vectors.
#' @param align vector of one of "center" (default), "left", "right", or a
#'   positive integer representing what part of the window aligns with the base
#'   index on the vector, where "left" is the part of the window nearest the
#'   first element of the vector.  Integer values represent the left-wards
#'   offset of the "left" end of the window relative to the base index, where
#'   `0L` is equivalent to "left" and `width - 1L` equivalent to "right".  For
#'   even length windows, the window will have one more element to the right
#'   than to the left of the base index.  It is not possible to j
#'   See details.
#' @return a numeric vector the same length as the first vector in `data`.
#' @examples

window_exec <- function(
  fun, width, data, MoreArgs=list(), by=1L, partial=FALSE,
  align='center', enclos=parent.frame()
) {
  # FIXME: add validation for shlib
  vetr(
    fun=is.function(.) && inherits(., 'r2c_fun'),
    width=INT.POS && length(.) > 0,
    data=(
      (numeric() || integer()) ||
      (list() && all(is.num_naked(.)) && length(.) > 0)
    ),
    by=INT.POS && length(.) > 0,
    partial=LGL.1,
    align=CHR.1 && . %in% c('center', 'left', 'right'),
    MoreArgs=list(),
    enclos=is.environment(.)
  )
  obj <- get_r2c_dat(fun)
  call <- sys.call()
  window_exec_int(
    obj, formals=formals(fun), enclos=enclos, groups=groups, data=data,
    MoreArgs=MoreArgs, call=call
  )
}

## Should we be able for indices to specify a starting point for the index that
## is independent of the data?  Really it should be a start and end point

window_i_exec <- function(
  fun, width, index, data, MoreArgs=list(), by=1L,
  partial=FALSE, align='center', enclos=parent.frame()
) {
  NULL
}

