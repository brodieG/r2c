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

# - Helper Functions -----------------------------------------------------------

## Run steps that share a close resemblance to those in `group_exec`

roll_prep <- function(
  obj, data, r.len, formals, enclos, call, runner, MoreArgs, wmax
) {
  if(!r.len > 0) stop("Internal Error: prep only when there is result length.")
  preproc <- obj[['preproc']]
  shlib <- obj[['so']]

  if(!is.list(data)) data <- list(data)
  if(length(d.len <- unique(lengths(data))) > 1L)
    stop("All `data` vectors must be the same length.")
  if(r.len > 2^48)  # See R_ints 12.1
    stop("Result length exceeds allowed 2^48 (would be ", r.len, ")")
  if(wmax > 2^48)  # See R_ints 12.1
    stop("Max window element count of 2^48 exceeded (would be ", wmax, ")")

  # - Match Data to Parameters and Allocate ------------------------------------

  alloc <- match_and_alloc(
    do=data, MoreArgs=MoreArgs, preproc=preproc, formals=formals,
    enclos=enclos, call=call, runner=runner,
    gmax=wmax
  )
  stack <- alloc[['stack']]

  if(ncol(stack) != 1L) stop("Internal Error: unexpected stack state at exit.")
  if(stack['size', 1L] != 1L || stack['group', 1L] != 0L)
    stop("`fun` must be guaranteed to return scalar values.")

  # - Run ----------------------------------------------------------------------

  handle <- obj[['handle']]
  if(!is.na(shlib) && !is.loaded("run", PACKAGE=handle[['name']])) {
    handle <- dyn.load(shlib)
  }
  if(!is.loaded("run", PACKAGE=handle[['name']]))
    stop("Could not load native code.")

  prep_alloc(alloc, r.len)
}
## Convert result to integer if it came in that way, and issue recycling
## warnings if any were generated.

roll_finalize <- function(prep, status) {
  alloc <- prep[[c('alloc','alloc')]]
  res.i <- which(alloc[['type']] == "res")
  res <- prep[['dat']][[res.i]]
  if(alloc[['typeof']][res.i] == "integer") res <- as.integer(res)
  if(status) {
    warning("longer object length is not a multiple of shorter object length.")
  }
  res
}
## Run common steps across all roll functions
##
## Allocation computations, running the C code, cleaning up results.

roll_call <- function(
  r.len, data, fun, enclos, call, runner, crunner, csizer, MoreArgs, ...
) {
  if(r.len) {
    size <-
      if(!is.numeric(csizer)) .Call(csizer, r.len, ...)
      else as.numeric(csizer)

    obj <- get_r2c_dat(fun)
    prep <- roll_prep(
      obj, data=data, r.len=r.len, formals=formals(fun), enclos=enclos,
      call=call, runner=runner, MoreArgs=MoreArgs, wmax=size
    )
    status <- .Call(
      crunner,
      obj[[c('handle', 'name')]],
      prep[['dat']],
      prep[['dat_cols']],
      prep[['ids']],
      prep[['flag']],
      prep[['control']],
      ...
    )
    # Result vector is modified by reference
    roll_finalize(prep, status)
  } else numeric()
}
# Map bounds to integer in 0-3, (see window.c ROLL_WINDOW).

bounds_num <- function(bounds) match(bounds, c("()", "[)", "(]", "[]")) - 1L

#' Compute on Sequential Windows on Data
#'
#' @description
#'
#' A [runner][runners] that calls the native code associated with `fun` on
#' sequential windows along `data` vector(s) with "elements" positioned on the
#' real line.  Data element positions can be specified and irregular, so equal
#' sized windows may contain different number of elements.  Window positions may
#' be specified independent of data element positions.  Each `roll*_exec`
#' function provides a different mechanism for defining the space covered by
#' each window.  All of them will compute `fun` for each iteration with the set
#' of data "elements" that fall within that window.
#'
#' * `rollby_exec`: equal width windows spaced `by` apart.
#' * `rollat_exec`: equal width windows at specific positions given in `at`.
#' * `rollbw_exec`: windows with ends defined explicitly in `left` and `right`.
#'
#' Additionally, [`rolli_exec`] is available for variable integer-width windows
#' spaced `by` apart, but `data` elements are rank-positioned only.
#'
#' @section Data Elements:
#'
#' `data` is made up of "elements", where an "element" is a vector element if
#' `data` is an atomic vector, or a "row" if it is a "data.frame" / list of
#' equal-length atomic vectors.  Elements of `data` are arrayed on the real line
#' by `position`.  The default is for each element to be located at its integer
#' rank, i.e. the first element is at 1, the second at 2, and so on.  Rank
#' position is the sole and implicit option for [`rolli_exec`], which will be
#' more efficient for that case, slightly so for `by = 1`, and more so for
#' larger values of `by`.
#'
#' @section Windows:
#'
#' Windows are intervals on the real line aligned (adjustably) relative to an
#' "anchor" point given by `at` for  `rollat_exec`, or derived from `start` and
#' `by` for `rollby_exec`.  `rollbw_exec` defines the ends of each window
#' explicitly via `left` and `right`.  Interval bounds are closed on the left
#' and open on the right by default.
#'
#' As an illustration for `rollby_exec` and `rollat_exec`, consider the case of
#' `width = 3` windows at the fourth iteration, with various `offset` values.
#' The offset is the distance from the left end of the window to the anchor:
#'
#' ```
#' ## rollby_exec(..., by=1, width=3)
#'                    +------------- 4th iteration, anchor is 4.0
#'                    V
#' 1.0   2.0   3.0   4.0   5.0   6.0   7.0 | < Real Line
#'  1     2     3     4     5     6     7  | < Element Position
#'                    |
#'                    |                      Offset     In-window Elements
#'                    [-----------------)  |     0      {4, 5, 6}
#'           [-----------------)           |  -w/2      {3, 4, 5}
#'  [-----------------)                    |    -w      {1, 2, 3}
#' ```
#'
#' In each case we get three elements in the window, although this is only
#' because the positions of the elements are on the integers.  Because the
#' windows are open on the right, elements that align exactly on the right end
#' of the window are excluded.  With irregularly spaced elements, e.g. with
#' `position = c(1, 1.25, 2.5, 5.3, 7, ...)`, we might see (positions approximate):
#'
#' ```
#' ## rollby_exec(..., by=1, width=3, position=c(1, 1.25, 2.5, 5.3, 7))
#'                    +------------- 4th iteration, base index is 4.0
#'                    V
#' 1.0   2.0   3.0   4.0   5.0   6.0   7.0 | < Real Line
#'  1 2      3        |       4         5  | < Element ~Position Elements
#'                    |
#'                    |                      Offset     In-window
#'                    [-----------------)  |     0      {4}
#'           [-----------------)           |  -w/2      {3, 4}
#'  [-----------------)                    |    -w      {1, 2, 3}
#' ```
#'
#' Unlike with [`rolli_exec`] there is no `partial` parameter as there is no
#' expectation of a fixed number of elements in any given window.
#'
#' A restriction is that both ends of a window must be monotonically increasing
#' relative to their counterparts in the prior window.  This restriction might
#' be relaxed for `rollbw_exec` in the future, likely at the cost of
#' performance.
#'
#' @section Equivalence:
#'
#' The `roll*_exec` functions can be ordered by increasing generality:
#'
#' [`rolli_exec`] < `rollby_exec` < `rollat_exec` < `rollbw_exec`
#'
#' Each of the functions can replicate the semantics of any of the less general
#' functions, but with increased generality come efficiency decreases (see
#' "Performance").  One exception is that [`rolli_exec`] supports fully variable
#' width windows.  `rollbw_exec` supports variable width windows, with the
#' constraint that window ends must be increase monotonically for each
#' iteration.
#'
#' `rolli_exec` has semantics similar to the simple use case for
#' `zoo::rollapply`, `data.table::froll*`, `RcppRoll::roll*`, and
#' `slider::slide_<fun>`.  `rollat_exec(..., position=x, at=x)` has semantics
#' similar to `slider::slide_index`, but is more flexible because `at` need not
#' be `position`.
#'
#' @section Performance:
#'
#' In general `{r2c}` should perform better than most alternate window
#' functions for "arbitrary" statistics (i.e. those that can be composed from
#' `{r2c}` supported functions).  Some packages implement algorithms that will
#' outperform `{r2c}` on wide windows for a small set of simple predefined
#' statistics.  For example, for rolling means `{data.table}` and `{roll}` offer
#' the "on-line" algorithm, and `{slider}` the "segment tree" algorithm, each
#' with different performance and precision trade-offs.
#'
#' In testing with sums we've found the `{slider}` (v0.2.2) "segment tree"
#' algorithm to start outperforming `{r2c}` at window size ~100 for
#' `slider::slide_sum` and at window size ~1000 for `slider::slide_index_sum`.
#'
#' The "on-line" algorithms are significantly faster than either `{r2c}` or
#' `{slider}`, and at least on systems with 80 bit long doubles the precision
#' loss (tested on `{data.table}` 1.4.16) seems tolerable for many applications.
#' The `{data.table}` "exact" algorithm in single thread mode has performance
#' near identical to [`rolli_exec`].
#'
#' For `by` values wider than the typical difference between `position` values,
#' implementations that adjust the search stride along `position` taking advantage of
#' its ordered nature will likely be faster.  [`rolli_exec`] does this.
#'
#' Any ALTREP objects generated for use in `position`, `at`, `left`, or `right`
#' will be expanded.  Implementing ALTREP access for them is desirable, but
#' would complicate the code substantially so is unlikely to get implemented.
#'
#' Recall that the less general the `roll*_` function is, the better performance
#' it will have (see "Equivalence").  The differences are slight between the
#' by/at/bw implementations, and also for `rolli_exec` if `by << n`.  If
#' `by >> n`, `rolli_exec` can be much faster.
#'
#' Testing was done on a 6th generation Skylake.
#'
#' @note For the purposes of this documentation, the first value in a set or the
#'   lowest value in a range are considered to be the "leftmost" values.
#'   We think of vectors as starting on the "left" and ending on the "right",
#'   and of the real line as having negative infinity to the "left" of positive
#'   infinity.
#' @note Position vectors are expected to be monotonically increasing and devoid
#'   of NA and non-finite values.  Additionally it is expected that `right >=
#'   left`.  It is the user's responsibility to ensure these expectations are
#'   met.  Window bounds are compared to element positions sequentially using by
#'   LT, LTE, GT, GTE relational operators in C, the exact set of which
#'   depending on `bounds`.  If any of the position vectors are out of order, or
#'   contain NAs, or non-finite values, some, or all windows may not contain the
#'   elements they should.  Further, if there are any NAs the result may depend
#'   on the C implementation used to compile this package.  Future versions may
#'   check for and disallow disordered, NA, and/or non-finite values in the
#'   position vectors.
#'
#' @export
#' @inheritParams group_exec
#' @family runners
#' @seealso [`r2c`] for more details on the behavior and constraints of
#'   "r2c_fun" functions, [`base::eval`] for the semantics of `enclos`,
#'   [`first_vec`] to retrieve first atomic vector.
#' @param fun an "r2c_fun" function as produced by [`r2c`], except with the
#'   additional restriction that it must be guaranteed to produce scalar
#'   results as used with this function.
#' @param width scalar positive numeric giving the width of the window interval.
#'   Unlike with [`rolli_exec`]'s `n`, `width` must be scalar.
#' @param position finite, non-NA, monotonically increasing numeric vector with
#'   as many elements as `data`.  Each element in `position` is the position on
#'   the real line of the corresponding `data` element (see notes).  Integer
#'   vectors are coerced to numeric.
#' @param by strictly positive, finite, non-NA scalar numeric, interpreted
#'   as the stride to increment the anchor by after each `fun` application.
#' @param at non-NA, finite, monotonically increasing numeric vector of anchor
#'   positions on the real line for each window (see notes).
#'   Integer vectors are coerced to numeric.
#' @param left non-NA, finite, monotonically increasing numeric
#'   positions of the left end of each window on the real line (see notes).
#'   Integer vectors are coerced to numeric.
#' @param right non-NA, finite, monotonically increasing numeric
#'   positions of the left end of each window on the real line, where
#'   `right >= left` (see notes).  Integer vectors are coerced to numeric.
#' @param offset finite, non-na, scalar numeric representing the offset
#'   of the window from its "anchor".  Defaults to 0, which means the left end
#'   of the window is aligned with the anchor (i.e. conceptually equivalent to
#'   `align="left"` for [`rolli_exec`]).  Use `-width/2` for center aligned, and
#'   `-width` for right aligned.  See "Intervals".  Note this default is
#'   different to that for [`rolli_exec`].
#' @param start non-na, finite scalar numeric position on real line of first
#'   "anchor".  Windows may extend to the left of `start` (or to the right of
#'   `end`) based on `offset`, and will include all data elements inside the
#'   window, even if they are outside `[start,end]`.
#' @param end non-na, finite scalar numeric position on real line of last
#'   "anchor", see `start`.
#' @param bounds scalar character to determine whether elements positions on
#'   a window boundary are included or excluded from the window:
#'
#' * "[)": include elements on left boundary, exclude those on right (default).
#' * "(]": include elements on right boundary, exclude those on left.
#' * "[]": include elements on either boundary.
#' * "()": exclude elements on either boundary.
#'
#' @return A numeric vector of length:
#'
#' * `(end - start) %/% by + 1` for `rollby_exec`.
#' * `length(at)` for `rollat_exec`.
#' * `length(left)` for `rollbw_exec`.
#'
#' @examples
#' ## Simulate transactions occurring ~4 days
#' old.opt <- options(digits=3)
#' set.seed(1)
#' count <- 150
#' frequency <- 1/(3600 * 24 * 4)
#' time <- as.POSIXct('2022-01-01') - rev(cumsum(rexp(count, frequency)))
#' revenue <- runif(count) * 100
#' data.frame(time, revenue)[c(1:3,NA,seq(count-2, count)),]
#'
#' r2c_mean <- r2cq(mean(x))
#'
#' ## Mean trailing quarter revenue, computed/reported "monthly"
#' month <- 3600 * 24 * 30  # more or less
#' by30 <- rollby_exec(
#'   r2c_mean, revenue, position=time, width=3 * month, by=month,
#'   start=as.POSIXct('2021-01-01'),
#'   offset=-3 * month   # trailing three months
#' )
#' by30
#'
#' ## Same, but explicit times via `at`; notice these are not exactly monthly
#' timeby30 <- seq(as.POSIXct('2021-01-01'), to=max(time), by=month)
#' timeby30[1:10]
#' at30 <- rollat_exec(
#'   r2c_mean, revenue, position=time, width=3 * month,
#'   at=timeby30, offset=-3 * month
#' )
#' at30
#' identical(by30, at30)
#'
#' ## Use exact monthly times with `at`
#' timebymo <- seq(as.POSIXct('2021-01-01'), to=max(time), by="+1 month")
#' timebymo[1:10]
#' atmo <- rollat_exec(
#'   r2c_mean, revenue, position=time, width=3 * month,
#'   at=timebymo, offset=-3 * month
#' )
#' (rev.90 <- data.frame(time=timebymo, prev.90=atmo))[1:5,]
#'
#' ## Exact intervals with `rollexec_bw`.
#' months <- seq(as.POSIXct('2020-10-01'), to=max(time), by="+1 month")
#' left <- head(months, -3)
#' right <- tail(months, -3)
#' bwmo <- rollbw_exec(r2c_mean, revenue, position=time, left=left, right=right)
#' (rev.qtr <- data.frame(time=right, prev.qtr=bwmo))[1:5,]
#'
#' ## These are not exactly the same because -90 days is not always 3 months
#' atmo - bwmo
#' ## Confirm bwmo is what we think it is (recall, right bound open)
#' identical(bwmo[1], mean(revenue[time >= '2020-10-01' & time < '2021-01-01']))
#'
#' ## Compare current month to trail quarter
#' months2 <- seq(
#'   as.POSIXct('2021-01-01'), length.out=nrow(rev.qtr) + 1, by="+1 month"
#' )
#' left <- months2[-length(months2)]
#' right <- months2[-1]
#' thismo <- rollbw_exec(r2c_mean, revenue, position=time, left=left, right=right)
#' transform(
#'   rev.qtr,
#'   this.month=thismo,
#'   pct.change=round((thismo - prev.qtr)/prev.qtr * 100, 1)
#' )
#' options(old.opt)

rollby_exec <- function(
  fun, data, width, by, offset=0,
  position=seq(1, length(first_vec(data)), 1),
  start=position[1L], end=position[length(position)],
  bounds="[)", MoreArgs=list(), enclos=parent.frame()
) {
  # FIXME: add validation for shlib
  vetr(
    fun=is.function(.) && inherits(., 'r2c_fun'),
    width=NUM.1.POS,
    position=(numeric() || integer()) && length(.) == length(first_vec(data)),
    data=(
      (numeric() || integer()) ||
      (list() && all(is.num_naked(.)))
    ),
    by=NUM.1.POS,
    offset=NUM.1,
    MoreArgs=list(),
    enclos=is.environment(.),
    start=NUM.1,
    end=NUM.1 && . >= start,
    bounds=CHR.1 && . %in% c("()", "[)", "(]", "[]")
  )
  # internal logic has offset as the leftwards offset, but that's confusing so
  # we changed the external definition
  offset <- -as.numeric(offset)
  width <- as.numeric(width)
  by <- as.numeric(by)
  start <- as.numeric(start)   # Coerce to avoid e.g. date arithmetic
  end <- as.numeric(end)
  # Don't coerce underlying numeric vectors (e.g. POSIXct)
  if(typeof(position) != "numeric") position <- as.numeric(position)
  r.len <- (end - start) %/% by + 1
  if(!is.finite(r.len))
    stop("`end`/`start`/`by` combine to produce a too-long result vector.")

  call <- sys.call()

  roll_call(
    runner=r2c::rollby_exec, crunner=R2C_run_window_by,
    csizer=R2C_size_window_by,
    r.len=r.len, data=data, fun=fun, enclos=enclos, call=call,
    MoreArgs=MoreArgs,
    width, offset, by, position,
    start, end, bounds_num(bounds)
  )
}
#' @export
#' @name rollby_exec

rollat_exec <- function(
  fun, data, width, at=position, offset=0,
  position=seq(1, length(first_vec(data)), 1),
  bounds="[)", MoreArgs=list(), enclos=parent.frame()
) {
  vetr(
    fun=is.function(.) && inherits(., 'r2c_fun'),
    width=NUM.1.POS,
    position=(numeric() || integer()) && length(.) == length(first_vec(data)),
    data=(
      (numeric() || integer()) ||
      (list() && all(is.num_naked(.)))
    ),
    at=numeric(),
    offset=NUM.1,
    MoreArgs=list(),
    enclos=is.environment(.),
    bounds=CHR.1 && . %in% c("()", "[)", "(]", "[]")
  )
  width <- as.numeric(width)
  # internal logic has offset as the leftwards offset, but that's confusing so
  # we changed the external definition
  offset <- -as.numeric(offset)
  # Don't coerce underlying numeric vectors (e.g. POSIXct)
  if(typeof(at) != "numeric") at <- as.numeric(at)
  if(typeof(position) != "numeric") position <- as.numeric(position)

  call <- sys.call()

  roll_call(
    runner=r2c::rollat_exec, crunner=R2C_run_window_at,
    csizer=R2C_size_window_at,
    r.len=length(at), data=data, fun=fun, enclos=enclos, call=call,
    MoreArgs=MoreArgs,
    width, offset, at, position,
    bounds_num(bounds)
  )
}
#' @export
#' @name rollby_exec

rollbw_exec <- function(
  fun, data, left, right,
  position=seq(1, length(first_vec(data)), 1),
  bounds="[)", MoreArgs=list(), enclos=parent.frame()
) {
  vetr(
    fun=is.function(.) && inherits(., 'r2c_fun'),
    position=(numeric() || integer()) && length(.) == length(first_vec(data)),
    data=(
      (numeric() || integer()) ||
      (list() && all(is.num_naked(.)))
    ),
    MoreArgs=list(),
    enclos=is.environment(.),
    bounds=CHR.1 && . %in% c("()", "[)", "(]", "[]")
  )
  # Don't coerce underlying numeric vectors (e.g. POSIXct)
  if(typeof(position) != "numeric") position <- as.numeric(position)
  if(typeof(left) != "numeric") left <- as.numeric(left)
  if(typeof(right) != "numeric") right <- as.numeric(right)

  call <- sys.call()

  roll_call(
    runner=r2c::rollbw_exec, crunner=R2C_run_window_bw,
    csizer=R2C_size_window_bw,
    r.len=length(left), data=data, fun=fun, enclos=enclos, call=call,
    MoreArgs=MoreArgs,
    left, right, position,
    bounds_num(bounds)
  )
}

#' Compute on Sequential Regular Windows on Equidistant Data
#'
#' A [runner][runners] that calls the native code associated with `fun` on
#' sequential regularly spaced windows along the `data` vector(s).  Each window
#' is aligned relative to a specific data "element" (anchor), and the set of
#' window size `n` contiguous elements around and including the "anchor" are
#' computed on.  This is a special case of [`rollby_exec`] intended to mimic the
#' semantics of `zoo::rollapply` where `width` is a scalar integer, and
#' implicitly the data elements are equally spaced.
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
#'      * * * *      | "center"  {3, 4, 5, 6}  <- default
#'  * * * *          | "right"   {1, 2, 3, 4}
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
#'     offset=((match(align, c('left', 'center', 'right')) - 1) / 2) * (1 - n)
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
#' Unlike `rolli_exec`, [`rollby_exec`] only supports fixed width windows.
#'
#' The `align` values correspond to numeric values as follows: "left"
#' to `0`, "center" to `-width/2`, and "right" to `-width`.  The default window
#' alignment is equivalent to "left" for [`rollby_exec`], which is different
#' than for this function.
#'
#' @inheritParams rollby_exec
#' @family runners
#' @export
#' @seealso [`r2c`] for more details on the behavior and constraints of
#'   "r2c_fun" functions, [`base::eval`] for the semantics of `enclos`.
#' @param n integer number of adjacent data "elements" to compute `fun` on.
#'   It is called `n` and not `width` to emphasize it is a discrete count
#'   instead of an interval width as in [`rollby_exec`] and friends.  Must be
#'   scalar, or have as many elements as data (see "Data Elements").  For the
#'   latter, specifies the element counts of each window.  Coerced to integer if
#'   numeric.
#' @param by strictly positive scalar integer interpreted as the stride to
#'   increment the "anchor" after each `fun` application.  Coerced to integer if
#'   numeric.
#' @param align scalar character one of "center" (default), "left", or "right",
#'   indicating what part of the window should align to the base index.
#'   Alternatively, a scalar integer where `0` is equivalent to "left", `1 - n`
#'   equivalent to "right", and `(1 - n) %/% 2` is equivalent to "center" (i.e.
#'   represents the offset of the window relative to its anchor).
#' @param partial TRUE or FALSE (default), whether to allow computation on
#'   partial windows that extent out of either end of the data.
#' @return a numeric vector of length `length(first_vec(data)) %/% by`.
#' @examples
#' r2c_mean <- r2cq(mean(x))
#' with(
#'   mtcars,
#'   rolli_exec(r2c_mean, hp, n=5)
#' )
#' r2c_len <- r2cq(length(x))
#'
#' ## Effect of align and partial
#' dat <- runif(5)
#' rolli_exec(r2c_len, dat, n=5, align='left', partial=TRUE)
#' rolli_exec(r2c_len, dat, n=5, align='center', partial=TRUE)
#' rolli_exec(r2c_len, dat, n=5, align='right', partial=TRUE)
#' rolli_exec(r2c_mean, dat, n=5, align='left')
#'
#' ## Variable length windows
#' rolli_exec(r2c_len, dat, n=c(1,3,1,3,1), align='left', partial=TRUE)

rolli_exec <- function(
  fun, data, n, by=1L, align='center', partial=FALSE,
  MoreArgs=list(), enclos=parent.frame()
) {
  # FIXME: add validation for shlib
  vetr(
    fun=is.function(.) && inherits(., 'r2c_fun'),
    n=(numeric() || integer()),
    data=(
      (numeric() || integer()) ||
      (list() && all(is.num_naked(.)))
    ),
    by=INT.1.POS && . <= .Machine[['integer.max']],
    partial=LGL.1,
    align=
      (CHR.1 && . %in% c('center', 'left', 'right')) ||
      (INT.1 && . <= .Machine[['integer.max']] && . >= -.Machine[['integer.max']]),
    MoreArgs=list(),
    enclos=is.environment(.)
  )
  if(!is.integer(n)) {
    n <- as.integer(n)
  }
  nmax <- max(n)
  if(is.na(nmax))
    stop(
      "Argument `n` contains NAs or values ",
      "greater than .Machine[['integer.max']]"
    )
  by <- as.integer(by)
  if(is.character(align)) {
    offset <- integer(length(align))
    offset[align == 'center'] <- as.integer((1 - n)/2)
    offset[align == 'right'] <- 1L - n
  } else offset <- as.integer(align)
  # internal logic has offset as the leftwards offset, but that's confusing so
  # we changed the external definition
  offset <- -offset

  obj <- get_r2c_dat(fun)
  call <- sys.call()
  status <- numeric(1)
  d.len <- length(first_vec(data))
  r.len <- (d.len - 1L) %/% by + 1L

  roll_call(
    runner=r2c::rolli_exec, crunner=R2C_run_window, csizer=nmax,
    r.len=r.len, data=data, fun=fun, enclos=enclos, call=call,
    MoreArgs=MoreArgs,
    n, offset, by, partial
  )
}

