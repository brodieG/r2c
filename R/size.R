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

# Retrieve Ids For Input Parameters
#
# For size resolution that depends on the size of potentially multiple
# candidates arguments, return the ids from the allocation data that
# correspond to the relevant candidates.
#
# @param ftype see `type` for `cgen`

input_args <- function(stack, depth, ftype, call, .CALL) {
  stack.cand <- stack['depth',] == depth + 1L
  if(is.character(ftype[[2L]])) {
    param.cand.tmp <- colnames(stack)[stack.cand]
    if(!all(param.cand.match <- ftype[[2L]] %in% param.cand.tmp))
      stack_param_missing(
        ftype[[2L]][!param.cand.match], param.cand.tmp,
        call, .CALL
      )
    param.cand <-
      seq_len(ncol(stack))[colnames(stack) %in% ftype[[2L]] & stack.cand]
  } else stop("Internal Error: unexpected arg index type.")

  # Arglen needs to disambiguate multiple params (e.g. `...` may show up
  # multiple times in `colnames(stack)` for any given depth).
  if(ftype[[1L]] == "arglen" && length(ftype) > 2L && is.function(ftype[[3L]])) {
    param.cand.tmp <- ftype[[3L]](param.cand)
    if(
      !is.integer(param.cand.tmp) ||
      !all(param.cand.tmp %in% param.cand)
    )
      stop("Internal Error: parameter disambiguation for sizing failed.")
    param.cand <- param.cand.tmp
  }
  stack['id', param.cand]
}
## Identify Missing Parameters on Stack
##
## Ends execution with Error

stack_param_missing <- function(params, stack.avail, call, .CALL) {
  stop(
    simpleError(
      paste0(
        "Parameter(s) ",
        deparse1(params[!params %in% stack.avail]),
        " missing but required for sizing in\n", deparseLines(call)
      ),
      .CALL
  ) )
}
# Length of a specific argument, like `probs` for `quantile`
# `depth + 1L` should be params to current call (this is only true for
# the stack, not necessarily for other things in `x`)

# Compute Possible Iteration Result Sizes
#
# Each computation result size will be deduced primarily from the input sizes
# accounting for the semantics of the function under computation.
# Special cases like `numeric(x)` will look at the value of their inputs, but
# these functions can only be used when the inputs are external (thus computed
# once before iteration, see `?r2c::r2cq`).  See `alloc` for context.
#
# `r2c` functions are classified by the relationship between their inputs and
# their output size.  The categories are:
#
# * constant: e.g. `mean(x)` always produces return size 1.
# * arglen: e.g. `seq_along(x)` produces result same size as input `x`.
# * eqlen: like `arglen`, except that multiple inputs are used and are expected
#   to be the same length (error if not).
# * vecrec: e.g. `a + b` produces a result the size of the larger input, except
#   if either is zero length the result is zero length.
# * concat: e.g. in `c(a, b)` the result size is the sum of the input sizes.
# * prod: e.g. `r2c::numeric_along(x, y)`. here the result size is
#   `length(x) * length(y)`.  `numeric_along` is substituted for things like
#   `numeric(x)` or `numeric(length(x) * length(y))`.
# * extern: e.g. `numeric(x)` where `x` is an external value (i.e. iteration
#   constant).
#
# Because the size of an output may depend on iteration/group size, we express
# size as a univariate polynomial of the iteration/group size (we'll call that
# size `i` going forward).  The polynomial is encoded as a vector of positive
# integer coefficients, so that the size of e.g.:
#
#     c(mean(x), c(x, x))
#
# Will be recorded as:
#
#     c(1L, 2L)
#
# Where 1L is the `i^0` (i.e. constant) coefficient, 2L the `i^1` coefficient,
# and so on.  Higher degree polynomials are possible in situations like
# `numeric_along(x, y)` where a length is the product of two input lenghts.
#
# With `vecrec` we cannot know which input is larger without fixing the value of
# the iteration size (e.g. imagine `(1:5) + x` where `x` is iteration-varying;
# which is larger?).  Because of this we keep track of sizes as lists of
# polynomial coefficients, which we only reduce to actual sizes when we have an
# iteration size we want to check against.  It is possible for the complexity of
# a size calculation to blow-up if there are many nested calculations that
# produce different size coefficients and we limit how many different
# coefficients a single input is allowed.  Expressions that exceed that limit
# will fail with an error.  `vecrec` right now is the only type of computation
# that produces these length > 1 coeficient lists.
#
# @param gmax scalar integer the size of the largest group / iteration.
# @param gmin scalar integer the size of the smallest group / iteration.
# @param size.coef list of integer vectors representing the size of the result
#   of an expression, or that of a single input into a function.  Each integer
#   vector represents a possible size for the result/input expressed as a
#   univariate polynomial on group size, where the first element is power 0
#   (i.e.  constant), second is group size, third group size squared, etc.  See
#   details for why there may be more than one such vector.
# @param size.args list of lists as described in `size.coef`, used to represent
#   the set of inputs into a function.
# @param ftype list corresponds to the `type` parameter for `cgen` (see docs for
#   that in code.R).
#
# @return a list with members:
#
# * "size.coef": a list as described in `size.coef`.
# * "asize": an integer-like number describing max allocation size to hold the
#   result of the current call for any iteration.

compute_size <- function(
  alloc, stack, depth, gmax, gmin, ftype, waive.eqlen, call, .CALL
) {
  # Compute result size
  if(ftype[[1L]] == "constant") {
    # Always constant size, e.g. 1 for `sum`
    size.coef <- list(ftype[[2L]])
  } else {
    # Select inputs
    inputs <- input_args(
      stack=stack, depth=depth, ftype=ftype, call=call, .CALL=.CALL
    )
    in.size <- alloc[['size.coefs']][inputs]
    # Compute group/iteration dependent size
    size.coef <- switch(
      ftype[[1L]],
      extern={
        if(!is.function(ftype[[3L]]))
          stop("Internal error: no function to resolve external size.")
        if(!any(alloc[['type']][inputs] == 'ext'))
          stop(
            "No external parameter(s) ", toString(names(inputs)),
            " in a function that requires some to be external:\n":
            deparseLines(clean_call(call, level=2L))
          )
        size <- ftype[[3L]](alloc, inputs, gmax, gmin)
        if(!is.size_coef(size))
          stop("Internal Error: external parameter size comp invalid.")
        size
      },
      eqlen={
        size <- size_eqlen(in.size, gmax, gmin)
        if(length(size) != 1L && !waive.eqlen) {
          stop(
            "Potentially unequal sizes for parameters ",
            toString(ftype[[2L]]), " in a function that requires them ",
            "to be equal sized:\n", deparseLines(clean_call(call, level=2L))
          )
        }
        size[1L]
      },
      arglen=size_arglen(in.size),
      vecrec=size_vecrec(in.size, gmax, gmin),
      prod=size_prod(in.size),
      concat=size_concat(in.size),
      stop("Internal Error: unknown function size type.")
    )
  }
  # Simplify result size; `vecrec` is the only type that natively produces > 1
  # results (eqlen does here, but that case is an error triggered lter) so we
  # operate under assumption that `vecrec` simplification is always correct.
  size.coef <- size_vecrec(list(size.coef), gmax, gmin)
  size.coef <- lapply(size.coef, as.numeric)

  # Determine allocation
  asize <- compute_asize_from_size(size.coef, gmax)
  list(size.coef=size.coef, asize=asize)
}

# Convert polynomial size vector to an actual size given an iteration size.
actual_size  <- function(x, base) sum(x * base ^ (seq_along(x) - 1L))

# Like `actual_size`, except that in this case `base` is not a scalar but a
# vector of all the group/iteration sizes in the result.  Special case is when
# result is zero size.
iter_result_sizes <- function(x, base) {
  if(length(x) < 1L)
    stop("Internal Error: size.coef expected to contain at least one entry.")
  else if(length(x) == 1L || all(x[-1L] == 0)) {
    # constant size
    rep(as.numeric(x[1L]), length(base))
  } else {
    Reduce("+", lapply(seq_along(x), function(xi) x[xi] * base ^ (xi - 1)))
  }
}

# Given multiple possible for a single input/result, what allocation size will
# hold the largest of them for any given iteration size.  Typically we compute
# this for the largest iteration size (`gmax`).
alloc_size <- function(size.coef, base)
  max(vapply(size.coef, actual_size, 0, base))

compute_asize_from_size <- function(size.coef, gmax) {
  alloc_size(size.coef, gmax)
}
is.size_coef <- function(size.coef) {
  if(is.list(size.coef)) {
    coef.ul <- unlist(size.coef)
    all(vapply(size.coef, is.numeric, TRUE)) &&
      !anyNA(coef.ul) && all(coef.ul >= 0) && all(round(coef.ul) == coef.ul)
  } else FALSE
}

valid_size_input <- function(size.args, error=TRUE) {
  s.ul <- unlist(size.args)
  test <- is.list(size.args) && all(vapply(size.args, is.size_coef, TRUE))
  if(error && !test)
    stop("Internal Error: bad input size list.")
  else test
}

# Vector Recycling

size_vecrec <- function(size.args, gmax, gmin) {
  # for vecrec, one arg with multiple potential lengths is the same as more
  # individual args each with one of those potential lengths
  size.ul <- unique(unlist(size.args, recursive=FALSE))
  sizes.max <- vapply(size.ul, actual_size, 0, gmax)
  sizes.min <- vapply(size.ul, actual_size, 0, gmin)
  # Try to reduce possible sizes under assumption that if a size.ul expr
  # is larger both at gmin & gmax than others, it will always be larger.
  if(any(sizes.max == 0 & sizes.min == 0)) list(0) # zero length dominates
  else if(which.max(sizes.max) == which.max(sizes.min))
    size.ul[which.max(sizes.max)]
  else size.ul
}
# Equal size parameters
#
# Does not actually check that the results are equal size; to confirm make sure
# return value is length 1.

size_eqlen <- function(size.args, gmax, gmin) {
  # for eqlen, one arg with multiple potential lengths is the same as more
  # individual args each with one of those potential lengths
  size.ul <- unlist(size.args, recursive=FALSE)

  # If gmax==gmin, what can we assume?  We can assume that if they are all equal
  # under that assumption the size becomes constant.  Recall that we check
  # elsewhere whether length of this is 1 and error if not.
  if(gmax == gmin) {
    unique(lapply(size.ul, actual_size, gmax))
  } else {
    unique(size.ul)
  }
}
# Size of Input
#
# Currently only supports one input.

size_arglen <- function(size.args) {
  if(length(size.args) > 1) stop("Internal Error: arglen may only use one arg.")
  size.args[[1L]]
}

# Add or Multiply parameter sizes
#
# Due to the possibility of parameters with multiple unresolved sizes (see
# vecrec), we generate a possible output size for each permutation of input
# sizes.
#
# `size_concat` corresponds to functions like `c`, whereas `size_prod`
# corresponds to functions like `outer` (the latter not part of `r2c` -  an
# `r2c` function with those size semantics is `numeric_along` with 2 arguments).
#
# @return A list with integer vectors representing potential result sizes.

size_concat <- function(size.args) {
  size.args.exp <- expand_sizes(size.args)
  lapply(
    size.args.exp,
    function(x) rowSums(do.call(cbind, pad_sizes(x)))
  )
}
size_prod <- function(size.args) {
  size.args.exp <- expand_sizes(size.args) # this validates size.args
  lapply(size.args.exp, Reduce, f=poly_mult)
}

# Generate Permutations of Size Inputs
#
# Returns a list of lists of integer vectors.  Each sub-list represents one
# possible permutation of input sizes and will have as many integer elements as
# there were sub-lists in `size.args`.  The result will have
# `prod(lengths(size.args))` elements, and will be zero-right-padded so that all
# integer vectors are the length of the longest integer vector in the input.
#
# @example
# arg1 <- list(c(1, 0, 2))
# arg2 <- list(c(1, 1), c(2, 3))  # Arg 2 has two size expressions
# r2c:::expand_sizes(list(arg1, arg2))

expand_sizes <- function(size.args) {
  valid_size_input(size.args)
  if(perms <- prod(lengths(size.args)) > 1024)
    stop(
      "Exceeded size expression complexity limit (", perms, ")."
    )
  size.ids <- lapply(size.args, seq_along)
  size.perm <- as.matrix(do.call(expand.grid, size.ids))
  size.exp.i <- split(size.perm, row(size.perm))
  pad_sizes(lapply(size.exp.i, function(x) Map("[[", size.args, x)))
}
# Pad all polynomial coefficients represented by `size.args` to degree of highest
# polynomial.

pad_fun <- function(x, len) {
  length(x) <- len
  x[is.na(x)] <- 0L  # there should not have been any NAs in the input
  x
}
pad_sizes <- function(size.coef) {
  max.degree <- max(lengths(size.coef))
  lapply(size.coef, pad_fun, max.degree)
}

# Multiply Two Polynomials
#
# Adapted from `polynom:::Ops.polynomial` package.  The package does not provide
# a copyright notice but is licensed GPL-2 with the following in the README:
#
# 'polynom' is an R collection of functions to implement a class for
# univariate polynomial manipulations.  It is based on the corresponding S
# package by Bill Venables <Bill.Venables@adelaide.edu.au>, and was
# adapted to R by Kurt Hornik <Kurt.Hornik@R-project.org> and Martin
# Maechler <maechler@stat.math.ethz.ch>.

poly_mult <- function(a, b) {
  factors <- outer(a, b)
  poly <- row(factors) + col(factors)
  as.vector(tapply(factors, poly, sum))
}
# Express complex size coefficients as strings (for errors)

size_coef_el_as_string <- function(x) {
  if(length(x) == 1) as.character(x)
  else if(length(x) >= 2) {
    xc <- as.character(x)
    xc[x == 1 & seq_along(x) > 1] <- ""
    base <- paste0(
      xc,
      c("", "i", paste0(rep("i^", length(x) - 2), (seq_along(x) - 1)[-(1:2)]))
    )
    if(!any(x != 0)) "0"
    else paste0(base[x != 0], collapse=" + ")
  }
  else stop("Internal Error: zero len size coef")
}
size_coef_as_string <- function(x) {
  paste0(vapply(x, size_coef_el_as_string, ""), collapse=", or ")
}
size_coefs_as_string <- function(size.args)
  unlist(lapply(size.args, size_coef_as_string))

