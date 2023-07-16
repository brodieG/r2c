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

target_inputs <- function(ftype, stack) {
}
# Retrieve Ids For Input Parameters
#
# For size resolution that depends on the size of potentially multiple
# candidates arguments, return the size data for the relevant candidates.
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
  } else if(is.integer(ftype[[2L]])) {
    param.cand <- seq_along(stack)[stack.cand][ftype[[2L]]]
    if(anyNA(param.cand))
      stack_param_missing(
        ftype[[2L]][is.na(param.cand)], seq_along(stack)[stack.cand],
        call, .CALL
      )
  } else stop("Internal Error: unexpected arg index type.")

  # Arglen needs to disambiguate multiple params (e.g. `...` may show up
  # multiple times in `colnames(stack)` for any given depth).
  if(length(ftype) > 2L && is.function(ftype[[3L]])) {
    param.cand.tmp <- ftype[[3L]](param.cand)
    if(
      !is.integer(param.cand.tmp) ||
      !all(param.cand.tmp %in% param.cand)
    )
      stop("Internal Error: parameter disambiguation for sizing failed.")
    param.cand <- param.cand.tmp
  }
  param.cand['id',]
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

compute_size <- compute_size(alloc, stack, depth, ftype, call, .CALL) {
  # Compute result size
  if(ftype[[1L]] == "constant") {
    # Always constant size, e.g. 1 for `sum`
    asize <- ftype[[2L]]
    size <- list(ftype[[2L]])
  } else if(ftype[[1L]] == "external") {
    # Find which arguments are in play?
    inputs <- input_args(
      statck=stack, depth=depth, ftype=ftype, call=call, .CALL=.CALL
    )
    if(!is.function(ftype[[3L]]))
      stop("Internal error: no function to resolve external size.")
    size <- ftype[[3L]](alloc[['size']][inputs])
  } else if(ftype[[1L]] %in% c("arglen", "vecrec")) {
    inputs <- input_args(
      statck=stack, depth=depth, ftype=ftype, call=call, .CALL=.CALL
    )
    size <- size_vecrec(alloc[['size']][inputs])
  } else if(ftype[[1L]] == "eqlen") {
    inputs <- input_args(
      statck=stack, depth=depth, ftype=ftype, call=call, .CALL=.CALL
    )
    size <- size_eqlen(alloc[['size']][inputs], gmax, gmin)
    if(length(size) != 1L) {
      stop(
        "Potentially unequal sizes for parameters ",
        toString(ftype[[2L]]), " in a function that requires them ",
        "to be equal sized:\n", deparseLines(clean_call(call, level=2L))
      )
    }
  } else if(ftype[[1L]] == "product") {
    stop("Not implemented")
  }
  asize <- compute_asize_from_size(size, type=ftype[[1L]], gmax, gmin)

  stop("Internal Error: unknown function type.")

}
# Compute Allocation Size Guaranteed to Hold Result
#
# @param size list of integer vectors representing coefs to univariate
#   polynomial on group/iteration size.
# @param numeric scalar maximum group/iteration size
# @return
#

actual_size  <- function(size, base) sum(size * base ^ (seq_along(x) - 1L))
alloc_size <- function(size, base) max(vapply(size, actual_size, 0, base))

compute_asize_from_size <- function(size, type, gmax, gmin) {
  if(length(size) > 1L && type != "vecrec")
    stop("Internal Error: size must resolve to 1 element except for vecrec.")
  alloc_size(sizes, gmax)
}


# Compute Possible Iteration Result Sizes
#
# Each iteration's result size will be affected by the interaction of the
# functions in the `r2c` expression with the sizes of the vectors they use.  For
# most `r2c` supported functions we can deduce the result size from the input
# sizes.  In some special cases like `numeric(x)` where `x` is an external
# variable we can also deduce it from the input value.
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
# * product: e.g. `numeric_along(x, y)`. here the result size is `length(x) *
#   length(y)`.  `numeric_along` is substituted for things like `numeric(x)` or
#   `numeric(length(x) * length(y))`.
# * external: e.g. `numeric(x)` where `x` is an external value (i.e. iteration
#   independent).
#
# Because the size of an output may depend on iteration/group size, we express
# size as a univariate polynomial of the iteration/group size (we'll call that
# size `g` going forward).  The polynomial is encoded as a vector of positive
# integer coefficients, so that the size of e.g.:
#
#     c(mean(x), c(x, x))
#
# Will be recorded as:
#
#     c(1L, 2L)
#
# Where 1L is the `g^0` (i.e. constant) coefficient, 2L the `g^1` coefficient,
# and so on.  Higher degree polynomials are possible in situations like
# `numeric_along(x, y)`.
#
# With `vecrec` we cannot know which input is larger without fixing the value of
# the iteration size.  Because of this we keep track of sizes as lists of
# polynomial coefficients, which we only reduce to actual sizes when we have an
# iteration size we want to check against.
#
#
# This requires keeping track of all the possible output
# polynomial expressions.  We can bind this a little bit using `gmax` and
# `gmin`.  We could also compute the sizes of every parameter against the known
# group size vector, but that is likely to become computationally expensive.
#
# @param size_fun a function taking in a list of integer size vectors and
#   returning a list of integer size vectors, where all but `size_product`
#   return length 1 lists.
# @param inputs a list of lists, where each element in the list matches the
#   description of the return value of this function.
# @param gmax scalar integer the size of the largest group / iteration.
# @param gmin scalar integer the size of the smallest group / iteration.
# @return a list of `size_fun` return values.



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

# Compute Result Size When It is a Product of The Inputs
#
# E.g. a function like `outer` (although that is not currently part of `r2c`).
#
# @param sizes a list of integer vectors, where each vector represents the
#   coefficients of an argument size expressed as a univariate polynomial of the
#   group/iteration size.
# @return an integer vector of the same nature as those in `sizes` that reflects
#   the polynomial multiplication of those in `sizes`.

size_product <- function(size) {
  Reduce(poly_mult, size)
}
# Vector Recycling

size_vecrec <- function(size, gmax, gmin) {
  size[length(size) == 0L] <- 0L
  size <-
    if(any(vapply(size, function(x) sum(x) == 0L, TRUE))) list(0L)
    else unique(size)
  # Try to further reduce possible sizes under assumption that if a size expr is
  # larger both at gmin & gmax than others, it will always be larger.
  sizes.max <- vapply(size, actual_size, 0, gmax)
  sizes.min <- vapply(size, actual_size, 0, gmin)
  if(which.max(sizes.max) == which.max(sizes.min)) size[which.max(sizes.max)]
  else size
}
# Equal size parameters
#
# Does not actually check that the results are equal size; to confirm make sure
# return value is length 1.

size_eqlen <- function(size, gmax, gmin) {
  # If gmax==gmin, what can we assume?  We can assume that if they are all equal
  # under that assumption the size becomes constant.
  size <- if(gmax == gmin) {
    list(alloc_size(size, gmax))
  } else {
    unique(size)
  }

}
# 

size_concat <- function(size) {
  len <- max(lengths(size))
  size <- lapply(
    size, function(x, len) {
      res <- numeric(len)
      res[seq_along(x)] <- x
    }
  )
  size.mx <- do.call(cbind, size)
  list(rowSums(size.mx))
}


#' Can All Vectors Be Considered Equal Size
#'
#' Each element in sizes and groups represents one parameter.
#'
#' @param sizes vector of size values (see `alloc_dat`)
#' @param groups vector of group designations (see `alloc_dat`)
#' @param gmax scalar maximum iteration varying iteration size
#' @param gmin scalar minimum iteration varying iteration size

vec_eqlen <- function(sizes, groups, gmax, gmin) {
  stop("Needs re-implementation")
  # If the known size is less the gmin this is effectively group size always
  # Unless special case of size 0
  sizes[!is.na(sizes) & groups != 0 & sizes < gmin & sizes != 0] <- NA_real_
  if(gmax == gmin) {
    sizes[is.na(sizes)] <- gmax
    length(unique(sizes)) == 1L
  } else if (all(groups != 0)) {
    all(is.na(sizes)) || length(unique(sizes)) == 1L
  } else FALSE
}





