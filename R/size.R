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

compute_size <- compute_size(alloc, stack, depth, ftype, call, .CALL) {
  # Compute result size
  if(ftype[[1L]] == "constant") {
    # Always constant size, e.g. 1 for `sum`
    asize <- ftype[[2L]]
    size <- list(ftype[[2L]])
  } else if(ftype[[1L]] == "external") {
    # Find which arguments are in play?
    inputs <- target_inputs(ftype)

    # Resolve them with a function?  Can this logic be merged?
    if(!is.function(ftype[[3L]]))
      stop("Internal error: no function to resolve external size.")

    # Probably needs input ids
    # DOES `alloc` CONTAIN SIZES?
    size <- ftype[[3L]](alloc, inputs)
  } else if(ftype[[1L]] %in% c("arglen", "vecrec")) {
    # Length of a specific argument, like `probs` for `quantile`
    # `depth + 1L` should be params to current call (this is only true for
    # the stack, not necessarily for other things in `x`)
    sizes.tmp <- input_arg_size_dat(stack, depth, ftype, call, .CALL)

    # asize uses max group size instead of NA so we can allocate for it
    asize  <- vecrec_max_size(sizes.tmp, gmax)
    size <- vecrec_known_size(sizes.tmp[1L,])  # knowable sizes could be NA
    group <- max(sizes.tmp['group',])          # any group size in the lot?
  } else if(ftype[[1L]] == "eqlen") {
    sizes.tmp <- input_arg_size_dat(stack, depth, ftype, call, .CALL)
    s.tmp <- sizes.tmp['size',]
    g.tmp <- sizes.tmp['group',]
    # All arguments must be equal length.  Branch exec funs are checked in
    # `reconcile_control_flow`, but not here because we don't actually care
    # if the results are unequal sizes in the case the result isn't used.
    if(!vec_eqlen(s.tmp, g.tmp, gmax, gmin) && !name %in% BRANCH.EXEC.SYM) {
      stop(
        "Potentially unequal sizes for parameters ",
        toString(ftype[[2L]]), " in a function that requires them ",
        "to be equal sized:\n", deparseLines(clean_call(call, level=2L))
      )
    }
    # This seems like it's backwards.  `asize` shouldn't be NA?
    asize  <- if(anyNA(s.tmp)) {
      warning("this seems like a bug")
      NA_real_
    } else s.tmp[1L]
    size <- if(anyNA(s.tmp)) gmax else s.tmp[1L]
    group <- max(g.tmp)  # any group size in the lot?
  } else if(ftype[[1L]] == "product") {
    stop("Not implemented")

  }
  asize <- compute_asize_from_size(size)

  stop("Internal Error: unknown function type.")

}


# Compute Possible Iteration Result Sizes
#
# Each iteration's result size will be affected by the interaction of the
# functions in the `r2c` expression with the sizes of the vectors they use.  The
# `r2c` supported functions are of the type that has a result size that can be
# computed from the input sizes.  In some cases the computation is very
# straightforward.  For example, `mean(x)` will always produce a size 1 result.
# More complex are things like `a + b`, where the result size is the larger of
# `a` or `b`.  This creates interesting examples such as with `a` as iteration
# variant and `b` constant size, where either `a` or `b` could be the larger one
# for any given iteration.
#
# In order to compute result sizes we classify all the r2c functions based on
# the relationship between input and output sizes.  The categories are:
#
# * constant: e.g. `mean(x)` always produces return size 1.
# * arglen: e.g. `seq_along(x)` produces result same size as input `x`.
# * eqlen: like `arglen`, except that multiple inputs are used and are expected
#   to be the same length (error if not).
# * vecrec: e.g. `a + b` produces a result the size of the larger input, except
#   if either is zero length the result is zero length.
# * concat: e.g. in `c(a, b)` the result size is the sum of the input sizes.
#
# Additionally there are categories that rely on input values instead of
# input lengths.  In this case the inputs must be of the external variety (i.e.
# group/iteration invariant).
#
# * external: e.g. `numeric(x)` where `x` is an external value (i.e. iteration
#   independent).
# * product: e.g. `numeric_along(x, y)`. here the result size is `length(x) *
#   length(y)`.  `numeric_along` is substituted for things like `numeric(x)` or
#   `numeric(length(x) * length(y))`.
#
# Because the size of an output is not knowable when there are iteration/group
# dependent variables, we record the output sizes as a univariate polynomial of
# the iteration/group size (we'll call that `g` going forward).  The polynomial
# is encoded as a vector of positive integer coefficients, so that the size of
# e.g.:
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
# `vecrec` is particularly challenging because we cannot know which input is
# larger without fixing the value of the iteration size.  This requires keeping
# track of all the possible output polynomial expressions.  We can bind this a
# little bit using `gmax` and `gmin`.  We could also compute the sizes of every
# parameter against the known group size vector, but that is likely to become
# computationally expensive.
#
# @param size_fun a function taking in a list of integer size vectors and
#   returning a list of integer size vectors, where all but `size_product`
#   return length 1 lists.
# @param inputs a list of lists, where each element in the list matches the
#   description of the return value of this function.
# @param gmax scalar integer the size of the largest group / iteration.
# @param gmin scalar integer the size of the smallest group / iteration.
# @return a list of `size_fun` return values.

sizer <- function(size_fun, inputs, gmax, gmin) {
  inputs <- unique(inputs)

  # Drop input sizes that are always smaller than all others, except for 0 size
  inputs.min <- lapply(inputs, "*", gmin)
  inputs.max <- lapply(inputs, "*", gmax)



}
# Compute Allocation Size Guaranteed to Hold Result
#
# @param size list of integer vectors representing coefs to univariate
#   polynomial on group/iteration size.
# @param numeric scalar maximum group/iteration size
# @return
#

alloc_size <- function(size, gmax)
  max(lapply(size, function(x) x * gmax ^ (seq_along(x) - 1L)))

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

size_product <- function(sizes) {
  Reduce(poly_mult, sizes)
}

size_vecrec <- function(sizes) {
  if(any(lengths(sizes) == 0L)) 0L
  else unique(sizes)
}

size_constant <-  function(sizes) {

}

size_concat <- function(sizes) {
  len <- max(lengths(sizes))
  sizes <- lapply(
    sizes, function(x, len) {
      res <- numeric(len)
      res[seq_along(x)] <- x
    }
  )
  sizes.mx <- do.call(cbind, sizes)
  list(rowSums(sizes.mx))
}




