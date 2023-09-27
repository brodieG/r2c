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

unitizer_sect("Parenthesis Removal", {
  fp0 <- r2cq(((1 + 2) * x))
  args(fp0)
  fp0(10)
  fp1 <- r2cq((1 + (2 * x)))
  args(fp1)
  fp1(10)
})
unitizer_sect("Bad Args", {
  r2c_sum('hello')
  with(iris, group_exec(r2c_sum, Species, Species))
})
unitizer_sect("Square Transform", {
  get_r_code(r2c_slope)
})
unitizer_sect("Dots in Def", {
  fsum_add0 <- r2cq(sum(..., x, na.rm=na.rm) - y, check=TRUE)
  fsum_add0(1:2, 3:4, y=10, x=3, na.rm=TRUE)
  fsum_add0(1:2, 3:4, 10, 3, TRUE)  # error

  fsum_add1 <- r2cq(y - sum(x, na.rm=na.rm, ...), check=TRUE)
  fsum_add1(1:2, 3:4, y=10, x=3, na.rm=TRUE)
  fsum_add1(1:2, 3:4, 1, 3, na.rm=TRUE)  # okay (na.rm must be specified)

  # multiple sets of dots
  fsum_add2 <- r2cq(sum(..., x, na.rm=na.rm) - sum(...) * 2, check=TRUE)
  fsum_add2(1:2, x=10, na.rm=TRUE)
})
unitizer_sect("Forwarded Dots", {
  x <- 1:10
  y <- c(20, 100)

  f1 <- function(...) {
    x <- 1
    y <- 2
    r2c_add(...)
  }
  f1(x, y)
  f2 <- function(...) {
    x <- 1
    y <- 2
    group_exec(r2c_add, list(...), rep(1:2, each=5))
  }
  f2(x, rep(y, 5))

  r2c_sumdots <- r2cq(sum(...))
  f2a <- function(...) {
    x <- 1
    y <- 2
    group_exec(r2c_sumdots, list(...), rep(1:2, each=5))
  }
  f2a(x, rep(y, 5))

  # Compile with dots in the code
  f3 <- (function(...) r2cq(sum(...)))(1, 2)
  f3()

  f4 <- (function(...) r2cq(sum(...)))(x, y)
  f4(x, y)
  (function(...) f4(...))(x, y)
  f4(x)

  # Make sure r2cq doesn't get confused about dots
  f5 <- (function(...) r2cq(sum(...)))(1:10, 2)
  f5()
  f5(1:10)
  f5(1:10, 2)
})
unitizer_sect("Self Check", {
  sum_check <- r2cq(sum(x), check=TRUE)
  sum_check(x)
})
unitizer_sect("Formals / Symbol Resolution", {
  r2c_sub_rev <- r2cq(x - y, formals=c('y', 'x'))
  r2c_sub_rev(1, -3)
  r2c_sub_rev2 <- r2cq(x - y, formals=alist(y=, x=10))
  r2c_sub_rev2(-1)

  # Resolve unbound in lexical environment
  x <- c(10, 20)
  r2c_sub_rev3 <- r2cf(function(y) x - y)
  local({x <- 1; group_exec(r2c_sub_rev3, 1:3, 1:3)})
  local({x <- 1; r2c_sub_rev3(1)})

  # Argument should be resolved in calling environment
  xghf <- 42
  local(r2c_sum(xghf))
  local(r2c_sum(xghf), list2env(list(xghf=51)))

  # But we cannot affect the actual internal symbol resolution
  local({
    eval <- stop
    r2c_test <- r2cq(sum(x))
    r2c_test(31)
  })
})
unitizer_sect("brackets/assign", {
  external <- 1:5
  bracket <- r2cq({
    a <- 10
    c <- {
      b <- 3
      b * a * external
    }
    d <- sum(tmp)
    c * d
  }, formals=c('tmp'))
  group_exec(bracket, 1:10, rep(1:2, each=5))

  # Can't assign inside arguments
  r2cq(sum(z <- x, z))

  # But double assignment okay
  r2cq(z <- y <- x)(42)

  # Sym return does not need copy if computed
  sym_return <- r2cq({a <- x + y; a})
  get_r_code(sym_return)
  sym_return(x, y)

  # Symbol return gains an r2c_copy
  sym_return2 <- r2cq(test)
  get_r_code(sym_return2)
  sym_return2(1)

  sym_return3 <- r2cq(test <- x)
  get_r_code(sym_return3)
  sym_return3(c(1, 2))

  # Equal assignment
  r2cq({mu_x = mean(x); x - mu_x})(c(1,2,3))
})
unitizer_sect("processing display", {
  get_r_code(r2c_int)
  get_r_code(r2c_int, raw=TRUE)
})
unitizer_sect("optimization", {
  get_r_code(r2c_int)
  get_r_code(r2cf(intercept, optimize=FALSE))
})
unitizer_sect("double colon", {
  local({`::` <- list; r2cq(base::sum(r2c::square(x)))})(5)
  detach("package:r2c")
  if("package:r2c" %in% search()) stop("r2c should not be attached yet.")
  r2c::r2cq(base::sum(r2c::square(x)))(5)
})
