## Copyright (C) 2022 Brodie Gaslam
##
## This file is part of "fapply - DSL For Fast Groupwise Numeric Calculations"
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

code_gen_pow <-  function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    identical(., "^"),
    args.reg=list(NULL, NULL),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  name <- "power"
  defn <- sprintf(bin_op_vec_rec, name, toString(F.ARGS.BASE), "pow", ",")
  code_res(defn=defn, name=name, args=ARGS.NM.BASE, headers="<math.h>")
}

pow_transform <- function(call) {
  if(
    !is.call(call) || !identical(as.character(call[[1L]]), "^") ||
    length(call) != 3
  )
    stop("Bad exponent call to transform: ", deparse1(call))

  exp <- call[[3L]]
  if(is.integer(exp)) exp <- as.numeric(exp)
  if(identical(exp, 2)) {
    call <- call("*", call[[2L]], call[[2L]])
  } else if(identical(exp, 3)) {
    val <- call[[2L]]
    call <- call("*", call("*", val, val), val)
  } else if(identical(exp, 4)) {
    val <- call[[2L]]
    call <- call("*", call("*", val, val), call("*", val, val))
  }
  call
}
