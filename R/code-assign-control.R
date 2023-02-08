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


f_braces <- '
// Braces are a no-op; the effect is handled allocation side
static void %s(%s) {
  data; di; narg; lens;
}'
# This function will get run, but the results discarded.  We started off using
# it, but decided later easier to skip braces altogether, and it was easier to
# leave this vestigial code here.
code_gen_braces <- function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    identical(., "{"),
    args.reg=list(),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  if(length(args.reg) < 1L) stop("Empty braces expresssions disallowed.")
  name <- "braces"
  defn <- sprintf(f_braces, name, toString(c(F.ARGS.BASE, F.ARGS.VAR)))
  code_res(defn=defn, narg=TRUE, name=name)
}

f_assign <- '
// Read-only assignments are a no-op; the effect is handled allocation side
static void %s(%s) {
  data; di; lens;
}'
code_gen_assign <- function(fun, args.reg, args.ctrl, args.flags) {
  vetr(
    isTRUE(. %in% c("=", "<-")),
    args.reg=list(NULL, NULL),
    args.ctrl=list() && length(.) == 0L,
    args.flags=list() && length(.) == 0L
  )
  name <- "assign"
  defn <- sprintf(f_assign, name, toString(F.ARGS.BASE))
  code_res(defn=defn, name=name)
}
