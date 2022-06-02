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

## Generate a Shared Library Object From C Code
##
## @param x character the C code to compile
## @return character file name of the SO; will be in a temporary directory, it is
##   the users responsibility to preserve and/or discard the file.

make_shlib <- function(x) {
  if(!is.character(x) || anyNA(x))
    stop("Argument `x` must be character and free of NAs.")

  dir <- tempfile()
  dir.create(dir)
  file.base <- file.path(dir, sprintf('code-%s', rand_string(8)))
  file.src <- paste0(file.base, ".c")
  file.obj <- paste0(file.base, ".so")
  writeLines(x, file.src)
  system2(R.home("bin/R"), c("CMD", "SHLIB", file.src))
  # is this what's returned on windows (we can specify, but should make sure if
  # the extension matters)?
  file.obj
}

rand_string <- function(len, pool=c(letters, 0:9))
  paste0(sample(pool, len, replace=TRUE), collapse="")

#' Compile an R Call Into Machine Instructions
#'
#' Translates an R call into C and compiles it into native instructions using
#' `R CMD SHLIB`, and returns a special "r2c_base_fun" function.  This function
#' will behave similarly to a normal R function with the call as the body and
#' the free symbols bound to parameters in the order they appear in the call
#' tree.  The body will be implemented in "r2c" C code.
#'
#' While "r2c_base_fun" functions can be called in the same way as normal R
#' functions, there is limited value in doing so (at least until such a time as
#' loop support is added).  Instead, they are intended to be invoked indirectly
#' with runners like [`group_exec`] (currently the only one implemented).  The
#' structure of "r2c_base_fun" objects is subject to change without notice in
#' future `r2c` releases.  The only supported uses of them are standard
#' invocation with the `(` operator and use with `r2c` functions that accept
#' them as inputs.
#'
#' The `get_*` functions extract possibly useful data from the `r2c` object
#' embedded in "r2c_base_fun" functions.  `get_r2c_dat` is intended primarily
#' for internal purposes, and thus the structure of the return value is subject
#' to change without notice in future versions of `r2c`.
#'
#' @note "r2c_base_fun" functions embed meta information within themselves for
#'   use by other `r2c` functions using environments.  If you modify these
#'   environments all copies of that "r2c_base_fun" that you may have made will
#'   be affected.  There is no `r2c`-endorsed reason for you to be modifying
#'   these environments.
#'
#' @export
#' @param call an R expression, for `compileq` it is captured unevaluated, for
#'   `compile` it should be pre-quoted.
#' @param dir character(1L) name of a file system directory to store the shared
#'   object file in.  The shared object will also be loaded, so the object file
#'   does not need to be preserved unless a function is serialized for re-use
#'   across sessions.  Currently such re-use is not well tested / supported, and
#'   is unlikely to work well across different machines.
#' @param env environment to use as enclosure to function evaluation environment
#' @param check TRUE or FALSE (default), if TRUE will evaluate the R expression
#'   with the input data and compare that result to the one obtained from the
#'   `r2c` C code evaluation, producing an error if not identical.
#' @param r2c.fun an "r2c_base_fun" object to extract meta data from.
#' @return an "r2c_base_fun" function; this is an unusual function so please see
#'   details.
#' @seealso [`group_exec`] to iterate this function groupwise on data.
#' @examples
#' r2c_sum_add <- r2cq(sum(x + y))
#' r2c(quote(sum(x + y))  ## equivalently
#'
#' ## Retrieve meta data
#' get_r_body(r2c_sum_add)
#' writeLines(get_c_code(r2c_sum_add))

r2c <- function(
  call, env=parent.frame(), dir=tempfile(),
  check=getOption('r2c.check.result', FALSE)
) {
  vetr(is.language(.), dir=character(), check=LGL.1)
  preproc <- preprocess(call)
  so <- make_shlib(preproc[['code']])
  obj <- list2env(list(preproc=preproc, so=so, call=call))

  # generate formals that match the free symbols in the call
  sym.free <- preproc[['sym.free']]
  formals <- replicate(length(sym.free), alist(a=))
  names(formals) <- sym.free

  fun <- fun.dummy <- function() NULL
  formals(fun) <- formals
  environment(fun) <- env
  DOC <- paste0(c("r2c implementation of:", "", deparse(call)), collapse="\n")
  # This is ugly, we embed the object in the call proper so we don't interfere
  # with symbol resolution.
  body(fun) <- if(!check) {
    bquote({
      .(DOC)
      .(obj)  # for ease of access
      group_exec_int(
          .(obj), formals=formals(), env=environment(), groups=NULL,
          data=as.list(environment()), MoreArgs=list(), sorted=TRUE
      )}
    )
  } else {
    # Symbol creation is order so that no created symbols will interfere with
    # symbols referenced in the evaluated expressions.
    bquote({
      .(paste0(DOC, "\nSelf-Check Enabled"))
      .(obj)  # for ease of access
      test <- identical(
        group_exec_int(
          .(obj), formals=formals(), env=environment(), groups=NULL,
          data=as.list(environment()), MoreArgs=list(), sorted=TRUE
        ),
        res <- eval(call, envir=env)
      )
      if(!test) stop("`r2c` eval does not match standard eval.")
    })
  }
  # pre-load to avoid cost on initial execution?  Mostly a benchmarking thing
  # as no matter what we have to load it sometime.
  dyn.load(so)

  class(fun) <- "r2c_base_fun"
  fun
}
#' @export
#' @rdname r2c

r2cq <- function(
  call, dir=tempfile(), check=getOption('r2c.check.result', FALSE)
)
  r2c(substitute(call), dir=dir, check=check)

#' @export
#' @rdname r2c

get_c_code <- function(r2c.fun) get_r2c_dat(r2c.fun)[['preproc']][['code']]
get_r_code <- function(r2c.fun) get_r2c_dat(r2c.fun)[['call']]

#' @export
#' @rdname r2c

get_r2c_dat <- function(r2c.fun) {
  body(r2c.fun)[[3L]]  # the object is embedded in the function
}
