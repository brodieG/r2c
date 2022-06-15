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

make_shlib <- function(x, dir, quiet) {
  if(!is.character(x) || anyNA(x))
    stop("Argument `x` must be character and free of NAs.")

  dir.create(dir)
  file.base <- file.path(dir, sprintf('r2c-%s', rand_string(10)))
  file.src <- paste0(file.base, ".c")
  file.obj <- paste0(file.base, ".so")
  if(file.exists(file.src))
    stop(
      "Randomly generated file name ", file.src, "' already exists. ",
      "Bad luck?  Try again."
    )
  writeLines(x, file.src)
  comp.out <-
    system2(R.home("bin/R"), c("CMD", "SHLIB", file.src), stdout=TRUE)
  if(!quiet) writeLines(comp.out)
  # is this what's returned on windows (we can specify, but should make sure if
  # the extension matters)?
  list(so=file.obj, out=comp.out)
}

rand_string <- function(len, pool=c(letters, 0:9))
  paste0(sample(pool, len, replace=TRUE), collapse="")

#' Compile Eligible R Calls Into Native Instructions
#'
#' Translates eligible R calls into C, compiles them into native instructions
#' using `R CMD SHLIB`, and returns an interface to that code in the form of an
#' "r2c_fun" function.  This function will behave like an R function that
#' has for body the provided `call` and for parameters the free parameter
#' symbols in the order they appear in the call tree.  Unlike the R function, it
#' will execute native instructions generated directly, and is compatible with
#' `r2c` runner functions like [`group_exec`].
#'
#' While "r2c_fun" functions can be called in the same way as normal R
#' functions, there is limited value in doing so.  Instead, they are intended to
#' be invoked indirectly with runners like [`group_exec`] (currently the only
#' one implemented).  The structure of "r2c_fun" objects is subject to change
#' without notice in future `r2c` releases.  The only supported uses of them
#' are standard invocation with the `(` operator and use with `r2c` functions
#' that accept them as inputs.
#'
#' Currently the following functions are supported in `call`:
#'
#' * Binary operators: `+`, `-`, `*`, `/`, and `^`.
#' * Statistics: `mean`, `sum`, `length`.
#'
#' All calls present in `call` must be in the form `fun(...)` or for operators
#' `a fun b` where `fun` is the unquoted name of the function (i.e. not
#' `"fun"(...)` or many of the other variations that R will normally allow for
#' function invocation).
#'
#' Parameters used with "r2c_fun" supported functions are categorized into data
#' parameters and control parameters.  For example, in `sum(x, na.rm=TRUE)`, `x`
#' is considered a data parameter and `na.rm` a control parameter.  All data
#' parameters must be attribute-less numeric vectors.  Integer vectors are
#' supported, but they are coerced to numeric for all intermediate calculations.
#' If all data inputs are integer and the R counterpart functions in `call`
#' support integer output, the result will be returned as integer.  There are no
#' general restrictions on control parameters, but each implemented function
#' will only accept values for them that would make sense for the R
#' counterparts.
#'
#' @export
#' @param call an R expression, for `r2cq` it is captured unevaluated, for
#'   `r2c` it should be quoted with e.g. [`quote`].
#' @param dir NULL (default), or character(1L) name of a file system directory
#'   to store the shared object file in.  If NULL a temporary directory will be
#'   used. The shared object will also be loaded, and if `dir` is NULL the
#'   directory with the file will be removed after loading.  Currently the
#'   capability to re-use generated shared objects across R sessions is not
#'   formally supported, but can likely be arranged for by preserving the
#'   directory.
#' @param env environment to use as enclosure to function evaluation environment
#' @param check TRUE or FALSE (default), if TRUE will evaluate the R expression
#'   with the input data and compare that result to the one obtained from the
#'   `r2c` C code evaluation, producing an error if not identical.
#' @param clean TRUE or FALSE, whether to remove the `dir` folder containing the
#'   generated C code and the shared object file after the shared object is
#'   [`dyn.load`]ed.  Normally this is an auto-generated temporary folder.  This
#'   will only delete folders that have the same directory root as one generated
#'   by `tempfile()` to avoid accidents.  If you manually provide `dir` you will
#'   need to manually delete the directory yourself.
#' @return an "r2c_fun" function; this is an unusual function so please see
#'   details.
#' @seealso [`group_exec`] to iterate this function groupwise on data,
#'   [`get_c_code`] to retrieve the generate C code use to produce the native
#'   instructions.
#' @examples
#' r2c_sum_add <- r2cq(sum(x + y))
#' r2c_sum_add <- r2c(quote(sum(x + y)))  ## equivalently
#' r2c_sum_add(1, runif(10))

r2c <- function(
  call, dir=NULL, check=getOption('r2c.check.result', FALSE),
  quiet=getOption('r2c.quiet', TRUE), clean=is.null(dir)
) {
  vetr(is.language(.), dir=CHR.1 || NULL, check=LGL.1, quiet=LGL.1, clean=LGL.1)
  preproc <- preprocess(call)
  if(is.null(dir)) dir <- tempfile()

  so <- make_shlib(preproc[['code']], dir=dir, quiet=quiet)
  # pre-load to avoid cost on initial execution?  Mostly a benchmarking thing
  # as no matter what we have to load it sometime.
  handle <- dyn.load(so[['so']])
  if(clean) {
    so[['so']] <- NA_character_
    unlink(dir, recursive=TRUE)
  }
  OBJ <- list2env(
    list(
      preproc=preproc, so=so[['so']], handle=handle,
      call=call, compile.out=so[['out']]
    ),
    parent=emptyenv()
  )
  # generate formals that match the free symbols in the call
  sym.free <- preproc[['sym.free']]
  formals <- replicate(length(sym.free), alist(a=))
  names(formals) <- sym.free

  fun <- fun.dummy <- function() NULL
  formals(fun) <- formals
  environment(fun) <- .BaseNamespaceEnv

  # This is ugly because:
  #
  # 1. We need the function itself to be able to recover the `r2c` object data,
  #    which means we need to embed the actual object (not a symbol referencing
  #    it) in the function (we could alternatively use the `sys.call()` trick
  #    from `rlang` to get the attribute, but that feels like it relies on an
  #    implementation detail).
  # 2. We need the function to survive a re-loading of r2c (but we probably
  #    shouldn't allow it to survive across different versions)
  #
  # Thus, we directly embed the object with `.(OBJ)`, and we make the parent of
  # the function the base environment.  We use the same trick for several other
  # objects, both directly those generated here, and also those that will be
  # generated at call time, to ensure that no run-time objects can interfere
  # with the symbol resolution of the "r2c_fun" against its parameters.

  GEXE <- quote(
    bquote(
      group_exec_int(
        NULL, formals=.(.FRM), env=.(.ENV), groups=NULL,
        data=.(.DAT[1L]), MoreArgs=.(.DAT[-1L]), sorted=TRUE
  ) ) )
  DOC <- as.call(
    list(
      as.name("{"),
      c(
        strrep("-", 60),
        paste0("| ",
          format(
            c(
              "**R2C** implementation of:", strrep(" ", 60),
              deparse(call, width.cutoff=40),
              if(check) c("", "self-check ON", "")
        ) ) ),
        paste0("+", strrep("-", 61))
  ) ) )
  PREAMBLE <- bquote({
    .(DOC)
    .(OBJ)  # for ease of access
    .DAT <- as.list(environment())  # first, so no other symbols
    .FRM <- formals()
    .ENV <- parent.frame()
    .CALL <- sys.call()
    # Force promises, otherwise access missing symbols via .DAT
    tryCatch(
      for(i in names(.DAT)) eval(as.name(i)),
      error=function(e) stop(simpleError(conditionMessage(e), .CALL))
    )
  })
  GEXE[[c(2L, 2L)]] <- OBJ  # nesting bquote a pain
  body(fun) <- if(!check) {
    bquote({
      .(PREAMBLE)
      eval(evalq(.(GEXE)), envir=getNamespace('r2c'))
    })
  } else {
    # Symbol creation is order so that no created symbols will interfere with
    # symbols referenced in the evaluated expressions.
    bquote({
      .(PREAMBLE)
      test <- identical(
        eval(evalq(.(GEXE)), envir=getNamespace('r2c')),
        res <- eval(.(call), envir=.ENV)
      )
      if(!test) stop("`r2c` eval does not match standard eval.")
      res
    })
  }

  class(fun) <- "r2c_fun"
  fun
}
#' @export
#' @rdname r2c

r2cq <- function(
  call, dir=NULL, check=getOption('r2c.check.result', FALSE),
  quiet=getOption('r2c.quiet', TRUE), clean=is.null(dir)
)
  r2c(substitute(call), dir=dir, check=check, quiet=quiet)

#' Extract Data from "r2c_fun" Objects
#'
#' "r2c_fun" functions contain embedded data used by the runners to call the
#' compiled native code associated with the functions.  The `get_*` functions
#' documented here extract various aspects of this data.
#'
#' * `get_so_loc` the file system location of the shared object file.
#' * `get_c_code` the generated C code used to produced the shared object.
#' * `get_r_code` the R call that was translated into the C code.
#' * `get_compile_out` the "stdout" produced during the compilation of the
#'   shared object.
#'
#' @rdname get_r2c_data
#' @aliases get_c_code get_r_code
#' @seealso [`r2c`].
#' @export
#' @param r2c.fun an "r2c_fun" function as generated by e.g. [`r2c`].
#' @return for `get_r_code` the R language object the C code is based on,
#'   otherwise a character vector.
#' @examples
#' r2c_sum_add <- r2cq(sum(x + y))
#' get_r_code(r2c_sum_add)
#' writeLines(get_c_code(r2c_sum_add))

get_c_code <- function(r2c.fun) get_r2c_dat(r2c.fun)[['preproc']][['code']]

#' @export
#' @rdname get_r2c_data

get_r_code <- function(r2c.fun) get_r2c_dat(r2c.fun)[['call']]

#' @export
#' @rdname get_r2c_data

get_so_loc <- function(r2c.fun) get_r2c_dat(r2c.fun)[['so']]

#' @export
#' @rdname get_r2c_data

get_compile_out <- function(r2c.fun) get_r2c_dat(r2c.fun)[['compile.out']]

get_r2c_dat <- function(r2c.fun) {
  as.list(body(r2c.fun)[[c(2L,3L)]])  # the object is embedded in the function
}

