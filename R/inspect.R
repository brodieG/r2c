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

#' Extract Data from "r2c_fun" Objects
#'
#' "r2c_fun" functions contain embedded data used by the runners to call the
#' compiled native code associated with the functions.  The functions documented
#' here extract various aspects of this data.
#'
#' * `get_so_handle` for the return value of the [`dyn.load`] call used to load
#'   the shared object file.
#' * `get_c_code` the generated C code used to produce the shared object, but
#'   for quick inspection `show_c_code` is best.
#' * `show_c_code` retrieves code with `get_c_code` and outputs to screen the
#'   portion corresponding to the compiled expression, or optionally all of it.
#' * `get_r_code` the R call that was translated into the C code; if
#'   processing modified the original call the processed version will also be
#'   shown (see [`r2cq`]).
#' * `get_compile_out` the "stdout" produced during the compilation of the
#'   shared object.
#' * `get_so_bin` the binary shared object file as a raw vector.
#' * `write_so` to write the binary shared object to disk.
#'
#' Most calls seen in the raw version of what `get_r_code` returns will have a C
#' level counterpart labeled with the R call in a comment.  This includes calls
#' that are nested as arguments to other calls, which will appear before the
#' outer call.  Due to how how control structures are implemented the R calls
#' and the C level counterparts will not match up exactly.
#'
#' `r2c` creates the shared objects as temporary files on disk, but subsequently
#' deletes them.  The binary file contents are preserved in memory as part of
#' the "r2c_fun" object.
#'
#' @name r2c-inspect
#' @seealso [`r2c-compile`], [`r2c-preprocess`], [`loaded_r2c_dynlibs`].
#' @export
#' @inheritParams group_exec
#' @param all TRUE or FALSE (default) whether to retrieve all of the C code, or
#'   just the portion directly corresponding to the translated R expression.
#' @param raw TRUE or FALSE (default) whether to display the processed R code
#'   exactly as `r2c` will use it, or to simplify it to make easier to read.  If
#'   a simplification occurred the processed member name will be "processed*".
#' @return For `get_r_code` a list with on or two members, the first "original"
#'   is the R language object provided to the [compilation
#'   functions][r2c-compile], the second "processed" (or "processed*", see `raw`
#'   parameter) is the version that the C code is based on.  For all other
#'   functions a character vector, invisibly for `show_c_code`.
#' @examples
#' r2c_sum_sub <- r2cq(sum(x + y))
#' get_r_code(r2c_sum_sub)
#' show_c_code(r2c_sum_sub)

get_c_code <- function(fun, all=TRUE) {
  vetr(all=LGL.1)
  code <- get_r2c_dat(fun)[['preproc']][['code']]
  if(!all) {
    start <- grep('^int run\\(', code)
    if(length(start) != 1L) stop("Could not detect runner function.")
    code <- code[seq(start + 1L, length(code))]
  }
  code
}
#' @export
#' @rdname r2c-inspect

get_r_code <- function(fun, raw=FALSE) {
  vetr(raw=LGL.1)

  orig <- get_r2c_dat(fun)[['call']]
  processed <- get_r2c_dat(fun)[['call.processed']]
  res <- list()
  different <- !identical(orig, processed)
  final <- if(!raw) clean_call(processed) else processed
  res[['original']] <- orig
  processed.name <-
    if(!identical(final, processed)) 'processed*' else 'processed'
  if(different) res[[processed.name]] <- final
  res
}
# Make the call a little friendlier
#
# In theory we could drop names when input was wholly positionally matched, but
# that's going to be annoying to trace back.  At some point we were planning on
# having the indices into the original call available, but didn't follow through
# because we would have to attach that as an attribute or somesuch (and
# we can't do that for leaves anyway?).
#
# @param level how aggressively to clean, if 2L will also remove calls
#   associated with reconciliation (e.g. vcopy/rec).

clean_call <- function(x, level=1L) {
  fun.name <- get_lang_name(x)
  if(is.call(x) && fun.name %in% REC.FUNS && level == 2L) {
    # drop e.g. vcopy/rec
    x <- clean_call(x[[2L]], level=level)
  } else if(is.call_w_args(x)) {
    if(get_lang_name(x) == "subassign") {
      x <- en_assign(call("[", x[[2L]], x[[3L]]), x[[4L]])
    }
    # Drop dots from e.g. `sum(...=x, )`
    if(!is.null(names(x))) names(x)[names(x) == "..."] <- ""
    # Drop defaults that are set to default values
    fun.defn <- VALID_FUNS[[c(fun.name, 'defn')]]
    if(!is.null(fun.defn) && !is.null(names(x))) {
      defn.frm <- formals(fun.defn)
      default.args <- default_params(defn.frm)
      default.args.nm <- names(defn.frm)[default.args]
      act.def.equal <- vapply(
        default.args.nm,
        function(i) !is.null(defn.frm[[i]]) && identical(defn.frm[[i]], x[[i]]),
        TRUE
      )
      x[default.args.nm[act.def.equal]] <- NULL
    }
    # Drop names if only one param
    if(length(x) == 2L) names(x) <- NULL

    # Undo the if decomposition
    x <- recompose_control(x)

    # Recurse
    if(length(x) > 1L) # Dropping default args can shorten call
      for(i in seq(2L, length(x))) x[[i]] <- clean_call(x[[i]], level=level)
  }
  x
}
#' @export
#' @rdname r2c-inspect

get_so_bin <- function(fun) get_r2c_dat(fun)[['so']]

#' @export
#' @rdname r2c-inspect

write_so <- function(fun, target=tempfile()) {
  vetr(target=CHR.1)
  if(file.exists(target))
    stop("`target` already exists and will not be overwritten.")

  writeBin(get_so_bin(fun), target)
  invisible(target)
}
#' @export
#' @rdname r2c-inspect

get_compile_out <- function(fun) get_r2c_dat(fun)[['compile.out']]

#' @export
#' @rdname r2c-inspect

show_c_code <- function(fun, all=FALSE) {
  vetr(all=LGL.1)
  code <- get_c_code(fun, all=all)
  writeLines(code)
  invisible(code)
}

get_r2c_dat <- function(fun, check=TRUE) {
  vetr(is.function(.) && inherits(., "r2c_fun"), check=LGL.1)
  dat <- try(body(fun)[[c(2L,3L)]])
  if(inherits(try, "try-error"))
    stop("`fun` does not appear to be structured like an r2c function.")
  if(!is.environment(dat))
    stop("Could not find data environment in `fun`")

  if(check) {
    dat.contents <- c(
      'preproc', 'call', 'call.processed', 'so', 'compile.out',
      'R.version', 'r2c.version'
    )
    if(!all(dat.contents %in% ls(dat)))
      stop("`fun` missing some expected components.")
    if(!identical(dat[['R.version']], R.version))
      stop(
        "`fun` was compiled with a different R.version:\n\n",
        paste0(
          utils::capture.output(print(dat[['R.version']])),
          collapse="\n"
      ) )
    if(!identical(dat[['r2c.version']], utils::packageVersion('r2c')))
      stop(
        "`fun` was compiled with a different r2c version (",
        dat[['r2c.version']], ")"
      )
  }
  dat
}
