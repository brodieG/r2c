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

# Would be better to have preprocessed to dedicated functions instead of having
# all the branches in here.

f_rep <- '
static void %s(%s) {
  double len_out = data[di[2]][0];
  double each = data[di[3]][0];
  double * times = data[di[1]];
  double * x = data[di[0]];
  R_xlen_t len_x = lens[di[0]];
  R_xlen_t len_times = lens[di[1]];
  double *res = data[di[4]];

  if(each > R_XLEN_T_MAX)
    Rf_error("Parameter `each` for `rep` overflows R_XLEN_T_MAX.");
  if(len_out > R_XLEN_T_MAX)
    Rf_error("Parameter `lenght.out` for `rep` overflows R_XLEN_T_MAX.");

  R_xlen_t eacht = (R_xlen_t) each;
  R_xlen_t len_outt = (R_xlen_t) len_out;

  if(ISNAN(len_out)) {
    if(each == 1) {
      if(len_times == 1) {
        double len_resd = (*times) * (double) len_x;
        // Checked already in rep_size so not strictly needed
        if(len_resd > R_XLEN_T_MAX) Rf_error("`rep` would overflow R_XLEN_TMAX");
        for(R_xlen_t time = 0; time < *times; ++time) {
          memcpy(res, x, sizeof(double) * len_x);
          res += len_x;
        }
      } else {
        // Checked already in rep_size, should we skip?
        if(len_x != len_times) Rf_error("Invalid \'times\' value for `rep`.");
        for(R_xlen_t i = 0; i < len_x; ++i) {
          for(R_xlen_t time = times[i]; time; --time) {
            *(res++) = x[i]; // checked this should fit in rep_size
          }
        }
      }
    } else if (each > 1) {
      if(len_times == 1) {
        double len_resd = (*times) * (double) len_x * each;
        // Checked already in rep_size so not strictly needed
        if(len_resd > R_XLEN_T_MAX) Rf_error("`rep` would overflow R_XLEN_TMAX");
        for(R_xlen_t time = 0; time < *times; ++time) {
          for(R_xlen_t i = 0; i < len_x; ++i) {
            for(R_xlen_t eachi = eacht; eachi; --eachi) {
              *(res++) = x[i];
            }
          }
        }
      } else {
        // Checked already in rep_size, should we skip?
        if(len_x * each != len_times)
          Rf_error("Invalid \'times\' value for `rep`.");

        for(R_xlen_t i = 0; i < len_x; ++i) {
          for(R_xlen_t eachi = eacht; eachi; --eachi) {
            for(R_xlen_t time = *(times++); time; --time) {
              *(res++) = x[i];
            }
          }
        }
      }
    }
    // each == 0 case doesnt do anything
  } else {
    if(!len_x) {
      for(R_xlen_t i = 0; i < len_outt; ++i) *(res++) = R_NaReal;
    } else if(each == 1) {
      R_xlen_t i = len_outt;
      while(i > len_x) {
        for(R_xlen_t j = 0; j < len_x; ++j) {
          *(res++) = x[j];
        }
        i -= len_x;
      }
      for(R_xlen_t j = 0; j < i; ++j) *(res++) = x[j];
    } else { // each cannot be 0 (error in `rep_size`).
      R_xlen_t i = len_outt;
      while(i > len_x * each) {
        for(R_xlen_t j = 0; j < len_x; ++j) {
          for(R_xlen_t eachi = eacht; eachi; --eachi) *(res++) = x[j];
        }
        i -= len_x * each;
      }
      for(R_xlen_t j = 0; j < len_x; ++j) {
        for(R_xlen_t eachi = eacht; eachi && i; --eachi, --i) *(res++) = x[j];
      }
    }
  }
  lens[di[4]] = res - data[di[4]];
}'

rep_size <- function(alloc, idx, gmax, gmin) {
  if(length(idx) != 4L)
    stop(
      "There should four sizing params for `rep`, got ", length(idx)
    )
  # See `compute_size(..., size.coef, ...)` for what coefs is.  Here it
  # represents the size of the `x` parameter
  coefs <- alloc[['size.coefs']][[idx[1L]]]

  if(!is.size_coef(coefs)) stop("Internal Error: expected size coef.")
  if(!all(alloc[['type']][idx[2:4]] == "ext"))
    stop(
      "Internal Error: `times`, `length.out` and `each` parameters to `rep` ",
      "must be external."
    )
  # These should be validated already (not their interactions)
  times <- alloc[['dat']][[idx[2L]]]
  length.out <- alloc[['dat']][[idx[3L]]]
  each <- alloc[['dat']][[idx[4L]]]

  if(length.out > 0 && each == 0) stop("invalid 'each' argument for `rep`.")

  if(length(length.out) > 1L)
    warning("first element used of 'length.out' argument.")
  if(length(each) > 1L)
    warning("first element used of 'each' argument.")

  length.out <- round(length.out[1L])
  each <- round(each[1L])

  # Return a new size.coef, computed depending on the external params
  size.coef <- if(!is.na(length.out)) {
    # length.out dominates
    list(length.out)
  } else {
    # `times` can be length 1, or of a constant size that matches coefs of `x`.
    is.size.const <- FALSE
    size.const <- NA_real_

    if(length(times) == 1) {
      lapply(coefs, function(x) x * times * each)
    } else {
      if(length(coefs) == 1L) {
        # Constant size if either only constant coef, or group size is constant.
        is.size.const <- sum(coefs[[1L]][-1L]) == 0 || gmax == gmin
      }
      if(is.size.const) {
        size.const <- actual_size(coefs[[1L]], gmax) * each
      }
      if(!isTRUE(size.const == length(times)))
        stop("invalid 'times' value for `rep`")
      list(sum(times))
    }
  }
  sizes <- vapply(size.coef, actual_size, 0, gmax)

  if(max(sizes) > IX["R_XLEN_T_MAX"])
    stop("`rep` produces a vector longer than R_XLEN_T_MAX (%0.d)", max(sizes))
  size.coef
}

# Not a complete validation, more is done in the sizing fun to make sure size is
# compatible.
valid_times <- function(times) vet(NUM.POS, times)
valid_each <- function(each) vet(NUM.POS && length(.) >= 1L, each)
valid_length.out <- function(length.out)
  vet(numeric() && (is.na(.) | . >= 0), length.out)

code_gen_rep <- function(fun, pars, par.types) {
  vetr(
    identical(., "rep"),
    pars=list(NULL, NULL, NULL, NULL),
    par.types=character(4) && all(.[-1L] %in% PAR.EXT.NUM) && .[1L] %in% PAR.INT
  )
  name <- FUN.NAMES[fun]
  defn <- sprintf(f_rep, name, toString(F.ARGS.BASE))
  code_res(defn=defn, name=name)
}

