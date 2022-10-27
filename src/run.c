/*
 * Copyright (C) 2022  Brodie Gaslam
 *
 * This file is part of "r2c - Fast Iterated Statistic Computation in R"
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 2 or 3 of the License.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * Go to <https://www.r-project.org/Licenses> for a copies of the licenses.
 */

// System headers if any go above ^^
#include "r2c.h"
#include <R_ext/Rdynload.h>

extern struct const_dat consts[];
extern int CONST_N;

// Taken from Rdynpriv.h; we'll need to work around this
// Dummy struct, per C99 all pointers to struct are the same size, and since
// we're just going to pass a NULL pointer which R_FindSymbol checks for this
// will work.  Of course not part of the APIS so this can't be how we do the
// final implementation.

struct Rf_RegisteredNativeSymbol {
    int dummy;
};
/*
 * Structure containing the varying data in a format for faster access
 */
struct R2C_dat {
  double ** data;  // Full data (see next for details)
  // data contains some meta data columns first, result vector, followed by the
  // iterations varying data, followed by "static" data (same for every call)
  int dat_start;   // First "iteration varying" data column
  int dat_end;     // Last "iteration varying" data column
  int dat_count;   // Number "iteration varying" data columns
  int ** datai;    // For each sub-fun, which indices in data are relevant
  int * narg;      // For each sub-fun, how many arguments it takes
  int * flags;     // Flag (T/F) control parameters, one for each sub-fun
  SEXP ctrl;       // Non data, non-flag parameters
  R_xlen_t * lens; // Length of each of the data vectors
  DL_FUNC fun;     // function to apply
};
/*
 * Common Data Restructure Steps
 *
 * Shared by group and window functions.  Uses a small amount of R_alloc memory.
 */
static struct R2C_dat prep_data(
  SEXP dat, SEXP dat_cols, SEXP ids, SEXP flag, SEXP ctrl, SEXP so
) {
  if(TYPEOF(so) != STRSXP || XLENGTH(so) != 1)
    Rf_error("Argument `so` should be a scalar string.");
  if(TYPEOF(dat_cols) != INTSXP && XLENGTH(dat_cols) != 1)
    Rf_error("Argument `dat_cols` should be scalar integer.");
  if(TYPEOF(dat) != VECSXP)
    Rf_error("Argument `data` should be a list.");
  if(TYPEOF(ids) != VECSXP)
    Rf_error("Argument `ids` should be a list.");
  if(TYPEOF(ctrl) != VECSXP)
    Rf_error("Argument `ctrl` should be a list.");
  if(TYPEOF(flag) != INTSXP)
    Rf_error("Argument `flag` should be an integer vector.");
  if(XLENGTH(ids) != XLENGTH(ctrl))
    Rf_error("Argument `ids` and `ctrl` should be the same length.");
  if(XLENGTH(flag) != XLENGTH(ctrl))
    Rf_error("Argument `flag` and `ctrl` should be the same length.");

  const char * fun_char = "run";
  const char * dll_char = CHAR(STRING_ELT(so, 0));
  struct Rf_RegisteredNativeSymbol * symbol = NULL;
  DL_FUNC fun = R_FindSymbol(fun_char, dll_char, symbol);
  int dat_count = Rf_asInteger(dat_cols);

  // Not a foolproof check, but we need at least group varying cols + I_GRP data
  if(dat_count < 0 || dat_count > XLENGTH(dat) - I_GRP)
    Rf_error("Internal Error: bad data col count.");

  // Retructure data to be seakable without overhead of VECTOR_ELT
  // R_alloc not guaranteed to align to pointers, blergh. FIXME.
  double ** data = (double **) R_alloc(XLENGTH(dat), sizeof(double*));
  R_xlen_t * lens = (R_xlen_t *) R_alloc(XLENGTH(dat), sizeof(R_xlen_t));
  for(R_xlen_t i = 0; i < XLENGTH(dat); ++i) {
    SEXP elt = VECTOR_ELT(dat, i);
    if(TYPEOF(elt) != REALSXP)
      Rf_error(
        "Internal Error: non-real data at %jd (%s).\n",
        (intmax_t) i, Rf_type2char(TYPEOF(elt))
      );
    *(data + i) = REAL(elt);
    *(lens + i) = XLENGTH(elt);
  }
  // Indices into data, should be as many as there are calls in the code
  int ** datai = (int **) R_alloc(XLENGTH(ids), sizeof(int*));
  int * narg = (int *) R_alloc(XLENGTH(ids), sizeof(int));
  R_xlen_t call_count = XLENGTH(ids);
  for(R_xlen_t i = 0; i < call_count; ++i) {
    SEXP elt = VECTOR_ELT(ids, i);
    if(TYPEOF(elt) != INTSXP)
      Rf_error(
        "Internal Error: non-integer data at %jd (%s).\n",
        (intmax_t) i, Rf_type2char(TYPEOF(elt))
      );
    // Each call accesses some numbers of elements from data, where the last
    // accessed element receives the result of the call (by convention)
    *(datai + i) = INTEGER(elt);
    *(narg + i) = (int) XLENGTH(elt) - 1;
  }
  struct R2C_dat res = {
    .data = data,
    .datai = datai,
    .dat_start = I_GRP,
    .dat_end = I_GRP + dat_count - 1,
    .dat_count = dat_count,
    .narg = narg,
    .lens = lens,
    .flags = INTEGER(flag),
    .ctrl = ctrl,
    .fun = fun
  };
  return res;
}
/*
 * Apply function by groups.
 *
 * See prep_data and R2C_dat struct for what many of the parameters are.
 */

SEXP R2C_run_group(
  SEXP so,
  SEXP dat,
  // How many of the columns of `dat` are of the group varying type.
  SEXP dat_cols,
  SEXP ids,
  SEXP flag,
  SEXP ctrl,
  // Size of each window / group
  SEXP grp_lens,
  SEXP res_lens
) {
  if(TYPEOF(grp_lens) != REALSXP)
    Rf_error("Argument `grp_lens` should be a real vector.");
  if(TYPEOF(res_lens) != REALSXP || XLENGTH(grp_lens) != XLENGTH(res_lens))
    Rf_error("Argument `res_lens` should REALSXP and same length as `grp_lens`.");

  struct R2C_dat datp = prep_data(dat, dat_cols, ids, flag, ctrl, so);

  double * g_lens = REAL(grp_lens);
  double * r_lens = REAL(res_lens);
  R_xlen_t g_count = XLENGTH(grp_lens);
  R_xlen_t grp_recycle_warn = 0;  // these will be stored 1-index

  if(g_count >= R_XLEN_T_MAX)
    Rf_error("Maximum allowed group count of %jd exceeded.", R_XLEN_T_MAX - 1);

  // Compute.  Result will be in `data[I_RES]` and is updated by reference
  for(R_xlen_t i = 0; i < g_count; ++i) {
    R_xlen_t g_len = (R_xlen_t) g_lens[i];  // rows in current group
    R_xlen_t r_len = (R_xlen_t) r_lens[i];  // group result size

    // Update group length and result length; note: `lens` will be updated by
    // each C function invoaked within (*fun), in the end leaving the final
    // result size in `lens[I_RES]`.
    for(int j = datp.dat_start; j <= datp.dat_end; ++j) datp.lens[j] = g_len;

    // Showtime.  This runs the compiled version of `get_c_code` output:
    (*(datp.fun))(
      datp.data, datp.lens, datp.datai, datp.narg, datp.flags, datp.ctrl
    );
    // Record recycling error if any
    if(datp.data[I_STAT][STAT_RECYCLE] && !grp_recycle_warn)
      grp_recycle_warn = i + 1; // g_count < R_XLEN_T_MAX

    // Increment the data pointers by group size; the last increment will be
    // one past end of data, but it will not be dereferenced so okay.
    for(int j = datp.dat_start; j <= datp.dat_end; ++j)
        *(datp.data + j) += g_len;
    if(datp.lens[I_RES] != r_len)
      Rf_error(
        "Group result size does not match expected (%jd vs expected %jd).",
        datp.lens[I_RES], r_len
      );

    // Increment to the next result slot; the last increment will be
    // one past end of data, but it will not be dereferenced so okay.
    *(datp.data + I_RES) += r_len;
  }
  return Rf_ScalarReal((double) grp_recycle_warn);
}
/*
SEXP run_window(
  double ** data,  // updated by ref
  R_xlen_t * lens, // updated by ref
  SEXP grp_lens,
  SEXP res_lens,
  SEXP off_lens,
  SEXP align,
  SEXP partial,
) {
  if(TYPEOF(grp_lens) != REALSXP)
    Rf_error("Argument `grp_lens` should be a real vector.");
  if(TYPEOF(res_lens) != REALSXP || XLENGTH(grp_lens) != XLENGTH(res_lens))
    Rf_error("Argument `res_lens` should REALSXP and same length as `grp_lens`.");

  // When window contains out of bound indices, need to fill if not partial.
  // Probably no, we just say NA.

  // 1. Compute base index offsets based on align and window size.
  //    * Need start and end index
  // 2. If start negative and/or end greater than data length
  //    * Set result to NA, or
  //    * Change start to first and/or end to be last
  // ...
  //
  // In practice, probably have three loops, for the indices we're missing at
  // the start, complete windows, missing at the end.  What about if window
  // bigger than input?  Maybe we also need an incomplete a both ends, and you
  // might be in the complete vs incomplete.
  //
  // For the index version, we probably want a separate function since logic
  // will be quite different.  We also want separate logic for variable width vs
  // fixed width.  With fixed width we know we can just keep advancing start and
  // end.  For variable we need to possibly rewind.
  //
  // In all cases we need to check for

  // Fixed sized windows
  //
  // Maybe start off with the variable size window and offset loop.

  // Variable sized windows and variable size offsets.
  //
  // For these we need to check at each iteration whether we are in bounds or
  // not.


  // Oob left (from center or left)

  // Inbounds

  // Oob right (from center or right)

  // Oob left and right?

  double * g_lens = REAL(grp_lens);
  double * r_lens = REAL(res_lens);
  R_xlen_t g_count = XLENGTH(grp_lens);
  if(g_count >= R_XLEN_T_MAX)
    Rf_error("Maximum allowed group count of %jd exceeded.", R_XLEN_T_MAX - 1);
  R_xlen_t grp_recycle_warn = 0;  // these will be stored 1-index

  // Compute.  Result will be in `data[I_RES]` and is updated by reference
  for(R_xlen_t i = 0; i < g_count; ++i) {
    R_xlen_t g_len = (R_xlen_t) g_lens[i];  // rows in current group
    R_xlen_t r_len = (R_xlen_t) r_lens[i];  // group result size

    // Update group length and result length; note: `lens` will be updated by
    // each C function invoked within (*fun), in the end leaving the final
    // result size in `lens[I_RES]`.
    for(int j = dat_start; j <= dat_end; ++j) lens[j] = g_len;

    // Showtime.  This runs the compiled version of `get_c_code` output:
    (*fun)(data, lens, datai, narg, flag_int, ctrl);

    // Record recycling error if any
    if(data[I_STAT][STAT_RECYCLE] && !grp_recycle_warn)
      grp_recycle_warn = i + 1; // g_count < R_XLEN_T_MAX

    // Increment the data pointers by group size; the last increment will be
    // one past end of data, but it will not be dereferenced so okay.
    for(int j = dat_start; j <= dat_end; ++j) *(data + j) += g_len;
    if(lens[I_RES] != r_len)
      Rf_error(
        "Group result size does not match expected (%jd vs expected %jd).",
        lens[I_RES], r_len
      );

    // Increment to the next result slot; the last increment will be
    // one past end of data, but it will not be dereferenced so okay.
    *(data + I_RES) += r_len;
  }
  return Rf_ScalarReal((double) grp_recycle_warn);
}
*/


/*
SEXP R2C_run_internal(
  SEXP so,
  // starts with the status vector, followed by the result vector, then followed
  // by the group varying data, and finally any collected external references
  // afterwards.
  SEXP dat,
  // How many of the columns of `dat` are of the group varying type.
  SEXP dat_cols,
  SEXP ids,
  SEXP flag,
  SEXP ctrl,
  // Size of each window / group
  SEXP grp_lens,
  SEXP res_lens,
  // For group_exec, same as grp_lens, but different for window_exec.
  // How much to offset the data window between each iteration.
  SEXP off_lens
) {
  if(TYPEOF(so) != STRSXP || XLENGTH(so) != 1)
    Rf_error("Argument `so` should be a scalar string.");
  if(TYPEOF(dat_cols) != INTSXP)
    Rf_error("Argument `dat_cols` should be scalar integer.");
  if(TYPEOF(grp_lens))
    Rf_error("Argument `grp_lens` should REALSXP.");
  if(TYPEOF(off_lens) != REALSXP || XLENGTH(grp_lens) != XLENGTH(off_lens))
    Rf_error("Argument `off_lens` should REALSXP and same length as `grp_lens`.");
  if(TYPEOF(dat) != VECSXP)
    Rf_error("Argument `data` should be a list.");
  if(TYPEOF(ids) != VECSXP)
    Rf_error("Argument `ids` should be a list.");
  if(TYPEOF(ctrl) != VECSXP)
    Rf_error("Argument `ctrl` should be a list.");
  if(TYPEOF(flag) != INTSXP)
    Rf_error("Argument `flag` should be an integer vector.");
  if(XLENGTH(ids) != XLENGTH(ctrl))
    Rf_error("Argument `ids` and `ctrl` should be the same length.");
  if(XLENGTH(flag) != XLENGTH(ctrl))
    Rf_error("Argument `flag` and `ctrl` should be the same length.");

  const char * fun_char = "run";
  const char * dll_char = CHAR(STRING_ELT(so, 0));
  struct Rf_RegisteredNativeSymbol * symbol = NULL;
  DL_FUNC fun = R_FindSymbol(fun_char, dll_char, symbol);
  int dat_count = Rf_asInteger(dat_cols);
  double * g_lens = REAL(grp_lens);
  double * r_lens = REAL(res_lens);
  double * o_lens = REAL(off_lens);

  // Not a foolproof check, but we need at least group varying cols + I_GRP data
  // (note, I_GRP added later, so not 100% sure it's the right constant).
  if(dat_count < 0 || dat_count > XLENGTH(dat) - I_GRP)
    Rf_error("Internal Error: bad data col count.");

  // Retructure data to be seakable without overhead of VECTOR_ELT
  // R_alloc not guaranteed to align to pointers, blergh. FIXME.
  double ** data = (double **) R_alloc(XLENGTH(dat), sizeof(double*));
  R_xlen_t * lens = (R_xlen_t *) R_alloc(XLENGTH(dat), sizeof(R_xlen_t));
  for(R_xlen_t i = 0; i < XLENGTH(dat); ++i) {
    SEXP elt = VECTOR_ELT(dat, i);
    if(TYPEOF(elt) != REALSXP)
      Rf_error(
        "Internal Error: non-real data at %jd (%s).\n",
        (intmax_t) i, Rf_type2char(TYPEOF(elt))
      );
    *(data + i) = REAL(elt);
    *(lens + i) = XLENGTH(elt);
  }
  // Indices into data, should be as many as there are calls in the code
  int ** datai = (int **) R_alloc(XLENGTH(ids), sizeof(int*));
  int * narg = (int *) R_alloc(XLENGTH(ids), sizeof(int));
  R_xlen_t call_count = XLENGTH(ids);
  for(R_xlen_t i = 0; i < call_count; ++i) {
    SEXP elt = VECTOR_ELT(ids, i);
    if(TYPEOF(elt) != INTSXP)
      Rf_error(
        "Internal Error: non-integer data at %jd (%s).\n",
        (intmax_t) i, Rf_type2char(TYPEOF(elt))
      );
    // Each call accesses some numbers of elements from data, where the last
    // accessed element receives the result of the call (by convention)
    *(datai + i) = INTEGER(elt);
    *(narg + i) = (int) XLENGTH(elt) - 1;
  }
  // Integer flags representing TRUE/FALSE control parameters
  int * flag_int = INTEGER(flag);
  int dat_start = I_GRP;
  int dat_end = I_GRP + dat_count - 1;
  R_xlen_t grp_recycle_warn = 0;  // these will be stored 1-index

  if(<rungroup>) run_group(...);
  else if(<runwindow>) run_window(...);

  // Return the recycle status flag; if we add more in the future we'll need to
  // adapt the logic a little for multiple flags
  return Rf_ScalarReal((double) grp_recycle_warn);
}
*/
