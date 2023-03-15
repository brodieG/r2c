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

#ifndef R2C_CONST_H
#define R2C_CONST_H
#define R_NO_REMAP

// vvvvvvvvvvvvvvvvvvvvvvvvvvv
// KEEP SYNC'ED WITH src/r2c.h (and rebuild all .o on update)

#define I_STAT      0   // status flags
#define I_RES       1   // final call result
#define I_GRP       2   // index start of group varying data
// Indices in the I_STAT element of the alloc_data
#define STAT_N       2  // STATUS entry count (not an index)
#define STAT_RECYCLE 0  // bad recycling
#define STAT_LOOP    1  // loop counter for interrupts

// KEEP SYNC'ED WITH src/r2c.h
// ^^^^^^^^^^^^^^^^^^^^^^^^^^^

#endif  /* R2C_CONST_H */
