/*
 * Copyright (C) Brodie Gaslam
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

/*
 * Basic loop with interrupts for use by the r2c package code.  See
 * inst/headers/loop-interrupt.h for a more complex implementation for use by
 * r2c compiled functions (as opposed to package code), although those are
 * disabled currently..
 *
 * Calling code should declare and initialize:
 * R_xlen_t i, len;
 * R_xlen_t next_interrupt = INTERRUPT_AT; // this needed for vecrec..
 */

#define LOOP_W_INTERRUPT_BASIC(IMAX, EXPR) do {                          \
  while(1) {                                                             \
    R_xlen_t i_stop = next_interrupt > (IMAX) ? (IMAX) : next_interrupt; \
    for(; i < i_stop; ++i) {                                             \
      EXPR                                                               \
    }                                                                    \
    if(i == (IMAX)) break;                                               \
    else if(i == next_interrupt) {                                       \
      R_CheckUserInterrupt();                                            \
      if(i <= R_XLEN_T_MAX - INTERRUPT_AT)                               \
        next_interrupt = i + INTERRUPT_AT;                               \
      else next_interrupt = (IMAX);                                      \
    }                                                                    \
  }                                                                      \
} while(0)



