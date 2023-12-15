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

// Loosely inspired by the R_ext/Itermacros.h from the GNU R sources

// See headers/r2c-const.h for some context.

// Implement interrupts at r2c Expression Level
//
// CURRENTLY INTERRUPTS AS IMPLEMENTED IN THIS HEADER ARE DISABLED!  Instead the
// interrupts are checked between groups / windows by the runner code (see
// src/loop-interrupt-basic.h, uses of R_CheckUserInterrupt in src/window.c).
//
// When enabled, these macros count iterations at the r2c sub-expression level
// and do so across groups so that interrupts are only triggered every
// INTERRUPT_AT iterations across every sub-expression and groups.  Each r2c
// sub-expression will provide EXPR and iteration counter variables limits like
// I_MAX.  Loosely inspired by the R macros in R_ext/itermacros.h (as of 4.2).
// We could use size_t like they do and save the overflow check assuming
// R_xlen_t is small enough in relation to size_t.  This also carries over the
// point of the last check as we don't want to trigger the interrupt for every
// group/iteration unless they are bigger than INTERRUPT_AT.
//
// The following must be defined in the code using the macro:
// * R_xlen_t i (b/c some loops need to know the `i` value on early exit)
// * double ** data
// * R_xlen_t k and/or j for higher order INTERRUPTN
//
// For whatever reason using this interrupt method causes slowdowns for small
// groups.  See extra/bench-interrupt.Rmd for details.  For now we're turning
// this off and relying on per-group/window checks instead of a more
// sophisticated version that check per-op as LOOP_W_INTERRUPT0 does here.
// The turning off is done by subbing in a dummy version of LOOP_W_INTERRUPT0
// which allows us to leave the interrupt code in the functions should we decide
// to switch back to this later.

#ifdef R2C_USE_INLOOP_INTERRUPT
// Prior to re-enabling:
// * Look at `rep`, do we want to add interrupts there?
#define LOOP_W_INTERRUPT0(I_MAX, LOOP) do {                          \
  R_xlen_t i_stop, next_interrupt;                                   \
  i = 0;                                                             \
  /* First iteration will always get an interrupt, b/c otherwise */  \
  /* we must maintain INTERRUPT_AT in src/run.c as well */           \
  next_interrupt = (R_xlen_t)data[I_STAT][STAT_LOOP];                \
  while(1) {                                                         \
    i_stop = next_interrupt > (I_MAX) ? I_MAX : next_interrupt;      \
                                                                     \
    LOOP                                                             \
                                                                     \
    /* < i_stop for `break` in EXPR (e.g. all/any) */                \
    if(i == (I_MAX) || i < i_stop) break;                            \
    else if(i == next_interrupt) {                                   \
      R_CheckUserInterrupt();                                        \
      if(i <= R_XLEN_T_MAX - INTERRUPT_AT)                           \
        next_interrupt = i + INTERRUPT_AT;                           \
      else next_interrupt = (I_MAX);                                 \
    }                                                                \
  }                                                                  \
  data[I_STAT][STAT_LOOP] = (double) next_interrupt - i;             \
} while(0)
#endif /* R2C_USE_INLOOP_INTERRUPT */

// A dummy version that doesn't do the interrupt (this is how we turn it off).
#define LOOP_W_INTERRUPT0(I_MAX, LOOP) do {              \
  i = 0; /* some calling code might need to consult i */ \
  R_xlen_t i_stop = I_MAX;                               \
  LOOP                                                   \
} while(0)

// Each _INTERRUPTN version below supports N iteration variables, each with it's
// own maximum limit to reset counters when hit.

#define LOOP_W_INTERRUPT1(I_MAX, EXPR) do {                          \
  LOOP_W_INTERRUPT0((I_MAX), for(; i < i_stop; ++i) EXPR);           \
} while(0)

// J_MAX in excess of I_MAX not interated over
#define LOOP_W_INTERRUPT2(I_MAX, J_MAX, EXPR) do {                   \
  j = 0;  /* some calling code might need to consult j */            \
  LOOP_W_INTERRUPT0((I_MAX),                                         \
    for(; i < i_stop; ++i, ++j) {                                    \
      if(j == J_MAX) j = 0;                                          \
      EXPR                                                           \
  } );                                                               \
} while(0)

// J_MAX/K_MAX in excess of I_MAX not interated over
#define LOOP_W_INTERRUPT3(I_MAX, J_MAX, K_MAX, EXPR) do {            \
  j = k = 0; /* some calling code might need to consult j, k */      \
  LOOP_W_INTERRUPT0((I_MAX),                                         \
    for(; i < i_stop; ++i, ++j, ++k) {                               \
      if(j == J_MAX) j = 0;                                          \
      if(k == K_MAX) k = 0;                                          \
      EXPR                                                           \
  } );                                                               \
} while(0)

