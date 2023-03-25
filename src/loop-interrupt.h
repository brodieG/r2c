/*
 * TESTING ONLY, TO SUPPORT r2c:::sum_add, delete when done.
 */

// Assumed this is smaller than R_XLEN_T_MAX
#define INTERRUPT_AT 10000000
// Repeat EXPR in for loop I_MAX times checking for interrupts every
// INTERRUPT_AT iterations.  Loosely inspired by the R macros in
// R_ext/itermacros.h (as of 4.2).  We could use size_t like they do and save
// the overflow check assuming R_xlen_t is small enough in relation to size_t.
// This also carries over the point of the last check as we don't want to
// trigger the interrupt for every group/iteration unless they are bigger than
// INTERRUPT_AT.
//
// Is there a more efficient way to implement 2 and 3?
//
// The following must be defined in the code using the macro:
// * R_xlen_t i (b/c some loops need to know the `i` value on early exit)
// * double ** data
// * R_xlen_t k and j for INTERRUPT1.

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

#define LOOP_W_INTERRUPT1(I_MAX, EXPR) do {                          \
  LOOP_W_INTERRUPT0((I_MAX), for(; i < i_stop; ++i) EXPR);           \
} while(0)

// J_MAX in excess of I_MAX not interated over
#define LOOP_W_INTERRUPT2(I_MAX, J_MAX, EXPR) do {                   \
  j = 0;                                                             \
  LOOP_W_INTERRUPT0((I_MAX),                                         \
    for(; i < i_stop; ++i, ++j) {                                    \
      if(j == J_MAX) j = 0;                                          \
      EXPR                                                           \
  } );                                                               \
} while(0)

// J_MAX/K_MAX in excess of I_MAX not interated over
#define LOOP_W_INTERRUPT3(I_MAX, J_MAX, K_MAX, EXPR) do {            \
  j = k = 0;                                                         \
  LOOP_W_INTERRUPT0((I_MAX),                                         \
    for(; i < i_stop; ++i, ++j, ++k) {                               \
      if(j == J_MAX) j = 0;                                          \
      if(k == K_MAX) k = 0;                                          \
      EXPR                                                           \
  } );                                                               \
} while(0)

