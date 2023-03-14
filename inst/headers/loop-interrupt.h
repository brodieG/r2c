// Assumed this is smaller than R_XLEN_T_MAX
#define INTERRUPT_AT 10000000
// Repeat EXPR in for loop I_MAX times checking for interrupts every
// INTERRUPT_AT iterations.
//
// The following must be defined in the code using the macro:
// * R_xlen_t i (b/c some loops need to know the `i` value on early exit)
// * double ** data
#define LOOP_W_INTERRUPT(I_MAX, EXPR) do {                           \
  R_xlen_t i_stop, next_interrupt;                                   \
  i = 0;                                                             \
  /* First iteration will always get an interrupt, b/c otherwise */  \
  /* we must maintain INTERRUPT_AT in src/run.c as well */           \
  next_interrupt = (R_xlen_t)data[I_STAT][STAT_LOOP];                \
  while(1) {                                                         \
    i_stop = next_interrupt > (I_MAX) ? I_MAX : next_interrupt;      \
                                                                     \
    for(; i < i_stop; ++i) EXPR                                      \
                                                                     \
    /* < i_stop for `break` in EXPR */                               \
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

  /*Rprintf("first next %d\n", next_interrupt);
      Rprintf("interrupt at %d\n", next_interrupt);
  Rprintf("next carry %d imax %d\n", (R_xlen_t)data[I_STAT][STAT_LOOP], I_MAX);
    Rprintf("next next %d\n", next_interrupt);*/
