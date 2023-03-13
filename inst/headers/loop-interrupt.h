#define INTERRUPT_AT 1000000
#define LOOP_W_INTERRUPT(I_MAX, EXPR) do {          \
  R_xlen_t i, i_prev, i_interrupt;                  \
  i = i_prev = 0;                                   \
  i_interrupt = (R_xlen_t) data[I_STAT][STAT_LOOP]; \
                                                    \
  while(1) {                                        \
    /* need to check overflow ? */                  \
    R_xlen_t i_stop = i + INTERRUPT_AT;             \
    if(i_stop > (I_MAX)) i_stop = (I_MAX);          \
                                                    \
    for(; i < i_stop; ++i) EXPR                     \
                                                    \
    i_interrupt += i_stop - i_prev;                 \
    if(i_interrupt >= INTERRUPT_AT) {               \
      R_CheckUserInterrupt();                       \
      i_interrupt = 0;                              \
    }                                               \
    if(i == (I_MAX)) break;                         \
    i_prev = i;                                     \
  }                                                 \
  data[I_STAT][STAT_LOOP] = (double) i_interrupt;   \
} while(0)
