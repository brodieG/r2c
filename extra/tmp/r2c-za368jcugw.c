#include <math.h>
#include <R.h>
#include <Rinternals.h>

static void sum(double ** data, R_xlen_t * lens, int * di, int flag) {
  int di0 = di[0];
  int di1 = di[1];
  long double tmp = 0;

  R_xlen_t len_n = lens[di0];
  double * dat = data[di0];
  int narm = flag;  // only one possible flag parameter

  if(!narm)
    for(R_xlen_t i = 0; i < len_n; ++i) tmp += dat[i];
  else
    for(R_xlen_t i = 0; i < len_n; ++i)
      if(!isnan(dat[i])) tmp += dat[i];

  *data[di1] = (double) tmp;
  lens[di1] = 1;
}

void run(
  double ** data, R_xlen_t * lens, int ** di, int * narg, int * flag, SEXP ctrl
) {
  (void) narg; // unused
  (void) ctrl; // unused
  
  // sum(x)
  sum(data, lens, *di++, *flag);
  ++flag;
}
