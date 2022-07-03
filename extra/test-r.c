#include <R.h>
#include <Rinternals.h>
#include <Rversion.h>

SEXP test(SEXP x) {
  double tmp = 0;

  for(R_xlen_t i = 0; i < XLENGTH(x); ++i) {
    tmp += REAL(x)[i];
  }
  return ScalarReal(tmp);
}
