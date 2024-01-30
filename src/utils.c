#include "jaid.h"

SEXP FillValue(SEXP x, double value) {
  R_xlen_t i, n;
  n = XLENGTH(x);
  switch(TYPEOF(x)) {
  case INTSXP:{
    int *ix = INTEGER(x);
    if (value == 0) {
      memset(ix, 0, n * sizeof(int));
    } else {
      for (i = 0; i < n; ++i) ix[i] = (int) value;
    }
  } break;
  case REALSXP:{
    double *ix = REAL(x);
    if (value == 0) {
      memset(ix, 0, n * sizeof(double));
    } else {
      for (i = 0; i < n; ++i) ix[i] = value;
    }
  } break;
  default:
    error(_("invalid input"));
  }
  return x;
}
