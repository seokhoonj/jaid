#include "jaid.h"

void CopyDimNames(SEXP from, SEXP to) {
  SEXP dimnames;
  dimnames = getAttrib(from, R_DimNamesSymbol);
  setAttrib(to, R_DimNamesSymbol, dimnames);
}

void FillValue(SEXP x, SEXP value) {
  R_xlen_t i, n;
  n = XLENGTH(x);
  switch(TYPEOF(x)) {
  case INTSXP:{
    int *ix = INTEGER(x);
    if (value == 0) {
      memset(ix, 0, n * sizeof(int));
    } else {
      for (i = 0; i < n; ++i) ix[i] = asInteger(value);
    }
  } break;
  case REALSXP:{
    double *ix = REAL(x);
    if (value == 0) {
      memset(ix, 0, n * sizeof(double));
    } else {
      for (i = 0; i < n; ++i) ix[i] = asReal(value);
    }
  } break;
  default:
    error(_("invalid input"));
  }
}
