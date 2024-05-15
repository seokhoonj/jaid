#include "jaid.h"

SEXP RepCol(SEXP x, SEXP each) {
  R_xlen_t i, j, k, p, q, m, n, col, s = 0;
  SEXP ans;
  m = nrows(x), n = ncols(x), p = XLENGTH(each);

  if (p != 1 && n != p)
    error("different length");

  if (TYPEOF(each) != INTSXP)
    each = coerceVector(each, INTSXP);

  // summation of each elements
  int *ie = INTEGER(each), *ee = ie + p;
  for (; ie != ee; ++ie) s += *ie;

  // repeat columns
  switch(TYPEOF(x)){
  case LGLSXP:{
    PROTECT(ans = allocMatrix(LGLSXP, m, s));
    int* ians = INTEGER(ans);
    int* ix = INTEGER(x);
    int* ieach = INTEGER(each);
    memset(ians, 0, m * s * sizeof(int));
    q = 0;
    for (i = 0; i < p; ++i) {
      col = i * m;
      for (j = 0; j < ieach[i]; ++j) {
        for (k = 0; k < m; ++k) {
          ians[q++] = ix[col + k];
        }
      }
    }
  } break;
  case INTSXP:{
    PROTECT(ans = allocMatrix(INTSXP, m, s));
    int* ians = INTEGER(ans);
    int* ix = INTEGER(x);
    int* ieach = INTEGER(each);
    memset(ians, 0, m * s * sizeof(int));
    q = 0;
    for (i = 0; i < p; ++i) {
      col = i * m;
      for (j = 0; j < ieach[i]; ++j) {
        for (k = 0; k < m; ++k) {
          ians[q++] = ix[col + k];
        }
      }
    }
  } break;
  case REALSXP:{
    PROTECT(ans = allocMatrix(REALSXP, m, s));
    double* ians = REAL(ans);
    double* ix = REAL(x);
    int* ieach = INTEGER(each);
    memset(ians, 0, m * s * sizeof(double));
    q = 0;
    for (i = 0; i < p; ++i) {
      col = i * m;
      for (j = 0; j < ieach[i]; ++j) {
        for (k = 0; k < m; ++k) {
          ians[q++] = ix[col + k];
        }
      }
    }
  } break;
  case STRSXP:{
    PROTECT(ans = allocMatrix(STRSXP, m, s));
    int* ieach = INTEGER(each);
    q = 0;
    for (i = 0; i < p; ++i) {
      col = i * m;
      for (j = 0; j < ieach[i]; ++j) {
        for (k = 0; k < m; ++k) {
          SET_STRING_ELT(ans, q++, STRING_ELT(x, col + k));
        }
      }
    }
  } break;
  default:
    error("invalid input");
  }
  UNPROTECT(1);
  return ans;
}
