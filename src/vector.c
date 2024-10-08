#include "jaid.h"

// Refer to 'kit'
SEXP Unilen(SEXP x) {
  if (isFactor(x)) {
    const int len = LENGTH(PROTECT(getAttrib(x, R_LevelsSymbol)));
    UNPROTECT(1);
    bool *count = (bool*)calloc(len+1, sizeof(bool));
    const int *px = INTEGER(x);
    const int xlen = LENGTH(x);
    int j = 0;
    for (int i = 0; i < xlen; ++i) {
      if (!count[px[i]]) {
        j++;
        if (j == len)
          break;
        count[px[i]] = true;
      }
    }
    R_Free(count);
    return ScalarInteger(j);
  }
  if (isLogical(x)) {
    bool *count = (bool*)calloc(3, sizeof(bool));
    const int *px = LOGICAL(x);
    const int xlen = LENGTH(x);
    int j = 0;
    for (int i = 0; i < xlen; ++i) {
      const int cs = px[i] == NA_LOGICAL ? 2 : px[i];
      if (!count[cs]) {
        j++;
        if (j == 3)
          break;
        count[cs] = true;
      }
    }
    R_Free(count);
    return ScalarInteger(j);
  }
  const R_xlen_t n = xlength(x);
  const SEXPTYPE tx = UTYPEOF(x);
  int K;
  size_t M;
  if (tx == INTSXP || tx == REALSXP || tx == STRSXP) {
    const size_t n2 = 2U * (size_t) n;
    M = 256;
    K = 8;
    while (M < n2) {
      M *= 2;
      K++;
    }
  } else if (tx == LGLSXP) {
    M = 4;
    K = 2;
  } else {
    error("Type %s is not supported", type2char(tx));
  }
  R_xlen_t count = 0;
  int* h = (int*)calloc(M, sizeof(int));
  switch(tx) {
  case INTSXP:{
    const int* px = INTEGER(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = (px[i] == NA_INTEGER) ? 0 : HASH(px[i], K);
      while (h[id]) {
        if (px[h[id]-1] == px[i]) {
          goto ibl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      count++;
      ibl:;
    }
  } break;
  case REALSXP:{
    const double* px = REAL(x);
    size_t id = 0;
    union uno tpv;
    for (int i = 0; i < n; i++) {
      tpv.d = R_IsNA(px[i]) ? NA_REAL : (R_IsNaN(px[i]) ? R_NaN : px[i]);
      id = HASH(tpv.u[0] + tpv.u[1], K);
      while (h[id]) {
        if (REQUAL(px[h[id]-1], px[i])) {
          goto rbl;
        }
        id++; id %= M;
      }
      h[id] = (int) i + 1;
      count++;
      rbl:;
    }
  } break;
  case STRSXP:{
    const SEXP *px = STRING_PTR(x);
    size_t id = 0;
    for (int i = 0; i < n; ++i) {
      id = HASH(((intptr_t) px[i] & 0xffffffff), K);
      while (h[id]) {
        if (px[h[id] - 1]==px[i]) {
          goto sbl;
        }
        id++; id %=M;
      }
      h[id] = (int) i + 1;
      count++;
      sbl:;
    }
  } break;
  default:
    error(_("invalid input"));
  }
  Free(h);
  return ScalarInteger(count);
}

SEXP Reverse(SEXP x) {
  R_xlen_t i, size;

  size = XLENGTH(x);
  PROTECT(x);

  switch(TYPEOF(x)) {
  case LGLSXP:{
    int *ix = LOGICAL(x);
    int tmp;
    for (i = 0; i < size/2; ++i) {
      tmp = ix[i];
      ix[i] = ix[size-1-i];
      ix[size-1-i] = tmp;
    }
  } break;
  case INTSXP:{
    int *ix = INTEGER(x);
    int tmp;
    for (i = 0; i < size/2; ++i) {
      tmp = ix[i];
      ix[i] = ix[size-1-i];
      ix[size-1-i] = tmp;
    }
  } break;
  case REALSXP:{
    double *ix = REAL(x);
    double tmp;
    for (i = 0; i < size/2; ++i) {
      tmp = ix[i];
      ix[i] = ix[size-1-i];
      ix[size-1-i] = tmp;
    }
  } break;
  case STRSXP:{
    SEXP *ix = STRING_PTR(x);
    SEXP tmp;
    for (i = 0; i < size/2; ++i) {
      tmp = ix[i];
      ix[i] = ix[size-1-i];
      ix[size-1-i] = tmp;
    }
  } break;
  default:
    error(_("invalid input"));
  }
  UNPROTECT(1);
  return x;
}

SEXP Traverse(SEXP x, SEXP y) {
  if (TYPEOF(x) != TYPEOF(y))
    error("different input types");

  R_xlen_t xn, yn, i, j, k;
  SEXP z;

  switch(TYPEOF(x)) {
  case LGLSXP:{
    PROTECT(z = allocVector(LGLSXP, XLENGTH(x) + XLENGTH(y)));
    int *xs = LOGICAL(x), *xe = xs + XLENGTH(x);
    int *ys = LOGICAL(y), *ye = ys + XLENGTH(y);
    int *zs = LOGICAL(z);
    int *xi = xs, *yi = ys, *zi = zs;
    // Traverse both array
    while (xi < xe && yi < ye) {
      *zi = *xi;
      ++zi, ++xi;
      *zi = *yi;
      ++zi, ++yi;
    }
    // Store remaining elements of first array
    while (xi < xe) {
      *zi = *xi;
      ++zi, ++xi;
    }
    // Store remaining elements of second array
    while (yi < ye) {
      *zi = *yi;
      ++zi, ++yi;
    }
  } break;
  case INTSXP:{
    PROTECT(z = allocVector(INTSXP, XLENGTH(x)+XLENGTH(y)));
    int *xs = INTEGER(x), *xe = xs + XLENGTH(x);
    int *ys = INTEGER(y), *ye = ys + XLENGTH(y);
    int *zs = INTEGER(z);
    int *xi = xs, *yi = ys, *zi = zs;
    // Traverse both array
    while (xi < xe && yi < ye) {
      *zi = *xi;
      ++zi, ++xi;
      *zi = *yi;
      ++zi, ++yi;
    }
    // Store remaining elements of first array
    while (xi < xe) {
      *zi = *xi;
      ++zi, ++xi;
    }
    // Store remaining elements of second array
    while (yi < ye) {
      *zi = *yi;
      ++zi, ++yi;
    }
  } break;
  case REALSXP:{
    xn = XLENGTH(x), yn = XLENGTH(y);
    PROTECT(z = allocVector(REALSXP, xn + yn));
    double *xs = REAL(x), *xe = xs + xn;
    double *ys = REAL(y), *ye = ys + yn;
    double *zs = REAL(z);
    double *xi = xs, *yi = ys, *zi = zs;
    // Traverse both array
    while (xi < xe && yi < ye) {
      *zi = *xi;
      ++zi, ++xi;
      *zi = *yi;
      ++zi, ++yi;
    }
    // Store remaining elements of first array
    while (xi < xe) {
      *zi = *xi;
      ++zi, ++xi;
    }
    // Store remaining elements of second array
    while (yi < ye) {
      *zi = *yi;
      ++zi, ++yi;
    }
  } break;
  case STRSXP:{
    xn = XLENGTH(x), yn = XLENGTH(y);
    PROTECT(z = allocVector(STRSXP, xn+yn));
    i = 0, j = 0, k = 0;
    // Traverse both array
    while (j < xn && k < yn) {
      SET_STRING_ELT(z, i++, STRING_ELT(x, j++));
      SET_STRING_ELT(z, i++, STRING_ELT(y, k++));
    }
    // Store remaining elements of first array
    while (j < xn) {
      SET_STRING_ELT(z, i++, STRING_ELT(x, j++));
    }
    // Store remaining elements of second array
    while (k < yn) {
      SET_STRING_ELT(z, i++, STRING_ELT(y, k++));
    }
  } break;
  default:
    error("invalid input");
  }
  UNPROTECT(1);
  return z;
}
