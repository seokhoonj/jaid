#include "jaid.h"

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
