#include "jaid.h"

SEXP BeforeChangeIndex(SEXP x) {
  R_xlen_t i, p, s;
  SEXP pos;

  i = 1; p = 0; s = xlength(x);

  PROTECT(pos = allocVector(INTSXP, s));
  int* ipos = INTEGER(pos);
  ipos[p++] = 0; // the first value is fixed to 0

  switch(TYPEOF(x)){
  case LGLSXP:{
    int *ix = INTEGER(x) + 1;
    for (i = 0; i < s; ++i, ++ix) {
      if (*(ix-1) != *ix) {
        ipos[p++] = i;
      }
    }
  } break;
  case INTSXP:{
    int *ix = INTEGER(x) + 1; // from 1, not 0
    for (i = 0; i < s; ++i, ++ix) {
      if (*(ix-1) != *ix)  {
        ipos[p++] = i;
      }
    }
  } break;
  case REALSXP:{
    double *ix = REAL(x) + 1;
    for (; i < s; ++i, ++ix) {
      if (*(ix-1) != *ix)  {
        ipos[p++] = i;
      }
    }
  } break;
  case STRSXP:{
    for (; i < s; ++i) {
      if (strcmp(CHAR(STRING_ELT(x, i-1)), CHAR(STRING_ELT(x, i)))) {
        ipos[p++] = i;
      }
    }
  } break;
  default:
    error("invalid input");
  }

  ipos[p++] = s; // the last value is fixed to the length of x
  SETLENGTH(pos, p);
  UNPROTECT(1);
  return pos;
}

void CopyDimNames(SEXP from, SEXP to) {
  SEXP dimnames;
  dimnames = getAttrib(from, R_DimNamesSymbol);
  setAttrib(to, R_DimNamesSymbol, dimnames);
}

void FillValue(SEXP x, SEXP value) {
  if (TYPEOF(x) != TYPEOF(value))
    error(_("different input types"));

  R_xlen_t i, n;
  n = xlength(x);

  switch(TYPEOF(x)) {
  case LGLSXP:{
    int *ix = INTEGER(x);
    bool v = asLogical(value);
    if (value == 0) {
      memset(ix, 0, n * sizeof(bool));
    } else {
      for (i = 0; i < n; ++i) ix[i] = v;
    }
  } break;
  case INTSXP:{
    int *ix = INTEGER(x);
    int v = asInteger(value);
    if (value == 0) {
      memset(ix, 0, n * sizeof(int));
    } else {
      for (i = 0; i < n; ++i) ix[i] = v;
    }
  } break;
  case REALSXP:{
    double *ix = REAL(x);
    double v = asReal(value);
    if (value == 0) {
      memset(ix, 0, n * sizeof(double));
    } else {
      for (i = 0; i < n; ++i) ix[i] = v;
    }
  } break;
  case STRSXP:{
    SEXP v = asChar(value);
    for (i = 0; i < n; ++i) SET_STRING_ELT(x, i, v);
  } break;
  default:
    error(_("invalid input"));
  }
}

void FillCBool(SEXP x, bool value) {
  if (TYPEOF(x) != LGLSXP)
    error(_("different input types"));
  R_xlen_t i, n;
  n = xlength(x);
  int *ix = INTEGER(x);
  if (value == 0) {
    memset(ix, 0, n * sizeof(bool));
  } else {
    for (i = 0; i < n; ++i) ix[i] = value;
  }
}

void FillCInt(SEXP x, int value) {
  if (TYPEOF(x) != INTSXP)
    error(_("different input types"));
  R_xlen_t i, n;
  n = xlength(x);
  int *ix = INTEGER(x);
  if (value == 0) {
    memset(ix, 0, n * sizeof(int));
  } else {
    for (i = 0; i < n; ++i) ix[i] = value;
  }
}

void FillCDouble(SEXP x, double value) {
  if (TYPEOF(x) != REALSXP)
    error(_("different input types"));
  R_xlen_t i, n;
  n = xlength(x);
  double *ix = REAL(x);
  if (value == 0) {
    memset(ix, 0, n * sizeof(double));
  } else {
    for (i = 0; i < n; ++i) ix[i] = value;
  }
}

void FillCString(SEXP x, const char *value) {
  if (TYPEOF(x) != STRSXP)
    error(_("different input types"));
  R_xlen_t i, n;
  SEXP string;
  n = XLENGTH(x);
  string = STRING_ELT(mkString(value), 0);
  for (i = 0; i < n; ++i)
    SET_STRING_ELT(x, i, string);
}

void FillCDoublePointer(SEXP x, void **value) {
  R_xlen_t i, n;
  n = xlength(x);
  switch(TYPEOF(x)) {
  case LGLSXP:{
    int *ix = INTEGER(x);
    bool v = **(bool **)value;
    if (v == 0) {
      memset(ix, 0, n * sizeof(bool));
    } else {
      for (i = 0; i < n; ++i) ix[i] = v;
    }
  } break;
  case INTSXP:{
    int *ix = INTEGER(x);
    int v = **(int **)value;
    if (v == 0) {
      memset(ix, 0, n * sizeof(int));
    } else {
      for (i = 0; i < n; ++i) ix[i] = v;
    }
  } break;
  case REALSXP:{
    double *ix = REAL(x);
    double v = **(double **)value;
    if (v == 0) {
      memset(ix, 0, n * sizeof(double));
    } else {
      for (i = 0; i < n; ++i) ix[i] = v;
    }
  } break;
  case STRSXP:{
    const char *v = *value;
    SEXP d = STRING_ELT(mkString(v), 0);
    for (i = 0; i < n; ++i) SET_STRING_ELT(x, i, d);
  } break;
  default:
    error(_("invalid input"));
  }
}

// SEXP ChangeToBiggerType(SEXP x, SEXP minval) {
//   if (TYPEOF(x) > TYPEOF(minval)) {
//     minval = coerceVector(minval, TYPEOF(x));
//   } else if (TYPEOF(x) < TYPEOF(minval)) {
//     x = coerceVector(x, TYPEOF(minval));
//   }
// }

/* Coercing scalars
 * There are a few helper functions that turn length one R vectors into C scalars:
 *
 * asLogical(x): LGLSXP -> int
 * asInteger(x): INTSXP -> int
 * asReal(x): REALSXP -> double
 * CHAR(asChar(x)): STRSXP -> const char*
 *
 * And helpers to go in the opposite direction:
 *
 * ScalarLogical(x): int -> LGLSXP
 * ScalarInteger(x): int -> INTSXP
 * ScalarReal(x): double -> REALSXP
 * mkString(x): const char* -> STRSXP
 */
