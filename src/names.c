#include "jaid.h"

// Set rownames, preserve colnames
SEXP SetRowNames(SEXP x, SEXP rownames) {
  SEXP dimnames = PROTECT(allocVector(VECSXP, 2));
  SEXP current = getAttrib(x, R_DimNamesSymbol);

  // keep existing colnames if any
  if (current != R_NilValue) {
    SEXP colnames = VECTOR_ELT(current, 1);
    if (colnames != R_NilValue) {
      SET_VECTOR_ELT(dimnames, 1, colnames);
    }
  }

  SET_VECTOR_ELT(dimnames, 0, rownames);
  setAttrib(x, R_DimNamesSymbol, dimnames);
  UNPROTECT(1);
  return x;
}

// Set colnames, preserve rownames
SEXP SetColNames(SEXP x, SEXP colnames) {
  SEXP dimnames = PROTECT(allocVector(VECSXP, 2));
  SEXP current = getAttrib(x, R_DimNamesSymbol);

  // keep existing rownames if any
  if (current != R_NilValue) {
    SEXP rownames = VECTOR_ELT(current, 0);
    if (rownames != R_NilValue) {
      SET_VECTOR_ELT(dimnames, 0, rownames);
    }
  }

  SET_VECTOR_ELT(dimnames, 1, colnames);
  setAttrib(x, R_DimNamesSymbol, dimnames);
  UNPROTECT(1);
  return x;
}

// Set full dimnames
SEXP SetDimNames(SEXP x, SEXP dimnames) {
  setAttrib(x, R_DimNamesSymbol, dimnames);
  return x; // no need to PROTECT if not allocating new objects here
}
