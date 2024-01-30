#include "jaid.h"

// set dim names
SEXP SetDimNm(SEXP x, SEXP dimnames) {
  setAttrib(x, R_DimNamesSymbol, dimnames);
  return x;
}

// set row names
SEXP SetRowNm(SEXP x, SEXP rownames) {
  SEXP colnames, dimnames, dimsymbols;
  PROTECT(dimnames = allocVector(VECSXP, 2));
  dimsymbols = getAttrib(x, R_DimNamesSymbol);
  if (!isNull(dimsymbols)) {
    colnames = VECTOR_ELT(dimsymbols, 1);
    if (!isNull(colnames)) {
      SET_VECTOR_ELT(dimnames, 1, colnames);
    }
  }
  SET_VECTOR_ELT(dimnames, 0, rownames);
  setAttrib(x, R_DimNamesSymbol, dimnames);
  UNPROTECT(1);
  return x;
}

// set col names
SEXP SetColNm(SEXP x, SEXP colnames) {
  SEXP dimnames, dimsymbols, rownames;
  PROTECT(dimnames = allocVector(VECSXP, 2));
  dimsymbols = getAttrib(x, R_DimNamesSymbol);
  if (!isNull(dimsymbols)) {
    rownames = VECTOR_ELT(dimsymbols, 0);
    if (!isNull(rownames)) {
      SET_VECTOR_ELT(dimnames, 0, rownames);
    }
  }
  SET_VECTOR_ELT(dimnames, 1, colnames);
  setAttrib(x, R_DimNamesSymbol, dimnames);
  UNPROTECT(1);
  return x;
}
