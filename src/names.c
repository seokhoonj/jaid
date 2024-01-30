#include "jaid.h"

// set dim names
SEXP SetDimNm(SEXP x, SEXP dimnames) {
  setAttrib(x, R_DimNamesSymbol, dimnames);
  return x;
}

// set row names
SEXP SetRowNm(SEXP x, SEXP rownames) {
  SEXP colnames, dimnames;
  PROTECT(dimnames = allocVector(VECSXP, 2));
  PROTECT(colnames = VECTOR_ELT(getAttrib(x, R_DimNamesSymbol), 1));
  SET_VECTOR_ELT(dimnames, 0, rownames);
  SET_VECTOR_ELT(dimnames, 1, colnames);
  setAttrib(x, R_DimNamesSymbol, dimnames);
  UNPROTECT(2);
  return x;
}

// set col names
SEXP SetColNm(SEXP x, SEXP colnames) {
  SEXP rownames, dimnames;
  PROTECT(dimnames = allocVector(VECSXP, 2));
  PROTECT(rownames = VECTOR_ELT(getAttrib(x, R_DimNamesSymbol), 0));
  SET_VECTOR_ELT(dimnames, 0, rownames);
  SET_VECTOR_ELT(dimnames, 1, colnames);
  setAttrib(x, R_DimNamesSymbol, dimnames);
  UNPROTECT(2);
  return x;
}
