#include "jaid.h"

// same as 'as.' but not break the shape
SEXP AsLogical(SEXP x) {
  return Rf_coerceVector(x, LGLSXP);
}

SEXP AsInteger(SEXP x) {
  return Rf_coerceVector(x, INTSXP);
}

SEXP AsDouble(SEXP x) {
  return Rf_coerceVector(x, REALSXP);
}

SEXP AsNumeric(SEXP x) {
  return Rf_coerceVector(x, REALSXP);
}

SEXP AsCharacter(SEXP x) {
  return Rf_coerceVector(x, STRSXP);
}
