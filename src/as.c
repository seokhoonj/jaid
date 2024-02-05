#include "jaid.h"

/* same as `as.logical`, `as.integer`, `as.numeric`, `as.character`
 * but don't break the shape. */
SEXP AsLogical(SEXP x) {
  return coerceVector(x, LGLSXP);
}

SEXP AsInteger(SEXP x) {
  return coerceVector(x, INTSXP);
}

SEXP AsDouble(SEXP x) {
  return coerceVector(x, REALSXP);
}

SEXP AsNumeric(SEXP x) {
  return coerceVector(x, REALSXP);
}

SEXP AsCharacter(SEXP x) {
  return coerceVector(x, STRSXP);
}
