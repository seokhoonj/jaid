#include "jaid.h"

SEXP IsNullExternalPtr(SEXP pointer) {
  return ScalarLogical(!R_ExternalPtrAddr(pointer));
}
