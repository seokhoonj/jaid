#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include "jaid.h"

#define CALLDEF(name, n) {#name, (DL_FUNC) &name, n} // # is a stringify operator

static const R_CallMethodDef callEntries[] = {
  CALLDEF(SetDimNm, 2),
  CALLDEF(SetRowNm, 2),
  CALLDEF(SetColNm, 2),

  CALLDEF(_jaid_fastMode, 2),
  CALLDEF(_jaid_fastModeX, 2),

  CALLDEF(MaxByColNm, 5),
  CALLDEF(MaxByRowNm, 5),
  CALLDEF(MinByColNm, 5),
  CALLDEF(MinByRowNm, 5),

  CALLDEF(RowMax, 1),
  CALLDEF(RowMin, 1),
  CALLDEF(RowSum, 1),

  CALLDEF(ColMax, 1),
  CALLDEF(ColMin, 1),
  CALLDEF(ColSum, 1),

  CALLDEF(SumByColNm, 4),
  CALLDEF(SumByRowNm, 4),

  CALLDEF(Reverse, 1),
  CALLDEF(Traverse, 2),
  {NULL, NULL, 0}
};

void attribute_visible R_init_jaid(DllInfo *info) {
  R_registerRoutines(info, NULL, callEntries, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);
}
