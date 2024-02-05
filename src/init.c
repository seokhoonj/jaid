#include <R_ext/Rdynload.h>
#include <R_ext/Visibility.h>
#include "jaid.h"

#define CALLDEF(name, n) {#name, (DL_FUNC) &name, n} // # is a stringify operator

static const R_CallMethodDef callEntries[] = {
  // as
  CALLDEF(AsLogical, 1),
  CALLDEF(AsInteger, 1),
  CALLDEF(AsDouble, 1),
  CALLDEF(AsNumeric, 1),
  CALLDEF(AsCharacter, 1),

  // Group
  CALLDEF(IndexDateRangeOverlap, 4),
  CALLDEF(SortGroupBy, 1),

  // Utils
  CALLDEF(BeforeChangeIndex, 1),

  // Mode
  CALLDEF(_jaid_fastMode, 2),
  CALLDEF(_jaid_fastModeX, 2),

  // Vector
  CALLDEF(Unilen, 1),
  CALLDEF(Reverse, 1),
  CALLDEF(Traverse, 2),

  // Matrix
  CALLDEF(Rotate, 2),
  CALLDEF(SetDimNames, 2),
  CALLDEF(SetRowNames, 2),
  CALLDEF(SetColNames, 2),

  CALLDEF(MaxByColNames, 5),
  CALLDEF(MaxByRowNames, 5),
  CALLDEF(MinByColNames, 5),
  CALLDEF(MinByRowNames, 5),

  CALLDEF(RowMax, 1),
  CALLDEF(RowMin, 1),
  CALLDEF(RowSum, 1),

  CALLDEF(ColMax, 1),
  CALLDEF(ColMin, 1),
  CALLDEF(ColSum, 1),

  CALLDEF(SumByColNames, 4),
  CALLDEF(SumByRowNames, 4),

  // First
  CALLDEF(FillZeroNotFirstPos, 3),
  CALLDEF(SetZeroNotFirstPos, 3),
  CALLDEF(FillOneBeforeFirstOne, 2),
  CALLDEF(SetOneBeforeFirstOne, 2),
  {NULL, NULL, 0}
};

void attribute_visible R_init_jaid(DllInfo *info) {
  R_registerRoutines(info, NULL, callEntries, NULL, NULL);
  R_useDynamicSymbols(info, FALSE);
  R_forceSymbols(info, TRUE);
}
