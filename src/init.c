#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

// .Call calls
extern SEXP IsNullExternalPtr(SEXP);
extern SEXP SeqDateList(SEXP, SEXP, SEXP);
extern SEXP IndexOverlappingDateRanges(SEXP, SEXP, SEXP, SEXP);
extern SEXP FindGroupBreaks(SEXP);
extern SEXP FindGroupSizes(SEXP);
extern SEXP CountStay(SEXP, SEXP, SEXP);
extern SEXP LimitStay(SEXP, SEXP, SEXP, SEXP);
extern SEXP Unilen(SEXP);
extern SEXP Reverse(SEXP);
extern SEXP Interleave(SEXP, SEXP);
extern SEXP Rotate(SEXP, SEXP);
extern SEXP SetRowNames(SEXP, SEXP);
extern SEXP SetColNames(SEXP, SEXP);
extern SEXP SetDimNames(SEXP, SEXP);
extern SEXP SumByRowNames(SEXP, SEXP);
extern SEXP MaxByRowNames(SEXP, SEXP);
extern SEXP MinByRowNames(SEXP, SEXP);
extern SEXP SumByColNames(SEXP, SEXP);
extern SEXP MaxByColNames(SEXP, SEXP);
extern SEXP MinByColNames(SEXP, SEXP);
extern SEXP SumByDimNames(SEXP, SEXP);
extern SEXP MaxByDimNames(SEXP, SEXP);
extern SEXP MinByDimNames(SEXP, SEXP);
extern SEXP RowSum(SEXP, SEXP);
extern SEXP RowMax(SEXP, SEXP);
extern SEXP RowMin(SEXP, SEXP);
extern SEXP ColSum(SEXP, SEXP);
extern SEXP ColMax(SEXP, SEXP);
extern SEXP ColMin(SEXP, SEXP);
extern SEXP RowDiff(SEXP, SEXP);
extern SEXP ColDiff(SEXP, SEXP);
extern SEXP ReplaceVecInMat(SEXP, SEXP, SEXP);
extern SEXP ReplaceValInMat(SEXP, SEXP, SEXP, SEXP);
extern SEXP MatXMat(SEXP, SEXP);
extern SEXP MatXRow(SEXP, SEXP);
extern SEXP MatXCol(SEXP, SEXP);
extern SEXP MatXNum(SEXP, SEXP);
extern SEXP RepCol(SEXP, SEXP);
extern SEXP FillZeroNotFirstPos(SEXP, SEXP, SEXP);
extern SEXP SetZeroNotFirstPos(SEXP, SEXP, SEXP);
extern SEXP FillOneBeforeFirstOne(SEXP, SEXP);
extern SEXP SetOneBeforeFirstOne(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"IsNullExternalPtr",           (DL_FUNC) &IsNullExternalPtr,           1},
    {"SeqDateList",                 (DL_FUNC) &SeqDateList,                 3},
    {"IndexOverlappingDateRanges",  (DL_FUNC) &IndexOverlappingDateRanges,  4},
    {"FindGroupBreaks",             (DL_FUNC) &FindGroupBreaks,             1},
    {"FindGroupSizes",              (DL_FUNC) &FindGroupSizes,              1},
    {"CountStay",                   (DL_FUNC) &CountStay,                   3},
    {"LimitStay",                   (DL_FUNC) &LimitStay,                   4},
    {"Unilen",                      (DL_FUNC) &Unilen,                      1},
    {"Reverse",                     (DL_FUNC) &Reverse,                     1},
    {"Interleave",                  (DL_FUNC) &Interleave,                  2},
    {"Rotate",                      (DL_FUNC) &Rotate,                      2},
    {"SetRowNames",                 (DL_FUNC) &SetRowNames,                 2},
    {"SetColNames",                 (DL_FUNC) &SetColNames,                 2},
    {"SetDimNames",                 (DL_FUNC) &SetDimNames,                 2},
    {"SumByRowNames",               (DL_FUNC) &SumByRowNames,               2},
    {"MaxByRowNames",               (DL_FUNC) &MaxByRowNames,               2},
    {"MinByRowNames",               (DL_FUNC) &MinByRowNames,               2},
    {"SumByColNames",               (DL_FUNC) &SumByColNames,               2},
    {"MaxByColNames",               (DL_FUNC) &MaxByColNames,               2},
    {"MinByColNames",               (DL_FUNC) &MinByColNames,               2},
    {"SumByDimNames",               (DL_FUNC) &SumByDimNames,               2},
    {"MaxByDimNames",               (DL_FUNC) &MaxByDimNames,               2},
    {"MinByDimNames",               (DL_FUNC) &MinByDimNames,               2},
    {"RowSum",                      (DL_FUNC) &RowSum,                      2},
    {"RowMax",                      (DL_FUNC) &RowMax,                      2},
    {"RowMin",                      (DL_FUNC) &RowMin,                      2},
    {"ColSum",                      (DL_FUNC) &ColSum,                      2},
    {"ColMax",                      (DL_FUNC) &ColMax,                      2},
    {"ColMin",                      (DL_FUNC) &ColMin,                      2},
    {"RowDiff",                     (DL_FUNC) &RowDiff,                     2},
    {"ColDiff",                     (DL_FUNC) &ColDiff,                     2},
    {"ReplaceVecInMat",             (DL_FUNC) &ReplaceVecInMat,             3},
    {"ReplaceValInMat",             (DL_FUNC) &ReplaceValInMat,             4},
    {"MatXMat",                     (DL_FUNC) &MatXMat,                     2},
    {"MatXRow",                     (DL_FUNC) &MatXRow,                     2},
    {"MatXCol",                     (DL_FUNC) &MatXCol,                     2},
    {"MatXNum",                     (DL_FUNC) &MatXNum,                     2},
    {"RepCol",                      (DL_FUNC) &RepCol,                      2},
    {"FillZeroNotFirstPos",         (DL_FUNC) &FillZeroNotFirstPos,         3},
    {"SetZeroNotFirstPos",          (DL_FUNC) &SetZeroNotFirstPos,          3},
    {"FillOneBeforeFirstOne",       (DL_FUNC) &FillOneBeforeFirstOne,       2},
    {"SetOneBeforeFirstOne",        (DL_FUNC) &SetOneBeforeFirstOne,        2},
    {NULL, NULL, 0}
};

void R_init_jaid(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
