#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

// .Call calls
extern SEXP _jaid_fastMode(SEXP, SEXP);
extern SEXP _jaid_fastModeX(SEXP, SEXP);
extern SEXP BeforeChangeIndex(SEXP);
extern SEXP IsNullExternalPtr(SEXP);
extern SEXP IndexOverlappingDateRange(SEXP, SEXP, SEXP, SEXP);
extern SEXP SortGroupBy(SEXP);
extern SEXP Unilen(SEXP);
extern SEXP Reverse(SEXP);
extern SEXP Interleave(SEXP, SEXP);
extern SEXP Rotate(SEXP, SEXP);
extern SEXP SetDimNames(SEXP, SEXP);
extern SEXP SetColNames(SEXP, SEXP);
extern SEXP SetRowNames(SEXP, SEXP);
extern SEXP MaxByColNames(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP MaxByRowNames(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP MinByColNames(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP MinByRowNames(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP RowMax(SEXP);
extern SEXP RowMin(SEXP);
extern SEXP RowSum(SEXP);
extern SEXP ColMax(SEXP);
extern SEXP ColMin(SEXP);
extern SEXP ColSum(SEXP);
extern SEXP ColDiff(SEXP);
extern SEXP SumByColNames(SEXP, SEXP, SEXP, SEXP);
extern SEXP SumByRowNames(SEXP, SEXP, SEXP, SEXP);
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
    {"_jaid_fastMode",              (DL_FUNC) &_jaid_fastMode,              2},
    {"_jaid_fastModeX",             (DL_FUNC) &_jaid_fastModeX,             2},
    {"BeforeChangeIndex",           (DL_FUNC) &BeforeChangeIndex,           1},
    {"IsNullExternalPtr",           (DL_FUNC) &IsNullExternalPtr,           1},
    {"IndexOverlappingDateRange",   (DL_FUNC) &IndexOverlappingDateRange,   4},
    {"SortGroupBy",                 (DL_FUNC) &SortGroupBy,                 1},
    {"Unilen",                      (DL_FUNC) &Unilen,                      1},
    {"Reverse",                     (DL_FUNC) &Reverse,                     1},
    {"Interleave",                  (DL_FUNC) &Interleave,                  2},
    {"Rotate",                      (DL_FUNC) &Rotate,                      2},
    {"SetDimNames",                 (DL_FUNC) &SetDimNames,                 2},
    {"SetColNames",                 (DL_FUNC) &SetColNames,                 2},
    {"SetRowNames",                 (DL_FUNC) &SetRowNames,                 2},
    {"MaxByColNames",               (DL_FUNC) &MaxByColNames,               5},
    {"MaxByRowNames",               (DL_FUNC) &MaxByRowNames,               5},
    {"MinByColNames",               (DL_FUNC) &MinByColNames,               5},
    {"MinByRowNames",               (DL_FUNC) &MinByRowNames,               5},
    {"RowMax",                      (DL_FUNC) &RowMax,                      1},
    {"RowMin",                      (DL_FUNC) &RowMin,                      1},
    {"RowSum",                      (DL_FUNC) &RowSum,                      1},
    {"ColMax",                      (DL_FUNC) &ColMax,                      1},
    {"ColMin",                      (DL_FUNC) &ColMin,                      1},
    {"ColSum",                      (DL_FUNC) &ColSum,                      1},
    {"ColDiff",                     (DL_FUNC) &ColDiff,                     1},
    {"SumByColNames",               (DL_FUNC) &SumByColNames,               4},
    {"SumByRowNames",               (DL_FUNC) &SumByRowNames,               4},
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
