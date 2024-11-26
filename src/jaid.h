#ifndef JAID_JAID_H
#define JAID_JAID_H

#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> /* qsort in group.c */
#include <stdint.h>
#include <stdbool.h>

#define USE_RINTERNALS 1

#ifdef WIN32
# include <windows.h>
#else
# include <sys/mman.h>
# include <sys/stat.h>
# include <sys/types.h>
# include <fcntl.h>
# include <unistd.h>
# include <errno.h>
#endif

/* for a message translation */
#ifdef ENABLE_NLS
# include <libintl.h>
# define _(String) dgettext ("jaid", String)
#else
# define _(String) (String)
#endif

/* structure */
union uno {
  double d;
  unsigned int u[2];
};

/* functions */
#define UTYPEOF(x) ((unsigned)TYPEOF(x))
#define DATAPTR_RO(x) ((const void *)DATAPTR(x))
#define SEXPPTR_RO(x) ((const SEXP *)DATAPTR_RO(x)) // to avoid overhead of looped STRING_ELT ans VECTOR_ELT
#define IS_BOOL(x) (LENGTH(x)==1 && TYPEOF(x)==LGLSXP && LOGICAL(x)[0]!=NA_LOGICAL)
#define N_ISNAN(x, y) (!ISNAN(x) && !ISNAN(y))
#define B_IsNA(x, y) (R_IsNA(x) && R_IsNA(y)) // both
#define B_IsNaN(x, y) (R_IsNaN(x) && R_IsNaN(y)) // both
#define REQUAL(x, y) (N_ISNAN(x, y) ? (x == y) : (B_IsNA(x, y) || B_IsNaN(x, y)))
#define HASH(key, K) (3141592653U * (unsigned int)(key) >> (32 - (K)))

/* Error messages */
#define R_ERR_MSG_NA	_("NaNs produced")

/* C */
#ifdef __cplusplus
extern "C" {
#endif

// as
SEXP AsLogical(SEXP x);
SEXP AsInteger(SEXP x);
SEXP AsDouble(SEXP x);
SEXP AsNumeric(SEXP x);
SEXP AsCharacter(SEXP x);

// Utils
SEXP BeforeChangeIndex(SEXP x);
void CopyDimNames(SEXP from, SEXP to);
void FillCBool(SEXP x, bool value);
void FillCInt(SEXP x, int value);
void FillCDouble(SEXP x, double value);
void FillCString(SEXP x, const char *value);
void FillValue(SEXP x, SEXP value);
void PrintArray(int arr[], int len);
SEXP PrintVector(SEXP x);

// ExternalPtr
SEXP IsNullExternalPtr(SEXP pointer);

/* Group */
SEXP IndexOverlappingDateRange(SEXP id, SEXP from, SEXP to, SEXP interval);
SEXP SortGroupBy(SEXP id);

// Mode
SEXP _jaid_fastMode(SEXP, SEXP);
SEXP _jaid_fastModeX(SEXP, SEXP);

// Vector
SEXP Unilen(SEXP x);
SEXP Reverse(SEXP x);
SEXP Traverse(SEXP x, SEXP y);

// Matrix
SEXP Rotate(SEXP x, SEXP angle);
SEXP SetDimNames(SEXP x, SEXP dimnames);
SEXP SetColNames(SEXP x, SEXP colnames);
SEXP SetRowNames(SEXP x, SEXP rownames);

SEXP MaxByColNames(SEXP x, SEXP g, SEXP uniqueg, SEXP snarm, SEXP minval);
SEXP MaxByRowNames(SEXP x, SEXP g, SEXP uniqueg, SEXP snarm, SEXP minval);

SEXP MinByColNames(SEXP x, SEXP g, SEXP uniqueg, SEXP snarm, SEXP maxval);
SEXP MinByRowNames(SEXP x, SEXP g, SEXP uniqueg, SEXP snarm, SEXP maxval);

SEXP RowMax(SEXP x);
SEXP RowMin(SEXP x);
SEXP RowSum(SEXP x);

SEXP ColMax(SEXP x);
SEXP ColMin(SEXP x);
SEXP ColSum(SEXP x);

SEXP ColDiff(SEXP x);

SEXP SumByColNames(SEXP x, SEXP g, SEXP uniqueg, SEXP snarm);
SEXP SumByRowNames(SEXP x, SEXP g, SEXP uniqueg, SEXP snarm);

// Replace
SEXP ReplaceVecInMat(SEXP mat, SEXP col, SEXP vec);
SEXP ReplaceValInMat(SEXP mat, SEXP val, SEXP refmat, SEXP refval);

// Mult
SEXP MatXMat(SEXP x, SEXP y);
SEXP MatXRow(SEXP mat, SEXP row);
SEXP MatXCol(SEXP mat, SEXP col);
SEXP MatXNum(SEXP mat, SEXP num);

// Repeat
SEXP RepCol(SEXP x, SEXP each);

// First
SEXP FillZeroNotFirstPos(SEXP x, SEXP id, SEXP ot);
SEXP SetZeroNotFirstPos(SEXP x, SEXP id, SEXP ot);
SEXP FillOneBeforeFirstOne(SEXP x, SEXP id);
SEXP SetOneBeforeFirstOne(SEXP x, SEXP id);

#ifdef __cplusplus
}
#endif

#endif // JAID_JAID_H


// no	SEXPTYPE   Description
//  0	NILSXP     NULL
//  1	SYMSXP     symbols
//  2	LISTSXP    pairlists
//  3	CLOSXP     closures
//  4	ENVSXP     environments
//  5	PROMSXP	   promises
//  6	LANGSXP	   language objects
//  7	SPECIALSXP special functions
//  8	BUILTINSXP builtin functions
//  9	CHARSXP    internal character strings
// 10	LGLSXP     logical vectors
// 13	INTSXP     integer vectors
// 14	REALSXP    numeric vectors
// 15	CPLXSXP    complex vectors
// 16	STRSXP     character vectors
// 17	DOTSXP     dot-dot-dot object
// 18	ANYSXP     make “any” args work
// 19	VECSXP     list (generic vector)
// 20	EXPRSXP    expression vector
// 21	BCODESXP   byte code
// 22	EXTPTRSXP  external pointer
// 23	WEAKREFSXP weak reference
// 24	RAWSXP     raw vector
// 25	S4SXP      S4 classes not of simple type

// 64 bit
// int               :  4 byte
// unsigned int      :  4 byte
// long int          :  8 byte
// unsigned long int :  8 byte
// long long int     :  8 byte
// float             :  4 byte
// double            :  8 byte
// long double       : 16 byte
// (void *)          :  8 byte

// INT_MAX = 2147483647
// UINT_MAX = 4294967295d
// LONG_MAX = 9223372036854775807
// ULONG_MAX = 18446744073709551615d
