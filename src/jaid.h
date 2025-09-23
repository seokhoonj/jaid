#ifndef JAID_JAID_H
#define JAID_JAID_H

#include <R.h>
#include <Rinternals.h>

#include <limits.h>   // INT_MIN, INT_MAX
#include <math.h>     // isinf, isnan
#include <stdlib.h>   // qsort in group.c
#include <stdint.h>
#include <stdbool.h>
#include <string.h>   // memcpy, strcmp

#define EMPTY (-1)

// Structure
union dbl_bits {
  double d;
  uint64_t u64;
  uint32_t u32[2];
};

// 32-bit Knuth multiplicative hash: floor(2^32 / phi)
#define HASH_KNUTH 0x9E3779B1U

// hash index
static inline uint32_t hash_index(uint32_t key, uint32_t K, uint32_t mask) {
  // initial index in [0, 2^K)
  return (uint32_t)((HASH_KNUTH * key) >> (32U - K)) & mask;
}

static inline uint32_t fold64_to_u32(uint64_t u) {
  return (uint32_t)(u ^ (u >> 32));
}

static inline double canonicalize_double(double x) {
  if (R_IsNA(x)) return NA_REAL;
  if (R_IsNaN(x)) return R_NaN;
  if (x == 0.0) return 0.0;      // +0.0 / -0.0 for complex numbers
  return x;
}

// linear probing for power-of-two table
static inline uint32_t probe_next(uint32_t id, uint32_t mask) {
  return (id + 1U) & mask;
}

// safe double equal
static inline int safe_equal_int(int a, int b) {
  const int na_a = (a == NA_INTEGER), na_b = (b == NA_INTEGER);
  if (na_a || na_b) return na_a && na_b;
  return a == b;
}

// safe double equal
static inline int safe_equal_dbl(double a, double b) {
  if (!ISNAN(a) && !ISNAN(b)) return a == b;
  if (R_IsNA(a) && R_IsNA(b)) return 1;
  if (R_IsNaN(a) && R_IsNaN(b)) return 1;
  return 0;
}

// safe complex equal
static inline int safe_equal_cplx(Rcomplex a, Rcomplex b) {
  return safe_equal_dbl(a.r, b.r) && safe_equal_dbl(a.i, b.i);
}

// double
static inline uint32_t key_from_double(double x) {
  x = canonicalize_double(x);
  uint64_t u64;
  memcpy(&u64, &x, sizeof(double));
  return fold64_to_u32(u64);
}

// complex (XOR)
static inline uint32_t key_from_complex(Rcomplex z) {
  z.r = canonicalize_double(z.r);
  z.i = canonicalize_double(z.i);
  uint64_t ur, ui;
  memcpy(&ur, &z.r, sizeof(double));
  memcpy(&ui, &z.i, sizeof(double));
  return fold64_to_u32(ur) ^ fold64_to_u32(ui);
}

// pointer
static inline uint32_t key_from_ptr(const void *p) {
  uintptr_t u = (uintptr_t)p;
  return (uint32_t) (u ^ (u >> 32));
}

// choose table size: M = 2^K >= 2n (load factor <= 0.5)
static inline void choose_table_size(R_xlen_t n, uint32_t *K, size_t *M) {
  size_t need = 2U * (size_t)n;
  size_t m = 256U;
  uint32_t k = 8U;
  while (m < need) { m <<= 1; k++; }
  *K = k; *M = m;
}

// copy dimnames
static inline void copy_dimnames(SEXP from, SEXP to) {
  setAttrib(to, R_DimNamesSymbol, getAttrib(from, R_DimNamesSymbol));
}


// C
#ifdef __cplusplus
extern "C" {
#endif

  // Date
  SEXP SeqDateList(SEXP from, SEXP to, SEXP label);
  SEXP IndexOverlappingDateRanges(SEXP id, SEXP from, SEXP to, SEXP interval);

  // Stay
  SEXP CountStay(SEXP id, SEXP from, SEXP to);
  SEXP LimitStay(SEXP x, SEXP gsize, SEXP limit, SEXP waiting);

  // ExternalPtr
  SEXP IsNullExternalPtr(SEXP pointer);

  // Group
  SEXP FindGroupBreaks(SEXP x);
  SEXP FindGroupSizes(SEXP x);

  // Vector
  SEXP Unilen(SEXP x);
  SEXP Reverse(SEXP x);
  SEXP Interleave(SEXP x, SEXP y);

  // Matrix
  SEXP Rotate(SEXP x, SEXP angle);
  SEXP SetRowNames(SEXP x, SEXP rownames);
  SEXP SetColNames(SEXP x, SEXP colnames);
  SEXP SetDimNames(SEXP x, SEXP dimnames);

  SEXP SumByRowNames(SEXP x, SEXP snarm);
  SEXP MaxByRowNames(SEXP x, SEXP snarm);
  SEXP MinByRowNames(SEXP x, SEXP snarm);

  SEXP SumByColNames(SEXP x, SEXP snarm);
  SEXP MaxByColNames(SEXP x, SEXP snarm);
  SEXP MinByColNames(SEXP x, SEXP snarm);

  SEXP SumByDimNames(SEXP x, SEXP snarm);
  SEXP MaxByDimNames(SEXP x, SEXP snarm);
  SEXP MinByDimNames(SEXP x, SEXP snarm);

  SEXP RowSum(SEXP x, SEXP snarm);
  SEXP RowMax(SEXP x, SEXP snarm);
  SEXP RowMin(SEXP x, SEXP snarm);

  SEXP ColSum(SEXP x, SEXP snarm);
  SEXP ColMax(SEXP x, SEXP snarm);
  SEXP ColMin(SEXP x, SEXP snarm);

  SEXP RowDiff(SEXP x, SEXP snarm);
  SEXP ColDiff(SEXP x, SEXP snarm);

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

/* ===========================================
 * NO	SEXPTYPE   DESCRIPTION
 *  0	NILSXP     NULL
 *  1	SYMSXP     symbols
 *  2	LISTSXP    pairlists
 *  3	CLOSXP     closures
 *  4	ENVSXP     environments
 *  5	PROMSXP	   promises
 *  6	LANGSXP	   language objects
 *  7	SPECIALSXP special functions
 *  8	BUILTINSXP builtin functions
 *  9	CHARSXP    internal character strings
 * 10	LGLSXP     logical vectors
 * 13	INTSXP     integer vectors
 * 14	REALSXP    numeric vectors
 * 15	CPLXSXP    complex vectors
 * 16	STRSXP     character vectors
 * 17	DOTSXP     dot-dot-dot object
 * 18	ANYSXP     make “any” args work
 * 19	VECSXP     list (generic vector)
 * 20	EXPRSXP    expression vector
 * 21	BCODESXP   byte code
 * 22	EXTPTRSXP  external pointer
 * 23	WEAKREFSXP weak reference
 * 24	RAWSXP     raw vector
 * 25	S4SXP      S4 classes not of simple type
 * =========================================== */

/* ===========================================
 * int               :  4 byte
 * unsigned int      :  4 byte
 * long int          :  8 byte
 * unsigned long int :  8 byte
 * long long int     :  8 byte
 * float             :  4 byte
 * double            :  8 byte
 * long double       : 16 byte
 * (void *)          :  8 byte
 * =========================================== */

/* ===========================================
 * INT_MAX = 2147483647
 * UINT_MAX = 4294967295d
 * LONG_MAX = 9223372036854775807
 * ULONG_MAX = 18446744073709551615d
 * =========================================== */

/* ===========================================
 * Coercing scalars
 * There are a few helper functions that turn
 * length one R vectors into C scalars:
 *
 * asLogical(x): LGLSXP -> int
 * asInteger(x): INTSXP -> int
 * asReal(x): REALSXP -> double
 * CHAR(asChar(x)): STRSXP -> const char*
 *
 * And helpers to go in the opposite direction:
 *
 * ScalarLogical(x): int -> LGLSXP
 * ScalarInteger(x): int -> INTSXP
 * ScalarReal(x): double -> REALSXP
 * mkString(x): const char* -> STRSXP
 * =========================================== */
