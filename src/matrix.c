#include "jaid.h"

/* ==================================================================
 * ensure_char_dimnames / ensure_char_rownames / ensure_char_colnames
 *
 * Guarantee that rownames/colnames exist and are of type STRSXP.
 *
 * Arguments:
 *   - x     : matrix object
 *   - which : 0 = rownames, 1 = colnames
 *
 * Behavior:
 *   - If names are missing: create sequential "1","2",... as names.
 *   - If names exist but are not character: coerce to STRSXP.
 *
 * Returns:
 *   - STRSXP name vector (rownames or colnames, depending on `which`)
 *   - Also updates dimnames(x) in-place.
 * ================================================================== */
static SEXP ensure_char_dimnames(SEXP x, int which){
  SEXP dn = getAttrib(x, R_DimNamesSymbol);
  SEXP nm = R_NilValue;
  const R_xlen_t n = (which == 0) ? nrows(x) : ncols(x);

  if (!isNull(dn) && LENGTH(dn) >= 2)
    nm = VECTOR_ELT(dn, which);

  if (isNull(nm)) {
    // Case 1: no names, create "1","2",...
    SEXP nm_out = PROTECT(allocVector(STRSXP, n));
    for (R_xlen_t i = 0; i < n; ++i){
      char buf[32]; snprintf(buf, sizeof(buf), "%lld", (long long)(i+1));
      SET_STRING_ELT(nm_out, i, Rf_mkChar(buf));
    }
    if (isNull(dn)) {
      // no dimnames list at all: allocate VECSXP length 2
      dn = PROTECT(allocVector(VECSXP, 2));
      SET_VECTOR_ELT(dn, 0, R_NilValue);
      SET_VECTOR_ELT(dn, 1, R_NilValue);
      setAttrib(x, R_DimNamesSymbol, dn);
      UNPROTECT(1);
    }
    SET_VECTOR_ELT(dn, which, nm_out);
    setAttrib(x, R_DimNamesSymbol, dn);
    UNPROTECT(1);
    nm = VECTOR_ELT(getAttrib(x, R_DimNamesSymbol), which);
  } else if (TYPEOF(nm) != STRSXP) {
    // Case 2: names exist but not character, coerce to STRSXP
    nm = PROTECT(coerceVector(nm, STRSXP));
    if (isNull(dn)) {
      dn = PROTECT(allocVector(VECSXP, 2));
      SET_VECTOR_ELT(dn, 0, R_NilValue);
      SET_VECTOR_ELT(dn, 1, R_NilValue);
      setAttrib(x, R_DimNamesSymbol, dn);
      UNPROTECT(1);
    }
    SET_VECTOR_ELT(dn, which, nm);
    setAttrib(x, R_DimNamesSymbol, dn);
    UNPROTECT(1);
    nm = VECTOR_ELT(getAttrib(x, R_DimNamesSymbol), which);
  }
  return nm;
}

#define ensure_char_rownames(x) ensure_char_dimnames((x), 0)
#define ensure_char_colnames(x) ensure_char_dimnames((x), 1)

/* ============================================================
 * ASCII/UTF-8 detection helpers
 * ============================================================ */

// Return 1 if character is pure ASCII (bytes < 0x80), else 0
static inline int is_ascii_charsxp(SEXP ch){
  if (ch == NA_STRING) return 0;
  const unsigned char *p = (const unsigned char *) CHAR(ch);
  for (; *p; ++p) if (*p >= 0x80) return 0;
  return 1;
}

// Return 1 if all elements are ASCII
static inline int all_ascii(SEXP s){
  if (TYPEOF(s) != STRSXP) return 1;
  const R_xlen_t n = XLENGTH(s);
  for (R_xlen_t i=0; i<n; ++i)
    if (!is_ascii_charsxp(STRING_ELT(s, i))) return 0;
  return 1;
}

// Return 1 if all elements are UTF-8 encoded
static inline int all_utf8(SEXP s){
  if (TYPEOF(s) != STRSXP) return 1;
  const R_xlen_t n = XLENGTH(s);
  for (R_xlen_t i=0; i<n; ++i){
    SEXP ch = STRING_ELT(s, i);
    if (ch == NA_STRING) return 0;
    if (getCharCE(ch) != CE_UTF8) return 0;
  }
  return 1;
}

// Need UTF-8 conversion on the fly if mixed encodings and non-ASCII present
static inline int need_utf8_conversion(SEXP s){
  if (TYPEOF(s) != STRSXP) return 0;
  if (all_ascii(s)) return 0;
  if (all_utf8(s))  return 0;
  return 1;
}

/* ============================================================
 * str_equal_rstyle
 *
 * Base-R style equality for CHARSXP:
 * - Pointer compare first (fast path).
 * - ASCII only: strcmp on CHAR().
 * - Both UTF-8: strcmp on CHAR().
 * - Else: translate both to UTF-8 and strcmp.
 * ============================================================ */
static inline int str_equal_rstyle(SEXP a, SEXP b){
  if (a == b) return 1;
  if (a == NA_STRING || b == NA_STRING) return 0;
  if (is_ascii_charsxp(a) && is_ascii_charsxp(b))
    return strcmp(CHAR(a), CHAR(b)) == 0;
  if (getCharCE(a) == CE_UTF8 && getCharCE(b) == CE_UTF8)
    return strcmp(CHAR(a), CHAR(b)) == 0;
  const void *vmax = vmaxget();
  const char *pa = translateCharUTF8(a);
  const char *pb = translateCharUTF8(b);
  int eq = (strcmp(pa, pb) == 0);
  vmaxset(vmax);
  return eq;
}

/* ============================================================
 * key_from_string
 *
 * Hash function for strings:
 * - If do_utf8=0: use pointer-based hash (key_from_ptr).
 * - Else: translate to UTF-8 and compute byte hash.
 * ============================================================ */
static inline uint32_t u32_hash_utf8_bytes(const char *p){
  uint32_t k = 0;
  for (; *p; ++p) k = 11U*k + (unsigned char)*p;
  return k;
}
static inline uint32_t key_from_string(SEXP ch, int do_utf8){
  if (!do_utf8) return key_from_ptr((const void*)ch);
  const void *vmax = vmaxget();
  const char *p = translateCharUTF8(ch);
  uint32_t h = u32_hash_utf8_bytes(p);
  vmaxset(vmax);
  return h;
}

/* ============================================================
 * setup_name_groups
 *
 * Build grouping info from row/column names.
 * Input:
 *   names : STRSXP vector of length n
 *   n     : number of elements
 * Output:
 *   out_gid  : INTSXP (length n), 0-based group id for each element
 *   out_uniq : STRSXP (length ng), unique names
 *   out_ng   : number of groups
 *
 * Uses open addressing with linear probing for hash table.
 * UTF-8 normalization handled on demand via str_equal_rstyle.
 * ============================================================ */
static void setup_name_groups(SEXP names, R_xlen_t n,
                              SEXP *out_gid, SEXP *out_uniq, R_xlen_t *out_ng) {
  const int do_utf8 = need_utf8_conversion(names);

  uint32_t K; size_t M;
  choose_table_size(n, &K, &M);
  const uint32_t mask = (uint32_t)M - 1U;

  // hash table: maps hash id -> slot index
  SEXP tab = PROTECT(allocVector(INTSXP, (R_xlen_t)M));
  int *ptab = INTEGER(tab);
  for (R_xlen_t i = 0; i < (R_xlen_t)M; ++i) ptab[i] = EMPTY;

  // group index vector (per element)
  SEXP gid = PROTECT(allocVector(INTSXP, n));
  int *pgid = INTEGER(gid);

  // unique names vector
  SEXP uniq = PROTECT(allocVector(STRSXP, n));
  R_xlen_t ng = 0;

  for (R_xlen_t i = 0; i < n; ++i) {
    SEXP ch = STRING_ELT(names, i);
    uint32_t key = key_from_string(ch, do_utf8);
    uint32_t id  = hash_index(key, K, mask);

    for (;;) {
      const int slot = ptab[id];
      if (slot == EMPTY) {
        // new group: record name, assign group id
        ptab[id] = (int)ng;
        SET_STRING_ELT(uniq, ng, ch);
        pgid[i] = (int)ng;
        ++ng;
        break;
      } else {
        // check equality with existing unique name
        SEXP u = STRING_ELT(uniq, (R_xlen_t)slot);
        const int eq = do_utf8 ? str_equal_rstyle(u, ch)
          : (u == ch || strcmp(CHAR(u), CHAR(ch)) == 0);
        if (eq) {
          pgid[i] = slot;
          break;
        }
        // collision: probe next slot
        id = probe_next(id, mask);
      }
    }
  }

  *out_gid  = gid;  // INTSXP, length n, 0-based group indices
  *out_uniq = uniq; // STRSXP, length ng, unique names
  *out_ng   = ng;   // number of groups

  UNPROTECT(1); // tab
}

/* =========================================================================
 * Group-wise matrix aggregators by row/column names
 *
 * These functions perform aggregation (sum, max, min) across a matrix,
 * where rows or columns sharing the same name are collapsed into groups.
 * The grouping is derived from `rownames(x)` or `colnames(x)`:
 *   - SumByRowNames / MaxByRowNames / MinByRowNames
 *       -> Collapse rows with identical rownames, producing a matrix
 *          with one row per unique name and the same number of columns.
 *   - SumByColNames / MaxByColNames / MinByColNames
 *       -> Collapse columns with identical colnames, producing a matrix
 *          with one column per unique name and the same number of rows.
 *
 * NA handling:
 *   - If na.rm = TRUE:
 *       * Missing values (NA/NaN) are ignored in the aggregation.
 *       * Groups with no non-NA values are returned as all-NA.
 *   - If na.rm = FALSE:
 *       * Any NA/NaN encountered in a group makes the result NA
 *         ("sticky NA" semantics).
 *
 * Supported types: integer and double matrices.
 * For integer results, overflow is detected during summation
 * and replaced with NA_INTEGER.
 *
 * Dimnames:
 *   - Row grouping preserves column names from the original matrix.
 *   - Column grouping preserves row names from the original matrix.
 *   - The grouped dimension (rows or columns) is replaced by
 *     the set of unique names in group order.
 *
 * Complexity:
 *   - Linear in the number of cells of `x` (O(n * p)),
 *     plus overhead for building name groups.
 *   - Implemented in C for memory efficiency and speed,
 *     avoiding intermediate copies created by R-level aggregation.
 * ========================================================================= */
SEXP SumByRowNames(SEXP x, SEXP snarm){
  // argument checks
  if (!isMatrix(x)) error("x must be a numeric matrix");
  const int narm = asLogical(snarm);
  if (narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");
  const SEXPTYPE xt = TYPEOF(x);
  if (!(xt == INTSXP || xt == REALSXP))
    error("x must be integer or double matrix");

  const R_xlen_t n = nrows(x);
  const R_xlen_t p = ncols(x);

  // build groups from row names: gid (0-based), uniq, ng
  SEXP rn = ensure_char_rownames(x);     // guarantee STRSXP
  SEXP gid, uniq; R_xlen_t ng = 0;
  setup_name_groups(rn, n, &gid, &uniq, &ng);  // gid, uniq are PROTECTED inside
  int *pgid = INTEGER(gid);

  // allocate result (ng x p)
  SEXP ans = PROTECT(allocMatrix(xt, ng, p));

  switch (xt) {
  case INTSXP: {
    int *px = INTEGER(x);
    int *pa = INTEGER(ans);

    // zero-init all cells
    for (R_xlen_t t = 0; t < (R_xlen_t)ng * p; ++t) pa[t] = 0;

    // column-wise accumulation using row group index
    for (R_xlen_t c = 0; c < p; ++c){
      const R_xlen_t off  = c * n;   // source offset
      const R_xlen_t goff = c * ng;  // destination offset
      for (R_xlen_t i = 0; i < n; ++i){
        const int xv = px[off + i];
        int *cell   = &pa[goff + pgid[i]];
        if (xv == NA_INTEGER) {
          if (!narm) *cell = NA_INTEGER;  // sticky NA if na.rm = FALSE
        } else if (*cell != NA_INTEGER) {
          const int cur = *cell;
          const double tmp = (double)cur + (double)xv; // overflow check
          *cell = (tmp < INT_MIN || tmp > INT_MAX) ? NA_INTEGER : cur + xv;
        }
      }
    }
  } break;

  case REALSXP: {
    double *px = REAL(x);
    double *pa = REAL(ans);

    // zero-init all cells
    for (R_xlen_t t = 0; t < (R_xlen_t)ng * p; ++t) pa[t] = 0.0;

    if (narm){
      // skip NA/NaN; cells with no non-NA contributions remain 0
      for (R_xlen_t c = 0; c < p; ++c){
        const R_xlen_t off  = c * n;
        const R_xlen_t goff = c * ng;
        for (R_xlen_t i = 0; i < n; ++i){
          const double xv = px[off + i];
          if (!ISNAN(xv)) pa[goff + pgid[i]] += xv;
        }
      }
    } else {
      // sticky NA: any NA/NaN makes the destination cell NA_REAL
      for (R_xlen_t c = 0; c < p; ++c){
        const R_xlen_t off  = c * n;
        const R_xlen_t goff = c * ng;
        for (R_xlen_t i = 0; i < n; ++i){
          const double xv = px[off + i];
          double *cell = &pa[goff + pgid[i]];
          if (ISNAN(xv)) *cell = NA_REAL;
          else if (!ISNAN(*cell)) *cell += xv;
        }
      }
    }
  } break;

  default:
    error("numeric matrix required");
  }

  // set dimnames: rows = uniq, cols = colnames(x)
  SEXP xdn = getAttrib(x, R_DimNamesSymbol);
  SEXP outdn = PROTECT(allocVector(VECSXP, 2));
  setAttrib(ans, R_DimNamesSymbol, outdn);

  // rownames from uniq (0..ng-1)
  SEXP rn_out = PROTECT(allocVector(STRSXP, ng));
  for (R_xlen_t i = 0; i < ng; ++i)
    SET_STRING_ELT(rn_out, i, STRING_ELT(uniq, i));
  SET_VECTOR_ELT(outdn, 0, rn_out);

  // colnames inherit from x if present
  if (!isNull(xdn) && LENGTH(xdn) >= 2 && !isNull(VECTOR_ELT(xdn, 1)))
    SET_VECTOR_ELT(outdn, 1, VECTOR_ELT(xdn, 1));
  else
    SET_VECTOR_ELT(outdn, 1, R_NilValue);

  // UNPROTECT: ans/outdn/rn_out (3) + setup_name_groups left (gid, uniq) (2)
  UNPROTECT(3);
  UNPROTECT(2);
  return ans;
}

SEXP MaxByRowNames(SEXP x, SEXP snarm){
  // checks
  if (!isMatrix(x)) error("x must be a numeric matrix");
  const int narm = asLogical(snarm);
  if (narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");
  const SEXPTYPE xt = TYPEOF(x);
  if (!(xt == INTSXP || xt == REALSXP)) error("x must be integer or double matrix");

  const R_xlen_t n = nrows(x), p = ncols(x);

  // groups from rownames
  SEXP rn = ensure_char_rownames(x);
  SEXP gid, uniq; R_xlen_t ng = 0;
  setup_name_groups(rn, n, &gid, &uniq, &ng);
  int *pgid = INTEGER(gid);

  // result (ng x p)
  SEXP ans = PROTECT(allocMatrix(xt, ng, p));

  switch (xt){
  case INTSXP: {
    const int *px = INTEGER(x);
    int *pa = INTEGER(ans);

    if (narm){
      // ignore NAs: lazy init with first non-NA then take max
      for (R_xlen_t c = 0; c < p; ++c){
        int *dst = pa + (R_xlen_t)c * ng;
        SEXP init = PROTECT(allocVector(LGLSXP, ng));
        int *pinit = LOGICAL(init);
        for (R_xlen_t g = 0; g < ng; ++g){ pinit[g] = 0; dst[g] = 0; }

        const R_xlen_t off = (R_xlen_t)c * n;
        for (R_xlen_t i = 0; i < n; ++i){
          const int v = px[off + i], g = pgid[i];
          if (v == NA_INTEGER) continue;
          if (!pinit[g]) { dst[g] = v; pinit[g] = 1; }
          else if (v > dst[g]) dst[g] = v;
        }
        for (R_xlen_t g = 0; g < ng; ++g) if (!pinit[g]) dst[g] = NA_INTEGER;
        UNPROTECT(1); // init
      }
    } else {
      // na.rm=FALSE: any NA in group/column -> NA, else max(non-NA)
      for (R_xlen_t c = 0; c < p; ++c){
        int *dst = pa + (R_xlen_t)c * ng;

        for (R_xlen_t g = 0; g < ng; ++g) dst[g] = INT_MIN;
        SEXP has = PROTECT(allocVector(LGLSXP, ng));
        int *phas = LOGICAL(has);
        for (R_xlen_t g = 0; g < ng; ++g) phas[g] = 0;

        const R_xlen_t off = (R_xlen_t)c * n;
        for (R_xlen_t i = 0; i < n; ++i){
          const int v = px[off + i], g = pgid[i];
          if (v == NA_INTEGER) { phas[g] = 1; }
          else if (!phas[g]) {
            dst[g] = (dst[g] == INT_MIN) ? v : (v > dst[g] ? v : dst[g]);
          }
        }
        for (R_xlen_t g = 0; g < ng; ++g) {
          if (phas[g]) dst[g] = NA_INTEGER;
          else if (dst[g] == INT_MIN) dst[g] = NA_INTEGER; // all NA
        }
        UNPROTECT(1); // has
      }
    }
  } break;

  case REALSXP: {
    const double *px = REAL(x);
    double *pa = REAL(ans);

    if (narm){
      for (R_xlen_t c = 0; c < p; ++c){
        double *dst = pa + (R_xlen_t)c * ng;
        SEXP init = PROTECT(allocVector(LGLSXP, ng));
        int *pinit = LOGICAL(init);
        for (R_xlen_t g = 0; g < ng; ++g){ pinit[g] = 0; dst[g] = 0.0; }

        const R_xlen_t off = (R_xlen_t)c * n;
        for (R_xlen_t i = 0; i < n; ++i){
          const double v = px[off + i]; const int g = pgid[i];
          if (ISNAN(v)) continue;
          if (!pinit[g]) { dst[g] = v; pinit[g] = 1; }
          else if (v > dst[g]) dst[g] = v;
        }
        for (R_xlen_t g = 0; g < ng; ++g) if (!pinit[g]) dst[g] = NA_REAL;
        UNPROTECT(1); // init
      }
    } else {
      for (R_xlen_t c = 0; c < p; ++c){
        double *dst = pa + (R_xlen_t)c * ng;

        for (R_xlen_t g = 0; g < ng; ++g) dst[g] = R_NegInf;
        SEXP has = PROTECT(allocVector(LGLSXP, ng));
        int *phas = LOGICAL(has);
        for (R_xlen_t g = 0; g < ng; ++g) phas[g] = 0;

        const R_xlen_t off = (R_xlen_t)c * n;
        for (R_xlen_t i = 0; i < n; ++i){
          const double v = px[off + i]; const int g = pgid[i];
          if (ISNAN(v)) { phas[g] = 1; }
          else if (!phas[g]) {
            dst[g] = (isinf(dst[g]) ? v : (v > dst[g] ? v : dst[g]));
          }
        }
        for (R_xlen_t g = 0; g < ng; ++g){
          if (phas[g]) dst[g] = NA_REAL;
          else if (isinf(dst[g]) && dst[g] < 0) dst[g] = NA_REAL; // all NA
        }
        UNPROTECT(1); // has
      }
    }
  } break;

  default: error("numeric matrix required");
  }

  // dimnames: rows=uniq, cols=colnames(x)
  SEXP xdn = getAttrib(x, R_DimNamesSymbol);
  SEXP outdn = PROTECT(allocVector(VECSXP, 2));
  setAttrib(ans, R_DimNamesSymbol, outdn);

  SEXP rn_out = PROTECT(allocVector(STRSXP, ng));
  for (R_xlen_t i = 0; i < ng; ++i) SET_STRING_ELT(rn_out, i, STRING_ELT(uniq, i));
  SET_VECTOR_ELT(outdn, 0, rn_out);
  if (!isNull(xdn) && LENGTH(xdn) >= 2 && !isNull(VECTOR_ELT(xdn, 1)))
    SET_VECTOR_ELT(outdn, 1, VECTOR_ELT(xdn, 1));
  else
    SET_VECTOR_ELT(outdn, 1, R_NilValue);

  UNPROTECT(3); // ans, outdn, rn_out
  UNPROTECT(2); // gid, uniq
  return ans;
}

SEXP MinByRowNames(SEXP x, SEXP snarm){
  // checks
  if (!isMatrix(x)) error("x must be a numeric matrix");
  const int narm = asLogical(snarm);
  if (narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");
  const SEXPTYPE xt = TYPEOF(x);
  if (!(xt == INTSXP || xt == REALSXP)) error("x must be integer or double matrix");

  const R_xlen_t n = nrows(x), p = ncols(x);

  // groups from rownames
  SEXP rn = ensure_char_rownames(x);
  SEXP gid, uniq; R_xlen_t ng = 0;
  setup_name_groups(rn, n, &gid, &uniq, &ng);
  int *pgid = INTEGER(gid);

  // result (ng x p)
  SEXP ans = PROTECT(allocMatrix(xt, ng, p));

  switch (xt){
  case INTSXP: {
    const int *px = INTEGER(x);
    int *pa = INTEGER(ans);

    if (narm){
      // ignore NAs: lazy init with first non-NA, then take min
      for (R_xlen_t c = 0; c < p; ++c){
        int *dst = pa + (R_xlen_t)c * ng;

        SEXP init = PROTECT(allocVector(LGLSXP, ng));
        int *pinit = LOGICAL(init);
        for (R_xlen_t g = 0; g < ng; ++g){ pinit[g] = 0; dst[g] = 0; }

        const R_xlen_t off = (R_xlen_t)c * n;
        for (R_xlen_t i = 0; i < n; ++i){
          const int v = px[off + i], g = pgid[i];
          if (v == NA_INTEGER) continue;
          if (!pinit[g]) { dst[g] = v; pinit[g] = 1; }
          else if (v < dst[g]) dst[g] = v;
        }
        for (R_xlen_t g = 0; g < ng; ++g) if (!pinit[g]) dst[g] = NA_INTEGER;
        UNPROTECT(1); // init
      }
    } else {
      // na.rm=FALSE: any NA in group/column -> NA, else min(non-NA)
      for (R_xlen_t c = 0; c < p; ++c){
        int *dst = pa + (R_xlen_t)c * ng;

        for (R_xlen_t g = 0; g < ng; ++g) dst[g] = INT_MAX;
        SEXP has = PROTECT(allocVector(LGLSXP, ng));
        int *phas = LOGICAL(has);
        for (R_xlen_t g = 0; g < ng; ++g) phas[g] = 0;

        const R_xlen_t off = (R_xlen_t)c * n;
        for (R_xlen_t i = 0; i < n; ++i){
          const int v = px[off + i], g = pgid[i];
          if (v == NA_INTEGER) { phas[g] = 1; }
          else if (!phas[g]) {
            dst[g] = (dst[g] == INT_MAX) ? v : (v < dst[g] ? v : dst[g]);
          }
        }
        for (R_xlen_t g = 0; g < ng; ++g){
          if (phas[g]) dst[g] = NA_INTEGER;
          else if (dst[g] == INT_MAX) dst[g] = NA_INTEGER; // all NA
        }
        UNPROTECT(1); // has
      }
    }
  } break;

  case REALSXP: {
    const double *px = REAL(x);
    double *pa = REAL(ans);

    if (narm){
      for (R_xlen_t c = 0; c < p; ++c){
        double *dst = pa + (R_xlen_t)c * ng;

        SEXP init = PROTECT(allocVector(LGLSXP, ng));
        int *pinit = LOGICAL(init);
        for (R_xlen_t g = 0; g < ng; ++g){ pinit[g] = 0; dst[g] = 0.0; }

        const R_xlen_t off = (R_xlen_t)c * n;
        for (R_xlen_t i = 0; i < n; ++i){
          const double v = px[off + i]; const int g = pgid[i];
          if (ISNAN(v)) continue;
          if (!pinit[g]) { dst[g] = v; pinit[g] = 1; }
          else if (v < dst[g]) dst[g] = v;
        }
        for (R_xlen_t g = 0; g < ng; ++g) if (!pinit[g]) dst[g] = NA_REAL;
        UNPROTECT(1); // init
      }
    } else {
      for (R_xlen_t c = 0; c < p; ++c){
        double *dst = pa + (R_xlen_t)c * ng;

        for (R_xlen_t g = 0; g < ng; ++g) dst[g] = R_PosInf;
        SEXP has = PROTECT(allocVector(LGLSXP, ng));
        int *phas = LOGICAL(has);
        for (R_xlen_t g = 0; g < ng; ++g) phas[g] = 0;

        const R_xlen_t off = (R_xlen_t)c * n;
        for (R_xlen_t i = 0; i < n; ++i){
          const double v = px[off + i]; const int g = pgid[i];
          if (ISNAN(v)) { phas[g] = 1; }
          else if (!phas[g]) {
            dst[g] = (isinf(dst[g]) ? v : (v < dst[g] ? v : dst[g]));
          }
        }
        for (R_xlen_t g = 0; g < ng; ++g){
          if (phas[g]) dst[g] = NA_REAL;
          else if (isinf(dst[g]) && dst[g] > 0) dst[g] = NA_REAL; // all NA
        }
        UNPROTECT(1); // has
      }
    }
  } break;

  default: error("numeric matrix required");
  }

  // dimnames: rows=uniq, cols=colnames(x)
  SEXP xdn = getAttrib(x, R_DimNamesSymbol);
  SEXP outdn = PROTECT(allocVector(VECSXP, 2));
  setAttrib(ans, R_DimNamesSymbol, outdn);

  SEXP rn_out = PROTECT(allocVector(STRSXP, ng));
  for (R_xlen_t i = 0; i < ng; ++i) SET_STRING_ELT(rn_out, i, STRING_ELT(uniq, i));
  SET_VECTOR_ELT(outdn, 0, rn_out);
  if (!isNull(xdn) && LENGTH(xdn) >= 2 && !isNull(VECTOR_ELT(xdn, 1)))
    SET_VECTOR_ELT(outdn, 1, VECTOR_ELT(xdn, 1));
  else
    SET_VECTOR_ELT(outdn, 1, R_NilValue);

  UNPROTECT(3); // ans, outdn, rn_out
  UNPROTECT(2); // gid, uniq
  return ans;
}

SEXP SumByColNames(SEXP x, SEXP snarm){
  // argument checks
  if (!isMatrix(x)) error("x must be a numeric matrix");
  const int narm = asLogical(snarm);
  if (narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");
  const SEXPTYPE xt = TYPEOF(x);
  if (!(xt == INTSXP || xt == REALSXP))
    error("x must be integer or double matrix");

  const R_xlen_t n = nrows(x), p = ncols(x);

  // 1) group by column names: gid (0-based), uniq, ng
  SEXP cn = ensure_char_colnames(x);     // guarantee STRSXP
  SEXP gid, uniq; R_xlen_t ng = 0;
  setup_name_groups(cn, p, &gid, &uniq, &ng);  // gid, uniq are PROTECTED inside
  int *pgid = INTEGER(gid);

  // 2) build compact buckets per group:
  // pcols[ poffs[g] .. poffs[g+1]-1 ] = column ids that belong to group g
  SEXP offs = PROTECT(allocVector(INTSXP, ng + 1));
  int *poffs = INTEGER(offs);
  for (R_xlen_t g = 0; g <= ng; ++g) poffs[g] = 0;
  for (R_xlen_t j = 0; j < p; ++j) ++poffs[pgid[j] + 1];
  for (R_xlen_t g = 1; g <= ng; ++g) poffs[g] += poffs[g - 1];

  SEXP cols = PROTECT(allocVector(INTSXP, p));
  int *pcols = INTEGER(cols);

  SEXP cur  = PROTECT(allocVector(INTSXP, ng));
  int *pcur = INTEGER(cur);
  for (R_xlen_t g = 0; g < ng; ++g) pcur[g] = poffs[g];
  for (R_xlen_t j = 0; j < p; ++j) pcols[pcur[pgid[j]]++] = (int)j;

  // 3) allocate result (n x ng)
  SEXP ans = PROTECT(allocMatrix(xt, n, ng));

  switch (xt) {
  case INTSXP: {
    int *px = INTEGER(x), *pa = INTEGER(ans);

    if (narm) {
      // na.rm = TRUE: lazy-init with first non-NA, then add with overflow check.
      // Rows that saw no non-NA in the group become NA (to match base rowsum).
      for (R_xlen_t g = 0; g < ng; ++g){
        int *dst = pa + (R_xlen_t)g * n;

        SEXP init = PROTECT(allocVector(LGLSXP, n));
        int *pinit = LOGICAL(init);
        for (R_xlen_t i = 0; i < n; ++i){ pinit[i] = 0; dst[i] = 0; }

        for (int k = poffs[g]; k < poffs[g + 1]; ++k){
          const int j = pcols[k];
          const int *src = px + (R_xlen_t)j * n;
          for (R_xlen_t i = 0; i < n; ++i){
            const int s = src[i];
            if (s == NA_INTEGER) continue;
            if (!pinit[i]) { dst[i] = s; pinit[i] = 1; }
            else {
              const double tmp = (double)dst[i] + (double)s; // overflow check
              dst[i] = (tmp < INT_MIN || tmp > INT_MAX) ? NA_INTEGER : dst[i] + s;
            }
          }
        }
        for (R_xlen_t i = 0; i < n; ++i) if (!pinit[i]) dst[i] = NA_INTEGER;
        UNPROTECT(1); // init
      }
    } else {
      // na.rm = FALSE: zero-init; any NA makes the cell sticky NA
      for (R_xlen_t t = 0; t < (R_xlen_t)n * ng; ++t) pa[t] = 0;
      for (R_xlen_t g = 0; g < ng; ++g){
        int *dst = pa + (R_xlen_t)g * n;
        for (int k = poffs[g]; k < poffs[g + 1]; ++k){
          const int j = pcols[k];
          const int *src = px + (R_xlen_t)j * n;
          for (R_xlen_t i = 0; i < n; ++i){
            const int s = src[i];
            if (s == NA_INTEGER) dst[i] = NA_INTEGER;
            else if (dst[i] != NA_INTEGER){
              const double tmp = (double)dst[i] + (double)s; // overflow check
              dst[i] = (tmp < INT_MIN || tmp > INT_MAX) ? NA_INTEGER : dst[i] + s;
            }
          }
        }
      }
    }
  } break;

  case REALSXP: {
    double *px = REAL(x), *pa = REAL(ans);

    if (narm) {
      // na.rm = TRUE: lazy-init with first non-NA, sum thereafter.
      // Rows that saw no non-NA in the group become NA.
      for (R_xlen_t g = 0; g < ng; ++g){
        double *dst = pa + (R_xlen_t)g * n;

        SEXP init = PROTECT(allocVector(LGLSXP, n));
        int *pinit = LOGICAL(init);
        for (R_xlen_t i = 0; i < n; ++i){ pinit[i] = 0; dst[i] = 0.0; }

        for (int k = poffs[g]; k < poffs[g + 1]; ++k){
          const int j = pcols[k];
          const double *src = px + (R_xlen_t)j * n;
          for (R_xlen_t i = 0; i < n; ++i){
            const double s = src[i];
            if (ISNAN(s)) continue;
            if (!pinit[i]) { dst[i] = s; pinit[i] = 1; }
            else dst[i] += s;
          }
        }
        for (R_xlen_t i = 0; i < n; ++i) if (!pinit[i]) dst[i] = NA_REAL;
        UNPROTECT(1); // init
      }
    } else {
      // na.rm = FALSE: zero-init; any NA makes the cell sticky NA
      for (R_xlen_t t = 0; t < (R_xlen_t)n * ng; ++t) pa[t] = 0.0;
      for (R_xlen_t g = 0; g < ng; ++g){
        double *dst = pa + (R_xlen_t)g * n;
        for (int k = poffs[g]; k < poffs[g + 1]; ++k){
          const int j = pcols[k];
          const double *src = px + (R_xlen_t)j * n;
          for (R_xlen_t i = 0; i < n; ++i){
            const double s = src[i];
            if (ISNAN(s)) dst[i] = NA_REAL;
            else if (!ISNAN(dst[i])) dst[i] += s;
          }
        }
      }
    }
  } break;

  default:
    error("numeric matrix required");
  }

  // 4) set dimnames: rows = rownames(x), cols = unique colnames
  SEXP xdn = getAttrib(x, R_DimNamesSymbol);
  SEXP outdn = PROTECT(allocVector(VECSXP, 2));
  setAttrib(ans, R_DimNamesSymbol, outdn);

  if (!isNull(xdn) && LENGTH(xdn) >= 2 && !isNull(VECTOR_ELT(xdn, 0)))
    SET_VECTOR_ELT(outdn, 0, VECTOR_ELT(xdn, 0));
  else
    SET_VECTOR_ELT(outdn, 0, R_NilValue);

  SEXP cn_out = PROTECT(allocVector(STRSXP, ng));
  for (R_xlen_t j = 0; j < ng; ++j)
    SET_STRING_ELT(cn_out, j, STRING_ELT(uniq, j));
  SET_VECTOR_ELT(outdn, 1, cn_out);

  // UNPROTECT: offs, cols, cur, ans, outdn, cn_out (6) + gid, uniq (2)
  UNPROTECT(6);
  UNPROTECT(2);
  return ans;
}

SEXP MaxByColNames(SEXP x, SEXP snarm){
  // checks
  if (!isMatrix(x)) error("x must be a numeric matrix");
  const int narm = asLogical(snarm);
  if (narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");
  const SEXPTYPE xt = TYPEOF(x);
  if (!(xt == INTSXP || xt == REALSXP)) error("x must be integer or double matrix");

  const R_xlen_t n = nrows(x), p = ncols(x);

  // group columns by names
  SEXP cn = ensure_char_colnames(x);
  SEXP gid, uniq; R_xlen_t ng = 0;
  setup_name_groups(cn, p, &gid, &uniq, &ng);
  int *pgid = INTEGER(gid);

  // buckets: pcols[poffs[g] .. poffs[g+1]-1] = column ids in group g
  SEXP offs = PROTECT(allocVector(INTSXP, ng + 1));
  int *poffs = INTEGER(offs);
  for (R_xlen_t g = 0; g <= ng; ++g) poffs[g] = 0;
  for (R_xlen_t j = 0; j < p; ++j) ++poffs[pgid[j] + 1];
  for (R_xlen_t g = 1; g <= ng; ++g) poffs[g] += poffs[g - 1];

  SEXP cols = PROTECT(allocVector(INTSXP, p));
  int *pcols = INTEGER(cols);

  SEXP cur  = PROTECT(allocVector(INTSXP, ng));
  int *pcur = INTEGER(cur);
  for (R_xlen_t g = 0; g < ng; ++g) pcur[g] = poffs[g];
  for (R_xlen_t j = 0; j < p; ++j) pcols[pcur[pgid[j]]++] = (int)j;

  // result (n x ng)
  SEXP ans = PROTECT(allocMatrix(xt, n, ng));

  switch (xt){
  case INTSXP: {
    const int *px = INTEGER(x);
    int *pa = INTEGER(ans);

    if (narm){
      // ignore NA: lazy init with first non-NA, then take max
      for (R_xlen_t g = 0; g < ng; ++g){
        int *dst = pa + (R_xlen_t)g * n;

        SEXP init = PROTECT(allocVector(LGLSXP, n));
        int *pinit = LOGICAL(init);
        for (R_xlen_t i = 0; i < n; ++i){ pinit[i] = 0; dst[i] = 0; }

        for (int k = poffs[g]; k < poffs[g + 1]; ++k){
          const int j = pcols[k];
          const int *src = px + (R_xlen_t)j * n;
          for (R_xlen_t i = 0; i < n; ++i){
            const int v = src[i];
            if (v == NA_INTEGER) continue;
            if (!pinit[i]) { dst[i] = v; pinit[i] = 1; }
            else if (v > dst[i]) dst[i] = v;
          }
        }
        for (R_xlen_t i = 0; i < n; ++i) if (!pinit[i]) dst[i] = NA_INTEGER;
        UNPROTECT(1); // init
      }
    } else {
      // na.rm=FALSE: per-row has_na -> any NA makes NA, else max(non-NA)
      for (R_xlen_t g = 0; g < ng; ++g){
        int *dst = pa + (R_xlen_t)g * n;

        for (R_xlen_t i = 0; i < n; ++i) dst[i] = INT_MIN;
        SEXP has = PROTECT(allocVector(LGLSXP, n));
        int *phas = LOGICAL(has);
        for (R_xlen_t i = 0; i < n; ++i) phas[i] = 0;

        for (int k = poffs[g]; k < poffs[g + 1]; ++k){
          const int j = pcols[k];
          const int *src = px + (R_xlen_t)j * n;
          for (R_xlen_t i = 0; i < n; ++i){
            const int v = src[i];
            if (v == NA_INTEGER) { phas[i] = 1; }
            else if (!phas[i]) {
              dst[i] = (dst[i] == INT_MIN) ? v : (v > dst[i] ? v : dst[i]);
            }
          }
        }
        for (R_xlen_t i = 0; i < n; ++i){
          if (phas[i]) dst[i] = NA_INTEGER;
          else if (dst[i] == INT_MIN) dst[i] = NA_INTEGER; // all NA
        }
        UNPROTECT(1); // has
      }
    }
  } break;

  case REALSXP: {
    const double *px = REAL(x);
    double *pa = REAL(ans);

    if (narm){
      for (R_xlen_t g = 0; g < ng; ++g){
        double *dst = pa + (R_xlen_t)g * n;

        SEXP init = PROTECT(allocVector(LGLSXP, n));
        int *pinit = LOGICAL(init);
        for (R_xlen_t i = 0; i < n; ++i){ pinit[i] = 0; dst[i] = 0.0; }

        for (int k = poffs[g]; k < poffs[g + 1]; ++k){
          const int j = pcols[k];
          const double *src = px + (R_xlen_t)j * n;
          for (R_xlen_t i = 0; i < n; ++i){
            const double v = src[i];
            if (ISNAN(v)) continue;
            if (!pinit[i]) { dst[i] = v; pinit[i] = 1; }
            else if (v > dst[i]) dst[i] = v;
          }
        }
        for (R_xlen_t i = 0; i < n; ++i) if (!pinit[i]) dst[i] = NA_REAL;
        UNPROTECT(1); // init
      }
    } else {
      for (R_xlen_t g = 0; g < ng; ++g){
        double *dst = pa + (R_xlen_t)g * n;

        for (R_xlen_t i = 0; i < n; ++i) dst[i] = R_NegInf;
        SEXP has = PROTECT(allocVector(LGLSXP, n));
        int *phas = LOGICAL(has);
        for (R_xlen_t i = 0; i < n; ++i) phas[i] = 0;

        for (int k = poffs[g]; k < poffs[g + 1]; ++k){
          const int j = pcols[k];
          const double *src = px + (R_xlen_t)j * n;
          for (R_xlen_t i = 0; i < n; ++i){
            const double v = src[i];
            if (ISNAN(v)) { phas[i] = 1; }
            else if (!phas[i]) {
              dst[i] = (isinf(dst[i]) ? v : (v > dst[i] ? v : dst[i]));
            }
          }
        }
        for (R_xlen_t i = 0; i < n; ++i){
          if (phas[i]) dst[i] = NA_REAL;
          else if (isinf(dst[i]) && dst[i] < 0) dst[i] = NA_REAL; // all NA
        }
        UNPROTECT(1); // has
      }
    }
  } break;

  default: error("numeric matrix required");
  }

  // dimnames: rows=rownames(x), cols=uniq colnames
  SEXP xdn = getAttrib(x, R_DimNamesSymbol);
  SEXP outdn = PROTECT(allocVector(VECSXP, 2));
  setAttrib(ans, R_DimNamesSymbol, outdn);

  if (!isNull(xdn) && LENGTH(xdn) >= 2 && !isNull(VECTOR_ELT(xdn, 0)))
    SET_VECTOR_ELT(outdn, 0, VECTOR_ELT(xdn, 0));
  else
    SET_VECTOR_ELT(outdn, 0, R_NilValue);

  SEXP cn_out = PROTECT(allocVector(STRSXP, ng));
  for (R_xlen_t j = 0; j < ng; ++j)
    SET_STRING_ELT(cn_out, j, STRING_ELT(uniq, j));
  SET_VECTOR_ELT(outdn, 1, cn_out);

  // UNPROTECT: offs, cols, cur, ans, outdn, cn_out (6) + gid, uniq (2)
  UNPROTECT(6);
  UNPROTECT(2);
  return ans;
}

SEXP MinByColNames(SEXP x, SEXP snarm){
  // checks
  if (!isMatrix(x)) error("x must be a numeric matrix");
  const int narm = asLogical(snarm);
  if (narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");
  const SEXPTYPE xt = TYPEOF(x);
  if (!(xt == INTSXP || xt == REALSXP)) error("x must be integer or double matrix");

  const R_xlen_t n = nrows(x), p = ncols(x);

  // group columns by names
  SEXP cn = ensure_char_colnames(x);
  SEXP gid, uniq; R_xlen_t ng = 0;
  setup_name_groups(cn, p, &gid, &uniq, &ng);
  int *pgid = INTEGER(gid);

  // buckets: pcols[poffs[g] .. poffs[g+1]-1] = column ids in group g
  SEXP offs = PROTECT(allocVector(INTSXP, ng + 1));
  int *poffs = INTEGER(offs);
  for (R_xlen_t g = 0; g <= ng; ++g) poffs[g] = 0;
  for (R_xlen_t j = 0; j < p; ++j) ++poffs[pgid[j] + 1];
  for (R_xlen_t g = 1; g <= ng; ++g) poffs[g] += poffs[g - 1];

  SEXP cols = PROTECT(allocVector(INTSXP, p));
  int *pcols = INTEGER(cols);

  SEXP cur  = PROTECT(allocVector(INTSXP, ng));
  int *pcur = INTEGER(cur);
  for (R_xlen_t g = 0; g < ng; ++g) pcur[g] = poffs[g];
  for (R_xlen_t j = 0; j < p; ++j) pcols[pcur[pgid[j]]++] = (int)j;

  // result (n x ng)
  SEXP ans = PROTECT(allocMatrix(xt, n, ng));

  switch (xt){
  case INTSXP: {
    const int *px = INTEGER(x);
    int *pa = INTEGER(ans);

    if (narm){
      // ignore NA: lazy init with first non-NA, then take min
      for (R_xlen_t g = 0; g < ng; ++g){
        int *dst = pa + (R_xlen_t)g * n;

        SEXP init = PROTECT(allocVector(LGLSXP, n));
        int *pinit = LOGICAL(init);
        for (R_xlen_t i = 0; i < n; ++i){ pinit[i] = 0; dst[i] = 0; }

        for (int k = poffs[g]; k < poffs[g + 1]; ++k){
          const int j = pcols[k];
          const int *src = px + (R_xlen_t)j * n;
          for (R_xlen_t i = 0; i < n; ++i){
            const int v = src[i];
            if (v == NA_INTEGER) continue;
            if (!pinit[i]) { dst[i] = v; pinit[i] = 1; }
            else if (v < dst[i]) dst[i] = v;
          }
        }
        for (R_xlen_t i = 0; i < n; ++i) if (!pinit[i]) dst[i] = NA_INTEGER;
        UNPROTECT(1); // init
      }
    } else {
      // na.rm=FALSE: per-row has_na -> any NA makes NA, else min(non-NA)
      for (R_xlen_t g = 0; g < ng; ++g){
        int *dst = pa + (R_xlen_t)g * n;

        for (R_xlen_t i = 0; i < n; ++i) dst[i] = INT_MAX;
        SEXP has = PROTECT(allocVector(LGLSXP, n));
        int *phas = LOGICAL(has);
        for (R_xlen_t i = 0; i < n; ++i) phas[i] = 0;

        for (int k = poffs[g]; k < poffs[g + 1]; ++k){
          const int j = pcols[k];
          const int *src = px + (R_xlen_t)j * n;
          for (R_xlen_t i = 0; i < n; ++i){
            const int v = src[i];
            if (v == NA_INTEGER) { phas[i] = 1; }
            else if (!phas[i]) {
              dst[i] = (dst[i] == INT_MAX) ? v : (v < dst[i] ? v : dst[i]);
            }
          }
        }
        for (R_xlen_t i = 0; i < n; ++i){
          if (phas[i]) dst[i] = NA_INTEGER;
          else if (dst[i] == INT_MAX) dst[i] = NA_INTEGER; // all NA
        }
        UNPROTECT(1); // has
      }
    }
  } break;

  case REALSXP: {
    const double *px = REAL(x);
    double *pa = REAL(ans);

    if (narm){
      for (R_xlen_t g = 0; g < ng; ++g){
        double *dst = pa + (R_xlen_t)g * n;

        SEXP init = PROTECT(allocVector(LGLSXP, n));
        int *pinit = LOGICAL(init);
        for (R_xlen_t i = 0; i < n; ++i){ pinit[i] = 0; dst[i] = 0.0; }

        for (int k = poffs[g]; k < poffs[g + 1]; ++k){
          const int j = pcols[k];
          const double *src = px + (R_xlen_t)j * n;
          for (R_xlen_t i = 0; i < n; ++i){
            const double v = src[i];
            if (ISNAN(v)) continue;
            if (!pinit[i]) { dst[i] = v; pinit[i] = 1; }
            else if (v < dst[i]) dst[i] = v;
          }
        }
        for (R_xlen_t i = 0; i < n; ++i) if (!pinit[i]) dst[i] = NA_REAL;
        UNPROTECT(1); // init
      }
    } else {
      for (R_xlen_t g = 0; g < ng; ++g){
        double *dst = pa + (R_xlen_t)g * n;

        for (R_xlen_t i = 0; i < n; ++i) dst[i] = R_PosInf;
        SEXP has = PROTECT(allocVector(LGLSXP, n));
        int *phas = LOGICAL(has);
        for (R_xlen_t i = 0; i < n; ++i) phas[i] = 0;

        for (int k = poffs[g]; k < poffs[g + 1]; ++k){
          const int j = pcols[k];
          const double *src = px + (R_xlen_t)j * n;
          for (R_xlen_t i = 0; i < n; ++i){
            const double v = src[i];
            if (ISNAN(v)) { phas[i] = 1; }
            else if (!phas[i]) {
              dst[i] = (isinf(dst[i]) ? v : (v < dst[i] ? v : dst[i]));
            }
          }
        }
        for (R_xlen_t i = 0; i < n; ++i){
          if (phas[i]) dst[i] = NA_REAL;
          else if (isinf(dst[i]) && dst[i] > 0) dst[i] = NA_REAL; // all NA
        }
        UNPROTECT(1); // has
      }
    }
  } break;

  default: error("numeric matrix required");
  }

  // dimnames: rows=rownames(x), cols=uniq colnames
  SEXP xdn = getAttrib(x, R_DimNamesSymbol);
  SEXP outdn = PROTECT(allocVector(VECSXP, 2));
  setAttrib(ans, R_DimNamesSymbol, outdn);

  if (!isNull(xdn) && LENGTH(xdn) >= 2 && !isNull(VECTOR_ELT(xdn, 0)))
    SET_VECTOR_ELT(outdn, 0, VECTOR_ELT(xdn, 0));
  else
    SET_VECTOR_ELT(outdn, 0, R_NilValue);

  SEXP cn_out = PROTECT(allocVector(STRSXP, ng));
  for (R_xlen_t j = 0; j < ng; ++j)
    SET_STRING_ELT(cn_out, j, STRING_ELT(uniq, j));
  SET_VECTOR_ELT(outdn, 1, cn_out);

  // UNPROTECT: offs, cols, cur, ans, outdn, cn_out (6) + gid, uniq (2)
  UNPROTECT(6);
  UNPROTECT(2);
  return ans;
}

SEXP SumByDimNames(SEXP x, SEXP snarm){
  // Argument checks
  if (!isMatrix(x)) error("x must be a numeric matrix");
  const int narm = asLogical(snarm);
  if (narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");
  const SEXPTYPE xt = TYPEOF(x);
  if (!(xt == INTSXP || xt == REALSXP))
    error("x must be integer or double matrix");

  const R_xlen_t n = nrows(x);   // number of rows
  const R_xlen_t p = ncols(x);   // number of columns

  // Build groups from row and column names
  SEXP rn = ensure_char_rownames(x);  // ensure STRSXP rownames
  SEXP cn = ensure_char_colnames(x);  // ensure STRSXP colnames
  SEXP gid_r, uniq_r; R_xlen_t ng_r = 0;
  SEXP gid_c, uniq_c; R_xlen_t ng_c = 0;
  setup_name_groups(rn, n, &gid_r, &uniq_r, &ng_r); // row groups
  setup_name_groups(cn, p, &gid_c, &uniq_c, &ng_c); // col groups
  int *pgid_r = INTEGER(gid_r);  // row group index
  int *pgid_c = INTEGER(gid_c);  // col group index

  // Allocate result matrix (ng_r x ng_c)
  SEXP ans = PROTECT(allocMatrix(xt, ng_r, ng_c));

  switch (xt) {
  case INTSXP: {
    int *px = INTEGER(x);
    int *pa = INTEGER(ans);

    // Initialize result with zeros
    for (R_xlen_t t = 0; t < (R_xlen_t)ng_r * ng_c; ++t) pa[t] = 0;

    // Accumulate by row and column groups
    for (R_xlen_t j = 0; j < p; ++j){
      const R_xlen_t off  = (R_xlen_t)j * n;         // source column offset
      const R_xlen_t goff = (R_xlen_t)pgid_c[j] * ng_r; // destination col offset
      for (R_xlen_t i = 0; i < n; ++i){
        const int v = px[off + i];
        int *cell = &pa[goff + pgid_r[i]];
        if (v == NA_INTEGER) {
          if (!narm) *cell = NA_INTEGER;  // sticky NA if na.rm=FALSE
        } else if (*cell != NA_INTEGER) {
          const int cur = *cell;
          const double tmp = (double)cur + (double)v; // check overflow
          *cell = (tmp < INT_MIN || tmp > INT_MAX) ? NA_INTEGER : (cur + v);
        }
      }
    }
  } break;

  case REALSXP: {
    double *px = REAL(x);
    double *pa = REAL(ans);

    // Initialize result with zeros
    for (R_xlen_t t = 0; t < (R_xlen_t)ng_r * ng_c; ++t) pa[t] = 0.0;

    if (narm){
      // Skip NA/NaN values
      for (R_xlen_t j = 0; j < p; ++j){
        const R_xlen_t off  = (R_xlen_t)j * n;
        const R_xlen_t goff = (R_xlen_t)pgid_c[j] * ng_r;
        for (R_xlen_t i = 0; i < n; ++i){
          const double v = px[off + i];
          if (!ISNAN(v)) pa[goff + pgid_r[i]] += v;
        }
      }
    } else {
      // Sticky NA: if any NA/NaN encountered, cell becomes NA_REAL
      for (R_xlen_t j = 0; j < p; ++j){
        const R_xlen_t off  = (R_xlen_t)j * n;
        const R_xlen_t goff = (R_xlen_t)pgid_c[j] * ng_r;
        for (R_xlen_t i = 0; i < n; ++i){
          const double v = px[off + i];
          double *cell = &pa[goff + pgid_r[i]];
          if (ISNAN(v)) *cell = NA_REAL;
          else if (!ISNAN(*cell)) *cell += v;
        }
      }
    }
  } break;

  default:
    error("numeric matrix required");
  }

  // Set dimnames: rows = unique row names, cols = unique col names
  SEXP outdn = PROTECT(allocVector(VECSXP, 2));
  setAttrib(ans, R_DimNamesSymbol, outdn);

  // Assign rownames
  SEXP rn_out = PROTECT(allocVector(STRSXP, ng_r));
  for (R_xlen_t i = 0; i < ng_r; ++i)
    SET_STRING_ELT(rn_out, i, STRING_ELT(uniq_r, i));
  SET_VECTOR_ELT(outdn, 0, rn_out);

  // Assign colnames
  SEXP cn_out = PROTECT(allocVector(STRSXP, ng_c));
  for (R_xlen_t j = 0; j < ng_c; ++j)
    SET_STRING_ELT(cn_out, j, STRING_ELT(uniq_c, j));
  SET_VECTOR_ELT(outdn, 1, cn_out);

  // Balance PROTECTs: ans, outdn, rn_out, cn_out (+ setup_name_groups PROTECTs)
  UNPROTECT(4);
  UNPROTECT(4); // if setup_name_groups used PROTECT twice
  return ans;
}

SEXP MaxByDimNames(SEXP x, SEXP snarm){
  // Argument checks
  if (!isMatrix(x)) error("x must be a numeric matrix");
  const int narm = asLogical(snarm);
  if (narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");
  const SEXPTYPE xt = TYPEOF(x);
  if (!(xt == INTSXP || xt == REALSXP))
    error("x must be integer or double matrix");

  const R_xlen_t n = nrows(x);
  const R_xlen_t p = ncols(x);

  // Build groups from row and column names
  SEXP rn = ensure_char_rownames(x);
  SEXP cn = ensure_char_colnames(x);
  SEXP gid_r, uniq_r; R_xlen_t ng_r = 0;
  SEXP gid_c, uniq_c; R_xlen_t ng_c = 0;
  setup_name_groups(rn, n, &gid_r, &uniq_r, &ng_r);
  setup_name_groups(cn, p, &gid_c, &uniq_c, &ng_c);
  int *pgid_r = INTEGER(gid_r);
  int *pgid_c = INTEGER(gid_c);

  // Allocate result matrix (ng_r x ng_c)
  SEXP ans = PROTECT(allocMatrix(xt, ng_r, ng_c));

  switch (xt) {
  case INTSXP: {
    const int *px = INTEGER(x);
    int *pa = INTEGER(ans);

    if (narm) {
      // Initialize all cells with NA; update with first non-NA then max
      for (R_xlen_t t = 0; t < (R_xlen_t)ng_r * ng_c; ++t) pa[t] = NA_INTEGER;

      for (R_xlen_t j = 0; j < p; ++j){
        const R_xlen_t off  = (R_xlen_t)j * n;
        const R_xlen_t goff = (R_xlen_t)pgid_c[j] * ng_r;
        for (R_xlen_t i = 0; i < n; ++i){
          const int v = px[off + i];
          int *cell = &pa[goff + pgid_r[i]];
          if (v != NA_INTEGER) {
            if (*cell == NA_INTEGER) *cell = v;
            else if (v > *cell) *cell = v;
          }
        }
      }
    } else {
      // NA is sticky: if any NA appears in a group → result NA
      for (R_xlen_t t = 0; t < (R_xlen_t)ng_r * ng_c; ++t) pa[t] = INT_MIN;

      SEXP has = PROTECT(allocVector(LGLSXP, ng_r * ng_c));
      int *phas = LOGICAL(has);
      for (R_xlen_t g = 0; g < ng_r * ng_c; ++g) phas[g] = 0;

      for (R_xlen_t j = 0; j < p; ++j){
        const R_xlen_t off  = (R_xlen_t)j * n;
        const R_xlen_t goff = (R_xlen_t)pgid_c[j] * ng_r;
        for (R_xlen_t i = 0; i < n; ++i){
          const int v = px[off + i];
          int idx = goff + pgid_r[i];
          if (v == NA_INTEGER) phas[idx] = 1;
          else if (!phas[idx]) {
            pa[idx] = (pa[idx] == INT_MIN) ? v : (v > pa[idx] ? v : pa[idx]);
          }
        }
      }

      for (R_xlen_t g = 0; g < ng_r * ng_c; ++g) {
        if (phas[g] || pa[g] == INT_MIN) pa[g] = NA_INTEGER;
      }
      UNPROTECT(1); // has
    }
  } break;

  case REALSXP: {
    const double *px = REAL(x);
    double *pa = REAL(ans);

    if (narm) {
      // Initialize all cells with NA; update with first non-NA then max
      for (R_xlen_t t = 0; t < (R_xlen_t)ng_r * ng_c; ++t) pa[t] = NA_REAL;

      for (R_xlen_t j = 0; j < p; ++j){
        const R_xlen_t off  = (R_xlen_t)j * n;
        const R_xlen_t goff = (R_xlen_t)pgid_c[j] * ng_r;
        for (R_xlen_t i = 0; i < n; ++i){
          const double v = px[off + i];
          double *cell = &pa[goff + pgid_r[i]];
          if (!ISNAN(v)) {
            if (ISNAN(*cell)) *cell = v;
            else if (v > *cell) *cell = v;
          }
        }
      }
    } else {
      // NA is sticky: if any NA appears in a group → result NA
      for (R_xlen_t t = 0; t < (R_xlen_t)ng_r * ng_c; ++t) pa[t] = R_NegInf;

      SEXP has = PROTECT(allocVector(LGLSXP, ng_r * ng_c));
      int *phas = LOGICAL(has);
      for (R_xlen_t g = 0; g < ng_r * ng_c; ++g) phas[g] = 0;

      for (R_xlen_t j = 0; j < p; ++j){
        const R_xlen_t off  = (R_xlen_t)j * n;
        const R_xlen_t goff = (R_xlen_t)pgid_c[j] * ng_r;
        for (R_xlen_t i = 0; i < n; ++i){
          const double v = px[off + i];
          int idx = goff + pgid_r[i];
          if (ISNAN(v)) phas[idx] = 1;
          else if (!phas[idx]) {
            pa[idx] = (isinf(pa[idx]) ? v : (v > pa[idx] ? v : pa[idx]));
          }
        }
      }

      for (R_xlen_t g = 0; g < ng_r * ng_c; ++g) {
        if (phas[g] || (isinf(pa[g]) && pa[g] < 0)) pa[g] = NA_REAL;
      }
      UNPROTECT(1); // has
    }
  } break;

  default: error("numeric matrix required");
  }

  // Set dimnames
  SEXP outdn = PROTECT(allocVector(VECSXP, 2));
  setAttrib(ans, R_DimNamesSymbol, outdn);

  SEXP rn_out = PROTECT(allocVector(STRSXP, ng_r));
  for (R_xlen_t i = 0; i < ng_r; ++i) SET_STRING_ELT(rn_out, i, STRING_ELT(uniq_r, i));
  SET_VECTOR_ELT(outdn, 0, rn_out);

  SEXP cn_out = PROTECT(allocVector(STRSXP, ng_c));
  for (R_xlen_t j = 0; j < ng_c; ++j) SET_STRING_ELT(cn_out, j, STRING_ELT(uniq_c, j));
  SET_VECTOR_ELT(outdn, 1, cn_out);

  UNPROTECT(4);
  UNPROTECT(4); // gid_r, uniq_r and gid_c, uniq_c
  return ans;
}

SEXP MinByDimNames(SEXP x, SEXP snarm){
  // Argument checks
  if (!isMatrix(x)) error("x must be a numeric matrix");
  const int narm = asLogical(snarm);
  if (narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");
  const SEXPTYPE xt = TYPEOF(x);
  if (!(xt == INTSXP || xt == REALSXP))
    error("x must be integer or double matrix");

  const R_xlen_t n = nrows(x);
  const R_xlen_t p = ncols(x);

  // Build groups from row and column names
  SEXP rn = ensure_char_rownames(x);
  SEXP cn = ensure_char_colnames(x);
  SEXP gid_r, uniq_r; R_xlen_t ng_r = 0;
  SEXP gid_c, uniq_c; R_xlen_t ng_c = 0;
  setup_name_groups(rn, n, &gid_r, &uniq_r, &ng_r);
  setup_name_groups(cn, p, &gid_c, &uniq_c, &ng_c);
  int *pgid_r = INTEGER(gid_r);
  int *pgid_c = INTEGER(gid_c);

  // Allocate result matrix (ng_r x ng_c)
  SEXP ans = PROTECT(allocMatrix(xt, ng_r, ng_c));

  switch (xt) {
  case INTSXP: {
    const int *px = INTEGER(x);
    int *pa = INTEGER(ans);

    if (narm) {
      // Initialize all cells with NA; update with first non-NA then min
      for (R_xlen_t t = 0; t < (R_xlen_t)ng_r * ng_c; ++t) pa[t] = NA_INTEGER;

      for (R_xlen_t j = 0; j < p; ++j){
        const R_xlen_t off  = (R_xlen_t)j * n;
        const R_xlen_t goff = (R_xlen_t)pgid_c[j] * ng_r;
        for (R_xlen_t i = 0; i < n; ++i){
          const int v = px[off + i];
          int *cell = &pa[goff + pgid_r[i]];
          if (v != NA_INTEGER) {
            if (*cell == NA_INTEGER) *cell = v;
            else if (v < *cell) *cell = v;
          }
        }
      }
    } else {
      // NA is sticky: if any NA appears in a group → result NA
      for (R_xlen_t t = 0; t < (R_xlen_t)ng_r * ng_c; ++t) pa[t] = INT_MAX;

      SEXP has = PROTECT(allocVector(LGLSXP, ng_r * ng_c));
      int *phas = LOGICAL(has);
      for (R_xlen_t g = 0; g < ng_r * ng_c; ++g) phas[g] = 0;

      for (R_xlen_t j = 0; j < p; ++j){
        const R_xlen_t off  = (R_xlen_t)j * n;
        const R_xlen_t goff = (R_xlen_t)pgid_c[j] * ng_r;
        for (R_xlen_t i = 0; i < n; ++i){
          const int v = px[off + i];
          int idx = goff + pgid_r[i];
          if (v == NA_INTEGER) phas[idx] = 1;
          else if (!phas[idx]) {
            pa[idx] = (pa[idx] == INT_MAX) ? v : (v < pa[idx] ? v : pa[idx]);
          }
        }
      }

      for (R_xlen_t g = 0; g < ng_r * ng_c; ++g) {
        if (phas[g] || pa[g] == INT_MAX) pa[g] = NA_INTEGER;
      }
      UNPROTECT(1); // has
    }
  } break;

  case REALSXP: {
    const double *px = REAL(x);
    double *pa = REAL(ans);

    if (narm) {
      // Initialize all cells with NA; update with first non-NA then min
      for (R_xlen_t t = 0; t < (R_xlen_t)ng_r * ng_c; ++t) pa[t] = NA_REAL;

      for (R_xlen_t j = 0; j < p; ++j){
        const R_xlen_t off  = (R_xlen_t)j * n;
        const R_xlen_t goff = (R_xlen_t)pgid_c[j] * ng_r;
        for (R_xlen_t i = 0; i < n; ++i){
          const double v = px[off + i];
          double *cell = &pa[goff + pgid_r[i]];
          if (!ISNAN(v)) {
            if (ISNAN(*cell)) *cell = v;
            else if (v < *cell) *cell = v;
          }
        }
      }
    } else {
      // NA is sticky: if any NA appears in a group → result NA
      for (R_xlen_t t = 0; t < (R_xlen_t)ng_r * ng_c; ++t) pa[t] = R_PosInf;

      SEXP has = PROTECT(allocVector(LGLSXP, ng_r * ng_c));
      int *phas = LOGICAL(has);
      for (R_xlen_t g = 0; g < ng_r * ng_c; ++g) phas[g] = 0;

      for (R_xlen_t j = 0; j < p; ++j){
        const R_xlen_t off  = (R_xlen_t)j * n;
        const R_xlen_t goff = (R_xlen_t)pgid_c[j] * ng_r;
        for (R_xlen_t i = 0; i < n; ++i){
          const double v = px[off + i];
          int idx = goff + pgid_r[i];
          if (ISNAN(v)) phas[idx] = 1;
          else if (!phas[idx]) {
            pa[idx] = (isinf(pa[idx]) ? v : (v < pa[idx] ? v : pa[idx]));
          }
        }
      }

      for (R_xlen_t g = 0; g < ng_r * ng_c; ++g) {
        if (phas[g] || (isinf(pa[g]) && pa[g] > 0)) pa[g] = NA_REAL;
      }
      UNPROTECT(1); // has
    }
  } break;

  default: error("numeric matrix required");
  }

  // Set dimnames
  SEXP outdn = PROTECT(allocVector(VECSXP, 2));
  setAttrib(ans, R_DimNamesSymbol, outdn);

  SEXP rn_out = PROTECT(allocVector(STRSXP, ng_r));
  for (R_xlen_t i = 0; i < ng_r; ++i) SET_STRING_ELT(rn_out, i, STRING_ELT(uniq_r, i));
  SET_VECTOR_ELT(outdn, 0, rn_out);

  SEXP cn_out = PROTECT(allocVector(STRSXP, ng_c));
  for (R_xlen_t j = 0; j < ng_c; ++j) SET_STRING_ELT(cn_out, j, STRING_ELT(uniq_c, j));
  SET_VECTOR_ELT(outdn, 1, cn_out);

  UNPROTECT(4);
  UNPROTECT(4);
  return ans;
}

/* ==========================================================================
 * Row-wise and Column-wise aggregators (sum, max, min)
 *
 * These functions compute summary statistics across either rows
 * or columns of a numeric matrix. Each function returns a vector
 * with one element per row (Row*) or per column (Col*).
 *
 * Functions:
 *   - RowSum / ColSum : sum of elements
 *   - RowMax / ColMax : maximum element
 *   - RowMin / ColMin : minimum element
 *
 * NA handling:
 *   - If na.rm = TRUE:
 *       * NA/NaN values are skipped in the aggregation.
 *       * If all values in a row/column are NA/NaN, result is NA.
 *   - If na.rm = FALSE:
 *       * Any NA/NaN in a row/column makes the result NA
 *         ("sticky NA" semantics).
 *
 * Integer specifics:
 *   - Overflow during summation is detected.
 *   - If overflow occurs, result is set to NA_INTEGER.
 *
 * Types supported: INTSXP and REALSXP matrices.
 *
 * Complexity:
 *   - O(m * n), scanning all matrix cells exactly once.
 *   - Implemented with pointer arithmetic to maximize memory
 *     efficiency and minimize intermediate allocations.
 *
 * Returned value:
 *   - A vector of length equal to the number of rows (Row*) or
 *     the number of columns (Col*).
 *   - Result has no dimnames, consistent with base R’s rowSums,
 *     rowMaxs, etc.
 * ========================================================================= */
// Row-wise sum with na.rm (+ int overflow -> NA)
SEXP RowSum(SEXP x, SEXP snarm) {
  R_xlen_t m, n;
  SEXP z;

  m = nrows(x), n = ncols(x);
  const int narm = asLogical(snarm);
  if (narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");

  switch (TYPEOF(x)) {
  case INTSXP: {
    PROTECT(z = allocVector(INTSXP, m));
    int *xs = INTEGER(x), *xe = xs + m * n;
    int *zs = INTEGER(z), *ze = zs + m;
    int *xi, *zi;

    if (narm) {
      // init: zeros; then add non-NA terms
      for (zi = zs; zi != ze; ++zi) *zi = 0;
      for (; xs != xe; ) {
        for (zi = zs, xi = xs, xs += m; xi != xs; ++zi, ++xi) {
          const int v = *xi;
          if (v == NA_INTEGER) continue;
          if (*zi != NA_INTEGER) {
            const double tmp = (double)(*zi) + (double)v;
            if (tmp < INT_MIN || tmp > INT_MAX) *zi = NA_INTEGER;
            else *zi += v;
          }
        }
      }
    } else {
      // init: copy 1st column; NA is sticky
      for (zi = zs, xi = xs; zi != ze; ++zi, ++xi)
        *zi = *xi;

      xs = xi; // now at column 2
      for (; xi != xe; ) {
        for (zi = zs, xi = xs, xs += m; xi != xs; ++zi, ++xi) {
          const int v = *xi;
          if (v == NA_INTEGER) {
            *zi = NA_INTEGER;                      // sticky NA
          } else if (*zi != NA_INTEGER) {
            const double tmp = (double)(*zi) + (double)v;
            if (tmp < INT_MIN || tmp > INT_MAX) *zi = NA_INTEGER;
            else *zi += v;
          }
        }
      }
    }
  } break;

  case REALSXP: {
    PROTECT(z = allocVector(REALSXP, m));
    double *xs = REAL(x), *xe = xs + m * n;
    double *zs = REAL(z), *ze = zs + m;
    double *xi, *zi;

    if (narm) {
      // init: zeros; then add non-NA terms
      for (zi = zs; zi != ze; ++zi) *zi = 0.0;
      for (; xs != xe; ) {
        for (zi = zs, xi = xs, xs += m; xi != xs; ++zi, ++xi) {
          const double v = *xi;
          if (!ISNAN(v)) *zi += v;
        }
      }
    } else {
      // init: copy 1st column; NA is sticky
      for (zi = zs, xi = xs; zi != ze; ++zi, ++xi)
        *zi = *xi;

      xs = xi; // now at column 2
      for (; xi != xe; ) {
        for (zi = zs, xi = xs, xs += m; xi != xs; ++zi, ++xi) {
          const double v = *xi;
          if (ISNAN(v)) *zi = NA_REAL;             // sticky NA
          else if (!ISNAN(*zi)) *zi += v;
        }
      }
    }
  } break;

  default:
    error("invalid input");
  }

  UNPROTECT(1);
  return z;
}

// Row-wise maximum with na.rm
SEXP RowMax(SEXP x, SEXP snarm) {
  R_xlen_t m, n;
  SEXP z;

  m = nrows(x), n = ncols(x);
  const int narm = asLogical(snarm);
  if (narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");

  switch (TYPEOF(x)) {
  case INTSXP: {
    PROTECT(z = allocVector(INTSXP, m));
    int *xs = INTEGER(x), *xe = xs + m * n;
    int *zs = INTEGER(z), *ze = zs + m;
    int *xi, *zi;

    // initialize: copy the first column as baseline
    for (zi = zs, xi = xs; zi != ze; ++zi, ++xi)
      *zi = *xi;

    // sweep over remaining columns and update maxima
    xs = xi; // xs now points to the second column
    for (; xi != xe;) {
      for (zi = zs, xi = xs, xs += m; xi != xs; ++zi, ++xi) {
        const int v = *xi;
        if (v == NA_INTEGER) {
          if (!narm) {
            *zi = NA_INTEGER;  // keep NA sticky if na.rm=FALSE
          }
        } else {
          if (*zi == NA_INTEGER) {
            if (narm) *zi = v; // replace NA with first non-NA
          } else if (v > *zi) {
            *zi = v;           // update with larger value
          }
        }
      }
    }
  } break;

  case REALSXP: {
    PROTECT(z = allocVector(REALSXP, m));
    double *xs = REAL(x), *xe = xs + m * n;
    double *zs = REAL(z), *ze = zs + m;
    double *xi, *zi;

    // initialize: copy the first column as baseline
    for (zi = zs, xi = xs; zi != ze; ++zi, ++xi)
      *zi = *xi;

    // sweep over remaining columns and update maxima
    xs = xi; // xs now points to the second column
    for (; xi != xe;) {
      for (zi = zs, xi = xs, xs += m; xi != xs; ++zi, ++xi) {
        const double v = *xi;
        if (ISNAN(v)) {
          if (!narm) {
            *zi = NA_REAL;     // keep NA sticky if na.rm=FALSE
          }
        } else {
          if (ISNAN(*zi)) {
            if (narm) *zi = v; // replace NA with first non-NA
          } else if (v > *zi) {
            *zi = v;           // update with larger value
          }
        }
      }
    }
  } break;

  default:
    error("invalid input");
  }

  UNPROTECT(1);
  return z;
}

// Row-wise minimum with na.rm
SEXP RowMin(SEXP x, SEXP snarm) {
  R_xlen_t m, n;
  SEXP z;

  m = nrows(x), n = ncols(x);
  const int narm = asLogical(snarm);
  if (narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");

  switch (TYPEOF(x)) {
  case INTSXP: {
    PROTECT(z = allocVector(INTSXP, m));
    int *xs = INTEGER(x), *xe = xs + m * n;
    int *zs = INTEGER(z), *ze = zs + m;
    int *xi, *zi;

    // init: copy 1st column
    for (zi = zs, xi = xs; zi != ze; ++zi, ++xi)
      *zi = *xi;

    // sweep remaining columns
    xs = xi; // now at column 2
    for (; xi != xe; ) {
      for (zi = zs, xi = xs, xs += m; xi != xs; ++zi, ++xi) {
        const int v = *xi;
        if (v == NA_INTEGER) {
          if (!narm) *zi = NA_INTEGER;            // sticky NA (na.rm=FALSE)
        } else {
          if (*zi == NA_INTEGER) {
            if (narm) *zi = v;                    // first non-NA
          } else if (v < *zi) {
            *zi = v;                              // smaller value
          }
        }
      }
    }
  } break;

  case REALSXP: {
    PROTECT(z = allocVector(REALSXP, m));
    double *xs = REAL(x), *xe = xs + m * n;
    double *zs = REAL(z), *ze = zs + m;
    double *xi, *zi;

    // init: copy 1st column
    for (zi = zs, xi = xs; zi != ze; ++zi, ++xi)
      *zi = *xi;

    // sweep remaining columns
    xs = xi; // now at column 2
    for (; xi != xe; ) {
      for (zi = zs, xi = xs, xs += m; xi != xs; ++zi, ++xi) {
        const double v = *xi;
        if (ISNAN(v)) {
          if (!narm) *zi = NA_REAL;               // sticky NA (na.rm=FALSE)
        } else {
          if (ISNAN(*zi)) {
            if (narm) *zi = v;                    // first non-NA
          } else if (v < *zi) {
            *zi = v;                              // smaller value
          }
        }
      }
    }
  } break;

  default:
    error("invalid input");
  }

  UNPROTECT(1);
  return z;
}

// Column-wise sum with NA handling (overflow -> NA_INTEGER)
SEXP ColSum(SEXP x, SEXP snarm) {
  R_xlen_t m, n;
  SEXP z;

  m = nrows(x);
  n = ncols(x);
  const int narm = asLogical(snarm);
  if (narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");

  switch (TYPEOF(x)) {
  case INTSXP: {
    PROTECT(z = allocVector(INTSXP, n));
    int *xs = INTEGER(x);
    int *zs = INTEGER(z), *ze = zs + n;
    int *xi, *zi;

    // initialize: copy first row
    for (zi = zs, xi = xs; zi != ze; ++zi, xi += m)
      *zi = *xi;

    // scan remaining rows
    ++xs;
    for (zi = zs; zi != ze; ++zi) {
      for (xi = xs, xs += m; xi != (xs - 1); ++xi) {
        const int v = *xi;
        if (v == NA_INTEGER) {
          if (!narm) *zi = NA_INTEGER;
        } else {
          if (*zi == NA_INTEGER) {
            if (narm) *zi = v;
          } else {
            *zi += v;
          }
        }
      }
    }
  } break;

  case REALSXP: {
    PROTECT(z = allocVector(REALSXP, n));
    double *xs = REAL(x);
    double *zs = REAL(z), *ze = zs + n;
    double *xi, *zi;

    // initialize: copy first row
    for (zi = zs, xi = xs; zi != ze; ++zi, xi += m)
      *zi = *xi;

    // scan remaining rows
    ++xs;
    for (zi = zs; zi != ze; ++zi) {
      for (xi = xs, xs += m; xi != (xs - 1); ++xi) {
        const double v = *xi;
        if (ISNAN(v)) {
          if (!narm) *zi = NA_REAL;
        } else {
          if (ISNAN(*zi)) {
            if (narm) *zi = v;
          } else {
            *zi += v;
          }
        }
      }
    }
  } break;

  default:
    error("invalid input");
  }

  UNPROTECT(1);
  return z;
}

// Column-wise maximum with NA handling
SEXP ColMax(SEXP x, SEXP snarm) {
  R_xlen_t m, n;
  SEXP z;

  m = nrows(x);
  n = ncols(x);
  const int narm = asLogical(snarm);
  if (narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");

  switch (TYPEOF(x)) {
  case INTSXP: {
    PROTECT(z = allocVector(INTSXP, n));
    int *xs = INTEGER(x);
    int *zs = INTEGER(z), *ze = zs + n;
    int *xi, *zi;

    // initialize: copy first row
    for (zi = zs, xi = xs; zi != ze; ++zi, xi += m)
      *zi = *xi;

    // scan remaining rows
    ++xs;
    for (zi = zs; zi != ze; ++zi) {
      for (xi = xs, xs += m; xi != (xs - 1); ++xi) {
        const int v = *xi;
        if (v == NA_INTEGER) {
          if (!narm) *zi = NA_INTEGER;
        } else {
          if (*zi == NA_INTEGER) {
            if (narm) *zi = v;
          } else if (v > *zi) {
            *zi = v;
          }
        }
      }
    }
  } break;

  case REALSXP: {
    PROTECT(z = allocVector(REALSXP, n));
    double *xs = REAL(x);
    double *zs = REAL(z), *ze = zs + n;
    double *xi, *zi;

    // initialize: copy first row
    for (zi = zs, xi = xs; zi != ze; ++zi, xi += m)
      *zi = *xi;

    // scan remaining rows
    ++xs;
    for (zi = zs; zi != ze; ++zi) {
      for (xi = xs, xs += m; xi != (xs - 1); ++xi) {
        const double v = *xi;
        if (ISNAN(v)) {
          if (!narm) *zi = NA_REAL;
        } else {
          if (ISNAN(*zi)) {
            if (narm) *zi = v;
          } else if (v > *zi) {
            *zi = v;
          }
        }
      }
    }
  } break;

  default:
    error("invalid input");
  }

  UNPROTECT(1);
  return z;
}

// Column-wise minimum with NA handling
SEXP ColMin(SEXP x, SEXP snarm) {
  R_xlen_t m, n;
  SEXP z;

  m = nrows(x);
  n = ncols(x);
  const int narm = asLogical(snarm);
  if (narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");

  switch (TYPEOF(x)) {
  case INTSXP: {
    PROTECT(z = allocVector(INTSXP, n));
    int *xs = INTEGER(x);
    int *zs = INTEGER(z), *ze = zs + n;
    int *xi, *zi;

    // initialize: copy first row
    for (zi = zs, xi = xs; zi != ze; ++zi, xi += m)
      *zi = *xi;

    // scan remaining rows
    ++xs;
    for (zi = zs; zi != ze; ++zi) {
      for (xi = xs, xs += m; xi != (xs - 1); ++xi) {
        const int v = *xi;
        if (v == NA_INTEGER) {
          if (!narm) *zi = NA_INTEGER;
        } else {
          if (*zi == NA_INTEGER) {
            if (narm) *zi = v;
          } else if (v < *zi) {
            *zi = v;
          }
        }
      }
    }
  } break;

  case REALSXP: {
    PROTECT(z = allocVector(REALSXP, n));
    double *xs = REAL(x);
    double *zs = REAL(z), *ze = zs + n;
    double *xi, *zi;

    // initialize: copy first row
    for (zi = zs, xi = xs; zi != ze; ++zi, xi += m)
      *zi = *xi;

    // scan remaining rows
    ++xs;
    for (zi = zs; zi != ze; ++zi) {
      for (xi = xs, xs += m; xi != (xs - 1); ++xi) {
        const double v = *xi;
        if (ISNAN(v)) {
          if (!narm) *zi = NA_REAL;
        } else {
          if (ISNAN(*zi)) {
            if (narm) *zi = v;
          } else if (v < *zi) {
            *zi = v;
          }
        }
      }
    }
  } break;

  default:
    error("invalid input");
  }

  UNPROTECT(1);
  return z;
}

// Row-wise first difference: result has (m-1) rows, n cols
SEXP RowDiff(SEXP x, SEXP snarm) {
  R_xlen_t i, j, m, n;
  SEXP dimnames, colnames, z;

  m = nrows(x);
  n = ncols(x);
  if (m < 2)
    error("The matrix must have at least two rows to compute row differences.");

  const int narm = asLogical(snarm);
  if (narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");

  switch (TYPEOF(x)) {
  case INTSXP: {
    PROTECT(z = allocMatrix(INTSXP, m - 1, n));
    const int *ix = INTEGER(x);
    int *iz = INTEGER(z);
    for (j = 0; j < n; ++j) {
      const R_xlen_t off = (R_xlen_t)j * m;
      const R_xlen_t zof = (R_xlen_t)j * (m - 1);
      for (i = 0; i < m - 1; ++i) {
        const int a = ix[off + i + 1];
        const int b = ix[off + i];
        // na.rm has no effect for pairwise diff: if either NA -> NA
        iz[zof + i] = (a != NA_INTEGER && b != NA_INTEGER) ? (a - b) : NA_INTEGER;
      }
    }
  } break;

  case REALSXP: {
    PROTECT(z = allocMatrix(REALSXP, m - 1, n));
    const double *ix = REAL(x);
    double *iz = REAL(z);
    for (j = 0; j < n; ++j) {
      const R_xlen_t off = (R_xlen_t)j * m;
      const R_xlen_t zof = (R_xlen_t)j * (m - 1);
      for (i = 0; i < m - 1; ++i) {
        const double a = ix[off + i + 1];
        const double b = ix[off + i];
        // na.rm has no effect for pairwise diff: if either NA/NaN -> NA
        iz[zof + i] = (!ISNAN(a) && !ISNAN(b)) ? (a - b) : NA_REAL;
      }
    }
  } break;

  default:
    error("invalid input");
  }

  /* preserve colnames */
  dimnames = getAttrib(x, R_DimNamesSymbol);
  if (!isNull(dimnames)) {
    colnames = VECTOR_ELT(dimnames, 1);
    SetColNames(z, colnames);
  }

  UNPROTECT(1);
  return z;
}

// Column-wise first difference: result has m rows, (n-1) cols
SEXP ColDiff(SEXP x, SEXP snarm) {
  R_xlen_t i, j, m, n;
  SEXP dimnames, rownames, z;

  m = nrows(x);
  n = ncols(x);
  if (n < 2)
    error("The matrix must have at least two columns to compute column differences.");

  const int narm = asLogical(snarm);
  if (narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");

  switch (TYPEOF(x)) {
  case INTSXP: {
    PROTECT(z = allocMatrix(INTSXP, m, n - 1));
    const int *ix = INTEGER(x);
    int *iz = INTEGER(z);
    for (i = 0; i < m; ++i) {
      for (j = 0; j < n - 1; ++j) {
        const int a = ix[i + (j + 1) * m];
        const int b = ix[i + j * m];
        // na.rm has no effect for pairwise diff: if either NA -> NA
        iz[i + j * m] = (a != NA_INTEGER && b != NA_INTEGER) ? (a - b) : NA_INTEGER;
      }
    }
  } break;

  case REALSXP: {
    PROTECT(z = allocMatrix(REALSXP, m, n - 1));
    const double *ix = REAL(x);
    double *iz = REAL(z);
    for (i = 0; i < m; ++i) {
      for (j = 0; j < n - 1; ++j) {
        const double a = ix[i + (j + 1) * m];
        const double b = ix[i + j * m];
        // na.rm has no effect for pairwise diff: if either NA/NaN -> NA
        iz[i + j * m] = (!ISNAN(a) && !ISNAN(b)) ? (a - b) : NA_REAL;
      }
    }
  } break;

  default:
    error("invalid input");
  }

  /* preserve rownames */
  dimnames = getAttrib(x, R_DimNamesSymbol);
  if (!isNull(dimnames)) {
    rownames = VECTOR_ELT(dimnames, 0);
    SetRowNames(z, rownames);
  }

  UNPROTECT(1);
  return z;
}

SEXP Rotate(SEXP x, SEXP angle) {
  if (!isMatrix(x))
    error("not a matrix");

  R_xlen_t i, j, m, n, p, degree;
  SEXP z;

  m = nrows(x), n = ncols(x), p = XLENGTH(x);
  degree = asInteger(angle);
  // normalize: allow negative angles & big angles
  degree = ((degree % 360) + 360) % 360;

  if (degree == 0)
    return x;

  switch(TYPEOF(x)){
  case LGLSXP:{
    if (degree == 90) {
    PROTECT(z = allocMatrix(LGLSXP, n, m));
    copy_dimnames(x, z);
    int* iz = LOGICAL(z);
    int* ix = LOGICAL(x);
    for (i = 0, j = m-1; i < p; ++i, j += m) {
      iz[i] = ix[j];
      if (j > p-m) j -= (p+1);
    }
  } else if (degree == 180) {
    PROTECT(z = allocMatrix(LGLSXP, m, n));
    copy_dimnames(x, z);
    int* iz = LOGICAL(z);
    int* ix = LOGICAL(x);
    for (i = 0, j = p-1; i < p; ++i, --j) {
      iz[i] = ix[j];
    }
  } else if (degree == 270) {
    PROTECT(z = allocMatrix(LGLSXP, n, m));
    copy_dimnames(x, z);
    int* iz = LOGICAL(z);
    int* ix = LOGICAL(x);
    for (i = 0, j = p-m; i < p; ++i, j -= m) {
      if (j < 0) j += (p+1);
      iz[i] = ix[j];
    }
  } else {
    error("invalid degree");
  }
  } break;

  case INTSXP:{
    if (degree == 90) {
    PROTECT(z = allocMatrix(INTSXP, n, m));
    copy_dimnames(x, z);
    int* iz = INTEGER(z);
    int* ix = INTEGER(x);
    for (i = 0, j = m-1; i < p; ++i, j += m) {
      iz[i] = ix[j];
      if (j > p-m) j -= (p+1);
    }
  } else if (degree == 180) {
    PROTECT(z = allocMatrix(INTSXP, m, n));
    copy_dimnames(x, z);
    int* iz = INTEGER(z);
    int* ix = INTEGER(x);
    for (i = 0, j = p-1; i < p; ++i, --j) {
      iz[i] = ix[j];
    }
  } else if (degree == 270) {
    PROTECT(z = allocMatrix(INTSXP, n, m));
    copy_dimnames(x, z);
    int* iz = INTEGER(z);
    int* ix = INTEGER(x);
    for (i = 0, j = p-m; i < p; ++i, j -= m) {
      if (j < 0) j += (p+1);
      iz[i] = ix[j];
    }
  } else {
    error("invalid degree");
  }
  } break;

  case REALSXP:{
    if (degree == 90) {
    PROTECT(z = allocMatrix(REALSXP, n, m));
    copy_dimnames(x, z);
    double* iz = REAL(z);
    double* ix = REAL(x);
    for (i = 0, j = m-1; i < p; ++i, j += m) {
      iz[i] = ix[j];
      if (j > p-m) j -= (p+1);
    }
  } else if (degree == 180) {
    PROTECT(z = allocMatrix(REALSXP, m, n));
    copy_dimnames(x, z);
    double* iz = REAL(z);
    double* ix = REAL(x);
    for (i = 0, j = p-1; i < p; ++i, --j) {
      iz[i] = ix[j];
    }
  } else if (degree == 270) {
    PROTECT(z = allocMatrix(REALSXP, n, m));
    copy_dimnames(x, z);
    double* iz = REAL(z);
    double* ix = REAL(x);
    for (i = 0, j = p-m; i < p; ++i, j -= m) {
      if (j < 0) j += (p+1);
      iz[i] = ix[j];
    }
  } else {
    error("invalid degree");
  }
  } break;

  case CPLXSXP:{
    if (degree == 90) {
    PROTECT(z = allocMatrix(CPLXSXP, n, m));
    copy_dimnames(x, z);
    Rcomplex* iz = COMPLEX(z);
    Rcomplex* ix = COMPLEX(x);
    for (i = 0, j = m-1; i < p; ++i, j += m) {
      iz[i] = ix[j];
      if (j > p-m) j -= (p+1);
    }
  } else if (degree == 180) {
    PROTECT(z = allocMatrix(CPLXSXP, m, n));
    copy_dimnames(x, z);
    Rcomplex* iz = COMPLEX(z);
    Rcomplex* ix = COMPLEX(x);
    for (i = 0, j = p-1; i < p; ++i, --j) {
      iz[i] = ix[j];
    }
  } else if (degree == 270) {
    PROTECT(z = allocMatrix(CPLXSXP, n, m));
    copy_dimnames(x, z);
    Rcomplex* iz = COMPLEX(z);
    Rcomplex* ix = COMPLEX(x);
    for (i = 0, j = p-m; i < p; ++i, j -= m) {
      if (j < 0) j += (p+1);
      iz[i] = ix[j];
    }
  } else {
    error("invalid degree");
  }
  } break;

  case STRSXP:{
    if (degree == 90) {
    PROTECT(z = allocMatrix(STRSXP, n, m));
    copy_dimnames(x, z);
    for (i = 0, j = m-1; i < p; ++i, j += m) {
      SET_STRING_ELT(z, i, STRING_ELT(x, j));
      if (j > p-m) j -= (p+1);
    }
  } else if (degree == 180) {
    PROTECT(z = allocMatrix(STRSXP, m, n));
    copy_dimnames(x, z);
    for (i = 0, j = p-1; i < p; ++i, --j) {
      SET_STRING_ELT(z, i, STRING_ELT(x, j));
    }
  } else if (degree == 270) {
    PROTECT(z = allocMatrix(STRSXP, n, m));
    copy_dimnames(x, z);
    for (i = 0, j = p-m; i < p; ++i, j -= m) {
      if (j < 0) j += (p+1);
      SET_STRING_ELT(z, i, STRING_ELT(x, j));
    }
  } else {
    error("invalid degree");
  }
  } break;

  default:
    error("invalid input");
  }

  UNPROTECT(1);
  return z;
}

SEXP MatXMat(SEXP x, SEXP y) {
  if (!isMatrix(x) || !isMatrix(y))
    error("not a matrix");

  if (TYPEOF(x) != TYPEOF(y))
    error("different input types");

  R_xlen_t i, j, m, n, o, p, col;
  m = nrows(x), n = ncols(x), o = nrows(y), p = ncols(y);

  if (m != o || n != p)
    error("different length");
  switch(TYPEOF(x)) {
  case INTSXP:{
    PROTECT(x), PROTECT(y);
    int* ix = INTEGER(x);
    int* iy = INTEGER(y);
    for (i = 0; i < n; ++i) {
      col = i * m;
      for (j = 0; j < m; ++j) {
        ix[col + j] = ix[col + j] * iy[col + j];
      }
    }
  } break;
  case REALSXP:{
    PROTECT(x), PROTECT(y);
    double* ix = REAL(x);
    double* iy = REAL(y);
    for (i = 0; i < n; ++i) {
      col = i * m;
      for (j = 0; j < m; ++j) {
        ix[col + j] = ix[col + j] * iy[col + j];
      }
    }
  } break;
  default:
    error("invalid input");
  }
  UNPROTECT(2);
  return R_NilValue;
}

SEXP MatXRow(SEXP x, SEXP y) {
  if (!isMatrix(x))
    error("not a matrix");

  if (TYPEOF(x) != TYPEOF(y))
    error("different input types");

  R_xlen_t i, j, m, n, p, col;
  m = nrows(x), n = ncols(x), p = XLENGTH(y);

  if (n != p)
    error("different length");

  switch(TYPEOF(x)) {
  case INTSXP:{
    PROTECT(x), PROTECT(y);
    int* ix = INTEGER(x);
    int* iy = INTEGER(y);
    for (i = 0; i < n; ++i) {
      col = i * m;
      for (j = 0; j < m; ++j) {
        ix[col + j] = ix[col + j] * iy[i];
      }
    }
  } break;
  case REALSXP:{
    PROTECT(x), PROTECT(y);
    double* ix = REAL(x);
    double* iy = REAL(y);
    for (i = 0; i < n; ++i) {
      col = i * m;
      for (j = 0; j < m; ++j) {
        ix[col + j] = ix[col + j] * iy[i];
      }
    }
  } break;
  default:
    error("invalid input");
  }
  UNPROTECT(2);
  return R_NilValue;
}

SEXP MatXCol(SEXP x, SEXP y) {
  if (!isMatrix(x))
    error("not a matrix");

  if (TYPEOF(x) != TYPEOF(y))
    error("different input types");

  R_xlen_t i, j, m, n, o, col;
  m = nrows(x), n = ncols(x), o = XLENGTH(y);

  if (m != o)
    error("different length");

  switch(TYPEOF(x)) {
  case INTSXP:{
    PROTECT(x), PROTECT(y);
    int* ix = INTEGER(x);
    int* iy = INTEGER(y);
    for (i = 0; i < n; ++i) {
      col = i * m;
      for (j = 0; j < m; ++j) {
        ix[col + j] = ix[col + j] * iy[j];
      }
    }
  } break;
  case REALSXP:{
    PROTECT(x), PROTECT(y);
    double* ix = REAL(x);
    double* iy = REAL(y);
    for (i = 0; i < n; ++i) {
      col = i * m;
      for (j = 0; j < m; ++j) {
        ix[col + j] = ix[col + j] * iy[j];
      }
    }
  } break;
  default:
    error("invalid input");
  }
  UNPROTECT(2);
  return R_NilValue;
}

SEXP MatXNum(SEXP x, SEXP y) {
  if (!isMatrix(x))
    error("not a matrix");

  if (TYPEOF(x) != TYPEOF(y))
    error("different input types");

  R_xlen_t i, j, m, n, col;
  m = nrows(x), n = ncols(x);

  switch(TYPEOF(x)) {
  case INTSXP:{
    PROTECT(x);
    int* ix = INTEGER(x);
    int  iy = asInteger(y);
    for (i = 0; i < n; ++i) {
      col = i * m;
      for (j = 0; j < m; ++j) {
        ix[col + j] = ix[col + j] * iy;
      }
    }
  } break;
  case REALSXP:{
    PROTECT(x);
    double* ix = REAL(x);
    double  iy = asReal(y);
    for (i = 0; i < n; ++i) {
      col = i * m;
      for (j = 0; j < m; ++j) {
        ix[col + j] = ix[col + j] * iy;
      }
    }
  } break;
  default:
    error("invalid length");
  }
  UNPROTECT(1);
  return R_NilValue;
}
