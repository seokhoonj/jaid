#include "jaid.h"

// compare int
static int compare_int(const void *a, const void *b) {
  int x = *(const int*)a;
  int y = *(const int*)b;
  if (x < y) return -1;
  if (x > y) return 1;
  return 0;
}

// unique with order preserved
SEXP iunique(int arr[], size_t n) {
  uint32_t K;
  size_t   M;
  choose_table_size((R_xlen_t)n, &K, &M);
  const uint32_t mask = (uint32_t)M - 1U;

  int *h = (int*)calloc(M, sizeof(int));
  int *p = (int*)calloc(n, sizeof(int));

  size_t cnt = 0;
  for (size_t i = 0; i < n; ++i) { // length of array
    const uint32_t key = (arr[i] == INT_MIN) ? 0U : (uint32_t)arr[i];
    uint32_t id = hash_index(key, (uint32_t)K, mask);
    bool found = false;
    while (h[id]) {
      if (arr[h[id]-1] == arr[i]) {
        found = true; break;
      }
      id = probe_next(id, mask);
    }
    if (!found) {
      h[id] = (int)i + 1;
      p[i]++;
      cnt++;
    }
  }

  SEXP ans = PROTECT(allocVector(INTSXP, cnt));
  int *pans = INTEGER(ans);

  size_t j = 0;
  for (size_t i = 0; j < cnt && i < n; ++i) {
    if (p[i]) pans[j++] = arr[i];
  }

  free(h); free(p);
  UNPROTECT(1);
  return ans;
}

/* ================================================================
 * FindGroupBreaks
 *
 * Given one or more ID vectors (possibly a list of vectors),
 * find the break positions where the group identity changes.
 *
 * Example:
 *   id = c("A","A","B","B","B","C")
 *   -> returns positions [0, 2, 5, 6]
 *
 * Returns an integer vector of positions marking:
 *   - the start is always 0.
 *   - each boundary where ID values change
 *   - the end position (m)
 * ================================================================ */
SEXP FindGroupBreaks(SEXP x) {
  R_xlen_t i, j, m, n, p = 0;
  SEXP pos, v;

  // if id is not a list, wrap it into a list of length 1
  const SEXPTYPE xt = TYPEOF(x);
  if (xt != VECSXP) {
    SEXP tmp = PROTECT(allocVector(VECSXP, 1));
    SET_VECTOR_ELT(tmp, 0, x);
    x = tmp; // from here on, treat as VECSXP
    // do not UNPROTECT(tmp) yet; we can unprotect at the end
  }

  // number of rows (m), number of grouping variables (n)
  m = XLENGTH(VECTOR_ELT(x, 0));
  n = XLENGTH(x);

  // temporary buffer to collect break positions
  int *ipos = (int*)calloc((m-1) * n + 2, sizeof(int));
  if (!ipos) error("memory allocation failed");

  ipos[p++] = 0; // always start at 0

  // loop over grouping variables (columns of id)
  for (i = 0; i < n; ++i) {
    v = VECTOR_ELT(x, i);
    switch (TYPEOF(v)) {
      case LGLSXP: {
        const int *iv = LOGICAL(v);
        for (j = 0; j < m - 1; ++j)
          if (iv[j] != iv[j+1]) ipos[p++] = j+1;
      } break;
      case INTSXP: {
        const int *iv = INTEGER(v);
        for (j = 0; j < m - 1; ++j)
          if (iv[j] != iv[j+1]) ipos[p++] = j+1;
      } break;
      case REALSXP: {
        const double *iv = REAL(v);
        for (j = 0; j < m - 1; ++j)
          if (!safe_equal_dbl(iv[j], iv[j+1]))
            ipos[p++] = j+1;
      } break;
      case CPLXSXP: {
        const Rcomplex *iv = COMPLEX(v);
        for (j = 0; j < m - 1; ++j)
          if (!safe_equal_cplx(iv[j], iv[j+1]))
            ipos[p++] = j+1;
      } break;
      case STRSXP: {
        const SEXP *iv = STRING_PTR_RO(v);
        for (j = 0; j < m - 1; ++j)
          if (strcmp(CHAR(iv[j]), CHAR(iv[j+1])) != 0)
            ipos[p++] = j+1;
      } break;
      default: {
        free(ipos);
        error("invalid input");
      }
    }
  }

  // always include end position
  ipos[p++] = m;

  // shrink buffer to actual size
  int *jpos = (int*)realloc(ipos, sizeof(int) * p);
  if (!jpos) { free(ipos); error("memory allocation failed"); }

  // sort + unique
  qsort(jpos, p, sizeof(int), compare_int);
  pos = PROTECT(iunique(jpos, p));
  free(jpos);

  if (xt != VECSXP) {
    UNPROTECT(1);
  }

  UNPROTECT(1);
  return pos;
}


/* ========================================================================
 * Return an integer vector of group sizes for consecutive runs.
 * - Input x: atomic vector OR list of columns (VECSXP).
 * Must be pre-sorted so that equal-adjacent rows belong to the same group.
 * - Output: INT vector of lengths, length == (#groups).
 *========================================================================= */
SEXP FindGroupSizes(SEXP x) {
  R_xlen_t i, j, m, n, p = 0;
  SEXP sizes, pos, v;

  // if x is not a list, wrap it into a list of length 1
  const SEXPTYPE xt = TYPEOF(x);
  if (xt != VECSXP) {
    SEXP tmp = PROTECT(allocVector(VECSXP, 1));
    SET_VECTOR_ELT(tmp, 0, x);
    x = tmp; // from here on, treat as VECSXP
  // do not UNPROTECT(tmp) yet; we unprotect at the end if xt != VECSXP
  }

  // number of rows (m), number of grouping variables (n)
  m = XLENGTH(VECTOR_ELT(x, 0));
  n = XLENGTH(x);

  // temporary buffer to collect break positions
  int *ipos = (int*)calloc((size_t)m * (size_t)n, sizeof(int));
  if (!ipos) error("memory allocation failed");

  ipos[p++] = 0; /* always start at 0 */

  // loop over grouping variables (columns of x)
  for (i = 0; i < n; ++i) {
    v = VECTOR_ELT(x, i);
    switch (TYPEOF(v)) {
    case LGLSXP: {
      const int *iv = LOGICAL(v);
      for (j = 0; j < m - 1; ++j)
        if (iv[j] != iv[j+1]) ipos[p++] = (int)(j + 1);
    } break;

    case INTSXP: {
      const int *iv = INTEGER(v);
      for (j = 0; j < m - 1; ++j)
        if (iv[j] != iv[j+1]) ipos[p++] = (int)(j + 1);
    } break;

    case REALSXP: {
      const double *iv = REAL(v);
      for (j = 0; j < m - 1; ++j)
        if (!safe_equal_dbl(iv[j], iv[j+1]))
          ipos[p++] = (int)(j + 1);
    } break;

    case CPLXSXP: {
      const Rcomplex *iv = COMPLEX(v);
      for (j = 0; j < m - 1; ++j)
        if (!safe_equal_cplx(iv[j], iv[j+1]))
          ipos[p++] = (int)(j + 1);
    } break;

    case STRSXP: {
      const SEXP *iv = STRING_PTR_RO(v);
      for (j = 0; j < m - 1; ++j)
        if (strcmp(CHAR(iv[j]), CHAR(iv[j+1])) != 0)
          ipos[p++] = (int)(j + 1);
    } break;

    default:
      free(ipos);
    error("invalid input");
    }
  }

  // always include end position
  // (we'll append m after sorting/unique to avoid being removed accidentally)
  ipos[p++] = (int)m;

  // shrink buffer to actual size
  int *jpos = (int*)realloc(ipos, sizeof(int) * (size_t)p);
  if (!jpos) { free(ipos); error("memory allocation failed"); }

  // sort + unique → break positions INCLUDING 0 and m
  qsort(jpos, (size_t)p, sizeof(int), compare_int);
  pos = PROTECT(iunique(jpos, (size_t)p));
  free(jpos); // pos now owns the break positions

  const R_xlen_t nb = XLENGTH(pos);
  if (nb < 2) {
    PROTECT(sizes = allocVector(INTSXP, 0));
    if (xt != VECSXP) UNPROTECT(1);   // tmp (wrap)
  UNPROTECT(2);                     // sizes, pos
  return sizes;
  }

  PROTECT(sizes = allocVector(INTSXP, nb - 1));
  int *ppos = INTEGER(pos);
  int *psz  = INTEGER(sizes);
  for (R_xlen_t k = 0; k < nb - 1; ++k) {
    psz[k] = ppos[k + 1] - ppos[k];
  }

  if (xt != VECSXP) UNPROTECT(1);     // tmp (wrap)
  UNPROTECT(2);                       // sizes, pos
  return sizes;
}
