#include "jaid.h"

// length(unique(x))
SEXP Unilen(SEXP x) {
  if (isFactor(x)) {
    const int nlvl = LENGTH(PROTECT(getAttrib(x, R_LevelsSymbol)));
    UNPROTECT(1);
    bool *cnt = (bool*)calloc((size_t)nlvl + 1U, sizeof(bool));
    const int *px = INTEGER(x);
    const int n = LENGTH(x);
    int uniq = 0;
    for (int i = 0; i < n; ++i) {
      int lvl = px[i];
      if (!cnt[lvl]) {
        if (++uniq == nlvl) break;
        cnt[lvl] = true;
      }
    }
    free(cnt);
    return ScalarInteger(uniq);
  }
  if (isLogical(x)) {
    bool *cnt = (bool*)calloc(3, sizeof(bool));
    const int *px = LOGICAL(x);
    const int n = LENGTH(x);
    int uniq = 0;
    for (int i = 0; i < n; ++i) {
      const int state = (px[i] == NA_LOGICAL) ? 2 : px[i];
      if (!cnt[state]) {
        if (++uniq == 3) break;
        cnt[state] = true;
      }
    }
    free(cnt);
    return ScalarInteger(uniq);
  }

  // size
  const R_xlen_t n = xlength(x);
  uint32_t K; size_t M;
  choose_table_size(n, &K, &M);
  const uint32_t mask = (uint32_t)M - 1U;

  // 0 = empty, otherwise index + 1
  int* h = (int*)calloc(M, sizeof(int));
  R_xlen_t uniq = 0;

  switch(TYPEOF(x)) {
  case INTSXP:{
    const int* px = INTEGER(x);
    uint32_t key = 0;
    uint32_t id = 0;
    for (int i = 0; i < n; ++i) {
      key = (px[i] == NA_INTEGER) ? 0U : (uint32_t)px[i];
      id  = hash_index(key, (uint32_t) K, mask);
      bool found = false;
      while (h[id]) {
        if (px[h[id]-1] == px[i]) {
          found = true; break;
        }
        id = probe_next(id, mask);
      }
      if (!found) {
        h[id] = (int)i + 1;
        uniq++;
      }
    }
  } break;
  case REALSXP:{
    const double* px = REAL(x);
    uint32_t key = 0;
    uint32_t id = 0;
    for (int i = 0; i < n; ++i) {
      key = key_from_double(px[i]);
      id  = hash_index(key, (uint32_t)K, mask);
      bool found = false;
      while (h[id]) {
        if (safe_equal_dbl(px[h[id]-1], px[i])) {
          found = true; break;
        }
        id = probe_next(id, mask);
      }
      if (!found) {
        h[id] = (int)i + 1;
        uniq++;
      }
    }
  } break;
  case CPLXSXP: {
    const Rcomplex *px = COMPLEX(x);
    for (R_xlen_t i = 0; i < n; ++i) {
      uint32_t key = key_from_complex(px[i]);
      uint32_t id  = hash_index(key, (uint32_t)K, mask);
      bool found = false;
      while (h[id]) {
        if (safe_equal_cplx(px[h[id] - 1], px[i])) {
          found = true; break;
        }
        id = probe_next(id, mask);
      }
      if (!found) {
        h[id] = (int)i + 1;
        uniq++;
      }
    }
  } break;
  case STRSXP:{
    const SEXP *px = STRING_PTR_RO(x);
    uint32_t key = 0;
    uint32_t id = 0;
    for (int i = 0; i < n; ++i) {
      key = key_from_ptr((const void*)px[i]);
      id  = hash_index(key, (uint32_t)K, mask);
      bool found = false;
      while (h[id]) {
        if (px[h[id] - 1] == px[i]) {
          found = true; break;
        }
        id = probe_next(id, mask);
      }
      if (!found) {
        h[id] = (int)i + 1;
        uniq++;
      }
    }
  } break;
  default:
    error("invalid input");
  }
  free(h);
  return ScalarInteger(uniq);
}

SEXP Reverse(SEXP x) {
  R_xlen_t i, n = XLENGTH(x);

  // in-place
  PROTECT(x);

  switch (TYPEOF(x)) {
  case LGLSXP: {
    int *px = LOGICAL(x);
    for (i = 0; i < n / 2; ++i) {
      int tmp = px[i];
      px[i] = px[n - 1 - i];
      px[n - 1 - i] = tmp;
    }
  } break;

  case INTSXP: {
    int *px = INTEGER(x);
    for (i = 0; i < n / 2; ++i) {
      int tmp = px[i];
      px[i] = px[n - 1 - i];
      px[n - 1 - i] = tmp;
    }
  } break;

  case REALSXP: {
    double *px = REAL(x);
    for (i = 0; i < n / 2; ++i) {
      double tmp = px[i];
      px[i] = px[n - 1 - i];
      px[n - 1 - i] = tmp;
    }
  } break;

  case CPLXSXP: {
    Rcomplex *px = COMPLEX(x);
    for (i = 0; i < n / 2; ++i) {
      Rcomplex tmp = px[i];
      px[i] = px[n - 1 - i];
      px[n - 1 - i] = tmp;
    }
  } break;

  case STRSXP: {
    for (i = 0; i < n / 2; ++i) {
      SEXP tmp = STRING_ELT(x, i);
      SET_STRING_ELT(x, i, STRING_ELT(x, n - 1 - i));
      SET_STRING_ELT(x, n - 1 - i, tmp);
    }
  } break;

  default: {
    UNPROTECT(1);
    error("invalid input");
  }
  }

  UNPROTECT(1);
  return x;
}


SEXP Interleave(SEXP x, SEXP y) {
  if (TYPEOF(x) != TYPEOF(y))
    error("different input types");

  R_xlen_t xn, yn, i, j, k;
  SEXP z;

  switch(TYPEOF(x)) {
  case LGLSXP:{
    PROTECT(z = allocVector(LGLSXP, XLENGTH(x) + XLENGTH(y)));
    int *xs = LOGICAL(x), *xe = xs + XLENGTH(x);
    int *ys = LOGICAL(y), *ye = ys + XLENGTH(y);
    int *zs = LOGICAL(z);
    int *xi = xs, *yi = ys, *zi = zs;
    // Interleave both array
    while (xi < xe && yi < ye) {
      *zi = *xi;
      ++zi, ++xi;
      *zi = *yi;
      ++zi, ++yi;
    }
    // Store remaining elements of first array
    while (xi < xe) {
      *zi = *xi;
      ++zi, ++xi;
    }
    // Store remaining elements of second array
    while (yi < ye) {
      *zi = *yi;
      ++zi, ++yi;
    }
  } break;
  case INTSXP:{
    PROTECT(z = allocVector(INTSXP, XLENGTH(x)+XLENGTH(y)));
    int *xs = INTEGER(x), *xe = xs + XLENGTH(x);
    int *ys = INTEGER(y), *ye = ys + XLENGTH(y);
    int *zs = INTEGER(z);
    int *xi = xs, *yi = ys, *zi = zs;
    // Interleave both array
    while (xi < xe && yi < ye) {
      *zi = *xi;
      ++zi, ++xi;
      *zi = *yi;
      ++zi, ++yi;
    }
    // Store remaining elements of first array
    while (xi < xe) {
      *zi = *xi;
      ++zi, ++xi;
    }
    // Store remaining elements of second array
    while (yi < ye) {
      *zi = *yi;
      ++zi, ++yi;
    }
  } break;
  case REALSXP:{
    xn = XLENGTH(x), yn = XLENGTH(y);
    PROTECT(z = allocVector(REALSXP, xn + yn));
    double *xs = REAL(x), *xe = xs + xn;
    double *ys = REAL(y), *ye = ys + yn;
    double *zs = REAL(z);
    double *xi = xs, *yi = ys, *zi = zs;
    // Interleave both array
    while (xi < xe && yi < ye) {
      *zi = *xi;
      ++zi, ++xi;
      *zi = *yi;
      ++zi, ++yi;
    }
    // Store remaining elements of first array
    while (xi < xe) {
      *zi = *xi;
      ++zi, ++xi;
    }
    // Store remaining elements of second array
    while (yi < ye) {
      *zi = *yi;
      ++zi, ++yi;
    }
  } break;
  case STRSXP:{
    xn = XLENGTH(x), yn = XLENGTH(y);
    PROTECT(z = allocVector(STRSXP, xn+yn));
    i = 0, j = 0, k = 0;
    // Interleave both array
    while (j < xn && k < yn) {
      SET_STRING_ELT(z, i++, STRING_ELT(x, j++));
      SET_STRING_ELT(z, i++, STRING_ELT(y, k++));
    }
    // Store remaining elements of first array
    while (j < xn) {
      SET_STRING_ELT(z, i++, STRING_ELT(x, j++));
    }
    // Store remaining elements of second array
    while (k < yn) {
      SET_STRING_ELT(z, i++, STRING_ELT(y, k++));
    }
  } break;
  default:
    error("invalid input");
  }
  UNPROTECT(1);
  return z;
}
