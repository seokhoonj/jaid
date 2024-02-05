#include "jaid.h"

int compare(const void *a, const void *b) {
  return *(int*)a - *(int*)b;
}

SEXP iunique(int arr[], size_t n) {
  const int n2 = 2U * (int) n; // 2 times of length of n
  int M = 256;
  size_t K = 8; // 2^8 = 256 (2^K = M)
  while (M < n2) {
    M *= 2;
    K++;
  } // M = 2^K; if n = 512, M = 1024, K = 10; if n = 1024, M = 2048, K = 11
  size_t count = 0;
  int *h = (int*)Calloc(M, int);
  int *p = (int*)Calloc(n, int);
  for (int i = 0; i < n; ++i) { // length of array
    int id = (arr[i] == INT_MIN) ? 0 : HASH(arr[i], K);
    while (h[id]) {
      if (arr[h[id]-1]==arr[i]) {
        goto iblt;
      }
      id++, id %= M;
    }
    h[id] = (int) i + 1;
    p[i]++;
    count++;
    iblt:;
  }
  size_t ct = 0;
  int *z = (int*)Calloc(count, int);
  for (int i = 0; ct < count; ++i) {
    if (p[i]) {
      z[ct++] = arr[i];
    }
  }
  SEXP ans;
  PROTECT(ans = allocVector(INTSXP, ct));
  int *pans = INTEGER(ans);
  for (int i = 0; i < ct; ++i) {
    pans[i] = z[i];
  }
  free(h); free(p); free(z);
  UNPROTECT(1);
  return ans;
}

SEXP SortGroupBy(SEXP id) {
  R_xlen_t i, j, m, n, p = 0;
  SEXP pos, v;
  switch(TYPEOF(id)) {
  case VECSXP:{
    m = XLENGTH(VECTOR_ELT(id, 0)), n = XLENGTH(id);
    int *ipos = (int*)Calloc(m*n, int);
    ipos[p++] = 0;
    for (i = 0; i < n; ++i) {
      v = VECTOR_ELT(id, i);
      switch(TYPEOF(v)){
      case LGLSXP:{
        int *iv = LOGICAL(v);
        for (j = 0; j < m-1; ++j) {
          if (iv[j] != iv[j+1]) ipos[p++] = j+1;
        }
      } break;
      case INTSXP:{
        int *iv = INTEGER(v);
        for (j = 0; j < m-1; ++j) {
          if (iv[j] != iv[j+1]) ipos[p++] = j+1;
        }
      } break;
      case REALSXP:{
        double *iv = REAL(v);
        for (j = 0; j < m-1; ++j) {
          if (!REQUAL(iv[j], iv[j+1])) ipos[p++] = j+1;
        }
      } break;
      case STRSXP:{
        SEXP *iv = STRING_PTR(v);
        for (j = 0; j < m-1; ++j) {
          if (strcmp(CHAR(iv[j]), CHAR(iv[j+1]))) ipos[p++] = j+1;
        }
      } break;
      default:
        error(_("invalid input"));
      }
    }
    ipos[p++] = m;
    int *jpos = (int*)realloc(ipos, sizeof(int)*p);
    qsort(jpos, p, sizeof(int), compare);
    pos = iunique(jpos, p);
  } break;
  default:
    error(_("invalid input"));
  }
  return pos;
}

SEXP IndexDateRangeOverlap(SEXP id, SEXP from, SEXP to, SEXP interval) {
  R_xlen_t m, n, i, j;
  SEXP loc, sub, v, z;
  m = XLENGTH(VECTOR_ELT(id, 0)), n = XLENGTH(id);

  int *ifr = INTEGER(from);
  int *ito = INTEGER(to);
  int vinterval = asInteger(interval);

  PROTECT(loc = allocVector(INTSXP, m));
  PROTECT(sub = allocVector(INTSXP, m));
  FillCInt(loc, 1);
  FillCInt(sub, 0);
  int *iloc = INTEGER(loc);
  int *isub = INTEGER(sub);

  int p = 1, mx = 0; // index, maximum `to`
  bool c1, c2; // condition 1, condition 2
  for (i = 1; i < m; ++i) {
    j = 0, c1 = true;
    while (j < n) {
      v = VECTOR_ELT(id, j);
      switch(TYPEOF(v)){
      case LGLSXP:{
        int *iv = LOGICAL(v);
        c1 = (iv[i-1] == iv[i]);
      } break;
      case INTSXP:{
        int *iv = INTEGER(v);
        c1 = (iv[i-1] == iv[i]);
      } break;
      case REALSXP:{
        double *iv = REAL(v);
        c1 = (iv[i-1] == iv[i]);
      } break;
      case STRSXP:{
        SEXP *iv = STRING_PTR(v);
        c1 = (!strcmp(CHAR(iv[i-1]), CHAR(iv[i])));
      } break;
      default:
        error(_("invalid input"));
      }
      if (c1 == false) break;
      j++;
    }
    mx = (ito[i-1] > mx) ? ito[i-1] : mx;
    c2 = ifr[i] <= (mx + 1 + vinterval);
    if (c1 && c2) {
      iloc[i] = p;
      if (ifr[i] > mx) isub[i] = ifr[i] - mx - 1;
    } else {
      iloc[i] = ++p;
      mx = ito[i];
    }
  }
  const char *names[] = {"loc", "sub", ""};
  PROTECT(z = mkNamed(VECSXP, names));
  SET_VECTOR_ELT(z, 0, loc);
  SET_VECTOR_ELT(z, 1, sub);
  UNPROTECT(3);
  return z;
}
