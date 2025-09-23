#include "jaid.h"

SEXP FillZeroNotFirstPos(SEXP x, SEXP id, SEXP ot) {
  R_xlen_t i, j, k, p, m, n, s, t, col, flag;
  SEXP pos, ans, cot;

  i = 1, p = 0;
  m = nrows(x), n = ncols(x);
  s = xlength(id), t = xlength(ot);

  if (m != s || n != t)
    error("different length");

  switch(TYPEOF(x)) {
  case INTSXP:{
    ans = PROTECT(allocMatrix(INTSXP, m, n));
    pos = FindGroupBreaks(id); p = xlength(pos);
    cot = PROTECT(coerceVector(ot, INTSXP));
    int* iot = INTEGER(cot); // one time

    int* ipos = INTEGER(pos);
    int* ix = INTEGER(x);
    int* ians = INTEGER(ans);

    for (i = 0; i < n; ++i) {
      if (iot[i] == 1) {
        col = i * m;
        for (j = 0; j < p-1; ++j) {
          flag = 0;
          for (k = ipos[j]; k < ipos[j+1]; ++k) {
            if (flag == 0 && ix[col + k] > 0) {
              ians[col + k] = ix[col + k];
              flag = 1;
            } else {
              ians[col + k] = 0;
            }
          }
        }
      } else {
        col = i * m;
        for (j = 0; j < m; ++j) {
          ians[col + j] = ix[col + j];
        }
      }
    }
  } break;
  case REALSXP:{
    ans = PROTECT(allocMatrix(REALSXP, m, n));
    pos = PROTECT(FindGroupBreaks(id)); p = xlength(pos);
    cot = PROTECT(coerceVector(ot, INTSXP));
    int* iot = INTEGER(cot); // one time
    int* ipos = INTEGER(pos);
    double* ix = REAL(x);
    double* ians = REAL(ans);
    for (i = 0; i < n; ++i) {
      if (iot[i] == 1) {
        col = i * m;
        for (j = 0; j < p-1; ++j) {
          flag = 0;
          for (k = ipos[j]; k < ipos[j+1]; ++k) {
            if (flag == 0 && ix[col + k] > 0) {
              ians[col + k] = ix[col + k];
              flag = 1;
            } else {
              ians[col + k] = 0;
            }
          }
        }
      } else {
        col = i * m;
        for (j = 0; j < n; ++j) {
          ians[col + j] = ix[col + j];
        }
      }
    }
  } break;
  default:
    error("invalid input");
  }
  copy_dimnames(x, ans);
  UNPROTECT(3);
  return ans;
}

SEXP SetZeroNotFirstPos(SEXP x, SEXP id, SEXP ot) {
  R_xlen_t i, j, k, p, m, n, s, t, col, flag;
  SEXP pos, cot;

  i = 1, p = 0;
  m = nrows(x), n = ncols(x), s = xlength(id), t = xlength(ot);

  if (m != s || n != t)
    error("different length");

  switch(TYPEOF(x)) {
  case INTSXP:{
    pos = PROTECT(FindGroupBreaks(id)); p = xlength(pos);
    cot = coerceVector(ot, INTSXP);
    int* iot = INTEGER(cot);

    int* ix = INTEGER(x);
    int* ipos = INTEGER(pos);

    for (i = 0; i < n; ++i) {
      if (iot[i] == 1) {
        col = i * m;
        for (j = 0; j < p-1; ++j) {
          flag = 0;
          for (k = ipos[j]; k < ipos[j+1]; ++k) {
            if (flag == 0 && ix[col + k] > 0) {
              // ix[col + k] = 1;
              flag = 1;
            } else {
              ix[col + k] = 0;
            }
          }
        }
      }
    }
  } break;
  case REALSXP:{
    pos = PROTECT(FindGroupBreaks(id)); p = xlength(pos);
    cot = coerceVector(ot, INTSXP);
    int* iot = INTEGER(cot); // one time

    double* ix = REAL(x);
    int* ipos = INTEGER(pos);
    for (i = 0; i < n; ++i) {
      if (iot[i] == 1) {
        col = i * m;
        for (j = 0; j < p-1; ++j) {
          flag = 0;
          for (k = ipos[j]; k < ipos[j+1]; ++k) {
            if (flag == 0 && ix[col + k] > 0) {
              // ix[col + k] = 1;
              flag = 1;
            } else {
              ix[col + k] = 0;
            }
          }
        }
      }
    }
  } break;
  default:
    error("invalid input");
  }
  UNPROTECT(2);

  return x;
}

SEXP FillOneBeforeFirstOne(SEXP x, SEXP id) {
  R_xlen_t i, j, m, n, p, col, flag;
  SEXP ans;

  m = nrows(x), n = ncols(x), p = XLENGTH(id);

  if (m != p)
    error("different length");

  if (TYPEOF(id) != STRSXP)
    id = coerceVector(id, STRSXP);

  switch(TYPEOF(x)) {
  case INTSXP:{
    PROTECT(ans = allocMatrix(TYPEOF(x), m, n));
    int* ians = INTEGER(ans);
    int* ix = INTEGER(x);
    memset(ians, 0, m * n * sizeof(int));
    for (j = 0; j < n; ++j) {
      col = j * m;
      flag = (ix[col] > 0) ? 0 : 1;
      ians[col] = 1;
      for (i = 1; i < m; ++i) {
        int pflag = ix[col + i];
        if (strcmp(CHAR(STRING_ELT(id, i-1)), CHAR(STRING_ELT(id, i)))) {
          flag = (pflag > 0) ? 0 : 1;
          ians[col + i] = 1;
        } else {
          ians[col + i] = flag;
          flag = (pflag > 0) ? 0 : flag; // before meeting one, flag turns to zero, if not flag is one.
        }
      }
    }
  } break;
  case REALSXP:{
    PROTECT(ans = allocMatrix(TYPEOF(x), m, n));
    double* ians = REAL(ans);
    double* ix = REAL(x);
    memset(ians, 0, m * n * sizeof(double));
    for (j = 0; j < n; ++j) {
      col = j * m;
      flag = (ix[col] > 0) ? 0 : 1;
      ians[col] = 1;
      for (i = 1; i < m; ++i) {
        double pflag = ix[col + i];
        if (strcmp(CHAR(STRING_ELT(id, i-1)), CHAR(STRING_ELT(id, i)))) {
          flag = (pflag > 0) ? 0 : 1;
          ians[col + i] = 1;
        } else {
          ians[col + i] = flag;
          flag = (pflag > 0) ? 0 : flag; // before meeting one, flag turns to zero, if not flag is one.
        }
      }
    }
  } break;
  default:
    error("invalid input");
  }
  copy_dimnames(x, ans);
  UNPROTECT(1);
  return ans;
}

SEXP SetOneBeforeFirstOne(SEXP x, SEXP id) {
  R_xlen_t i, j, m, n, p, col, flag;

  m = nrows(x), n = ncols(x), p = XLENGTH(id);

  if (m != p)
    Rf_error("different length");

  if (TYPEOF(id) != STRSXP)
    id = coerceVector(id, STRSXP);

  switch(TYPEOF(x)) {
  case INTSXP:{
    PROTECT(x);
    int* ix = INTEGER(x);
    for (j = 0; j < n; ++j) {
      col = j * m;
      flag = (ix[col] > 0) ? 0 : 1;
      ix[col] = 1;
      for (i = 1; i < m; ++i) {
        int pflag = ix[col + i];
        if (strcmp(CHAR(STRING_ELT(id, i-1)), CHAR(STRING_ELT(id, i)))) {
          flag = (pflag > 0) ? 0 : 1;
          ix[col + i] = 1;
        } else {
          ix[col + i] = flag;
          flag = (pflag > 0) ? 0 : flag; // before meeting one, flag turns to zero, if not flag is one.
        }
      }
    }
  } break;
  case REALSXP:{
    PROTECT(x);
    double* ix = REAL(x);
    for (j = 0; j < n; ++j) {
      col = j * m;
      flag = (ix[col] > 0) ? 0 : 1;
      ix[col] = 1;
      for (i = 1; i < m; ++i) {
        double pflag = ix[col + i];
        if (strcmp(CHAR(STRING_ELT(id, i-1)), CHAR(STRING_ELT(id, i)))) {
          flag = (pflag > 0) ? 0 : 1;
          ix[col + i] = 1;
        } else {
          ix[col + i] = flag;
          flag = (pflag > 0) ? 0 : flag; // before meeting one, flag turns to zero, if not flag is one.
        }
      }
    }
  } break;
  default:
    error("invalid input");
  }
  UNPROTECT(1);
  return x;
}
