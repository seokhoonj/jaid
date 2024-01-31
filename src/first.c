#include "jaid.h"

SEXP FillZeroNotFirstPos(SEXP x, SEXP id, SEXP ot) {
  R_xlen_t i, j, k, p, m, n, s, t, col, flag;
  SEXP pos, ans;

  i = 1, p = 0;
  m = nrows(x), n = ncols(x);
  s = XLENGTH(id), t = XLENGTH(ot);

  if (m != s || n != t)
    error(_("different length"));

  if (TYPEOF(id) != STRSXP)
    id = coerceVector(id, STRSXP);

  switch(TYPEOF(x)) {
  case INTSXP:{
    PROTECT(pos = allocVector(INTSXP, m));
    PROTECT(ans = allocMatrix(INTSXP, m, n));
    int* ix = INTEGER(x);
    int* iot = INTEGER(coerceVector(ot, INTSXP)); // one time
    int* ipos = INTEGER(pos);
    int* ians = INTEGER(ans);
    // id change point
    ipos[p++] = 0; // first 0
    for (; i < m; ++i) {
      if (strcmp(CHAR(STRING_ELT(id, i-1)), CHAR(STRING_ELT(id, i)))) {
        ipos[p++] = i;
      }
    }
    ipos[p++] = m; // last 0
    SETLENGTH(pos, p);
    // check first one by id
    ipos = INTEGER(pos);
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
    PROTECT(pos = allocVector(INTSXP, m));
    PROTECT(ans = allocMatrix(REALSXP, m, n));
    int* iot = INTEGER(coerceVector(ot, INTSXP)); // one time
    int* ipos = INTEGER(pos);
    double* ix = REAL(x);
    double* ians = REAL(ans);
    // id change point
    ipos[p++] = 0; // first 0
    for (; i < m; ++i) {
      if (strcmp(CHAR(STRING_ELT(id, i-1)), CHAR(STRING_ELT(id, i)))) {
        ipos[p++] = i;
      }
    }
    ipos[p++] = m; // last m
    SETLENGTH(pos, p);
    // check first one by id
    ipos = INTEGER(pos);
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
    error(_("invalid input"));
  }
  CopyDimNames(x, ans);
  UNPROTECT(2);
  return ans;
}

SEXP SetZeroNotFirstPos(SEXP x, SEXP id, SEXP ot) {
  R_xlen_t i, j, k, p, m, n, s, t, col, flag;
  SEXP pos;

  i = 1, p = 0;
  m = nrows(x), n = ncols(x), s = XLENGTH(id), t = XLENGTH(ot);

  if (m != s || n != t)
    error("different length");

  if (TYPEOF(id) != STRSXP)
    id = coerceVector(id, STRSXP);

  switch(TYPEOF(x)) {
  case INTSXP:{
    PROTECT(pos = allocVector(INTSXP, m));
    int* ix = INTEGER(x);
    int* iot = INTEGER(coerceVector(ot, INTSXP));
    int* ipos = INTEGER(pos);
    // id change point
    ipos[p++] = 0; // first 0
    for (; i < m; ++i) {
      if (strcmp(CHAR(STRING_ELT(id, i-1)), CHAR(STRING_ELT(id, i)))) {
        ipos[p++] = i;
      }
    }
    ipos[p++] = m; // last 0
    SETLENGTH(pos, p);
    // check first one by id
    ipos = INTEGER(pos);
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
    PROTECT(pos = allocVector(INTSXP, m));
    double* ix = REAL(x);
    int* iot = INTEGER(coerceVector(ot, INTSXP)); // one time
    int* ipos = INTEGER(pos);
    // id change point
    ipos[p++] = 0; // first 0
    for (; i < m; ++i) {
      if (strcmp(CHAR(STRING_ELT(id, i-1)), CHAR(STRING_ELT(id, i)))) {
        ipos[p++] = i;
      }
    }
    ipos[p++] = m; // last m
    SETLENGTH(pos, p);

    // check first one by id
    ipos = INTEGER(pos);
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
    error(_("invalid input"));
  }
  UNPROTECT(1);
  return x;
}

SEXP FillOneBeforeFirstOne(SEXP x, SEXP id) {
  R_xlen_t i, j, m, n, p, col, flag;
  SEXP ans;

  m = nrows(x), n = ncols(x), p = XLENGTH(id);

  if (m != p)
    error(_("different length"));

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
    error(_("invalid input"));
  }
  CopyDimNames(x, ans);
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
    error(_("invalid input"));
  }
  UNPROTECT(1);
  return x;
}
