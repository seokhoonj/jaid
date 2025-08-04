#include "jaid.h"

SEXP ReplaceVecInMat(SEXP mat, SEXP col, SEXP vec) {
  R_xlen_t i, m, p, icol;

  if (!isMatrix(mat))
    error(_("not a matrix"));

  if (!(TYPEOF(mat)==INTSXP && TYPEOF(vec)==LGLSXP) && TYPEOF(mat) != TYPEOF(vec))
    error(_("different input types"));

  m = nrows(mat), p = XLENGTH(vec);
  icol = asInteger(col);

  if (m != p)
    error(_("different length"));

  switch(TYPEOF(mat)) {
  case LGLSXP:{
    int* imat = INTEGER(mat);
    int* ivec = INTEGER(vec);
    for (i = 0; i < m; ++i) {
      imat[(icol-1)*m + i] = ivec[i];
    }
  } break;
  case INTSXP:{
    int* imat = INTEGER(mat);
    int* ivec = INTEGER(vec);
    for (i = 0; i < m; ++i) {
      imat[(icol-1)*m + i] = ivec[i];
    }
  } break;
  case REALSXP:{
    double* imat = REAL(mat);
    double* ivec = REAL(vec);
    for (i = 0; i < m; ++i) {
      imat[(icol-1)*m + i] = ivec[i];
    }
  } break;
  case STRSXP:{
    for (i = 0; i < m; ++i) {
      SET_STRING_ELT(mat, (icol-1)*m + i, STRING_ELT(vec, i));
    }
  } break;
  default:
    error(_("invalid input"));
  }
  return R_NilValue;
}

SEXP ReplaceValInMat(SEXP mat, SEXP val, SEXP refmat, SEXP refval) {
  // mat[refmat == refval] <- val (dim(mat) == dim(refmat))
  if (TYPEOF(mat) != TYPEOF(refmat))
    Rf_error("different input types");

  R_xlen_t i, j, m, n, col;
  m = nrows(mat), n = ncols(mat);

  switch(TYPEOF(mat)) {
  case LGLSXP:{
    int* imat = INTEGER(mat);
    int* irefmat = INTEGER(refmat);
    int* ival = INTEGER(val);
    int* irefval = INTEGER(refval);
    for (i = 0; i < n; ++i) {
      col = i * m;
      for (j = 0; j < m; ++j) {
        if (irefmat[col + j] == irefval[0])
          imat[col + j] = ival[0];
      }
    }
  } break;
  case INTSXP:{
    int* imat = INTEGER(mat);
    int* irefmat = INTEGER(refmat);
    int* ival = INTEGER(val);
    int* irefval = INTEGER(refval);
    for (i = 0; i < n; ++i) {
      col = i * m;
      for (j = 0; j < m; ++j) {
        if (irefmat[col + j] == irefval[0])
          imat[col + j] = ival[0];
      }
    }
  } break;
  case REALSXP:{
    double* imat = REAL(mat);
    double* irefmat = REAL(refmat);
    double* ival = REAL(val);
    double* irefval = REAL(refval);
    for (i = 0; i < n; ++i) {
      col = i * m;
      for (j = 0; j < m; ++j) {
        if (irefmat[col + j] == irefval[0])
          imat[col + j] = ival[0];
      }
    }
  } break;
  case STRSXP:{
    for (i = 0; i < n; ++i) {
      col = i * m;
      for (j = 0; j < m; ++j) {
        if (!strcmp(CHAR(STRING_ELT(refmat, col + j)), CHAR(STRING_ELT(refval, 0))))
          SET_STRING_ELT(mat, col + j, STRING_ELT(val, 0));
      }
    }
  } break;
  default:
    Rf_error("invalid input");
  }
  return R_NilValue;
}
