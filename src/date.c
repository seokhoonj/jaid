#include "jaid.h"

/* ========================================================================
 * Internal helper function
 * ======================================================================== */
// Fill an integer vector (INTSXP) with a given scalar value
static inline void fill_int_vector(SEXP x, int value) {
  if (TYPEOF(x) != INTSXP) {
    error("different input types");
  }

  R_xlen_t n = XLENGTH(x);
  int *px = INTEGER(x);

  if (value == 0) {
    // memset is faster for filling with zero
    memset(px, 0, (size_t)n * sizeof(int));
  } else {
    for (R_xlen_t i = 0; i < n; ++i) {
      px[i] = value;
    }
  }
}

/* ========================================================================
 * Expand each [from,to] into its own Date vector; return list of length n.
 * Optionally set names() of the list from `label` (character of length n).
 * ======================================================================== */
SEXP SeqDateList(SEXP from, SEXP to, SEXP label) {
  if (TYPEOF(from) != REALSXP || TYPEOF(to) != REALSXP)
    error("`from` and `to` must be of class Date (double).");
  if (XLENGTH(from) != XLENGTH(to))
    error("`from` and `to` must have the same length.");

  const R_xlen_t n  = XLENGTH(from);
  const double *pf  = REAL(from);
  const double *pt  = REAL(to);

  SEXP ans = PROTECT(allocVector(VECSXP, n));
  SEXP cls = PROTECT(mkString("Date"));

  // build one reusable empty Date vector (avoid repeated allocs)
  SEXP empty_date = PROTECT(allocVector(REALSXP, 0));
  classgets(empty_date, cls);

  for (R_xlen_t i = 0; i < n; ++i) {
    if (ISNAN(pf[i]) || ISNAN(pt[i]) || pt[i] < pf[i]) {
      SET_VECTOR_ELT(ans, i, empty_date);
      continue;
    }

    long long a = (long long)pf[i];
    long long b = (long long)pt[i];
    long long len_ll = b - a + 1LL;
    if (len_ll > (long long)R_XLEN_T_MAX)
      error("range too large at index %lld", (long long)i + 1);

    R_xlen_t len = (R_xlen_t)len_ll;
    SEXP vec = PROTECT(allocVector(REALSXP, len));
    double *pv = REAL(vec);
    for (R_xlen_t k = 0; k < len; ++k) pv[k] = (double)(a + (long long)k);
    classgets(vec, cls);
    SET_VECTOR_ELT(ans, i, vec);
    UNPROTECT(1); /* vec */
  }

  // optional names from `label`
  if (!isNull(label) && TYPEOF(label) == STRSXP && XLENGTH(label) == n) {
    namesgets(ans, label);
  }

  UNPROTECT(3); // ans, cls, empty_date
  return ans;
}

/* ==========================================================================
 * "loc" means numbers to be grouped. if the loc vector is like
 * c(1, 1, 1, 2, 2, 2, 2), we got two groups first 3 rows and second 4 rows.
 * "sub" means subtracting number of days when the interval argument is longer
 * than 0. if the two date ranges are like "2014-02-03 ~ 2014-02-04" and
 * "2014-02-12 ~ 2014-02-13" and the interval is 7, it is combined as
 * "2014-02-03 ~ 2014-02-13"
 * ==========================================================================*/
SEXP IndexOverlappingDateRanges(SEXP id, SEXP from, SEXP to, SEXP interval) {
  R_xlen_t m, n, i, j;
  SEXP loc, sub, v, z;
  if (isVectorList(id)) {
    m = XLENGTH(VECTOR_ELT(id, 0)), n = XLENGTH(id);
  } else {
    m = XLENGTH(id), n = 1;
  }

  // type of date is double
  double *pfr = REAL(from);
  double *pto = REAL(to);
  // interval is integer
  double vinterval = asReal(interval); // can be non-integer (kept as double)

  PROTECT(loc = allocVector(INTSXP, m));
  PROTECT(sub = allocVector(INTSXP, m));
  fill_int_vector(loc, 1);
  fill_int_vector(sub, 0);
  int *ploc = INTEGER(loc);
  int *psub = INTEGER(sub);

  int p = 1;       // current group id
  double mx = 0.0; // index, maximum `to`
  bool c1, c2; // condition 1, condition 2
  for (i = 1; i < m; ++i) {
    j = 0, c1 = true;
    while (j < n) {
      v = isVectorList(id) ? VECTOR_ELT(id, j) : id;

      switch(TYPEOF(v)){
      case LGLSXP:{
        const int *iv = LOGICAL(v);
        c1 = (iv[i-1] == iv[i]);
      } break;
      case INTSXP:{
        const int *iv = INTEGER(v);
        c1 = (iv[i-1] == iv[i]);
      } break;
      case REALSXP:{
        const double *iv = REAL(v);
        c1 = (iv[i-1] == iv[i]);
      } break;
      case CPLXSXP: {
        const Rcomplex *iv = COMPLEX(v);
        c1 = safe_equal_cplx(iv[i-1], iv[i]);
      } break;
      case STRSXP:{
        const SEXP *iv = STRING_PTR_RO(v);
        c1 = (!strcmp(CHAR(iv[i-1]), CHAR(iv[i])));
      } break;
      default:
        error("invalid input");
      }
      if (!c1) break;
      j++;
    }
    // update running max of 'to' within current group
    mx = (pto[i-1] > mx) ? pto[i-1] : mx;
    // c2: next 'from' starts within (mx + 1 + interval)
    c2 = pfr[i] <= (mx + 1 + vinterval);
    if (c1 && c2) {
      ploc[i] = p;
      if (pfr[i] > mx) {
        psub[i] = (int)(pfr[i] - mx - 1.0);
      }
    } else {
      ploc[i] = ++p;
      mx = pto[i]; // reset max 'to' for new group
    }
  }
  const char *names[] = {"loc", "sub", ""};
  PROTECT(z = mkNamed(VECSXP, names));
  SET_VECTOR_ELT(z, 0, loc);
  SET_VECTOR_ELT(z, 1, sub);
  UNPROTECT(3);
  return z;
}
