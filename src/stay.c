#include "jaid.h"

/* =========================================================================
 * CountStay
 *
 * Purpose:
 *   For each ID group, count the number of unique days covered by the union
 *   of all [from, to] ranges. Overlaps are merged; gaps are excluded.
 *
 * Arguments:
 *   id   : grouping keys (vector or list). FindGroupBreaks(id) is assumed to
 *          return boundary indices for contiguous groups.
 *   from : Date vector (REALSXP; days since 1970-01-01).
 *   to   : Date vector (REALSXP; same length as from).
 *
 * Returns:
 *   Integer vector of length = number of groups. Each element is the total
 *   unique days covered by the ranges of that group.
 *
 * Notes:
 *   - Requires that (id, from, to) are already ordered so that within each
 *     group, `from` is non-decreasing. Then merging can be done in O(n).
 *   - If NA or invalid ranges (to < from) occur, they are skipped.
 *   - Large results are clamped to INT_MAX / INT_MIN.
 *   - If group order is not sorted by `from`, copy and sort per group first.
 * ========================================================================= */
SEXP CountStay(SEXP id, SEXP from, SEXP to) {
  // Argument checks
  if (TYPEOF(from) != REALSXP || TYPEOF(to) != REALSXP)
    error("`from` and `to` must be of class Date.");
  if (XLENGTH(from) != XLENGTH(to))
    error("`from` and `to` must have the same length.");

  const R_xlen_t n = XLENGTH(from);
  const double *pf = REAL(from);
  const double *pt = REAL(to);

  // Group boundaries
  SEXP pos = PROTECT(FindGroupBreaks(id));
  if (TYPEOF(pos) != INTSXP) {
    UNPROTECT(1);
    error("FindGroupBreaks() must return an integer vector of cut positions.");
  }
  const int *pp = INTEGER(pos);
  const R_xlen_t nb = XLENGTH(pos);   // number of boundaries = groups + 1
  if (nb < 2) {
    SEXP ans0 = PROTECT(allocVector(INTSXP, 0));
    UNPROTECT(2);
    return ans0;
  }
  const R_xlen_t ng = nb - 1;

  // Result vector
  SEXP stay = PROTECT(allocVector(INTSXP, ng));
  int *ps = INTEGER(stay);

  // Process each group
  for (R_xlen_t g = 0; g < ng; ++g) {
    R_xlen_t start = (R_xlen_t)pp[g];
    R_xlen_t end   = (R_xlen_t)pp[g+1];
    if (start < 0 || end < start || end > n) {
      UNPROTECT(2);
      error("Invalid group boundary from FindGroupBreak().");
    }

    long long total = 0;        // total days
    long long cur_s = 0, cur_e = -1;
    int have = 0;               // flag if current merged interval exists

    for (R_xlen_t j = start; j < end; ++j) {
      double a_d = pf[j], b_d = pt[j];
      if (ISNAN(a_d) || ISNAN(b_d)) continue;
      long long s = (long long)a_d;
      long long e = (long long)b_d;
      if (e < s) continue;

      if (!have) {
        // start first interval
        cur_s = s; cur_e = e; have = 1;
      } else {
        // merge if overlapping or adjacent, else close interval
        if (s <= cur_e + 1LL) {
          if (e > cur_e) cur_e = e;
        } else {
          total += (cur_e - cur_s + 1LL);
          cur_s = s; cur_e = e;
        }
      }
    }
    if (have) total += (cur_e - cur_s + 1LL);

    // clamp to int range
    if (total > (long long)INT_MAX) ps[g] = INT_MAX;
    else if (total < (long long)INT_MIN) ps[g] = INT_MIN;
    else ps[g] = (int)total;
  }

  UNPROTECT(2); // pos, stay
  return stay;
}

/* =========================================================================
 * LimitStay(x, gsize, limit, waiting)
 *
 * Apply per-group "stay" limits with a mandatory waiting (cooldown) period.
 *
 * Inputs
 *   x      : INTSXP or REALSXP, length n. Interpreted as binary:
 *            - integer: 1 if != 0 and != NA, else 0
 *            - double : 1 if != 0.0 and not NaN, else 0
 *   gsize  : INTSXP or REALSXP, group sizes whose sum must equal n.
 *            (double sizes are rounded via llround)
 *   limit  : integer(1) >= 0, max number of 1's allowed consecutively per group
 *            window
 *   waiting: integer(1) >= 0, number of forced 0's after reaching the limit
 *
 * Behavior
 *   For each group segment in order:
 *     - Maintain counters (sum = number of emitted 1's so far, gap = forced-0's so far).
 *     - While sum < limit: pass through input bit; sum += bit; if bit==1 then reset gap=0.
 *     - Once sum == limit: emit 0's until gap reaches waiting (gap++ each step).
 *     - After waiting is satisfied: reset (sum=0, gap=0) and resume.
 *
 * Returns
 *   Integer vector (0/1) of length n.
 * ========================================================================= */
SEXP LimitStay(SEXP x, SEXP gsize, SEXP limit, SEXP waiting) {
  // limit = 90; waiting = 90
  // ex) x = rep(1, 100)
  // ex) p = c(30, 40, 30) # split
  // ex) count_limit_and_waiting(x, p, 3, 6)
  // waiting periods == elimination periods == qualifying periods
  // printf("%d\n", isInteger(x));

  // validate scalar args
  const int lim  = asInteger(limit);
  const int wait = asInteger(waiting);
  if (lim  < 0) error("`limit` must be non-negative");
  if (wait < 0) error("`waiting` must be non-negative");

  // validate x
  const SEXPTYPE xt = TYPEOF(x);
  if (!(xt == INTSXP || xt == REALSXP))
    error("`x` must be integer or double");
  const R_xlen_t n = XLENGTH(x);

  // normalize/validate gsize and compute total
  const SEXPTYPE tg = TYPEOF(gsize);
  if (!(tg == INTSXP || tg == REALSXP))
    error("`gsize` must be integer or double");

  R_xlen_t ng = XLENGTH(gsize);
  R_xlen_t total = 0;

  if (tg == INTSXP) {
    const int *pg = INTEGER(gsize);
    for (R_xlen_t i = 0; i < ng; ++i) {
      if (pg[i] < 0) error("`gsize` must be non-negative");
      total += (R_xlen_t)pg[i];
    }
  } else { // REALSXP
  const double *pg = REAL(gsize);
    for (R_xlen_t i = 0; i < ng; ++i) {
      if (ISNAN(pg[i]) || pg[i] < 0.0) error("`gsize` must be non-negative");
      total += (R_xlen_t) llround(pg[i]);
    }
  }
  if (total != n)
    error("sum(`gsize`) must equal length(`x`)");

  // allocate result (always integer 0/1)
  SEXP ans = PROTECT(allocVector(INTSXP, n));
  int *pa = INTEGER(ans);

  // main loop over groups
  R_xlen_t off = 0;  // current offset into x / ans

  if (tg == INTSXP) {
    const int *pg = INTEGER(gsize);

    switch (xt) {

    case INTSXP: {
      const int *px = INTEGER(x);
      for (R_xlen_t g = 0; g < ng; ++g) {
        const R_xlen_t len = (R_xlen_t)pg[g];
        int sum = 0, gap = 0;

        for (R_xlen_t t = 0; t < len; ++t, ++off) {
          // interpret input as binary
          const int v   = px[off];
          const int bit = (v == NA_INTEGER) ? 0 : (v != 0);

          if (sum < lim) {
            pa[off] = bit;
            sum += bit;
            if (bit) gap = 0;
          } else if (gap < wait) {
            pa[off] = 0;
            gap += 1;
          } else {
            // reset after cooldown window
            sum = 0;
            gap = 0;
            pa[off] = bit;
            sum += bit;
            if (bit) gap = 0;
          }
        }
      }
    } break;

    case REALSXP: {
      const double *px = REAL(x);
      for (R_xlen_t g = 0; g < ng; ++g) {
        const R_xlen_t len = (R_xlen_t)pg[g];
        int sum = 0, gap = 0;

        for (R_xlen_t t = 0; t < len; ++t, ++off) {
          // interpret input as binary
          const double xv = px[off];
          const int bit = (ISNAN(xv) ? 0 : (xv != 0.0));

          if (sum < lim) {
            pa[off] = bit;
            sum += bit;
            if (bit) gap = 0;
          } else if (gap < wait) {
            pa[off] = 0;
            gap += 1;
          } else {
            sum = 0;
            gap = 0;
            pa[off] = bit;
            sum += bit;
            if (bit) gap = 0;
          }
        }
      }
    } break;

    default:
      UNPROTECT(1);
    error("unreachable");
    }

  } else { // tg == REALSXP
          const double *pg = REAL(gsize);

    switch (xt) {

    case INTSXP: {
      const int *px = INTEGER(x);
      for (R_xlen_t g = 0; g < ng; ++g) {
        const R_xlen_t len = (R_xlen_t) llround(pg[g]);
        int sum = 0, gap = 0;

        for (R_xlen_t t = 0; t < len; ++t, ++off) {
          const int v   = px[off];
          const int bit = (v == NA_INTEGER) ? 0 : (v != 0);

          if (sum < lim) {
            pa[off] = bit;
            sum += bit;
            if (bit) gap = 0;
          } else if (gap < wait) {
            pa[off] = 0;
            gap += 1;
          } else {
            sum = 0;
            gap = 0;
            pa[off] = bit;
            sum += bit;
            if (bit) gap = 0;
          }
        }
      }
    } break;

    case REALSXP: {
      const double *px = REAL(x);
      for (R_xlen_t g = 0; g < ng; ++g) {
        const R_xlen_t len = (R_xlen_t) llround(pg[g]);
        int sum = 0, gap = 0;

        for (R_xlen_t t = 0; t < len; ++t, ++off) {
          const double xv = px[off];
          const int bit = (ISNAN(xv) ? 0 : (xv != 0.0));

          if (sum < lim) {
            pa[off] = bit;
            sum += bit;
            if (bit) gap = 0;
          } else if (gap < wait) {
            pa[off] = 0;
            gap += 1;
          } else {
            sum = 0;
            gap = 0;
            pa[off] = bit;
            sum += bit;
            if (bit) gap = 0;
          }
        }
      }
    } break;

    default:
      UNPROTECT(1);
    error("unreachable");
    }
  }

  UNPROTECT(1);
  return ans;
}

/* Validate CountStay function
SEXP ValidCountStay(SEXP id, SEXP from, SEXP to) {
  SEXP pos, stay;
  PROTECT(pos = FindGroupBreaks(id));
  int nrow = XLENGTH(pos);
  int *pp = INTEGER(pos);
  int *pf = INTEGER(from);
  int *pt = INTEGER(to);

  PROTECT(stay = allocVector(INTSXP, nrow-1));
  int *ps = INTEGER(stay);
  int p = 0;
  int *days = (int*)calloc(100000, sizeof(int));
  int *day = (int*)calloc(10000, sizeof(int));
  for (int k = 0; k < nrow-1; ++k) {
    int q = 0;
    for (int j = pp[k]; j < pp[k+1]; ++j) {
      int m = pt[j] - pf[j] + 1;
      day[0] = pf[j];
      days[q++] = day[0];
      for (int i = 1; i < m; ++i) {
        day[i] = day[i-1] + 1;
        days[q++] = day[i];
      }
    }
    ps[p++] = length(iunique(days, q));
  }
  free(day); free(days);
  UNPROTECT(2);
  return stay;
}
*/
