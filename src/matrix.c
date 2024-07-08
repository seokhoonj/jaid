#include "jaid.h"

// refer to `r-source/src/main/unique.c`
typedef size_t hlen;

#define NIL -1
#define HTDATA_INT(d) (INTEGER0((d)->HashTable))
#define HTDATA_DBL(d) (REAL0((d)->HashTable))

// Hash function and equality test for keys
typedef struct _HashData HashData;

struct _HashData {
  int K;
  hlen M;
  R_xlen_t nmax;
#ifdef LONG_VECTOR_SUPPORT
  Rboolean isLong;
#endif
  hlen (*hash)(SEXP, R_xlen_t, HashData *);
  int (*equal)(SEXP, R_xlen_t, SEXP, R_xlen_t);
  SEXP HashTable;
  int nomatch;
  Rboolean useUTF8;
  Rboolean useCache;
};

/* Integer keys are hashed via a random number generator
   based on Knuth's recommendations.  The high order K bits
   are used as the hash code.
   NB: lots of this code relies on M being a power of two and
   on silent integer overflow mod 2^32.
   <FIXME> Integer keys are wasteful for logical and raw vectors, but
   the tables are small in that case.  It would be much easier to
   implement long vectors, though. */

/* Currently the hash table is implemented as a (signed) integer
   array.  So there are two 31-bit restrictions, the length of the
   array and the values.  The values are initially NIL (-1).  O-based
   indices are inserted by isDuplicated, and invalidated by setting
   to NA_INTEGER. */

static hlen scatter(unsigned int key, HashData *d) {
  return 3141592653U * key >> (32 - d->K);
}

/* Pointer hashing as used here isn't entirely portable (we do it in
   several other places, sometimes in slightly different ways) but it
   could be made so by computing a unique value based on the
   allocation page and position in the page.
   Pointer hashes will not be valid if serialized and unserialized in
   another process.
   Hash values are int, For 64 bit pointers, we do (upper ^ lower) */
static R_INLINE hlen PTRHASH(void *x) {
  intptr_t z = (intptr_t) x;
  unsigned int z1 = (unsigned int)(z & 0xffffffff), z2 = 0;
#if SIZEOF_LONG == 8
  z2 = (unsigned int)(z/0x100000000L);
#endif
  return z1 ^ z2;
}

// Hash CHARSXP by address.
static R_INLINE hlen cshash(SEXP x, R_xlen_t indx, HashData *d) {
  return scatter(PTRHASH(STRING_ELT(x, indx)), d);
}

static R_INLINE hlen shash(SEXP x, R_xlen_t indx, HashData *d) {
  unsigned int k;
  const char *p;
  const void *vmax = vmaxget();
  if (!d->useUTF8 && d->useCache) return cshash(x, indx, d);
  /* Not having d->useCache really should not happen anymore. */
  p = translateCharUTF8(STRING_ELT(x, indx));
  k = 0;
  while (*p++)
    k = 11 * k + (unsigned int) *p; /* was 8 but 11 isn't a power of 2 */
  vmaxset(vmax); /* discard any memory used by translateChar */
  return scatter(k, d);
}

static R_INLINE int sequal(SEXP x, R_xlen_t i, SEXP y, R_xlen_t j) {
  if (i < 0 || j < 0) return 0;
  SEXP xi = STRING_ELT(x, i);
  SEXP yj = STRING_ELT(y, j);
  /* Two strings which have the same address must be the same,
   so avoid looking at the contents */
  if (xi == yj) return 1;
  /* Then if either is NA the other cannot be */
  /* Once all CHARSXPs are cached, Seql will handle this */
  if (xi == NA_STRING || yj == NA_STRING)
    return 0;
  return !strcmp(CHAR(xi), CHAR(yj));
}

/* Choose M to be the smallest power of 2
   not less than 2*n and set K = log2(M).
   Need K >= 1 and hence M >= 2, and 2^M < 2^31-1, hence n <= 2^29.

   Dec 2004: modified from 4*n to 2*n, since in the worst case we have
   a 50% full table, and that is still rather efficient -- see
   R. Sedgewick (1998) Algorithms in C++ 3rd edition p.606. */
static void MKsetup(R_xlen_t n, HashData *d, R_xlen_t nmax) {

#ifdef LONG_VECTOR_SUPPORT
  /* M = 2^32 is safe, hence n <= 2^31 -1 */
  if (n < 0) /* protect against overflow to -ve */
  error(_("length %lld is too large for hashing"), (long long int)n);
#else
  if( n < 0 || n >= 1073741824) /* protect against overflow to -ve */
  error(_("length %lld is too large for hashing"), (long long int)n);
#endif

  if (nmax != NA_INTEGER && nmax != 1) n = nmax; // n: XLENGTH(uniqueg), nmax: NA_INTEGER
  size_t n2 = 2U * (size_t) n;
  d->M = 2;
  d->K = 1;
  while (d->M < n2) {
    d->M *= 2;
    d->K++;
  }
  d->nmax = n;
}

static void HashTableSetup(SEXP x, HashData *d, R_xlen_t nmax) {

  d->hash = shash;
  d->equal = sequal;
  MKsetup(XLENGTH(x), d, nmax); // x: uniqueg, nmax: NA_INTEGER

#ifdef LONG_VECTOR_SUPPORT
  d->isLong = (Rboolean) IS_LONG_VEC(x);
  if (d->isLong) {
    d->HashTable = allocVector(REALSXP, (R_xlen_t) d->M); // d->M is usually double of n
    for (R_xlen_t i = 0; i < d->M; i++) HTDATA_DBL(d)[i] = NIL; // -1
  } else
#endif
  {
    d->HashTable = allocVector(INTSXP, (R_xlen_t) d->M);
    for (R_xlen_t i = 0; i < d->M; i++) HTDATA_INT(d)[i] = NIL;
  }
}

// Open address hashing
// Collision resolution is by linear probing
// The table is guaranteed large so this is sufficient

static int isDuplicated(SEXP x, R_xlen_t indx, HashData *d) {
#ifdef LONG_VECTOR_SUPPORT
  if (d->isLong) {
    double *h = HTDATA_DBL(d);
    hlen i = d->hash(x, indx, d); // x: unique group
    while (h[i] != NIL) {
      if (d->equal(x, (R_xlen_t) h[i], x, indx))
        return h[i] >= 0 ? 1 : 0;
      i = (i + 1) % d->M;
    }
    if (d->nmax-- < 0) error("hash table is full");
    h[i] = (double) indx;
  } else
#endif
  {
    int *h = HTDATA_INT(d);
    hlen i = d->hash(x, indx, d);
    while (h[i] != NIL) {
      if (d->equal(x, h[i], x, indx))
        return h[i] >= 0 ? 1 : 0;
      i = (i + 1) % d->M;
    }
    if (d->nmax-- < 0) error("hash table is full");
    h[i] = (int) indx;
  }
  return 0;
}

static void DoHashing(SEXP table, HashData *d) {
  R_xlen_t i, n = XLENGTH(table);
  for (i = 0; i < n; i++) {
    (void) isDuplicated(table, i, d); // table: unique group
  }
}

static int Lookup(SEXP table, SEXP x, R_xlen_t indx, HashData *d) {
  int *h = HTDATA_INT(d);
  hlen i = d->hash(x, indx, d);
  while (h[i] != NIL) {
    if (d->equal(table, h[i], x, indx)) // table: unique group, x: group
      return h[i] >= 0 ? h[i] + 1 : d->nomatch;
    i = (i + 1) % d->M;
  }
  return d->nomatch;
}

// Now do the table lookup
static SEXP HashLookup(SEXP table, SEXP x, HashData *d) {
  SEXP ans;
  R_xlen_t i, n;

  n = XLENGTH(x);
  PROTECT(ans = allocVector(INTSXP, n));
  int *pa = INTEGER0(ans);
  for (i = 0; i < n; i++) {
    pa[i] = Lookup(table, x, i, d); // table: unique group, x: group
  }
  UNPROTECT(1);
  return ans;
}

SEXP lookup(SEXP g, SEXP uniqueg) {
  SEXP matches;
  HashData data = { 0 };
  data.nomatch = 0;
  HashTableSetup(uniqueg, &data, NA_INTEGER);
  PROTECT(data.HashTable);
  DoHashing(uniqueg, &data);
  matches = HashLookup(uniqueg, g, &data);
  UNPROTECT(1);
  return matches;
}

SEXP MaxByColNames(SEXP x, SEXP g, SEXP uniqueg, SEXP snarm, SEXP minval) {
  SEXP matches, z;
  int n, p, ng, narm;
  HashData data = { 0 };
  data.nomatch = 0;

  n = LENGTH(g);
  ng = length(uniqueg);
  narm = asLogical(snarm);
  if (narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");
  if (isMatrix(x)) p = nrows(x); else p = 1;

  HashTableSetup(uniqueg, &data, NA_INTEGER);
  PROTECT(data.HashTable);
  DoHashing(uniqueg, &data);
  PROTECT(matches = HashLookup(uniqueg, g, &data));
  int *pmatches = INTEGER(matches);

  PROTECT(z = allocMatrix(TYPEOF(x), p, ng));

  switch(TYPEOF(x)){
  case INTSXP:{
    // int value = asInteger(minval);
    FillValue(z, minval);
    int* iz = INTEGER(z);
    int* ix = INTEGER(x);
    for (int i = 0; i < n; i++) {
      int colz = (pmatches[i] - 1) * p;
      int colx = i*p;
      for (int j = 0; j < p; j++) {
        iz[colz + j] = (iz[colz + j] > ix[colx + j]) ? iz[colz + j] : ix[colx + j];
      }
    }
  } break;
  case REALSXP:{
    // double value = asReal(minval);
    FillValue(z, minval);
    double* iz = REAL(z);
    double* ix = REAL(x);
    for (int i = 0; i < n; i++) {
      int colz = (pmatches[i] - 1) * p;
      int colx = i*p;
      for (int j = 0; j < p; j++) {
        iz[colz + j] = (iz[colz + j] > ix[colx + j]) ? iz[colz + j] : ix[colx + j];
      }
    }
  } break;
  default:
    error("non-numeric matrix in row_max_by_cn(): this should not happen");
  }
  if (TYPEOF(uniqueg) != STRSXP) error("row names are not character");
  SEXP dn = allocVector(VECSXP, 2), dn2, dn3;
  setAttrib(z, R_DimNamesSymbol, dn);
  SET_VECTOR_ELT(dn, 1, uniqueg);
  dn2 = getAttrib(x, R_DimNamesSymbol);
  if (length(dn2) >= 2 && !isNull(dn3 = VECTOR_ELT(dn2, 0)))
    SET_VECTOR_ELT(dn, 0, dn3);
  UNPROTECT(3); /* HashTable, matches, z */
  return z;
}

SEXP MaxByRowNames(SEXP x, SEXP g, SEXP uniqueg, SEXP snarm, SEXP minval) {
  SEXP matches, ans;
  int n, p, ng, narm;
  R_xlen_t offset = 0, offsetg = 0;
  HashData data = { 0 };
  data.nomatch = 0;

  n = LENGTH(g);
  ng = length(uniqueg);
  narm = asLogical(snarm);
  if (narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");
  if (isMatrix(x)) p = ncols(x); else p = 1;

  HashTableSetup(uniqueg, &data, NA_INTEGER);
  PROTECT(data.HashTable);
  DoHashing(uniqueg, &data);
  PROTECT(matches = HashLookup(uniqueg, g, &data));
  int *pmatches = INTEGER(matches);

  PROTECT(ans = allocMatrix(TYPEOF(x), ng, p));

  switch(TYPEOF(x)){
  case INTSXP:{
    // int value = asInteger(minval);
    FillValue(ans, minval);
    for (int i = 0; i < p; i++) {
      int *pa = INTEGER(ans);
      for(int j = 0; j < n; j++) {
        int xjpo = INTEGER_ELT(x, j + offset);
        if (!narm || xjpo != NA_INTEGER)
          pa[pmatches[j] - 1 + offsetg] = (pa[pmatches[j] - 1 + offsetg] > xjpo) ? pa[pmatches[j] - 1 + offsetg] : xjpo;
      }
      offset += n;
      offsetg += ng;
    }
  } break;
  case REALSXP:{
    // double value = asReal(minval);
    FillValue(ans, minval);
    for (int i = 0; i < p; i++) {
      double *pa = REAL(ans);
      for(int j = 0; j < n; j++) {
        double xjpo = REAL_ELT(x, j + offset);
        if (!narm || !ISNAN(xjpo))
          pa[pmatches[j] - 1 + offsetg] = (pa[pmatches[j] - 1 + offsetg] > xjpo) ? pa[pmatches[j] - 1 + offsetg] : xjpo;
      }
      offset += n;
      offsetg += ng;
    }
  } break;
  default:
    error("non-numeric matrix in row_max_by_rn(): this should not happen");
  }
  if (TYPEOF(uniqueg) != STRSXP) error("row names are not character");
  SEXP dn = allocVector(VECSXP, 2), dn2, dn3;
  setAttrib(ans, R_DimNamesSymbol, dn);
  SET_VECTOR_ELT(dn, 0, uniqueg);
  dn2 = getAttrib(x, R_DimNamesSymbol);
  if (length(dn2) >= 2 && !isNull(dn3 = VECTOR_ELT(dn2, 1)))
    SET_VECTOR_ELT(dn, 1, dn3);

  UNPROTECT(3); /* HashTable, matches, ans */
  return ans;
}

SEXP MinByColNames(SEXP x, SEXP g, SEXP uniqueg, SEXP snarm, SEXP maxval) {
  SEXP matches, z;
  int n, p, ng, narm;
  HashData data = { 0 };
  data.nomatch = 0;

  n = LENGTH(g);
  ng = length(uniqueg);
  narm = asLogical(snarm);
  if (narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");
  if (isMatrix(x)) p = nrows(x); else p = 1;

  HashTableSetup(uniqueg, &data, NA_INTEGER);
  PROTECT(data.HashTable);
  DoHashing(uniqueg, &data);
  PROTECT(matches = HashLookup(uniqueg, g, &data));
  int *pmatches = INTEGER(matches);

  PROTECT(z = allocMatrix(TYPEOF(x), p, ng));

  switch(TYPEOF(x)){
  case INTSXP:{
    // int value = asInteger(maxval);
    FillValue(z, maxval);
    int* iz = INTEGER(z);
    int* ix = INTEGER(x);
    for (int i = 0; i < n; i++) {
      int colz = (pmatches[i] - 1) * p;
      int colx = i*p;
      for (int j = 0; j < p; j++) {
        iz[colz + j] = (iz[colz + j] < ix[colx + j]) ? iz[colz + j] : ix[colx + j];
      }
    }
  } break;
  case REALSXP:{
    // double value = asReal(maxval);
    FillValue(z, maxval);
    double* iz = REAL(z);
    double* ix = REAL(x);
    for (int i = 0; i < n; i++) {
      int colz = (pmatches[i] - 1) * p;
      int colx = i*p;
      for (int j = 0; j < p; j++) {
        iz[colz + j] = (iz[colz + j] < ix[colx + j]) ? iz[colz + j] : ix[colx + j];
      }
    }
  } break;
  default:
    error("non-numeric matrix in row_min_by_cn(): this should not happen");
  }
  if (TYPEOF(uniqueg) != STRSXP) error("row names are not character");
  SEXP dn = allocVector(VECSXP, 2), dn2, dn3;
  setAttrib(z, R_DimNamesSymbol, dn);
  SET_VECTOR_ELT(dn, 1, uniqueg);
  dn2 = getAttrib(x, R_DimNamesSymbol);
  if (length(dn2) >= 2 && !isNull(dn3 = VECTOR_ELT(dn2, 0)))
    SET_VECTOR_ELT(dn, 0, dn3);
  UNPROTECT(3); /* HashTable, matches, z */
  return z;
}

SEXP MinByRowNames(SEXP x, SEXP g, SEXP uniqueg, SEXP snarm, SEXP maxval) {
  SEXP matches, ans;
  int n, p, ng, narm;
  R_xlen_t offset = 0, offsetg = 0;
  HashData data = { 0 };
  data.nomatch = 0;

  n = LENGTH(g);
  ng = length(uniqueg);
  narm = asLogical(snarm);
  if (narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");
  if (isMatrix(x)) p = ncols(x); else p = 1;

  HashTableSetup(uniqueg, &data, NA_INTEGER);
  PROTECT(data.HashTable);
  DoHashing(uniqueg, &data);
  PROTECT(matches = HashLookup(uniqueg, g, &data));
  int *pmatches = INTEGER(matches);

  PROTECT(ans = allocMatrix(TYPEOF(x), ng, p));

  switch(TYPEOF(x)){
  case INTSXP:{
    // int value = asInteger(maxval);
    FillValue(ans, maxval);
    for (int i = 0; i < p; i++) {
      int *pa = INTEGER(ans);
      for(int j = 0; j < n; j++) {
        int xjpo = INTEGER_ELT(x, j + offset);
        if (!narm || xjpo != NA_INTEGER)
          pa[pmatches[j] - 1 + offsetg] = (pa[pmatches[j] - 1 + offsetg] < xjpo) ? pa[pmatches[j] - 1 + offsetg] : xjpo;
      }
      offset += n;
      offsetg += ng;
    }
  } break;
  case REALSXP:{
    // double value = asReal(maxval);
    FillValue(ans, maxval);
    for (int i = 0; i < p; i++) {
      double *pa = REAL(ans);
      for(int j = 0; j < n; j++) {
        double xjpo = REAL_ELT(x, j + offset);
        if (!narm || !ISNAN(xjpo))
          pa[pmatches[j] - 1 + offsetg] = (pa[pmatches[j] - 1 + offsetg] < xjpo) ? pa[pmatches[j] - 1 + offsetg] : xjpo;
      }
      offset += n;
      offsetg += ng;
    }
  } break;
  default:
    error("non-numeric matrix in row_min_by_rn(): this should not happen");
  }
  if (TYPEOF(uniqueg) != STRSXP) error("row names are not character");
  SEXP dn = allocVector(VECSXP, 2), dn2, dn3;
  setAttrib(ans, R_DimNamesSymbol, dn);
  SET_VECTOR_ELT(dn, 0, uniqueg);
  dn2 = getAttrib(x, R_DimNamesSymbol);
  if (length(dn2) >= 2 && !isNull(dn3 = VECTOR_ELT(dn2, 1)))
    SET_VECTOR_ELT(dn, 1, dn3);

  UNPROTECT(3); /* HashTable, matches, ans */
  return ans;
}

SEXP SumByColNames(SEXP x, SEXP g, SEXP uniqueg, SEXP snarm) {
  SEXP matches, z;
  int n, p, ng, narm;
  HashData data = { 0 };
  data.nomatch = 0;

  n = LENGTH(g);
  ng = length(uniqueg);
  narm = asLogical(snarm);
  if (narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");
  if (isMatrix(x)) p = nrows(x); else p = 1;

  HashTableSetup(uniqueg, &data, NA_INTEGER);
  PROTECT(data.HashTable);
  DoHashing(uniqueg, &data);
  PROTECT(matches = HashLookup(uniqueg, g, &data));
  int *pmatches = INTEGER(matches);

  PROTECT(z = allocMatrix(TYPEOF(x), p, ng));

  switch(TYPEOF(x)){
  case INTSXP:{
    Memzero(INTEGER(z), p*ng);
    int* iz = INTEGER(z);
    int* ix = INTEGER(x);
    for (int i = 0; i < n; i++) {
      int colz = (pmatches[i] - 1) * p;
      int colx = i*p;
      for (int j = 0; j < p; j++) {
        iz[colz + j] += ix[colx + j];
      }
    }
  } break;
  case REALSXP:{
    Memzero(REAL(z), p*ng);
    double* iz = REAL(z);
    double* ix = REAL(x);
    for (int i = 0; i < n; i++) {
      int colz = (pmatches[i] - 1) * p;
      int colx = i*p;
      for (int j = 0; j < p; j++) {
        iz[colz + j] += ix[colx + j];
      }
    }
  } break;
  default:
    error("non-numeric matrix in row_sum_by_cn(): this should not happen");
  }
  if (TYPEOF(uniqueg) != STRSXP) error("row names are not character");
  SEXP dn = allocVector(VECSXP, 2), dn2, dn3;
  setAttrib(z, R_DimNamesSymbol, dn);
  SET_VECTOR_ELT(dn, 1, uniqueg);
  dn2 = getAttrib(x, R_DimNamesSymbol);
  if (length(dn2) >= 2 && !isNull(dn3 = VECTOR_ELT(dn2, 0)))
    SET_VECTOR_ELT(dn, 0, dn3);
  UNPROTECT(3); /* HashTable, matches, z */
  return z;
}

SEXP SumByRowNames(SEXP x, SEXP g, SEXP uniqueg, SEXP snarm) {
  SEXP matches,ans;
  int n, p, ng, narm;
  R_xlen_t offset = 0, offsetg = 0;
  HashData data = { 0 };
  data.nomatch = 0;

  n = LENGTH(g);
  ng = length(uniqueg);
  narm = asLogical(snarm);
  if(narm == NA_LOGICAL) error("'na.rm' must be TRUE or FALSE");
  if(isMatrix(x)) p = ncols(x); else p = 1;

  HashTableSetup(uniqueg, &data, NA_INTEGER);
  PROTECT(data.HashTable);
  DoHashing(uniqueg, &data);
  PROTECT(matches = HashLookup(uniqueg, g, &data));
  int *pmatches = INTEGER(matches);

  PROTECT(ans = allocMatrix(TYPEOF(x), ng, p));

  switch(TYPEOF(x)){
  case INTSXP:{
    Memzero(INTEGER(ans), ng*p);
    for(int i = 0; i < p; i++) {
      int *pa = INTEGER(ans);
      for(int j = 0; j < n; j++) {
        int xjpo = INTEGER_ELT(x, j + offset);
        if (xjpo == NA_INTEGER) {
          if(!narm)
            pa[pmatches[j] - 1 + offsetg] = NA_INTEGER;
        } else if (pa[pmatches[j] - 1 + offsetg] != NA_INTEGER) {
          /* check for integer overflows */
          int itmp = pa[pmatches[j] - 1 + offsetg];
          double dtmp = itmp;
          dtmp += xjpo;
          if (dtmp < INT_MIN || dtmp > INT_MAX) itmp = NA_INTEGER;
          else itmp += xjpo;
          pa[pmatches[j] - 1 + offsetg] = itmp;
        }
      }
      offset += n;
      offsetg += ng;
    }
  } break;
  case REALSXP:{
    Memzero(REAL(ans), ng*p);
    for(int i = 0; i < p; i++) {
      double *pa = REAL(ans);
      for(int j = 0; j < n; j++) {
        double xjpo = REAL_ELT(x, j + offset);
        if(!narm || !ISNAN(xjpo))
          pa[pmatches[j] - 1 + offsetg] += xjpo;
      }
      offset += n;
      offsetg += ng;
    }
  } break;
  default:
    error("non-numeric matrix in row_sum_by_rn(): this should not happen");
  }
  if (TYPEOF(uniqueg) != STRSXP) error("row names are not character");
  SEXP dn = allocVector(VECSXP, 2), dn2, dn3;
  setAttrib(ans, R_DimNamesSymbol, dn);
  SET_VECTOR_ELT(dn, 0, uniqueg);
  dn2 = getAttrib(x, R_DimNamesSymbol);
  if(length(dn2) >= 2 &&
     !isNull(dn3 = VECTOR_ELT(dn2, 1))) SET_VECTOR_ELT(dn, 1, dn3);
     UNPROTECT(3); /* HashTable, matches, ans */
          return ans;
}

SEXP RowMax(SEXP x) {
  R_xlen_t m, n;
  SEXP z;

  m = nrows(x), n = ncols(x);

  switch(TYPEOF(x)){
  case INTSXP:{
    PROTECT(z = allocVector(INTSXP, m));
    int *xs = INTEGER(x), *xe = xs + m * n;
    int *zs = INTEGER(z), *ze = zs + m;
    int *xi, *zi;
    for (zi = zs, xi = xs; zi != ze; ++zi, ++xi)
      *zi = *xi;
    xs = xi;
    for (;xi != xe;) {
      for (zi = zs, xi = xs, xs += m; xi != xs; ++zi, ++xi) {
        *zi = (*zi > *xi) ? *zi : *xi;
      }
    }
  } break;
  case REALSXP:{
    PROTECT(z = allocVector(REALSXP, m));
    double *xs = REAL(x), *xe = xs + m * n;
    double *zs = REAL(z), *ze = zs + m;
    double *xi, *zi;
    for (zi = zs, xi = xs; zi != ze; ++zi, ++xi)
      *zi = *xi;
    xs = xi;
    for (;xi != xe;) {
      for (zi = zs, xi = xs, xs += m; xi != xs; ++zi, ++xi) {
        *zi = (*zi > *xi) ? *zi : *xi;
      }
    }
  } break;
  default:
    error(_("invalid input"));
  }
  UNPROTECT(1);
  return z;
}

SEXP RowMin(SEXP x) {
  R_xlen_t m, n;
  SEXP z;

  m = nrows(x), n = ncols(x);

  switch(TYPEOF(x)){
  case INTSXP:{
    PROTECT(z = allocVector(INTSXP, m));
    int *xs = INTEGER(x), *xe = xs + m * n;
    int *zs = INTEGER(z), *ze = zs + m;
    int *xi, *zi;
    for (zi = zs, xi = xs; zi != ze; ++zi, ++xi)
      *zi = *xi;
    xs = xi;
    for (;xi != xe;) {
      for (zi = zs, xi = xs, xs += m; xi != xs; ++zi, ++xi) {
        *zi = (*zi < *xi) ? *zi : *xi;
      }
    }
  } break;
  case REALSXP:{
    PROTECT(z = allocVector(REALSXP, m));
    double *xs = REAL(x), *xe = xs + m * n;
    double *zs = REAL(z), *ze = zs + m;
    double *xi, *zi;
    for (zi = zs, xi = xs; zi != ze; ++zi, ++xi)
      *zi = *xi;
    xs = xi;
    for (;xi != xe;) {
      for (zi = zs, xi = xs, xs += m; xi != xs; ++zi, ++xi) {
        *zi = (*zi < *xi) ? *zi : *xi;
      }
    }
  } break;
  default:
    error(_("invalid input"));
  }
  UNPROTECT(1);
  return z;
}

SEXP RowSum(SEXP x) {
  R_xlen_t m, n;
  SEXP z;

  m = nrows(x), n = ncols(x);

  switch(TYPEOF(x)){
  case INTSXP:{
    PROTECT(z = allocVector(INTSXP, m));
    int *xs = INTEGER(x), *xe = xs + m * n;
    int *zs = INTEGER(z), *ze = zs + m;
    int *xi, *zi;
    for (zi = zs, xi = xs; zi != ze; ++zi, ++xi)
      *zi = *xi;
    xs = xi;
    for (;xi != xe;) {
      for (zi = zs, xi = xs, xs += m; xi != xs; ++zi, ++xi) {
        *zi = (*zi != NA_INTEGER) ? *zi + *xi : NA_INTEGER;
      }
    }
  } break;
  case REALSXP:{
    PROTECT(z = allocVector(REALSXP, m));
    double *xs = REAL(x), *xe = xs + m * n;
    double *zs = REAL(z), *ze = zs + m;
    double *xi, *zi;
    for (zi = zs, xi = xs; zi != ze; ++zi, ++xi)
      *zi = *xi;
    xs = xi;
    for (;xi != xe;) {
      for (zi = zs, xi = xs, xs += m; xi != xs; ++zi, ++xi) {
        *zi = (*zi != NA_REAL) ? *zi + *xi : NA_REAL;
      }
    }
  } break;
  default:
    error(_("invalid input"));
  }
  UNPROTECT(1);
  return z;
}

SEXP ColMax(SEXP x) {
  R_xlen_t m, n;
  SEXP z;

  m = nrows(x), n = ncols(x);

  switch(TYPEOF(x)){
  case INTSXP:{
    PROTECT(z = allocVector(INTSXP, m));
    int *xs = INTEGER(x);
    int *zs = INTEGER(z), *ze = zs + n;
    int *xi, *zi;
    for (zi = zs, xi = xs; zi != ze; ++zi, xi += m)
      *zi = *xi;
    ++xs;
    for (zi = zs; zi != ze; ++zi) {
      for (xi = xs, xs += m; xi != (xs - 1); ++xi) {
        *zi = (*zi > *xi) ? *zi : *xi;
      }
    }
  } break;
  case REALSXP:{
    PROTECT(z = allocVector(REALSXP, n));
    double *xs = REAL(x);
    double *zs = REAL(z), *ze = zs + n;
    double *xi, *zi;
    for (zi = zs, xi = xs; zi != ze; ++zi, xi += m)
      *zi = *xi;
    ++xs;
    for (zi = zs; zi != ze; ++zi) {
      for (xi = xs, xs += m; xi != (xs - 1); ++xi) {
        *zi = (*zi > *xi) ? *zi : *xi;
      }
    }
  } break;
  default:
    error(_("invalid input"));
  }
  UNPROTECT(1);
  return z;
}

SEXP ColMin(SEXP x) {
  R_xlen_t m, n;
  SEXP z;

  m = nrows(x), n = ncols(x);

  switch(TYPEOF(x)){
  case INTSXP:{
    PROTECT(z = allocVector(INTSXP, m));
    int *xs = INTEGER(x);
    int *zs = INTEGER(z), *ze = zs + n;
    int *xi, *zi;
    for (zi = zs, xi = xs; zi != ze; ++zi, xi += m)
      *zi = *xi;
    ++xs;
    for (zi = zs; zi != ze; ++zi) {
      for (xi = xs, xs += m; xi != (xs - 1); ++xi) {
        *zi = (*zi < *xi) ? *zi : *xi;
      }
    }
  } break;
  case REALSXP:{
    PROTECT(z = allocVector(REALSXP, n));
    double *xs = REAL(x);
    double *zs = REAL(z), *ze = zs + n;
    double *xi, *zi;
    for (zi = zs, xi = xs; zi != ze; ++zi, xi += m)
      *zi = *xi;
    ++xs;
    for (zi = zs; zi != ze; ++zi) {
      for (xi = xs, xs += m; xi != (xs - 1); ++xi) {
        *zi = (*zi < *xi) ? *zi : *xi;
      }
    }
  } break;
  default:
    error(_("invalid input"));
  }
  UNPROTECT(1);
  return z;
}

SEXP ColSum(SEXP x) {
  R_xlen_t m, n;
  SEXP z;

  m = nrows(x), n = ncols(x);

  switch(TYPEOF(x)){
  case INTSXP:{
    PROTECT(z = allocVector(INTSXP, n));
    int *xs = INTEGER(x);
    int *zs = INTEGER(z), *ze = zs + n;
    int *xi, *zi;
    for (zi = zs, xi = xs; zi != ze; ++zi, xi += m)
      *zi = *xi;
    ++xs;
    for (zi = zs; zi != ze; ++zi) {
      for (xi = xs, xs += m; xi != (xs - 1); ++xi) {
        *zi = (*zi != NA_INTEGER) ? *zi + *xi : NA_INTEGER;
      }
    }
  } break;
  case REALSXP:{
    PROTECT(z = allocVector(REALSXP, n));
    double *xs = REAL(x);
    double *zs = REAL(z), *ze = zs + n;
    double *xi, *zi;
    for (zi = zs, xi = xs; zi != ze; ++zi, xi += m)
      *zi = *xi;
    ++xs;
    for (zi = zs; zi != ze; ++zi) {
      for (xi = xs, xs += m; xi != (xs - 1); ++xi) {
        *zi = (*zi != NA_INTEGER) ? *zi + *xi : NA_INTEGER;
      }
    }
  } break;
  default:
    error(_("invalid input"));
  }
  UNPROTECT(1);
  return z;
}

SEXP Rotate(SEXP x, SEXP angle) {
  R_xlen_t i, j, m, n, p, degree;
  SEXP z;

  m = nrows(x), n = ncols(x), p = XLENGTH(x);
  degree = asInteger(angle);

  if (degree % 360 == 0)
    return x;

  switch(TYPEOF(x)){
    case LGLSXP:{
      if (degree % 360 == 90) {
        PROTECT(z = allocMatrix(LGLSXP, n, m));
        int* iz = INTEGER(z);
        int* ix = INTEGER(x);
        for (i = 0, j = n-1; i < p; ++i, j += m) {
          iz[i] = ix[j];
          if (j > p-m) j -= (p+1); // add j += m right after this process
        }
      } else if (degree % 360 == 180) {
        PROTECT(z = allocMatrix(LGLSXP, m, n));
        int* iz = INTEGER(z);
        int* ix = INTEGER(x);
        for (i = 0, j = p-1; i < p; ++i, --j) {
          iz[i] = ix[j];
        }
      } else if (degree % 360 == 270) {
        PROTECT(z = allocMatrix(LGLSXP, n, m));
        int* iz = INTEGER(z);
        int* ix = INTEGER(x);
        for (i = 0, j = p-m; i < p; ++i, j -= m) {
          if (j < 0) j += (p+1);
          iz[i] = ix[j];
        }
      } else {
        error("invalid degree");
      }
      break;
    }
    case INTSXP:{
      if (degree % 360 == 90) {
        PROTECT(z = allocMatrix(INTSXP, n, m));
        int* iz = INTEGER(z);
        int* ix = INTEGER(x);
        for (i = 0, j = m-1; i < p; ++i, j += m) {
          iz[i] = ix[j];
          if (j > p-m) j -= (p+1); // add j += m right after this process
        }
      } else if (degree % 360 == 180) {
        PROTECT(z = allocMatrix(INTSXP, m, n));
        int* iz = INTEGER(z);
        int* ix = INTEGER(x);
        for (i = 0, j = p-1; i < p; ++i, --j) {
          iz[i] = ix[j];
        }
      } else if (degree % 360 == 270) {
        PROTECT(z = allocMatrix(INTSXP, n, m));
        int* iz = INTEGER(z);
        int* ix = INTEGER(x);
        for (i = 0, j = p-m; i < p; ++i, j -= m) {
          if (j < 0) j += (p+1);
          iz[i] = ix[j];
        }
      } else {
        error("invalid degree");
      }
      break;
    }
    case REALSXP:{
      if (degree % 360 == 90) {
        PROTECT(z = allocMatrix(REALSXP, n, m));
        double* iz = REAL(z);
        double* ix = REAL(x);
        for (i = 0, j = m-1; i < p; ++i, j += m) {
          iz[i] = ix[j];
          if (j > p-m) j -= (p+1); // add j += m right after this process
        }
      } else if (degree % 360 == 180) {
        PROTECT(z = allocMatrix(REALSXP, m, n));
        double* iz = REAL(z);
        double* ix = REAL(x);
        for (i = 0, j = p-1; i < p; ++i, --j) {
          iz[i] = ix[j];
        }
        setAttrib(z, R_DimNamesSymbol, getAttrib(x, R_DimNamesSymbol));
      } else if (degree % 360 == 270) {
        PROTECT(z = allocMatrix(REALSXP, n, m));
        double* iz = REAL(z);
        double* ix = REAL(x);
        for (i = 0, j = p-m; i < p; ++i, j -= m) {
          if (j < 0) j += (p+1);
          iz[i] = ix[j];
        }
      } else {
        error("invalid degree");
      }
      break;
    }
    case STRSXP:{
      if (degree % 360 == 90) {
        PROTECT(z = allocMatrix(STRSXP, n, m));
        for (i = 0, j = m-1; i < p; ++i, j += m) {
          SET_STRING_ELT(z, i, STRING_ELT(x, j));
          if (j > p-m) j -= (p+1); // add j += m right after this process
        }
      } else if (degree % 360 == 180) {
        PROTECT(z = allocMatrix(STRSXP, m, n));
        for (i = 0, j = p-1; i < p; ++i, --j) {
          SET_STRING_ELT(z, i, STRING_ELT(x, j));
        }
      } else if (degree % 360 == 270) {
        PROTECT(z = allocMatrix(STRSXP, n, m));
        for (i = 0, j = p-m; i < p; ++i, j -= m) {
          if (j < 0) j += (p+1);
          SET_STRING_ELT(z, i, STRING_ELT(x, j));
        }
      } else {
        error("invalid degree");
      }
      break;
    }
    default:{
      error("invalid input");
    }
  }
  UNPROTECT(1);
  return z;
}

SEXP MatXMat(SEXP x, SEXP y) {
  if (!isMatrix(x) || !isMatrix(y))
    error(_("not a matrix"));

  if (TYPEOF(x) != TYPEOF(y))
    error(_("different input types"));

  R_xlen_t i, j, m, n, o, p, col;
  m = nrows(x), n = ncols(x), o = nrows(y), p = ncols(y);

  if (m != o || n != p)
    error(_("different length"));
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
    error(_("invalid input"));
  }
  UNPROTECT(2);
  return R_NilValue;
}

SEXP MatXRow(SEXP x, SEXP y) {
  if (!isMatrix(x))
    error(_("not a matrix"));

  if (TYPEOF(x) != TYPEOF(y))
    error(_("different input types"));

  R_xlen_t i, j, m, n, p, col;
  m = nrows(x), n = ncols(x), p = XLENGTH(y);

  if (n != p)
    error(_("different length"));

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
    error(_("invalid input"));
  }
  UNPROTECT(2);
  return R_NilValue;
}

SEXP MatXCol(SEXP x, SEXP y) {
  if (!isMatrix(x))
    error(_("not a matrix"));

  if (TYPEOF(x) != TYPEOF(y))
    error(_("different input types"));

  R_xlen_t i, j, m, n, o, col;
  m = nrows(x), n = ncols(x), o = XLENGTH(y);

  if (m != o)
    error(_("different length"));

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
    error(_("invalid input"));
  }
  UNPROTECT(2);
  return R_NilValue;
}

SEXP MatXNum(SEXP x, SEXP y) {
  if (!isMatrix(x))
    error(_("not a matrix"));

  if (TYPEOF(x) != TYPEOF(y))
    error(_("different input types"));

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
    error(_("invalid length"));
  }
  UNPROTECT(1);
  return R_NilValue;
}
