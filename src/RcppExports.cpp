// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// fastMode
SEXP fastMode(SEXP x, bool narm);
RcppExport SEXP _jaid_fastMode(SEXP xSEXP, SEXP narmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type narm(narmSEXP);
    rcpp_result_gen = Rcpp::wrap(fastMode(x, narm));
    return rcpp_result_gen;
END_RCPP
}
// fastModeX
SEXP fastModeX(SEXP x, bool narm);
RcppExport SEXP _jaid_fastModeX(SEXP xSEXP, SEXP narmSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::traits::input_parameter< SEXP >::type x(xSEXP);
    Rcpp::traits::input_parameter< bool >::type narm(narmSEXP);
    rcpp_result_gen = Rcpp::wrap(fastModeX(x, narm));
    return rcpp_result_gen;
END_RCPP
}