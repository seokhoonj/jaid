// [[Rcpp::plugins(cpp11)]]

#include <Rcpp.h>
#include <unordered_map>

using namespace Rcpp;
using namespace std;

// Source
// https://stackoverflow.com/questions/55212746/rcpp-fast-statistical-mode-function-with-vector-input-of-any-type
// Author: Ralf Stubner, Joseph Wood

// Copied from DescTools packges
// https://github.com/cran/DescTools/blob/f3d8c37bbe9b1bd83aa4e9f0d851a781ff91aa8b/src/extremes.cpp

template <int RTYPE>
Vector<RTYPE> fastModeImpl(Vector<RTYPE> x, bool narm){
  if (narm) x = x[!is_na(x)];
  int myMax = 1;
  Vector<RTYPE> myMode(1);
  // special case for factors == INTSXP with "class" and "levels" attribute
  if (x.hasAttribute("levels")){
    myMode.attr("class") = x.attr("class");
    myMode.attr("levels") = x.attr("levels");
  }
  std::unordered_map<typename Rcpp::traits::storage_type<RTYPE>::type, int> modeMap;
  modeMap.reserve(x.size());

  for (std::size_t i = 0, len = x.size(); i < len; ++i) {
    auto it = modeMap.find(x[i]);

    if (it != modeMap.end()) {
      ++(it->second);
      if (it->second > myMax) {
        myMax = it->second;
        myMode[0] = x[i];
      }
    } else {
      modeMap.insert({x[i], 1});
    }
  }

  myMode.attr("freq") = myMax;

  return myMode;
}

template <>
Vector<CPLXSXP> fastModeImpl(Vector<CPLXSXP> x, bool narm) {
  stop("Not supported SEXP type!");
}


// [[Rcpp::export(name="fastMode", rng=false)]]
SEXP fastMode( SEXP x, bool narm = false ){
  RCPP_RETURN_VECTOR(fastModeImpl, x, narm);
}



template <int RTYPE>
Vector<RTYPE> fastModeImplX(Vector<RTYPE> x, bool narm){
  if (narm) x = x[!is_na(x)];
  int myMax = 1;
  std::vector<typename Rcpp::traits::storage_type<RTYPE>::type> modes;
  std::unordered_map<typename
    Rcpp::traits::storage_type<RTYPE>::type, int> modeMap;
  modeMap.reserve(x.size());

  for (std::size_t i = 0, len = x.size(); i < len; ++i) {
    auto it = modeMap.find(x[i]);

    if (it != modeMap.end()) {
      ++(it->second);
      if (it->second > myMax) {
        myMax = it->second;
        modes.clear();
        modes.push_back(x[i]);
      } else if (it->second == myMax) {
        modes.push_back(x[i]);
      }
    } else {
      modeMap.insert({x[i], 1});
    }
  }

  Rcpp::Vector<RTYPE> myMode(modes.size());
  std::copy(modes.cbegin(), modes.cend(), myMode.begin());
  // special case for factors == INTSXP with "class" and "levels" attribute
  if (x.hasAttribute("levels")){
    myMode.attr("class") = x.attr("class");
    myMode.attr("levels") = x.attr("levels");
  }
  myMode.attr("freq") = myMax;
  return myMode;
}


template <>
Vector<CPLXSXP> fastModeImplX(Vector<CPLXSXP> x, bool narm) {
  stop("Not supported SEXP type!");
}


// [[Rcpp::export(name="fastModeX", rng=false)]]
SEXP fastModeX( SEXP x, bool narm = false ){
  RCPP_RETURN_VECTOR(fastModeImplX, x, narm);
}
