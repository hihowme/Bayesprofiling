// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

// [[Rcpp::export]]
arma::rowvec Arma_colmean(const arma::mat& x) {
  return arma::mean(x, 0);
}
