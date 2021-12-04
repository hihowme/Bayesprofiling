// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

// [[Rcpp::export]]
arma::rowvec Arma_colmax(const arma::mat& x) {
  return arma::max(x, 0);
}
