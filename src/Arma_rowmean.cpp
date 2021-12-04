// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

// [[Rcpp::export]]
arma::colvec Arma_rowmean(const arma::mat& x) {
  return arma::mean(x, 1);
}
