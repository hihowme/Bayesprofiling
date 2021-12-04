// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

// [[Rcpp::export]]
arma::colvec Arma_rowmax(const arma::mat& x) {
  return arma::max(x, 1);
}
