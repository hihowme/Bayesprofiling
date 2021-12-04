// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

// [[Rcpp::export]]
arma::colvec Arma_rowSums(const arma::mat& x) {
  return arma::sum(x, 1);
}
