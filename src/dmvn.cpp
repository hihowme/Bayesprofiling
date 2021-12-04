// [[Rcpp::depends("RcppArmadillo")]]
#include <RcppArmadillo.h>

// Using RcppArmadillo to code the function for density of log multinomial normal distribution (dlmvn)

static double const log2pi = std::log(2.0 * M_PI);

// [[Rcpp::export]]
arma::vec dmvn(arma::mat const &x,
               arma::rowvec const &mu,
               arma::mat const &cov,
               bool const logd = false){
  using arma::uword; //for unsigned interger value, for indexing purpose
  uword const n = x.n_rows, xdim = x.n_cols;
  arma::vec density(n);
  arma::mat const rooti = arma::inv(trimatu(arma::chol(cov)));
  double const rootisum = arma::sum(log(rooti.diag())), //determinant
    constants = -(double)xdim/2.0 * log2pi,
    other_terms = rootisum + constants;

  arma::rowvec z;
  for (uword i = 0; i < n; i++) {
    z = (x.row(i) - mu) * rooti;
    density(i) = other_terms - 0.5 * arma::dot(z, z);
  }

  if (logd)
    return density;
  return exp(density);
}
