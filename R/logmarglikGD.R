#' logmarglikGD
#'
#' @description This function estimates the probabilities of being included in the target list
#' conditional on unobserved group S using the observed indicator X using the Metropolis-Hastings Algorithm.
#'
#'
#' @param wmc data of both aggregate data and target list (dataframe/matrix)
#' @param n_S mean of the prior distribution (vector)
#' @param loglikmc the information matrix of prior distribution (matrix)
#' @param mu0 MCMC iteration numbers (int)
#' @param inf0 burn-in iterations (int)
#' @param L Dirichlet smoothing or not (bool)
#'
#' @return A list consists of all interations of Markov Chain, the normalized markov chain,
#' probabilities of being included in the listin each chain, log-likelihood of each chain, and
#' whether this the morkov chain is rejected to move forward at each step.
#' @export
#'
#' @examples
#'

# Computing the marginal likelihood
logmarglikGD <- function(wmc, n_S, loglikmc, mu0, inf0, L){
  muab=as.matrix(Arma_colmean(wmc))
  #invVab=solve(cov(wmc), diag(n_S))
  invVab=cov(wmc)
  logimpq=rep(0,L)
  logprior=rep(0,L)
  obj1 <- function(x){
    dmvn(t(as.matrix(x)), muab, as.matrix(invVab),logd=TRUE)
  }
  prmu0=matrix(0,n_S,mu0)
  prinf0=diag(n_S)*inf0
  prinf0=solve(prinf0, diag(n_S))
  obj2 <- function(x){
    dmvn(t(as.matrix(x)), prmu0, prinf0,logd=TRUE)
  }
  logimpq=apply(wmc, 1, obj1)
  logprior=apply(wmc, 1, obj2)
  logGDmc=logimpq-logprior-loglikmc
  logGD=-(log(mean(exp(logGDmc - max(logGDmc)))) + max(logGDmc))
  return(logGD)
}
