#' Bayesian Profiling using the Hamiltonian Monte Carlo Algorithm
#'
#' @description This function estimates the probabilities of being included in the target list
#' conditional on unobserved group S using the observed indicator X using the Hamiltonian Monte Carlo Algorithm.
#'
#' @param data data of both aggregate data and target list (dataframe/matrix)
#' @param prmu0 mean of the prior distribution (vector)
#' @param prinf0 the information matrix of prior distribution (matrix)
#' @param M MCMC iteration numbers (int)
#' @param chains burn-in iterations (int)
#'
#' @return  the fitted result as an instance of stanfit
#' @export
#'
#' @examples
#'
#'
#'
#'
#'

Bayesprofile_HMC <- function(data, prmu0, prinf0, M, chains){

  # get the data distribution from the data we have
  dist = data_dist(data)

  # get data ready for stan
  data_stan <- list(
    ncatX = dist$n_X,
    ncatS = dist$n_S,
    log_pXcondS = dist$log_pXcondS,
    mu = prmu0,
    sigma = prinf0,
    y = dist$target
  )

  # run the stan code: change the directory to your github repository
  fit <- stan("R_code/Bayes_HMC/election_model.stan",
              data = data_stan,
              chains = chains,
              iter = M)

  return(fit)
}
