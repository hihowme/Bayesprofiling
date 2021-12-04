#' Bayesian Profiling using the Metropolis-Hastings Algorithm
#'
#' @description This function estimates the probabilities of being included in the target list
#' conditional on unobserved group S using the observed indicator X using the Metropolis-Hastings Algorithm.
#'
#'
#' @param data data of both aggregate data and target list (dataframe/matrix)
#' @param mu0 mean of the prior distribution (vector)
#' @param inf0 the information matrix of prior distribution (matrix)
#' @param M MCMC iteration numbers (int)
#' @param it0 burn-in iterations (int)
#' @param flag_smooth Dirichlet smoothing or not (bool)
#'
#' @return A list consists of all interations of Markov Chain, the normalized markov chain,
#' probabilities of being included in the listin each chain, log-likelihood of each chain, and
#' whether this the morkov chain is rejected to move forward at each step.
#' @export
#'
#' @examples
#'
#'
#'
#'
#'

Bayesprofile_MH <- function(data, mu0, inf0, M, it0, flag_smooth=FALSE){

  # get the data distribution from the data we have
  dist = data_dist(data)
  n_X=dist$n_X
  n_S=dist$n_S
  log_pXcondS = as.matrix(dist$log_pXcondS)
  countX = dist$target
  prmu0=matrix(0,n_S,mu0)
  prinf0=diag(n_S)*inf0
  prinf0=solve(prinf0, diag(n_S))
  wmc=matrix(0,M-it0,n_S)   #weight generators (unrestricted)
  nwmc=matrix(0,M-it0,n_S)  #normalized weights (on the simplex)
  Smc=matrix(0,M-it0,n_S)   #posterior list profile
  loglikmc=matrix(0,M-it0,1)
  rejectmc=matrix(0,M-it0,1)

  w=rnorm(n_S)
  if (flag_smooth){
    logprdw = -0.5*w*K/prtau*t(w)
  } else {
    logprdw=dmvn(t(as.matrix(w)),prmu0,prinf0)
  }

  #numerically stabilized normalization of weights
  wstab=w-max(w)-log(sum(exp(w - max(w))))

  loglikind = outer(rep.int(1L, n_X), wstab)+log_pXcondS
  hilfmax = Arma_rowmax(loglikind)
  loglikind = loglikind -outer(as.vector(hilfmax), rep.int(1L, n_S))

  loglik_=sum((log(Arma_rowSums(as.matrix(exp(loglikind))))+hilfmax)*countX)
  loglik=loglik_
  nw=exp(wstab)

  for (m in 1:M){
    reject=0

    # random walk proposal for (non-normalized) candidate weights
    w_cand=w+rnorm(n_S)*0.02

    # evaluate prior
    if (flag_smooth){
      logprdw_cand = -0.5*w*(K/prtau)*t(w)
    } else {
      logprdw_cand=dmvn(t(as.matrix(w_cand)),prmu0,prinf0)
    }

    wstab=w_cand-max(w_cand)-log(sum(exp(w_cand - max(w_cand))))

    #compute log-likelihood (see Equaton 6 in the technical paper)
    loglikind = outer(rep.int(1L, n_X), wstab)+log_pXcondS
    hilfmax = Arma_rowmax(loglikind)
    loglikind = loglikind -outer(as.vector(hilfmax), rep.int(1L, n_S))

    loglik_cand=sum((log(Arma_rowSums(as.matrix(exp(loglikind))))+hilfmax)*countX)
    loglik_cand_=loglik_cand

    # compute MH ratio
    lalpha = loglik_cand + log(logprdw_cand) - (loglik + log(logprdw))

    # sample
    if (runif(1)<exp(lalpha)){
      w=w_cand
      nw=exp(wstab)
      loglik=loglik_cand
      loglik_=loglik_cand_
      logprdw=logprdw_cand
      reject=1
    }

    # computing the posterior list profile (see first line of equation 8 in the technical paper)
    logprofcondX=outer(rep.int(1L, n_X), log(nw)) + log_pXcondS
    nconst=log(Arma_rowSums(as.matrix(exp(logprofcondX))))
    logprofcondX=logprofcondX-outer(as.vector(nconst), rep.int(1L, n_S))

    S=Arma_colSums(as.matrix(outer(as.vector(countX), rep.int(1L, n_S))*exp(logprofcondX)))

    if (m > it0){
      wmc[m-it0,]=w
      nwmc[m-it0,]=nw
      Smc[m-it0,]=S
      loglikmc[m-it0]=loglik_
      rejectmc[m-it0]=reject
    }

    cat('\r', m)

  }

  return(list(wmc, nwmc, Smc, loglikmc, rejectmc))
}
