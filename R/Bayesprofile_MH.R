#' Bayesian Profiling using the Metropolis-Hastings Algorithm
#'
#' @description \code{Bayesprofile_MH} estimates the probabilities of being included in the target list
#' conditional on unobserved group S using the observed indicator X using the Metropolis-Hastings Algorithm.
#'
#'
#' @param dist a list containing the distribution information of both the aggregate data and target list (list)
#' @param mu0 a vector specifying the mean of the prior distribution (vector)
#' @param inf0 a matrix specifying the information matrix of prior distribution (matrix)
#' @param M an integer specifying MCMC iteration numbers (int)
#' @param it0 an integer specifying burn-in iteration numbers (int)
#' @param flag_smooth an optional logical variable specifying Dirichlet smoothing or not (bool)
#'
#' @details
#'
#'Smc=Smc, loglikmc=loglikmc, rejectmc=rejectmc, dist_plot
#' @return \code{Bayesprofile_MH} returns a list containing the following fields:
#' \item{wmc}{the posterior markov chain}
#' \item{nwmc}{normalized markov chain}
#' \item{Smc}{probabilities of being included in the list in markov chain}
#' \item{loglikmc}{log-likelihood of markov chain}
#' \item{rejectmc}{the rejection record of the markov chain}
#' \item{dist_plot}{The plot of posterior distribution}
#'
#'
#' @references De Bruyn, Arnaud, and Thomas Otter (2022)
#' \cite{Bayesian Consumer Profiling: How to Estimate Consumer Characteristics from Aggregate Data},
#' Journal of Marketing Research (2022), 59(4), 755–774
#'
#' @examples
#'
#' # data preparation
#' data(VillagesSample)
#' data(XS_pop)
#' Election = data_pre(VillagesSample,XS_pop,category = "zipcode_group",base= 'target')
#' # prior mu = 0, cov = 0.01, M = 4000, burn-in iterations = 200
#' Result_MH = Bayesprofile_MH(Election, 0, 0.01, 4000, 2000, FALSE)
#' # posterior distribution plot
#' Result_MH$dist_plot
#'
#' @export

Bayesprofile_MH <- function(dist, mu0, inf0, M, it0, flag_smooth=FALSE){

  # get the data distribution from the data we have
  n_X=dist$n_X
  n_S=dist$n_S
  log_pXcondS = as.matrix(dist$log_pXcondS)
  countX = dist$target
  prmu0=matrix(mu0,n_S,1)
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

  df_profile = data.frame(t(rbind(dist$margin_on_S/sum(dist$margin_on_S),
                                  Arma_colmean(Smc)/sum(Arma_colmean(Smc)))))
  df_profile$name = names(dist$pScondX)
  colnames(df_profile) = c('population',
                           'MCMC',
                           'name')
  p <- ggplot2::ggplot(df_profile) +
    ggplot2::geom_segment(ggplot2::aes(x=name, xend=name, y=0, yend=population,color='population')) +
    ggplot2::geom_point(ggplot2::aes(size=1, x=name, y=population,color='population', alpha=0.7,stroke=2)) +
    #geom_histogram(aes(size=1, x=name, y=population,color='population', alpha=0.7)) +
    ggplot2::geom_segment(ggplot2::aes(x=name, xend=name, y=0, yend=MCMC,color='MCMC')) +
    ggplot2::geom_point(ggplot2::aes(size=1, x=name, y=MCMC,color='MCMC', alpha=0.7,stroke=2)) +
    ggplot2::labs(colour = "Method") + ggplot2::scale_alpha(guide="none") + ggplot2::scale_size(guide="none")

  return(list(wmc=wmc, nwmc=nwmc, Smc=Smc, loglikmc=loglikmc, rejectmc=rejectmc, dist_plot=p))
}
