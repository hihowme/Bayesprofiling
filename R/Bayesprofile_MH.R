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
#' One thing that users could self-define is to use the Hamiltonian Monte Carlo algorithm (HMC) over the
#' Metropolis-Hastings(MH) method in this function. Generally, the HMC performs better than the MH algorithm:
#' Given the same length of generated chains, the HMC chains usually reaches the steady-state sooner and have
#' a lower log-likelihood. It is hard to incorporate stan in this package, but we gave a block of code in the
#' example to show how to use the HMC algorithm to do Bayesian Profiling, so that the user could run it on their
#' end and do the comparison.
#'
#'
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
#' library('rstan')
#' library('reshape2')
#' library("inline")
#' library("bayesplot")
#' library("ggplot2")
#' rstan_options(auto_write = TRUE)
#' options(mc.cores = parallel::detectCores())
#'
#'
#'
#' prmu0=rep(0,dists_Election$n_S)
#' prinf0=diag(dists_Election$n_S)*0.01
#'
#' data_stan <- list(
#'   ncatX = dists_Election$n_X,
#'   ncatS = dists_Election$n_S,
#'   log_pXcondS = dists_Election$log_pXcondS,
#'   mu = prmu0,
#'   sigma = prinf0,
#'   y = dists_Election$target
#' )
#'
#' election.stan = 'functions {
#'   vector getsimples(vector countX, vector wstab, int ncatS, int ncatX, matrix log_pXcondS){
#'     matrix[ncatX, ncatS] logprofcondX;
#'     vector[ncatX] nconst;
#'     vector[ncatS] S;
#'
#'     for (i in 1:ncatX){
#'       for (j in 1:ncatS){
#'         logprofcondX[i,j] = log_pXcondS[i,j] + wstab[j];
#'       }
#'       nconst[i] = log_sum_exp(logprofcondX[i,]);
#'       for (j in 1:ncatS){
#'         logprofcondX[i,j] = logprofcondX[i,j] - nconst[i];
#'       }
#'     }
#'
#'     for (i in 1:ncatS){
#'       S[i] = 0;
#'       for (j in 1:ncatX){
#'         S[i] = S[i] + countX[j] * exp(logprofcondX[j,i]);
#'       }
#'     }
#'
#'     return S;
#'   }
#'
#'   real bayesprofile_lpdf(real y, vector wstab, int ncatS, row_vector log_pXcondS){
#'
#'      //first define additional local variables
#'      real lprob;
#'      vector[ncatS] loglik;
#'      real hilfmax;
#'
#'      lprob = 0;
#'      hilfmax = 0;
#'      for (i in 1:ncatS){
#'        loglik[i] = 0;
#'      }
#'
#'      for (i in 1:ncatS){
#'        loglik[i] = wstab[i] + log_pXcondS[i];
#'      }
#'      hilfmax = max(loglik);
#'
#'      for (i in 1:ncatS){
#'        loglik[i] = loglik[i] - hilfmax;
#'      }
#'      lprob = (log_sum_exp(loglik) + hilfmax) * y;
#'
#'      return lprob;
#'   }
#' }
#'
#' data {
#'   // Data inputs
#'   int ncatX; // number of observations (observed groups)
#'   int ncatS; // number of covariates (unobserved groups) (s1,s2...)
#'   matrix[ncatX, ncatS] log_pXcondS; //logp(X|S)
#'   vector[ncatS] mu; // vector of prior mean
#'   matrix[ncatS, ncatS] sigma; // matrix of prior covariance matrix
#'   vector[ncatX] y; // vector of target list
#' }
#'
#' parameters {
#'   // the parameters we want to estimate would go in here
#'   vector[ncatS] w; // vector of probabilities to be in unobserved groups
#' }
#'
#' transformed parameters {
#'   vector[ncatS] wstab;
#'   for (i in 1:ncatS){
#'     wstab[i] = w[i] - max(w);
#'   }
#'   wstab = wstab - log_sum_exp(wstab);
#' }
#'
#' model {
#'   // This is where the probability model we want to estimate would go
#'   // Define the priors
#'   w ~ multi_normal(mu, sigma);
#'   // The likelihood
#'   for (i in 1:ncatX){
#'     y[i] ~ bayesprofile(wstab, ncatS, log_pXcondS[i,]);
#'   }
#' }
#'
#' generated quantities {
#'   vector[ncatS] S;
#'   vector[ncatS] nw;
#'   nw=exp(wstab);
#'   S = getsimples(y, wstab, ncatS, ncatX, log_pXcondS);
#' }
#' '
#' # run the stan code
#' fit <- stan(model_code=election.stan,
#'             data = data_stan,
#'             chains = 4,
#'             iter = 2000)
#'
#'
#' posterior <- as.array(fit)
#'
#' color_scheme_set("red")
#' my_labels <- c('ABSTENTION','BLANK','SARKOZY','ROYAL','BAYROU','LE PEN','BESANCENOT',
#'                'VILLIERS','BUFFET','VOYNET','BOVE','LAGUILLER','NIHOUS','SCHIVARDI')
#' mcmc_intervals(posterior, pars = c("S[1]", "S[2]", "S[14]", "S[10]",
#'                                    "S[6]", "S[12]", "S[3]", "S[9]",
#'                                    "S[4]", "S[8]", "S[7]", "S[13]",
#'                                    "S[11]", "S[5]")) + scale_y_discrete(labels = my_labels)
#'
#'
#' nw <- extract(fit, 'nw[1]')
#' nw <- unlist(nw, use.names = FALSE)
#'
#' plot(density(nw),
#'      xlab=expression(nw), col=grey(0, 0.8),
#'      main="Parameter distribution")
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
