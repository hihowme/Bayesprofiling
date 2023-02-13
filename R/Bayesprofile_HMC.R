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
#' @return the fitted result as an instance of stanfit
#' @export
#'
#' @examples
#'
#'
#'
#'
#'

Bayesprofile_HMC <- function(dist, prmu0, prinf0, M, chains){

  # get data ready for stan
  data_stan <- list(
    ncatX = dist$n_X,
    ncatS = dist$n_S,
    log_pXcondS = dist$log_pXcondS,
    mu = prmu0,
    sigma = prinf0,
    y = dist$target
  )

  election.stan = 'functions {
  vector getsimples(vector countX, vector wstab, int ncatS, int ncatX, matrix log_pXcondS){
    matrix[ncatX, ncatS] logprofcondX;
    vector[ncatX] nconst;
    vector[ncatS] S;

    for (i in 1:ncatX){
      for (j in 1:ncatS){
        logprofcondX[i,j] = log_pXcondS[i,j] + wstab[j];
      }
      nconst[i] = log_sum_exp(logprofcondX[i,]);
      for (j in 1:ncatS){
        logprofcondX[i,j] = logprofcondX[i,j] - nconst[i];
      }
    }

    for (i in 1:ncatS){
      S[i] = 0;
      for (j in 1:ncatX){
        S[i] = S[i] + countX[j] * exp(logprofcondX[j,i]);
      }
    }

    return S;
  }

  real bayesprofile_lpdf(real y, vector wstab, int ncatS, row_vector log_pXcondS){

     //first define additional local variables
     real lprob;
     vector[ncatS] loglik;
     real hilfmax;

     lprob = 0;
     hilfmax = 0;
     for (i in 1:ncatS){
       loglik[i] = 0;
     }

     for (i in 1:ncatS){
       loglik[i] = wstab[i] + log_pXcondS[i];
     }
     hilfmax = max(loglik);

     for (i in 1:ncatS){
       loglik[i] = loglik[i] - hilfmax;
     }
     lprob = (log_sum_exp(loglik) + hilfmax) * y;

     return lprob;
  }
}

data {
  // Data inputs
  int ncatX; // number of observations (observed groups)
  int ncatS; // number of covariates (unobserved groups) (s1,s2...)
  matrix[ncatX, ncatS] log_pXcondS; //logp(X|S)
  vector[ncatS] mu; // vector of prior mean
  matrix[ncatS, ncatS] sigma; // matrix of prior covariance matrix
  vector[ncatX] y; // vector of target list
}

parameters {
  // the parameters we want to estimate would go in here
  vector[ncatS] w; // vector of probabilities to be in unobserved groups
}

transformed parameters {
  vector[ncatS] wstab;
  for (i in 1:ncatS){
    wstab[i] = w[i] - max(w);
  }
  wstab = wstab - log_sum_exp(wstab);
}

model {
  // This is where the probability model we want to estimate would go
  // Define the priors
  w ~ multi_normal(mu, sigma);
  // The likelihood
  for (i in 1:ncatX){
    y[i] ~ bayesprofile(wstab, ncatS, log_pXcondS[i,]);
  }
}

generated quantities {
  vector[ncatS] S;
  vector[ncatS] nw;
  nw=exp(wstab);
  S = getsimples(y, wstab, ncatS, ncatX, log_pXcondS);
}
'
  # run the stan code: change the directory to your github repository
  fit <- stan(model_code=election.stan,
              data = data_stan,
              chains = chains,
              iter = M)

  return(fit)
}
