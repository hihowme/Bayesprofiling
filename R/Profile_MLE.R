# Title     : Profile_MLE
# Objective : TODO
# Created by: haihaoguo
# Created on: 03/25/21
#' Consumer Profiling using the Maximum Likelihood Method
#'
#' @description This function estimates the probabilities of being included in the target list
#' using the Maximum Likelihood Method.
#'
#' @param data data of both aggregate data and target list (dataframe/matrix)
#'
#' @return the list of probability of being included in the target list and the optimization result given by nlm.
#' @export
#'
#' @examples
#'
#'
#'
#'
#'
#'
# Maximum Likelihood Estimation
Profile_MLE <- function(data){
  dist = data_dist(data)
  n_X=dist$n_X
  n_S=dist$n_S
  log_pXcondS = as.matrix(dist$log_pXcondS)
  countX=dist$target

  obj <- function(w){
    wstab=w-max(w)-log(sum(exp(w - max(w))))
    loglikind = outer(rep.int(1L, n_X), wstab)+log_pXcondS
    hilfmax = Arma_rowmax(loglikind)
    loglikind = loglikind -outer(as.vector(hilfmax), rep.int(1L, n_S))

    loglik_=sum((log(Arma_rowSums(as.matrix(exp(loglikind))))+hilfmax)*countX)
    return(-loglik_)
  }
  st = rnorm(n_S)
  result_MLE = nlm(obj, st, hessian = TRUE)
  w=result_MLE$estimate
  nw = exp(w-max(w)-log(sum(exp(w - max(w)))))
  logprofcondX=outer(rep.int(1L, n_X), log(nw)) + log_pXcondS
  nconst=log(Arma_rowSums(as.matrix(exp(logprofcondX))))
  logprofcondX=logprofcondX-outer(as.vector(nconst), rep.int(1L, n_S))

  S=Arma_colSums(as.matrix(outer(as.vector(countX), rep.int(1L, n_S))*exp(logprofcondX)))
  return(list(w, nw, S, result_MLE))
}
