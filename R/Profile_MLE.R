# Title     : Profile_MLE
# Objective : TODO
# Created by: haihaoguo
# Created on: 03/25/21
#' Consumer Profiling using the Maximum Likelihood Method
#'
#' @description \code{Profile_MLE} estimates the probabilities of being included in the target list
#' using the Maximum Likelihood Method.
#'
#' @param dist a list containing the distribution information of both the aggregate data and target list (list)
#'
#' @return \code{Profile_MLE} returns a list containing the following fields:
#' \item{w}{the posterior markov chain}
#' \item{nw}{normalized markov chain}
#' \item{S}{probabilities of being included in the list in markov chain}
#' \item{result_MLE}{log-likelihood of markov chain}
#' \item{dist_plot}{The plot of posterior distribution}
#'
#' @examples
#'
#' # data preparation
#' data(VillagesSample)
#' data(XS_pop)
#' Election = data_pre(VillagesSample,XS_pop,category = "zipcode_group",base= 'target')
#' Result_MLE = Profile_MLE(Election)
#' Result_MLE$dist_plot
#'
#' @export
# Maximum Likelihood Estimation
Profile_MLE <- function(dist){
  n_X=dist$n_X
  n_S=dist$n_S
  log_pXcondS = as.matrix(dist$log_pXcondS)
  countX=dist$target

  obj <- function(w1){
    w = c(1,w1)
    wstab=w-max(w)-log(sum(exp(w - max(w))))
    loglikind = outer(rep.int(1L, n_X), wstab)+log_pXcondS
    hilfmax = Arma_rowmax(loglikind)
    loglikind = loglikind -outer(as.vector(hilfmax), rep.int(1L, n_S))

    loglik_=sum((log(Arma_rowSums(as.matrix(exp(loglikind))))+hilfmax)*countX)
    return(-loglik_)
  }
  st = dist$margin_on_S/sum(dist$margin_on_S)
  result_MLE = nlm(obj, st[2:length(st)], hessian = TRUE)
  w1=result_MLE$estimate
  w = c(1,w1)
  nw = exp(w-max(w)-log(sum(exp(w - max(w)))))
  logprofcondX=outer(rep.int(1L, n_X), log(nw)) + log_pXcondS
  nconst=log(Arma_rowSums(as.matrix(exp(logprofcondX))))
  logprofcondX=logprofcondX-outer(as.vector(nconst), rep.int(1L, n_S))

  S=Arma_colSums(as.matrix(outer(as.vector(countX), rep.int(1L, n_S))*exp(logprofcondX)))



  df_profile = data.frame(t(rbind(dist$margin_on_S/sum(dist$margin_on_S),
                                  S/sum(S))))
  df_profile$name = names(dist$pScondX)
  colnames(df_profile) = c('population',
                           'MLE',
                           'name')
  p <- ggplot2::ggplot(df_profile) +
    ggplot2::geom_segment(ggplot2::aes(x=name, xend=name, y=0, yend=population,color='population')) +
    ggplot2::geom_point(ggplot2::aes(size=1, x=name, y=population,color='population', alpha=0.7,stroke=2)) +
    #geom_histogram(aes(size=1, x=name, y=population,color='population', alpha=0.7)) +
    ggplot2::geom_segment(ggplot2::aes(x=name, xend=name, y=0, yend=MLE,color='MLE')) +
    ggplot2::geom_point(ggplot2::aes(size=1, x=name, y=MLE,color='MLE', alpha=0.7,stroke=2)) +
    ggplot2::labs(colour = "Method") + ggplot2::scale_alpha(guide="none") + ggplot2::scale_size(guide="none")


  return(list(w=w, nw=nw, S=S, result_MLE=result_MLE, dist_plot = p))
}
