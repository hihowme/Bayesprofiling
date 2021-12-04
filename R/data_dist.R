#' Distribution information of panel data
#'
#' @description Get the distribution information of data for further analysis
#'
#' @param data data of both aggregate data and target list (dataframe/matrix)
#'
#' @return  a list of data distribution informations
#' @export
#'
#' @examples
#'
#'
#'
#'

data_dist <- function(data){
  # return data distribution information and subdata that we need for bayesian analysis
  # X: observed categories, S:unobserved categories
  # observed category: Zipcode group k where k=1,...,K
  category = data[,1]

  # target group: #N_k, number of individual in the target list with the kth category
  # # \sum_{i=1}^{I}(Xi = k) for k=1,...,K
  target = data[,2]

  # population group: Y_{j,k}, means the number in the population with jth unobserved catrgory
  # and kth observed category
  population = data[,3:length(data[1,])]

  # get the dimensional data: number of observed categories and unobserved categories
  n_X=dim(population)[1]
  n_S=dim(population)[2]

  # margial distributions based on observed categories and unobserved categories
  margin_on_X=rowSums(population, na.rm = FALSE, dims = 1)
  margin_on_S=colSums(population, na.rm = FALSE, dims = 1)

  # Computing logp(X) and logp(S)
  log_px = log(margin_on_X/sum(population))
  log_ps = log(margin_on_S/sum(population))

  # p(X|S) = P(X,S)/P(S), conditional probability
  pXcondS = population/t(replicate(n_X, margin_on_S))
  log_pXcondS=log(pXcondS)
  log_pXcondS[log_pXcondS==-Inf]=min(log_pXcondS[log_pXcondS>-Inf])

  # P(S|X) = P(X,S)/P(X)
  pScondX = population/replicate(n_S, margin_on_X)
  log_pScondX=log(pScondX)
  log_pScondX[log_pScondX==-Inf]=min(log_pScondX[log_pScondX>-Inf])

  # 1 = log(Arma_rowSums(exp(1+outer(rep.int(1L, n_X), log_ps))))
  # baseline likelihood with equal weights on each choice
  loglikind=outer(rep.int(1L, n_X), log_ps)+as.matrix(log_pXcondS)
  hilfmax=Arma_rowmax(loglikind)
  loglikind = loglikind -outer(as.vector(hilfmax), rep.int(1L, n_S))
  loglik=sum((log(Arma_rowSums(as.matrix(exp(loglikind))))+hilfmax)*target)

  return(list(category=category,
              target=target,
              population=population,
              n_X=n_X,
              n_S=n_S,
              margin_on_X=margin_on_X,
              margin_on_S=margin_on_S,
              log_px=log_px,
              log_ps=log_ps,
              pXcondS=pXcondS,
              log_pXcondS=log_pXcondS,
              pScondX=pScondX,
              log_pScondX=log_pScondX,
              loglik_base = loglik))
}
