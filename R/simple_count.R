# Title     : simple_count
# Objective : apply the simple count method
# Created by: haihaoguo
# Created on: 10/7/20
#' Consumer Profiling using the Simple count method
#'
#' @description \code{simple_count} estimates the probabilities of being included in the target list
#' conditional on observed indicator X using the observed indicator X using the simple count method.
#'
#' @param data a data frame containing the distribution of both the aggregate data and target list (dataframe/matrix)
#'
#' @return \code{Bayesprofile_MH} returns a list containing the following fields:
#' \item{profile}{The probabilities of each variable being included in the list.}
#' \item{plot}{The plot of outcome distribution}
#'
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
#' dists_Election <- data_dist(Election)
#' result_simple_count = simple_count(dists_Election)
#' result_simple_count$profile
#' result_simple_count$plot
#' @export
#'
simple_count <- function (dist){

  # first calculate the observation numbers
  simple_count = colSums(dist$pScondX * replicate(dist$n_S, dist$target))

  # return in the probability form for comparison
  #simple_count_prob = simple_count/sum(simple_count)
  df_profile = data.frame(t(rbind(dist$margin_on_S/sum(dist$margin_on_S),
                                  simple_count/sum(simple_count))))
  df_profile$name = names(dist$pScondX)
  colnames(df_profile) = c('population',
                           'simple_count',
                           'name')
  p <- ggplot2::ggplot(df_profile) +
    ggplot2::geom_segment(ggplot2::aes(x=name, xend=name, y=0, yend=population,color='population')) +
    ggplot2::geom_point(ggplot2::aes(size=1, x=name, y=population,color='population', alpha=0.7,stroke=2)) +
    #geom_histogram(aes(size=1, x=name, y=population,color='population', alpha=0.7)) +
    ggplot2::geom_segment(ggplot2::aes(x=name, xend=name, y=0, yend=simple_count,color='simple_count')) +
    ggplot2::geom_point(ggplot2::aes(size=1, x=name, y=simple_count,color='simple_count', alpha=0.7,stroke=2)) +
    ggplot2::labs(colour = "Method") + ggplot2::scale_alpha(guide="none") + ggplot2::scale_size(guide="none")

  return(list(profile = simple_count,plot = p))


}
