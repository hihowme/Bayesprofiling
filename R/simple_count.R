# Title     : simple_count
# Objective : apply the simple count method
# Created by: haihaoguo
# Created on: 10/7/20
#' Profiling using the Simple count method
#'
#' @description This function estimates the probabilities of being included in the target list
#' conditional on observed indicator X using the observed indicator X with the simple count method.
#'
#' @param data data of both aggregate data and target list (dataframe/matrix)
#'
#' @return The probabilities of being included in the list
#' @export
#'
#' @examples
#'
#'
#'
#'
#'
#'
simple_count <- function (dist){

  # first calculate the observation numbers
  simple_count = colSums(dist$pScondX * replicate(dist$n_S, dist$target))

  # return in the probability form for comparison
  # simple_count = simple_count/sum(simple_count)
  return(simple_count)
}
