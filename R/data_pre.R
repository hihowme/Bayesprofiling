# ---------------------------------------------
# Preprocessing of panel data
# Haihao Guo
# Created: Sep 28th
# Update: Oct 7th
# ---------------------------------------------

#' Preprocessing of panel data
#'
#' @description \code{data_pre} creates a panel data that contains the distribution information
#' of the observed categories and unobserved categories on both the target list
#' and population distribution from the separate data of distribution of target list and population data
#'
#' @param target a dataframe specifying the distribution of observed categories of target list. (dataframe/matrix/vector)
#' @param population a dataframe specifying the joint distribution of observed categories and unobserved categories of population. (dataframe/matrix)
#' @param category a string specifying the name of the observed categories。
#' @param base an optional logical variable specifying the base of the merge. if base = "population"
#' then the intended list is the whole population and probability 0 would be assigned to those category that is
#' not in the list while in the population, if base = "target" then the target list is the range of consumer of
#' interest, and the observed categories not shown in the target list will be not considered when doing inferences.
#'
#' @return \code{data_pre} returns a data frame \code{datap}
#'
#' \item{datap}{a data frame that contains information on both the target list and population distribution}
#' @export
#'
#' @examples
#'
#' # data preparation
#' # The distribution of observaed categories in target list
#' data(VillagesSample)
#' # The joint distribution of both observaed and unobserved categories in the population
#' data(XS_pop)
#' # Get the Dataframe
#' Election = data_pre(VillagesSample,XS_pop,category = "zipcode_group")
#'
#'
#'
#'
#'
# input:dataset, column index of the target list, column index of data broker
# ideally input: a group distribution in the List and the population distribution in the list
# group distribution: number of members in observed list,
# rows: numbers, columns: list
# population distribution: joint distribution of observed categories and unobserved categories
# rows


# general case:
data_pre <- function (target,population,category,base= 'population'){
  #merge 2 datasets
  # after the merging process, the first column would be the category,
  # the second column would be the target group, and then goes the population data
  if (base == 'population'){
    data_temp = merge(target, population, by=category, all.x=TRUE)
    data_temp[is.na(data_temp)] <- 0

    # find observed categories with no unobserved category in population
    # should the target group also included here? or do we need any warning or something for the corner case?
    rows_zero = rowSums(data_temp[,3:dim(data_temp)[2]])==0
    datap = data_temp[!rows_zero,]
  } else if (base == 'target'){
    data_temp = merge(target, population, by=category, all=TRUE)
    data_temp[is.na(data_temp)] <- 0

    # find observed categories with no unobserved category in population
    # should the target group also included here? or do we need any warning or something for the corner case?
    rows_zero = rowSums(data_temp[,3:dim(data_temp)[2]])==0
    datap = data_temp[!rows_zero,]
  }
  return(datap)
}

# corner case: one of it is 0
