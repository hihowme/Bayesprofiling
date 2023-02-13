#' XS_pop
#'
#' A data set containing the information of the voting preferences of the population
#' residents in each of the zipcode group. The 36239 zipcode groups are normalized to the
#' integer of 1 to 36239. The variables are as follows:
#'
#' \itemize{
#'   \item zipcode_group. The zipcode group. (1 to 36239)
#'   \item Other columns are the name of the president candidates.
#' }
#'
#' @name XS_pop
#' @docType data
#' @keywords data
#' @usage data(XS_pop)
#' @format A data frame with 36239 rows and 15 variables
"XS_pop"

#' VillagesSample
#'
#' A data set containing the information of the votes of a targeting
#' group. There are 23610 zipcode groups. The variables are as follows:
#'
#' \itemize{
#'   \item zipcode_group. The zipcode group. (1 to 23610)
#'   \item voter. The number of voters in each zipcode group.
#' }
#'
#' @name VillagesSample
#' @docType data
#' @keywords data
#' @usage data(VillagesSample)
#' @format A data frame with 23610 rows and 2 variables
"VillagesSample"

#' Election
#'
#' A data set containing the information of the votes of a targeting
#' group, and the population voting preference distribution of the zipcode
#' groups. There are 23610 zipcode groups (The zipcode group not in the targeting list
#' are not included). The variables are as follows:
#'
#' \itemize{
#'   \item zipcode_group. The zipcode group. (1 to 23610)
#'   \item voter. The number of voters in each zipcode group.
#'   \item Other columns are the name of the president candidates.
#' }
#'
#' @name Election
#' @docType data
#' @keywords data
#' @usage data(Election)
#' @format A data frame with 23610 rows and 16 variables
"Election"


#' Occupation_population
#'
#' A data set containing the information of the career choices of the population
#' in each of the zipcode group. There are 32557 zipcodes.:
#'
#' \itemize{
#'   \item zip_code. The zipcode group.
#'   \item Other columns are the job types.
#' }
#'
#' @name Occupation_population
#' @docType data
#' @keywords data
#' @usage data(Occupation_population)
#' @format A data frame with 32557 rows and 13 variables
"Occupation_population"


#' Occupation_target
#'
#' A data set containing the information of the number of the visitors
#' in each of the zipcode group. There are 2817 zipcodes.:
#'
#' \itemize{
#'   \item zip_code. The zipcode group.
#'   \item visitors. The number of visitors in each zipcode group.
#' }
#'
#' @name Occupation_target
#' @docType data
#' @keywords data
#' @usage data(Occupation_target)
#' @format A data frame with 2817 rows and 2 variables
"Occupation_target"


#' Occupation
#'
#' A data set containing the information of the jobs of a targeting
#' group, and the population job distribution of the zipcode
#' groups. There are 32557 zipcodes.:
#'
#' \itemize{
#'   \item zip_code. The zipcode group.
#'   \item visitors. The number of visitors in each zipcode group.
#'   \item Other columns are the job types.
#' }
#'
#' @name Occupation
#' @docType data
#' @keywords data
#' @usage data(Occupation)
#' @format A data frame with 32557 rows and 15 variables
"Occupation"
