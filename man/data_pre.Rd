% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_pre.R
\name{data_pre}
\alias{data_pre}
\title{Preprocessing of panel data}
\usage{
data_pre(target, population, category, base = "population")
}
\arguments{
\item{target}{a dataframe specifying the distribution of observed categories of target list. (dataframe/matrix/vector)}

\item{population}{a dataframe specifying the joint distribution of observed categories and unobserved categories of population. (dataframe/matrix)}

\item{category}{a string specifying the name of the observed categories。}

\item{base}{an optional logical variable specifying the base of the merge. if base = "population"
then the intended list is the whole population and probability 0 would be assigned to those category that is
not in the list while in the population, if base = "target" then the target list is the range of consumer of
interest, and the observed categories not shown in the target list will be not considered when doing inferences.}
}
\value{
\code{data_pre} returns a data frame \code{datap}

\item{datap}{a data frame that contains information on both the target list and population distribution}
}
\description{
\code{data_pre} creates a panel data that contains the distribution information
of the observed categories and unobserved categories on both the target list
and population distribution from the separate data of distribution of target list and population data
}
\examples{

# data preparation
# The distribution of observaed categories in target list
data(VillagesSample)
# The joint distribution of both observaed and unobserved categories in the population
data(XS_pop)
# Get the Dataframe
Election = data_pre(VillagesSample,XS_pop,category = "zipcode_group")





}
