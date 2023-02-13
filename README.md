# Bayesian-Consumer-Profiling

A R_package realization of De Bruyn, Arnaud, and Thomas Otter (2022)
\cite{Bayesian Consumer Profiling: How to Estimate Consumer Characteristics from Aggregate Data}, Journal of Marketing Research (2022), 59(4), 755–774 \
This is an ongoing project and this repository records the progress of this project.

## Installation
```
# Install the development version from GitHub:
# install.packages("devtools")
devtools::install_github("hihowme/Bayesprofiling")
```

## Objective

This package is corresponding to the idea of the paper, which is using the **joint distribution of observed categories and the unobserved categories in the reference population** and the **distribution of observed categories in the target list** to estimate the **distribution of unobserved categories in the target list**.

### Datasets

The package provide 2 dataset, the occupation data and election data. The documentation could be found using (Take the occupation data as an example):

```
?Occupation
```

### Data preprocessing

Users are expected to input 2 datasets: 

1.The target list they want to estimate, and the distribution of the observed categories in the target list.\
2.The reference population that gives us the information of the joint distribution of the observed categories and the unobserved categories in the population.

The details of the data preprocessing could be found in R/data_pre.R and R/data_dist.R.
### Methods

**Bayesian Consumer Profiling**

The package contains 2 algorithms to do the Bayesian Consumer Profiling, using the random walk Metropolis-Hastings MCMC and the Hamiltonian Monte Carlo. The function could be found R/Bayesprofile_MH.R.

**Simple Count**

The simple count method could be found in R/simple_count.R.

**MLE**

Functions using Maximum likelihood estimation could be found in R/Profile_MLE.R.


## Exmaple of Usage
Here is an example of usage trying different methods:
```
# example: election data

# VillagesSample: the target list
# containing 2 parts: 1. the category classifier id, 2. the observed characteristics distribution
data(VillagesSample)
# XS_pop: the population list
# containing 2 parts: 1. the category classifier id, 2. the observed characteristics distribution
data(XS_pop)

####### add the unobserved structural 0s
# get the joint dataframe
Election = data_pre(VillagesSample,XS_pop,category = "zipcode_group",base= 'population')

# get the data distribution that is needed for further analysis
dists_Election <- data_dist(VillagesSample,XS_pop,category = "zipcode_group",base= 'population')

#################################################
############ simple count method ################
#################################################

result_simple_count = simple_count(dists_Election)

# show the result
# profiling output
result_simple_count$profile
# plot
result_simple_count$plot

##################################################
############ MCMC method #########################
##################################################

# prior mu = 0, cov = 0.01, M = 4000, burn-in iterations = 2000
Result_MH = Bayesprofile_MH(dists_Election, 0, 0.01, 4000, 2000, FALSE)
# plot
Result_MH$dist_plot



Result_MLE = Profile_MLE(dists_Election)
Result_MLE$dist_plot
```


