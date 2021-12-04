# Bayesian-Consumer-Profiling

A R_package realization of De Bruyn, A., &amp; Otter, T. (2019). Bayesian Consumer Profiling. Available at [SSRN 2740293](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=2740293).\
This is an ongoing project and this repository records the progress of this project.

## Objective

This package is corresponding to the idea of the paper, which is using the **joint distribution of observed categories and the unobserved categories in the reference population** and the **distribution of observed categories in the target list** to estimate the **distribution of unobserved categories in the target list**.

## Parts/Package components

### Datasets & Examples

The package provide 3 datasets, corresponding to 3 examples we used in the paper. Codes are also provided with this package to estimate using the 3 datasets.

Datasets could be found in data, and the examples could be found in examples.

### Data preprocessing

Users are expected to input 2 datasets: 

1.The target list they want to estimate, and the distribution of the observed categories in the target list.\
2.The reference population that gives us the information of the joint distribution of the observed categories and the unobserved categories in the population.

All the files regarding the data preprocessing process, are in the data_prepare.

### Methods

**Bayesian Consumer Profiling**

The package contains 2 algorithms to do the Bayesian Consumer Profiling, using the random walk Metropolis-Hastings MCMC and the Hamiltonian Monte Carlo. The functions could be found in bayes_profiling.

**Simple Count**

The simple count method could be found in simple_count.

**MLE**

Functions using Maximum likelihood estimation could be found in MLE.

### Results

The package provides several ways to present the result, the functions could be found in result_present.