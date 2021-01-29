# Overconfidence in bootstrap-based estimates of model fit based on likelihoods

Many measure used to evaluate models are based on likelihoods. For a given model, or model comparison, any likelihood based estimate will vary somewhat when evaluated based on different samples from a given data-generating process. We can think of this as a sampling variability inherent in a certain model evaluation or comparison. Bootstrapping is often used to estimate the uncertainty of estimates, and as it approximates the data-generating process it could be used to estimate this sampling variability.

My conjecture is that the variability in the bootstrap based estimate, compared to that based on samples from the dgp, will vary greatly as a function of the entropy of the data-generating process, fatter tails leading to a higher prevalence of *non-representative* results.


## Project structure

## Code

### Analysis

Code to analyse the data and generate the figure used in paper 1.

### Local R-only version

Used to generate the simulations for n=100.

### Old versions

Contains old versions of the code. Both clean R versions and an initial Rcpp version written by Sebastian.

### Uppmax-ver

Version of the code made to be run on UPPMAX.


## Data sets

Rds objects with 1k draws from each DGP, the index indicates which design matrix was used.

## Images

Some saved images from runs. Don't Kondo this away! Version 1 contains images from the first couple of runds, the bootplots are for different desgin matrices than those used in the paper.

## tex-coding

### Test

Tex document used to write out formatted tables with results.

### Long version

Contains the theory and derivations relevant to this project.