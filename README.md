# Overconfidence in bootstrap-based estimates of model fit based on likelihoods

Many measure used to evaluate models are based on likelihoods. For a given model, or model comparison, any likelihood based estimate will vary somewhat when evaluated based on different samples from a given data-generating process. We can think of this as a sampling variability inherent in a certain model evaluation or comparison. Bootstrapping is often used to estimate the uncertainty of estimates, and as it approximates the data-generating process it could be used to estimate this sampling variability.

My conjecture is that the variability in the bootstrap based estimate, compared to that based on samples from the dgp, will vary greatly as a function of the entropy of the data-generating process, fatter tails leading to a higher prevalence of *non-representative* results.

## To-do




## Project structure

### Analysis

Code to analyse the data (most of which is generated using uppmax). Also contains a data frame with all draws. Results for the small data set (n=100) are generated locally.

#### all_data.RData

A data frame where I store all my runs.

### Code/Old versions

Contains old versions of the code. Both clean R versions and an initial Rcpp version written by Sebastian.

### Uppmax-ver

Version of the code optimized to run on UPPMAX.


### tex-coding

#### Test

Tex document used to write out formatted tables with results.

#### Long version

Contains the theory and derivations relevant to this project.

### Images

Some saved images from runs. Don't Kondo this away!

### Appendix draft

Contains a draft of the appendix text.



### Hurdles on the way: Making sampling distributions with different tails comparable

A first step is to simulate the actual sampling variance by using a large amount of draws from the data-generating process.

The sampling variance will be different depending on what value I use for *df*. Since I'm interested in if the is a radicalizing effect from fat tails and not a general increase in variance, I want to somehow make the sampling distributions for the different values of *df* comparable.

Using the t-distribution, I can simply rescale the error in the dgp so that the variance is always equal to one. Since the variance of the t-distribution is df / (df - 2) this is straight forward. Note that for two or less degrees of freedom the variance is undefined, so there is no way of making it comparable. (I still feel like it's interesting to look at the Cauchy case.)

# Hurdles along the way: Measuring the radicalization

How do we measure if the bootstrap-estimated sampling variance differs more or less? There are many potential approaches to take.

## Variance decomposition / multilevel modeling

Given that the bootstrap samples can be grouped based on their parent sample, it should be easy to decompose the total variance into two parts and look at how the percentage of variance at each level changes when we change the degrees of freedom.

## Graphing it out

Plot a whole bunch of bootstrap based estimates of the sampling variance together with the true (simulation based) value.

## Probability of radicalization

Let's call an estimate of sampling variance *radicalized* if it is greater than the true sampling variance by some factor. It is then straightforward to calculate the percentage of samples that are radicalized for each degree of freedom and see if it changes in a systematic way.