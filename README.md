# Overconfidence in bootstrap-based estimates of model fit based on likelihoods

Many measure used to evaluate models are based on likelihoods. For a given model, or model comparison, any likelihood based estimate will vary somewhat when evaluated based on different samples from a given data-generating process. We can think of this as a sampling variability inherent in a certain model evaluation or comparison. Bootstrapping is often used to estimate the uncertainty of estimates, and as it approximates the data-generating process it could be used to estimate this sampling variability.

My conjecture is that the variability in the bootstrap based estimate, compared to that based on samples from the dgp, will vary greatly as a function of the entropy of the data-generating process, fatter tails leading to a higher prevalence of *non-representative* results.

## To-do

- [ ] Go through and structure everything
- [ ] Go through the code with a fine tooth comb


## Project structure

### Code/Main version

Contains the main version of the code.

### Code/Old version

Contains an old version of the code, should probably be deleted.

### Code/Rcpp version

Contains a Rcpp implementation of the code written by Sebastian.

### Code/Till-UPPMAX

Code to be run on UPMMAX, work in process.

### all_data.RData

A data frame where I store all my runs.

### tex-coding

Tex document used to write out formatted tables with results.

### Images

Some saved images from runs. Don't Kondo this away!

### Appendix draft

Contains a draft of the appendix text.



### Hurdles on the way: Making sampling distributions with different tails comparable

A first step is to simulate the actual sampling variance by using a large amount of draws from the data-generating process.

The sampling variance will be different depending on what value I use for *df*. Since I'm interested in if the is a radicalizing effect from fat tails and not a general increase in variance, I want to somehow make the sampling distributions for the different values of *df* comparable.

Using the t-distribution, I can simply rescale the error in the dgp so that the variance is always equal to one. Since the variance of the t-distribution is df / (df - 2) this is straight forward. Note that for two or less degrees of freedom the variance is undefined, so there is no way of making it comparable. (I still feel like it's interesting to look at the Cauchy case.)

### Hurdles on the way: Measuring the radicalization

How do we measure if the bootstrap-estimated sampling variance differs more or less? There are many potential approaches to take.

### Hurdles on the way: Variance decomposition / multilevel modeling

Given that the bootstrap samples can be grouped based on their parent sample, it should be easy to decompose the total variance into two parts and look at how the percentage of variance at each level changes when we change the degrees of freedom.

## Graphing it out

Plot a whole bunch of bootstrap based estimates of the sampling variance together with the true (simulation based) value.

## Probability of radicalization

Let's call an estimate of sampling variance *radicalized* if it is greater than the true sampling variance by some factor. It is then straightforward to calculate the percentage of samples that are radicalized for each degree of freedom and see if it changes in a systematic way.


## Some simulation results

The true sampling variance seems to be decreasing as the degrees of freedom are increasing, I always get numbers like
(df = sampling variance) 3 = 2.2, 5 = 2.0, 10 = 1.8, 100 = 1.7, 1000 = 1.7 (still smaller, 1.71 vs 1.69).

