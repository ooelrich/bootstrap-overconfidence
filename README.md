# Overconfidence in boostrap-based estimates of model fit based on likelihoods

Many measure used to evaluate models are based on likelihoods. For a given model, or for a given model comparison, any likelyhood based estimate will vary somewhat when evaluated based on different samples from a given data-generating process. We can think of this as a sampling variability inherent in a certain model evaluation or comparison. Bootstrapping is often used to estimate the uncertainty of estimates, and as it approximates the data-generating process it could be used to estimate this sampling variability.

My conjecture is that the variability in the boostrap based estimate, compared to that based on samples from the dgp, will vary greatly as a function of the entropy of the data-generating process, fatter tails leading to more *non-representative* results.

## Testing out the hypothesis with a simulation

Consider the data-generating process

y = beta_1 x_1 + beta_2 x2 + e

where the error term follows a t distribution with *df* degrees of freedom.

I want to examine what happens to the usefulness of boostrapping to estimate the sampling variance of the Bayes factor for the models

y = beta_1^* x_1 + e
y = beta_2^* x_2 + e

where the error terms are normal. These two models are misspecified in two ways: they each lack one of the covariates of interest, and the have the wrong error term distribution.

My conjecture implies that esimating the sampling varaince using the nonparametric boostrap will be more risky when the degrees of freedom is low.

To simplify matters, I will ignore the whole Bayesian machinery and just calculate the likelihood under each (misspecified) model, since it's the instability in the estimate of the variability of a likelihood function I am interested in.

### Hurdles on the way

#### Making sampling distributions with different tails comparable

A first step is to simulate the actual sampling variance by using a large amount of draws from the data-generating process.

The sampling variance will be different depending on what value I use for *df*. Since I'm interested in if the is a radicalizing effect from fat tails and not a general increase in varaince, I want to somehow make the sampling distributions for the different values of *df* comparable.

Using the t-distrubution, I can simply rescale the error in the dgp so that the variance is always equal to one. Since the variance of the t-distribution is df / (df - 2) this is straight forward. Note that for two or less degrees of freedom there is no variance, so there is no way of making it comparable. (I still feel like it's interesting to look at the Cauchy case.)

#### Measuring the radicalization

How do we measure if the bootstrap-estimated sampling variance differs more or less? There are many potential approaches to take.

##### Variance decomposition / multilevel modeling

Given that the bootstrap samples can be grouped based on their parent sample, it should be easy to decompose the total variance into two parts and look at how the percentage of varaince at each level changes when we change the degrees of freedom.

##### Graphing it out

Plot a whole bunch of bootstrap based estimates of the sampling varaince together with the true (simulation based) value.

##### Probability of radicalization

Let's call an estiamate of sampling variance *radicalized* if it is greater than the true sampling variance by some factor. It is then straightforward to calculate the percentage of samples that are radicalized for each degree of freedom and see if it changes in a systematic way.


## Some simulation results

The true sampling variance seems to be drecreasing as the degrees of freedom are increasing, I always get numbers like
(df = sampling variance) 3 = 2.2, 5 = 2.0, 10 = 1.8, 100 = 1.7, 1000 = 1.7 (still smaller, 1.71 vs 1.69).

