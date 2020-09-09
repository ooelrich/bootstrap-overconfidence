# Overconfidence in boostrap-based estimates of model fit based on likelihoods

Many measure used to evaluate models are based in some way on likelihoods. For a given model, or for a given model comparison, any likelyhood based estimate will vary somewhat when evaluated based on different samples from a given data-generating process. Bootstrapping is often used to estimate the uncertainty of estimates. My conjecture is that the variability in the boostrap based estimate, as opposed to that based on samples from the dgp, will vary greatly as a function of the entropy of the data-generating process.

## Testing out the hypothesis with a simulation

Consider the data-generating process

y = beta_1 x_1 + beta_2 x2 + e

where the error term follows a t distribution with *df* degrees of freedom.

I want to examine what happens to the usefulness of boostrapping to estimate the sampling variance of the Bayes factor for the models

y = beta_1^* x_1 + e
y = beta_2^* x_2 + e

where the error terms are normal. These two models are misspecified in two ways: they each lack one of the covariates of interest, and the have the wrong error term distribution.

My conjecture implies that esimating the sampling varaince using the nonparametric boostrap will be more risky when the degrees of freedom is low.

### Hurdles on the way

#### Making sampling distributions with different tails comparable

A first step is to simulate the actual sampling variance by using a large amount of draws from the data-generating process.

The sampling variance will be different depending on what value I use for *df*. Since I'm interested in if the is a radicalizing effect from fat tails and not a general increase in varaince, I want to somehow make the sampling distributions for the different values of *df* comparable.

# Measuring the radicalization

