# Overconfidence in boostrap-based estimates of model fit based on likelihoods

Many measure used to evaluate models are based in some way on likelihoods. For a given model, or for a given model comparison, any likelyhood based estimate will vary somewhat when evaluated based on different samples from a given data-generating process. Bootstrapping is often used to estimate the uncertainty of estimates. My conjecture is that the variability in the boostrap based estimate, as opposed to that based on samples from the dgp, will vary greatly as a function of the entropy of the data-generating process.

## Testing out the hypothesis with a simulation

Consider the data-generating process

\\( y = \beta_1 x_1 + \beta_2 x2 + \epsilon \\)

where the error term follows a t distribution with *df* degrees of freedom. 