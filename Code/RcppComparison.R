# Need to source 1_data_generation.R first

df <- 5
n_obs <- 1000
sim_reps <- 200
data <- cbind(rnorm(n_obs), rnorm(n_obs), rnorm(n_obs))
sigma2 <- 0


microbenchmark::microbenchmark(R = sim_baseline_t(df, n_obs, sim_reps, data, sigma2),
Cpp = sim_baseline_t_Rcpp(df, n_obs, sim_reps, data, sigma2))
