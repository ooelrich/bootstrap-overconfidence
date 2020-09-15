# Need to source 1_data_generation.R first
Rcpp::sourceCpp("funsRcpp.cpp")
df <- 5
n_obs <- 100
sim_reps <- 5
data <- cbind(rnorm(n_obs), rnorm(n_obs), rnorm(n_obs))
sigma2 <- 0
n_bss <- 2


microbenchmark::microbenchmark(R = sim_baseline_t(df, n_obs, sim_reps, data, sigma2),
Cpp = sim_baseline_t_Rcpp(df, n_obs, sim_reps, data, sigma2))

microbenchmark::microbenchmark(
    R = sim_baseline_t_boot(df, n_obs, data, sim_reps, n_bss, sigma2),
    Cpp = sim_baseline_t_boot_Rcpp(df, n_obs, data, sim_reps, n_bss, sigma2))
