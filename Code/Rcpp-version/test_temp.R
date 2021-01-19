library(RcppArmadillo)
library(RcppDist)
library(dgpsim)
library(mvtnorm)

Rcpp::sourceCpp("Code/Rcpp-version/funsRcpp2.cpp")
 

initialize_all_data <- function() {
  all_data <- data.frame(boot_reps = integer(),
                        n_obs = integer(),
                        deg_f = double(),
                        lbf = double(),
                        var_lbf = double(),
                        p_radical = double())
  save(all_data, file = "all_data.RData")
}

set.seed(861226)
design_mat_100_a <- as.matrix(dgp(100, c(1, 1), true_mean = TRUE))
design_mat_500_a <- as.matrix(dgp(500, c(1, 1), true_mean = TRUE))
design_mat_1000_a <- as.matrix(dgp(1000, c(1, 1), true_mean = TRUE))

# wrapper for the Rcpp stuff

bootstrap_batch <- function(deg_f, n_bss, runs, design_mat) {
                              
  n_obs <- nrow(design_mat)
  sim_vals <- generate_rows(df, n_obs, design_mat, n_bss, runs)
  cbind(rep(n_bss, runs), rep(n_obs, runs), rep(deg_f, runs))

}

aaa <- Sys.time()
for (i in 1:3) {

    if (i == 1) {
        data_set <- design_mat_100_a
    } else if (i == 2) {
        data_set <- design_mat_500_a
    } else if (i == 3) {
        data_set <- design_mat_1000_a
    }

    print("Starting data set: ")
    print(i)

    for (df in c(2.5, 5, 30)) {

        new_rows <- generate_rows(df = df, design_mat = data_set,
                                  n_bss = 1e3, n_parents = 1)

        rm(all_data)
        load("all_data.RData")
        all_data <- rbind(all_data, new_rows)
        save(all_data, file = "all_data.RData")
        print("Current number of rows: ")
        print(nrow(all_data))
    }
}
Sys.time() - aaa
