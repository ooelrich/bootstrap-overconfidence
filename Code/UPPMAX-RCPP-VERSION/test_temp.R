library(RcppArmadillo)
library(RcppDist)
library(dgpsim)
library(mvtnorm)
library(parallel)

Rcpp::sourceCpp("/proj/dennis/test-r/funsRcpp2.cpp")
 

initialize_all_data <- function() {
  all_data <- data.frame(boot_reps = integer(),
                        n_obs = integer(),
                        deg_f = double(),
                        lbf = double(),
                        var_lbf = double(),
                        p_radical = double())
  save(all_data, file = "/proj/dennis/nobackup/all_data.RData")
}

initialize_all_data()

load("/proj/dennis/nobackup/data100.RData")
load("/proj/dennis/nobackup/data500.RData")
load("/proj/dennis/nobackup/data1000.RData")

ncl <- 20

for (i in 1:3) {

    if (i == 1) {
        data_set <- design_mat_100_a
    } else if (i == 2) {
        data_set <- design_mat_500_a
    } else if (i == 3) {
        data_set <- design_mat_1000_a
    }

    for (df in c(2.5, 5, 30)) {

        new_rows <- mclapply(rep(1, ncl), generate_rows, df = df,
                            design_mat = data_set, n_bss = 1e3, mc.cores = ncl)
        new_rows <- do.call("rbind", new_rows)
        rm(all_data)
        load("/proj/dennis/nobackup/all_data.RData")
        all_data <- rbind(all_data, new_rows)
        save(all_data, file = "/proj/dennis/nobackup/all_data.RData")
    }
}
