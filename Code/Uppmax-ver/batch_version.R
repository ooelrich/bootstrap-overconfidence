library(Rcpp)
library(RcppArmadillo)
library(devtools)
library(mvtnorm)
library(parallel)

if ("dgpsim" %in% installed.packages()[, 1]) {
    library(dgpsim)
} else {
    install_github("ooelrich/dgpsim")
    library(dgpsim)
}

Rcpp::sourceCpp("/proj/dennis/test-r/funsRcpp2.cpp")
 
slurm_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
ncl <- as.numeric(Sys.getenv("N_CORES"))
n_obs <- as.numeric(Sys.getenv("N_OBS"))

if ((slurm_id %% 3) == 1) {
    df <- 2.5
} else if ((slurm_id %% 3) == 2) {
    df <- 5
} else {
    df <- 30
}

if (n_obs == 100) {
    load("/proj/dennis/nobackup/data100.RData")
    data_set <- as.matrix(design_mat_100_a)
} else if (n_obs == 500) {
    load("/proj/dennis/nobackup/data500.RData")
    data_set <- as.matrix(design_mat_500_a)
} else if (n_obs == 1000) {
    load("/proj/dennis/nobackup/data1000.RData")
    data_set <- as.matrix(design_mat_1000_a)
}



new_rows <- mclapply(rep(10, ncl), generate_rows, df = df,
                        design_mat = data_set, n_bss = 1e3, mc.cores = ncl)
new_rows <- do.call("rbind", new_rows)
rm(all_data)
load("/proj/dennis/nobackup/all_data.RData")
all_data <- rbind(all_data, new_rows)
save(all_data, file = "/proj/dennis/nobackup/all_data.RData")