aaa <- Sys.time()

library(Rcpp)
library(RcppArmadillo)
library(devtools)
library(mvtnorm)

if ("dgpsim" %in% installed.packages()[, 1]) {
    library(dgpsim)
} else {
    install_github("ooelrich/dgpsim")
    library(dgpsim)
}

Rcpp::sourceCpp("/proj/dennis/test-r/funsRcpp2.cpp")
 
slurm_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
n_obs <- as.numeric(Sys.getenv("N_OBS"))

if ((slurm_id %% 3) == 1) {
    df <- 2.5
} else if ((slurm_id %% 3) == 2) {
    df <- 5
} else {
    df <- 30
}

if (n_obs == 500) {
    load("/proj/dennis/nobackup/data500.RData")
    data_set <- as.matrix(design_mat_500_a)
} else if (n_obs == 1000) {
    load("/proj/dennis/nobackup/data1000.RData")
    data_set <- as.matrix(design_mat_1000_a)
}

new_rows <- generate_rows(df = df, design_mat = data_set, n_bss = 1e3, n_parents = 5)
saveRDS(new_rows, file = sprintf("/proj/dennis/nobackup/data%s.Rds", slurm_id))

bbb <- Sys.time() - aaa
print(bbb)