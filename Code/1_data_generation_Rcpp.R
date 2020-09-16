#######################
### PACKAGES NEEDED ###
#######################

library(dgpsim)
library(ggplot2)
library(cowplot)
library(reshape2)
library(parallel)
library(dplyr)
library(lme4)
library(gridExtra)
library(microbenchmark)
library(RcppArmadillo)

Rcpp::sourceCpp("Code/funsRcpp.cpp")

##################################
### BASELINE SAMPLING VARIANCE ###
##################################



###################################
### Generate baseline variances ###
###################################

workers <- length(dfs)
cl <- makeCluster(workers)
clusterEvalQ(cl, {
        library(Rcpp)
        library(dgpsim)
        library(dgpsim)
        library(ggplot2)
        library(cowplot)
        library(reshape2)
        library(parallel)
        library(dplyr)
        library(lme4)
        library(gridExtra)
        library(microbenchmark)
        library(RcppArmadillo)
        Rcpp::sourceCpp("Code/funsRcpp.cpp")
})

Sys.time()
starting_time <- Sys.time()
baseline_dat <- parSapply(cl, dfs, sim_baseline_t_Rcpp,
            n_obs, sim_reps, design_mat, sigma2)
Sys.time() - starting_time
stopCluster(cl)

names_vec <- c()
for (i in seq_len(length(dfs))) {
    name <- paste("df", dfs[i], sep = "")
    names_vec <- c(names_vec, name)
}
colnames(baseline_dat) <- names_vec
baseline_dat <- data.frame(baseline_dat)

#########################
### FOR THE BOOTSTRAP ###
#########################

######################################
### Generate bootstrap simulations ###
######################################

cl <- makeCluster(length(dfs))
clusterEvalQ(cl, {
        library(dgpsim)
})

Sys.time() # Just to be able to see when the sim started
time_start <- Sys.time()
dfs_boot <- parLapply(cl, dfs, sim_baseline_t_boot_Rcpp,
                        n_obs, design_mat, n_parents, n_bss, sigma2)
Sys.time() - time_start
stopCluster(cl)