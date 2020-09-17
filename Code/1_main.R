# Clean memory and load packages
rm(list = ls())
source("Code/0_packages.R")

#########################
### PARAMETERS TO SET ###
#########################

seed_val <- floor(runif(1) * 1e9)
set.seed(seed_val)
n_obs <- 1e2 # Sample size
design_mat <- dgp(n_obs, 2, 2) # NEED TO UPDATE
n_parents <- 1e2 # no data sets to boostrap from
n_bss <- 1e3 # no bootstrap replicates per parent
sim_reps <- 1e4 # no reps to determine true sampling variance
dfs <- c(3, 5) # degrees of freedom of the dgp
sigma2 <- 0 # error variance of misspecified models, 0 means estimated freely

####################
### RUN ALL CODE ###
####################

Sys.time()
time_tot <- Sys.time()
source("Code/2_data_generation.R")
source("Code/3_bootstrap_exploration.R")
Sys.time() - time_tot
