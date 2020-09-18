# Clean memory and load packages
rm(list = ls())
source("Code/0_packages.R")

#########################
### PARAMETERS TO SET ###
#########################

seed_val <- floor(runif(1) * 1e9)
set.seed(seed_val)
n_obs <- 1e2 # Sample size
design_mat <- dgp(n_obs, 2, 2, trueMean = TRUE)
n_parents <- 3e3 # no data sets to boostrap from
n_bss <- 3e3 # no bootstrap replicates per parent
sim_reps <- 1e6 # no reps to determine true sampling variance
dfs <- c(3, 4, 5, 6, 7, 8, 9, 10) # degrees of freedom of the dgp
sigma2 <- 0 # error variance of misspecified models, 0 means estimated freely

####################
### RUN ALL CODE ###
####################

Sys.time()
time_tot <- Sys.time()
source("Code/2_data_generation.R")
source("Code/3_bootstrap_exploration.R")
Sys.time() - time_tot