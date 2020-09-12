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

#########################
### PARAMETERS TO SET ###
#########################

set.seed(861226)
design_mat <- dgp(n_obs, 2, 2, TRUE)
n_obs <- 1e2 # Sample size
n_parents <- 1e1 # no data sets to boostrap from
n_bss <- 1e2 # no bootstrap replicates per parent
sim_reps <- 1e4 # no reps to determine true sampling variance
dfs <- c(3, 4, 5, 6, 7) # degrees of freedom of the dgp

# Each experiment is run using a specific design
# matrix, which includes a response variable WITHOUT
# any noise added to it (the true response). This
# response matrix is stored in *design_mat*. The
# number of observations in each data set is stored in
# *n_ob*. *n_parents* determine how many different data
# sets are used to do the bootstrapping. The number of
# boostrap samples drawn for each parent is given by
# *n_bss*. Everything is run once for each values of
# degrees of freedom in the *dfs* vector. Note that
# the code is parallized in such a way that it runs one
# version for each value in *dfs* at the same time, so
# don't specify more different degrees of freedom than
# you have cores (times two). *sim_reps* determines the
# number of replicates used to obtain the true sampling
# variance.