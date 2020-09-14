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

seed_val <- floor(runif(1) * 1e9)
set.seed(seed_val)
design_mat <- dgp(n_obs, 2, 2, TRUE)
n_obs <- 1e2 # Sample size
n_parents <- 1e3 # no data sets to boostrap from
n_bss <- 1e4 # no bootstrap replicates per parent
sim_reps <- 1e3 # no reps to determine true sampling variance
dfs <- c(3, 4, 5, 6, 7, 8, 9, 10) # degrees of freedom of the dgp
sigma2 <- 1 # error variance of misspecified models, 0 means estimated freely


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
# variance. If you want to specify the variance of the
# misspecified model, do this using *sigma2*. If you do
# not want to specify the error variance, set *sigma2*
# to be zero.


##################################
### BASELINE SAMPLING VARIANCE ###
##################################

sim_baseline_t <- function(df, n_obs, sim_reps, design_etc, sigma2) {

    log_bf <- rep(NA, sim_reps)
    data <- design_etc

    if (sigma2 == 0) {

        for (i in seq_len(sim_reps)) {

            y <- data[, 1] + sqrt((df - 2) / df) * rt(n_obs, df)
            m1 <- lm(y ~ 0 + data[, 2])
            m2 <- lm(y ~ 0 + data[, 3])

            log_ml1 <- sum(pnorm(y, fitted(m1), summary(m1)$sigma, log = TRUE))
            log_ml2 <- sum(pnorm(y, fitted(m2), summary(m2)$sigma, log = TRUE))
            log_bf[i] <- log_ml1 - log_ml2
        }

    } else {

        for (i in seq_len(sim_reps)) {

            y <- data[, 1] + sqrt((df - 2) / df) * rt(n_obs, df)
            m1 <- lm(y ~ 0 + data[, 2])
            m2 <- lm(y ~ 0 + data[, 3])

            log_ml1 <- sum(pnorm(y, fitted(m1), sqrt(sigma2), log = TRUE))
            log_ml2 <- sum(pnorm(y, fitted(m2), sqrt(sigma2), log = TRUE))
            log_bf[i] <- log_ml1 - log_ml2
        }

    }

    return(log_bf)

}


# Set up parallelization

workers <- length(dfs)
cl <- makeCluster(workers)
clusterEvalQ(cl, {
        library(dgpsim)
})

# Run the simulation in parallel

Sys.time()
starting_time <- Sys.time()
resdat <- parSapply(cl, dfs, sim_baseline_t,
            n_obs, sim_reps, design_mat, sigma2)
Sys.time() - starting_time

stopCluster(cl)

# Reasonable names for the data frame with
# simulation results

names_vec <- c()
for (i in seq_len(length(dfs))) {
    name <- paste("df", dfs[i], sep = "")
    names_vec <- c(names_vec, name)
}
colnames(resdat) <- names_vec
resdat <- data.frame(resdat)

#########################
### FOR THE BOOTSTRAP ###
#########################


sim_baseline_t_boot <- function(df, n_obs, design_mat,
                                n_parents, n_bss, sigma2) {

    n_data_sets <- n_parents
    n_bootstrap_reps <- n_bss
    log_bf <- matrix(NA, ncol = n_data_sets, nrow = n_bootstrap_reps)
    data <- design_mat

    if (sigma2 = 0) {

        for (i in seq_len(n_data_sets)) {

            data[, 1] <- data[, 1] + rt(n_obs, df)

            for (j in seq_len(n_bootstrap_reps)) {

                index <- sample(seq_len(n_obs), n_obs, replace = TRUE)
                bss <- data[index, ]
                mod1 <- lm(bss[, 1] ~ 0 + bss[, 2])
                mod2 <- lm(bss[, 1] ~ 0 + bss[, 3])
                log_ml1 <- sum(pnorm(bss[, 1],
                               fitted(mod1),
                               summary(mod1)$sigma,
                               log = T))
                log_ml2 <- sum(pnorm(bss[, 1],
                               fitted(mod2),
                               summary(mod2)$sigma,
                               log = T))
                log_bf[j, i] <- log_ml1 - log_ml2

            }

        }

    } else {

        for (i in seq_len(n_data_sets)) {

            data[, 1] <- data[, 1] + rt(n_obs, df)

            for (j in seq_len(n_bootstrap_reps)) {

                index <- sample(seq_len(n_obs), n_obs, replace = TRUE)
                bss <- data[index, ]
                mod1 <- lm(bss[, 1] ~ 0 + bss[, 2])
                mod2 <- lm(bss[, 1] ~ 0 + bss[, 3])
                log_ml1 <- sum(pnorm(bss[, 1],
                               fitted(mod1),
                               sqrt(sigma2),
                               log = T))
                log_ml2 <- sum(pnorm(bss[, 1],
                               fitted(mod2),
                               sqrt(sigma2),
                               log = T))
                log_bf[j, i] <- log_ml1 - log_ml2

        }

    }

    }

    return(log_bf)
}

##############################
### SET UP PARALLELIZATION ###
##############################

cl <- makeCluster(length(dfs))
clusterEvalQ(cl, {
        library(dgpsim)
})

Sys.time() # Just to be able to see when the sim started
time_start <- Sys.time()
dfs_boot <- parLapply(cl, dfs, sim_baseline_t_boot,
                    n_obs, design_mat, n_parents, n_bss)
Sys.time() - time_start

stopCluster(cl)