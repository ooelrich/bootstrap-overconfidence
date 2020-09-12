library(dgpsim)
library(ggplot2)
library(cowplot)
library(reshape2)
library(parallel)
library(dplyr)


# The data genarating process
# is y = X1 + X2 + e, where e
# is student's t with 1, 10 or
# 100 degrees of freedom

#############################
### DESCRIPTION OF SCRIPT ###
#############################

# This script starts with a design matrix. Given this design matrix
# it generates *n_parent* data sets, each obtained by adding an error
# term following a t distribution with *df* degrees of freedom to the
# design matrix (by design matrix I mean a design matrix with an included
# response variable, and there is no intercept included).
# Each of these data sets are used to generate *n_bss* boostrap samples.


sim_baseline_t_boot <- function(df, n_obs, design_mat, n_parents, n_bss) {

    n_data_sets <- n_parents
    n_bootstrap_reps <- n_bss
    log_bf <- matrix(NA, ncol = n_data_sets, nrow = n_bootstrap_reps)
    data <- design_mat

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

    return(log_bf)
}

###############################
### SET UP PARAMETER VALUES ###
###############################

# Note that it's a good idea to use a seed for the data, so that
# you can generate the true sampling distribution for the exact
# same design matrix

# The degrees of freedom is given as a vector, this is done for
# the parallelization set up, each setup will be sent to one core
# so make sure you have enough cores...

set.seed(861226)
design_mat <- dgp(n_obs, 2, 2, TRUE)
n_obs <- 1e2
n_parents <- 1e2
n_bss <- 1e4
dfs <- c(3, 5, 10, 100, 1000)


##############################
### Set up parallelization ###
##############################

workers <- length(dfs)
cl <- makeCluster(workers)
clusterEvalQ(cl, {
        library(dgpsim)
})

Sys.time() # Just to be able to see when the sim started
time_start <- Sys.time()
all_dfs_boot <- parLapply(cl, dfs, sim_baseline_t_boot,
                    n_obs, design_mat, n_parents, n_bss)
Sys.time() - time_start

stopCluster(cl)

save(all_dfs_boot, file = "boostrapSmallSaturday.RData")