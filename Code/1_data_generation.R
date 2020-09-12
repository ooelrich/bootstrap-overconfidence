##################################
### BASELINE SAMPLING VARIANCE ###
##################################

sim_baseline_t <- function(df, n_obs, sim_reps, design_etc) {

    log_bf <- rep(NA, sim_reps)
    data <- design_etc

    for (i in seq_len(sim_reps)) {

        y <- data[, 1] + sqrt( (df - 2) / df) * rt(n_obs, df)
        m1 <- lm(y ~ 0 + data[, 2])
        m2 <- lm(y ~ 0 + data[, 3])

        log_ml1 <- sum(pnorm(y, fitted(m1), summary(m1)$sigma, log = TRUE))
        log_ml2 <- sum(pnorm(y, fitted(m2), summary(m2)$sigma, log = TRUE))
        log_bf[i] <- log_ml1 - log_ml2
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
            n_obs, sim_reps, design_mat)
Sys.time() - starting_time

stopCluster(cl)

# Reasonable names for the data frame with
# simulation results

names_vec <- c()
for (i in 1:length(dfs)) {
    name <- paste("df", dfs[i], sep = "")
    names_vec <- c(names_vec, name)
}
colnames(resdat) <- names_vec
resdat <- data.frame(resdat)

#########################
### FOR THE BOOTSTRAP ###
#########################


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