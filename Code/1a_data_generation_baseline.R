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