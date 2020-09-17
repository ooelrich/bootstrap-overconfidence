

########################
### HELPER FUNCTIONS ###
########################

sigma_fn <- if (sigma2 > 0) {
    function(m) {
        sqrt(sigma2)
    }
} else {
    function(m) {
        summary(m)$sigma
    }     
}

##################################
### BASELINE SAMPLING VARIANCE ###
##################################

sim_baseline_t <- function(df, n_obs, sim_reps, design_etc, sigma2) {

    log_bf <- rep(NA, sim_reps)
    error_terms <- sqrt((df - 2) / df) * rt(n_obs * sim_reps, df)
    t_rand <- matrix(error_terms, n_obs, sim_reps)

    for (i in seq_len(sim_reps)) {

        y <- design_etc[, 1] + t_rand[, i]
        m1 <- lm(y ~ 0 + design_etc[, 2])
        m2 <- lm(y ~ 0 + design_etc[, 3])

        log_ml1 <- sum(dnorm(y, fitted(m1), sigma_fn(m1), log = TRUE))
        log_ml2 <- sum(dnorm(y, fitted(m2), sigma_fn(m2), log = TRUE))
        log_bf[i] <- log_ml1 - log_ml2

    }

    return(log_bf)

}


#log_linear_regression_variable <- function(sigma, y, m) {
#    sum(dnorm(y, fitted(m), sigma(m), log = TRUE))
#}

###################################
### Generate baseline variances ###
###################################

workers <- length(dfs)
cl <- makeCluster(workers)
clusterExport(cl, "sigma_fn")
clusterEvalQ(cl, {
        library(dgpsim)
})

Sys.time()
starting_time <- Sys.time()
baseline_dat <- parSapply(cl, dfs, sim_baseline_t,
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

sim_baseline_t_boot <- function(df, n_obs, design_mat,
                                n_parents, n_bss, sigma2) {

    log_bf <- matrix(NA, ncol = n_parents, nrow = n_bss)


    for (i in seq_len(n_parents)) {

        data_temp <- design_mat
        data_temp[, 1] <- design_mat[, 1] + rt(n_obs, df)

        for (j in seq_len(n_bss)) {

            index <- sample(seq_len(n_obs), n_obs, replace = TRUE)
            bss <- data_temp[index, ]
            mod1 <- lm(bss[, 1] ~ 0 + bss[, 2])
            mod2 <- lm(bss[, 1] ~ 0 + bss[, 3])
            log_ml1 <- sum(dnorm(bss[, 1],
                               fitted(mod1),
                               sigma_fn(mod1),
                               log = T))
            log_ml2 <- sum(dnorm(bss[, 1],
                               fitted(mod2),
                               sigma_fn(mod2),
                               log = T))
            log_bf[j, i] <- log_ml1 - log_ml2

        }

    }

    return(log_bf)
}

######################################
### Generate bootstrap simulations ###
######################################

cl <- makeCluster(length(dfs))
clusterExport(cl, "sigma_fn")
clusterEvalQ(cl, {
        library(dgpsim)
})

Sys.time() # Just to be able to see when the sim started
time_start <- Sys.time()
dfs_boot <- parLapply(cl, dfs, sim_baseline_t_boot,
                        n_obs, design_mat, n_parents, n_bss, sigma2)
Sys.time() - time_start
stopCluster(cl)