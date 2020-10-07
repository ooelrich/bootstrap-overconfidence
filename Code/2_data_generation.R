########################
### HELPER FUNCTIONS ###
########################

sigma_fn_gen <- function(sigma2) {
    if (sigma2 > 0) {
        sigma <- sqrt(sigma2)
        function(m) { sigma }
    } else {
        function(m) { summary(m)$sigma }
    } 
}

error_matrix <- function(n_row, n_col, df) {
    t_errors <- sqrt((df - 2) / df) * rt(n_row * n_col, df)
    return(matrix(t_errors, n_row, n_col))
}

log_bf_fun <- function(data, sigma_fn) {
    m1 <- lm(data[, 1] ~ 0 + data[, 2])
    m2 <- lm(data[, 1] ~ 0 + data[, 3])
    log_ml1 <- sum(dnorm(data[, 1], fitted(m1), sigma_fn(m1), log = T))
    log_ml2 <- sum(dnorm(data[, 1], fitted(m2), sigma_fn(m2), log = T))
    return(log_ml1 - log_ml2)
}

##################################
### BASELINE SAMPLING VARIANCE ###
##################################

sim_baseline_t <- function(df, n_obs, sim_reps, design_mat, sigma2) {

    sigma_fn <- sigma_fn_gen(sigma2)
    log_bf <- numeric(sim_reps)

    for (i in seq_len(sim_reps)) {        
        data_temp <- design_mat
        data_temp[, 1] <- design_mat[, 1] + rt(n_obs, df) * sqrt((df - 2) / 2)
        log_bf[i] <- log_bf_fun(data_temp, sigma_fn)
    }

    return(log_bf)
}

#########################
### FOR THE BOOTSTRAP ###
#########################

sim_baseline_t_boot <- function(df, n_obs, design_mat,
                                n_parents, n_bss, sigma2) {

    log_bf <- matrix(NA_real_, ncol = n_parents, nrow = n_bss)
    sigma_fn <- sigma_fn_gen(sigma2)
    # t_err <- error_matrix(n_row = n_obs, n_col = n_parents, df)
    #log_bf_jack <- matrix(NA_real_, ncol = n_parents, nrow = n_obs)
    all_obs <- seq_len(n_obs)

    for (i in seq_len(n_parents)) {

        data_temp <- design_mat
        data_temp[, 1] <- design_mat[, 1] + rt(n_obs, df) * sqrt((df - 2) / 2)

        # bootstrap estimate
        for (j in seq_len(n_bss)) {
            index <- sample(seq_len(n_obs), n_obs, replace = TRUE)
            bss <- data_temp[index, ]
            log_bf[j, i] <- log_bf_fun(bss, sigma_fn)
        }

        #jackknife estimate
        #for (j in seq_len(n_obs)){
        #    jks <- data_temp[all_obs[-j], ]
        #    log_bf_jack[j, i] <- log_bf_fun(jks, sigma_fn)
        #}
    }
    
    log_bf_jack <- 0

    return(list(log_bf, log_bf_jack))
}

###################################
### Generate baseline variances ###
###################################

workers <- length(dfs)
cl <- makeCluster(workers)
clusterExport(cl, c("sigma_fn_gen", "log_bf_fun"))
clusterEvalQ(cl, {
        library(dgpsim)
})

Sys.time()
starting_time <- Sys.time()
baseline_dat <- parSapply(cl, dfs, sim_baseline_t, n_obs,
                        sim_reps, design_mat, sigma2)
#dfs_boot <- parLapply(cl, dfs, sim_baseline_t_boot, n_obs,
#                    design_mat, n_parents, n_bss, sigma2)
stopCluster(cl)
Sys.time() - starting_time
#names_vec <- c()
#for (i in seq_len(length(dfs))) {
#    name <- paste("df", dfs[i], sep = "")
#    names_vec <- c(names_vec, name)
#}
#colnames(baseline_dat) <- names_vec
#baseline_dat <- data.frame(baseline_dat)