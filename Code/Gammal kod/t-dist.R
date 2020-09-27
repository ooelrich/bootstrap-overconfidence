
df_set <- c(1, 2, 5, 100)
dgp_sims <- 100000 # number of obs used to estimate true likelihood variance
sample_size <- 100 
like_var_true <- c() # vector to store the (estimated) true vars
boot_reps <- 2000 # How many boostrap samples to use to estimate the 
# distribution of the variance
boot_samps <- 2000 # how many boostrap reps to use for each estimate
like_boot_est <- matrix(NA, nrow = boot_reps, ncol = length(df_set))

for(i in 1:4){

    like_dgp_sim <- rep(NA, dgp_sims)
    
    for(j in seq_len(dgp_sims)){
        like_dgp_sim[j] <- sum(dt(rt(sample_size, i), i, log = TRUE))
    }

    like_var_true <- c(like_var_true, var(like_dgp_sim))

    for(k in seq_len(boot_reps)){
        bss <- rt(sample_size, i)
        bootsamps <- c()
        for(l in seq_len(boot_samps)){
            bootsamps[l] <- sum(dt(sample(bss, replace = TRUE), i, log = TRUE))
        }
        like_boot_est[k, i] <- var(bootsamps)
    }

}

var(like_boot_est[,1])
like_var_true[1]

var(like_boot_est[,4])
like_var_true[4]


par(mfrow=c(2,2))

hist(like_boot_est[, 1], breaks = 20)
abline(v = like_var_true[1], col = "red")

hist(like_boot_est[, 2], breaks = 20)
abline(v = like_var_true[2], col = "red")

hist(like_boot_est[, 3], breaks = 20)
abline(v = like_var_true[3], col = "red")

hist(like_boot_est[, 4], breaks = 20)
abline(v = like_var_true[4], col = "red")





for(i in 1:1000){
    bss <- rt(100, df)
    like_bss <- c()
    bootsamps <- c()
    for(j in 1:10000){
        bootsamps[j] <- sum(dt(sample(bss, replace = TRUE), df, log = TRUE))
    }
    boot_var[i] <- var(bootsamps)
}


hist(like_dgp)
hist(boot_var, breaks = 20)
abline(v = var(like_dgp), col = "red")

var(like_dgp)