library(reshape2)
library(ggplot2)

# Use density(whatever) to make kernel density plots

likelihood_sampling_variance_boostrap <- function(df, samp_size){

    ll_true <- rep(NA, 1e6)
    boot_matrix <- matrix(NA, 2000, 100)

    for(j in seq_len(length(ll_true))){
        ll_true[j] <- sum(dt(rt(samp_size, df), df, log = TRUE))
    }

    for(k in 1:100){        
        dat_obs <- rt(samp_size, df)
        bss_var_est <- c(NA, 2000)

        for(l in 1:2000){
            bss <- sample(dat_obs, replace = TRUE)
            bss_var_est[l] <- sum(dt(bss, df, log = TRUE))
        }

        boot_matrix[, k] <- bss_var_est
    }

    boot_matrix <- melt(data.frame(boot_matrix))
    ggplot(boot_matrix, aes(value, color = variable)) + geom_density()
    
    final_plot <- ggplot() +
                    geom_density(data = boot_matrix,
                        aes(value, color = variable)) + 
                    geom_density(data = data.frame(ll_true),
                        aes(x = ll_true),
                        color = "black",
                        size = 1.5) +
                    theme_minimal() +
                    theme(legend.position = "none")

    return(final_plot)
}


df1 <- likelihood_sampling_variance_boostrap(1, 100)
png(filename = "1df.png")
df1 
dev.off()

df10 <- likelihood_sampling_variance_boostrap(10, 100)
png(filename = "10df.png")
df10 
dev.off()

df100 <- likelihood_sampling_variance_boostrap(100, 100)
png(filename = "100df.png")
df100 
dev.off()


# Trying out a smaller sample size

df1ss50 <- likelihood_sampling_variance_boostrap(1, 50)
png(filename = "1dfss50.png")
df1ss50
dev.off()

df10ss50 <- likelihood_sampling_variance_boostrap(10, 50)
png(filename = "10dfss50.png")
df10ss50 
dev.off()

df100ss50 <- likelihood_sampling_variance_boostrap(100, 50)
png(filename = "100dfss50.png")
df100ss50 
dev.off()

df01ss50 <- likelihood_sampling_variance_boostrap(.1, 50)
png(filename = "01dfss50.png")
df01ss50 
dev.off()
