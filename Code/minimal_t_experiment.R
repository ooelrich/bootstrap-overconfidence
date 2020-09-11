
# The data genarating process
# is y = X1 + X2 + e, where e
# is student's t with 1, 10 or
# 100 degrees of freedom

library(dgpsim)
library(ggplot2)
library(cowplot)
library(reshape2)
library(parallel)

sim_baseline_t <- function(df, n_obs, sim_reps) {

    log_bf <- rep(NA, sim_reps)
    data <- dgp(n_obs, 2, 2, TRUE)

    for (i in seq_len(sim_reps)) {

        y <- data[, 1] + rt(n_obs, df)
        mod1 <- lm(y ~ 0 + data[, 2])
        mod2 <- lm(y ~ 0 + data[, 3])

        log_ml1 <- sum(pnorm(y, fitted(mod1), summary(mod1)$sigma, log = T))
        log_ml2 <- sum(pnorm(y, fitted(mod2), summary(mod2)$sigma, log = T))
        log_bf[i] <- log_ml1 - log_ml2
    }

    return(log_bf)
}


# Run 10 million reps for each of the different degrees of freedom

# Helper function to reduce amount of code when producing
# the baselines
baseline_fun <- function(df, n_obs) {
    resdat <- parSapply(cl, rep(worker_reps, workers),
                        sim_baseline_t, df = df, n_obs = n_obs)
    df_base <- data.frame(as.vector(resdat))
    colnames(df_base) <- c("log_BF")
    return(df_base)
}

workers <- 5
worker_reps <- 2e6

cl <- makeCluster(workers)

clusterEvalQ(cl, {
        library(dgpsim)
})

df1_baseline <- baseline_fun(1, 100)
df2_baseline <- baseline_fun(2, 100)
df5_baseline <- baseline_fun(5, 100)
df10_baseline <- baseline_fun(10, 100)
df100_baseline <- baseline_fun(100, 100)

stopCluster(cl)

plot_df1 <- ggplot(df1_baseline, aes(x = log_BF)) +
                geom_histogram(binwidth = .01) +
                labs(title = "df = 1")

plot_df2 <- ggplot(df2_baseline, aes(x = log_BF)) +
                geom_histogram(binwidth = .1) +
                labs(title = "df = 2")

plot_df5 <- ggplot(df5_baseline, aes(x = log_BF)) +
                geom_histogram(binwidth = .1) +
                labs(title = "df = 5")

plot_df10 <- ggplot(df10_baseline, aes(x = log_BF)) +
                geom_histogram(binwidth = .1) +
                labs(title = "df = 10")

plot_df100 <- ggplot(df100_baseline, aes(x = log_BF)) +
                geom_histogram(binwidth = .1) +
                labs(title = "df = 100")

pdf("10millionEachTry1.pdf")
plot_grid(plot_df1, plot_df2, plot_df5, plot_df10, plot_df100)
dev.off()

# Making the variances comparable across different
# degrees of freedom


# Using boostrap instead

sim_baseline_t_boot <- function(df, n_obs) {

    n_data_sets <- 1e1
    n_bootstrap_reps <- 1e3
    log_bf <- matrix(NA, ncol = n_data_sets, nrow = n_bootstrap_reps)
    data <- dgp(n_obs, 2, 2, TRUE)

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


plot_radicalization <- function(d_f, n_obs, ref_data) {
    dfr <- sim_baseline_t_boot(d_f, n_obs)
    df_melt <- melt(dfr)[, 2:3]
    colnames(df_melt) <- c("variable", "value")
    df_melt$variable <- as.factor(df_melt$variable)
    radical_plot <- ggplot() + 
                    geom_density(data = df_melt,
                        aes(x = value, color = variable)) +
                    geom_density(data = ref_data,
                        aes(x = log_BF),
                        color = "black",
                        size = 1.5) +
                    theme_minimal() +
                    theme(legend.position = "none")
    return(radical_plot)
}

radicalization_df1 <- plot_radicalization(1, 100, df1_baseline)
radicalization_df10 <- plot_radicalization(10, 100, df10_baseline)
radicalization_df100 <- plot_radicalization(100, 100, df100_baseline)

plot_grid(radicalization_df1,
          radicalization_df10,
          radicalization_df100)

# Make graphs showing the variation in the boostrap samples
# (like in the first draft)

df1_plot <- ggplot() +
                    geom_density(data = df1_boot_melt,
                        aes(x = value, color = variable)) +
                    geom_density(data = df1_baseline,
                        aes(x = log_BF),
                        color = "black",
                        size = 1.5) +
                    theme_minimal() +
                    theme(legend.position = "none")


# Make som multilevel decomposition in the variance