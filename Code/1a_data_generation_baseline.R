library(dgpsim)
library(ggplot2)
library(cowplot)
library(reshape2)
library(parallel)
library(dplyr)
library(lme4)

# Function to simulate the sampling variance not using bootstrap

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

# Helper function to reduce amount of code when producing
# the baselines, using parallelization

baseline_fun <- function(df, n_obs, design_etc) {
    resdat <- parSapply(cl, rep(worker_reps, workers),
                        sim_baseline_t, df = df, n_obs = n_obs, design_etc)
    df_base <- data.frame(as.vector(resdat))
    colnames(df_base) <- c("log_BF")
    return(df_base)
}

# Set up parallelization, at least 2e4 reps per worker to get decent
# looking histograms. Five workers and 2e5 reps means one million each

n_obs <- 100
set.seed(861226)
design_etc <- dgp(n_obs, 2, 2, TRUE)


workers <- 5
worker_reps <- 2e4
cl <- makeCluster(workers)

clusterEvalQ(cl, {
        library(dgpsim)
})

# Specify ONE design matrix and run the simulations using that 
# one matrix. Need to rerun everything a lot of times so that 
# I can see how things changes between simulations!

df3_base <- baseline_fun(3, n_obs, design_etc)
df5_base <- baseline_fun(5, n_obs, design_etc)
df10_base <- baseline_fun(10, n_obs, design_etc)
df100_base <- baseline_fun(100, n_obs, design_etc)
df1000_base <- baseline_fun(1000, n_obs, design_etc)

stopCluster(cl)

# collect all the data in one data frame

df_all_baseline <- data.frame(df3_base,
                     df5_base,
                     df10_base,
                     df100_base,
                     df1000_base)
colnames(df_all_baseline) <- c("df3", "df5", "df10", "df100", "df1000")

# Check the overall sampling variances
df_all_baseline %>% summarise_if(is.numeric, var)


df_all_baseline_melt <- melt(df_all_baseline)
df_all_baseline_melt$variable <- as.factor(df_all_baseline_melt$variable)
plot_all <- ggplot(df_all_baseline_melt, aes(x = value, col = variable)) +
                geom_density()

pdf("baselineSamplingVariancesAgain.pdf")
plot_all
dev.off()

save(df_all_baseline, file = "boostrapSmallSaturday.RData")