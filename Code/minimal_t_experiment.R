
# The data genarating process
# is y = X1 + X2 + e, where e
# is student's t with 1, 10 or
# 100 degrees of freedom

library(dgpsim)
library(ggplot2)
library(cowplot)
library(reshape2)
library(parallel)
library(dplyr)


# Making the variances comparable across different
# degrees of freedom


# Using boostrap instead

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


# Set up parallelization, at least 2e4 reps per worker to get decent
# looking histograms. Five workers and 2e5 reps means one million each

n_obs <- 100
workers <- 5
cl <- makeCluster(workers)

clusterEvalQ(cl, {
        library(dgpsim)
})

# Specify ONE design matrix and run the simulations using that 
# one matrix. Need to rerun everything a lot of times so that 
# I can see how things changes between simulations!

set.seed(861226)
design_etc2 <- dgp(n_obs, 2, 2, TRUE)

Sys.time()
startingg <- Sys.time()
all_dfs_boot <- parLapply(cl, c(3, 5, 10, 100, 1000), sim_baseline_t_boot,
                    n_obs = n_obs, design_mat = design_etc2,
                    n_parents = 1e2, n_bss = 1e3)
Sys.time() - startingg

stopCluster(cl)

df3_boot <- all_dfs_boot[[1]]
df5_boot <- all_dfs_boot[[2]]
df10_boot <- all_dfs_boot[[3]]
df100_boot <- all_dfs_boot[[4]]
df1000_boot <- all_dfs_boot[[5]]

var(colVars(df3_boot))
var(colVars(df5_boot))
var(colVars(df10_boot))
var(colVars(df100_boot))
var(colVars(df1000_boot))


for (i in 1:10) {

    rad_df3 <- plot_radicalization(1, 100, df3_baseline, design_etc)
    rad_df5 <- plot_radicalization(10, 100, df5_baseline, design_etc)
    rad_df10 <- plot_radicalization(100, 100, df10_baseline, design_etc)
    rad_df100 <- plot_radicalization(1000, 100, df10_baseline, design_etc)

    plotname <- paste("bootsamp", i, sep = "_")
    plotname <- paste(plotname, ".pdf", sep = "")

    pdf(plotname)
    plot_grid(rad_df3[[1]],
              rad_df5[[1]],
              rad_df10[[1]],
              rad_df100[[1]])
    dev.off()

}



plot_radicalization <- function(d_f, n_obs, ref_data, design_mat) {
    dfr <- sim_baseline_t_boot(d_f, n_obs, design_mat)
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
    return(list(radical_plot, df_melt))
}