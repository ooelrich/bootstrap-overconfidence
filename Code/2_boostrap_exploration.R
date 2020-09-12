library(lme4)

# Extract matrices, each one contains n_bss replications for each
# of n_parents data sets

df3_boot <- data.frame(all_dfs_boot[[1]])
df5_boot <- data.frame(all_dfs_boot[[2]])
df10_boot <- data.frame(all_dfs_boot[[3]])
df100_boot <- data.frame(all_dfs_boot[[4]])
df1000_boot <- data.frame(all_dfs_boot[[5]])


# Calculate the variance between the variances. Each data set generated
# gives us a bunch of boostrap samples. There is a variance within each
# set of boostrapped variances, and there is also one between the variances
# within (this makes sense!!!). In the radicalization theory, I would predict
# that the variance *between* the boostrap-estimates variances increases
# when we fatten the tail.

between_samples_bss_var <- function(df3_boot,
                                    df5_boot,
                                    df10_boot,
                                    df100_boot,
                                    df1000_boot) {

    object_df3 <- df3_boot %>% summarise_if(is.numeric, var)
    object_df5 <- df5_boot %>% summarise_if(is.numeric, var)
    object_df10 <- df10_boot %>% summarise_if(is.numeric, var)
    object_df100 <- df100_boot %>% summarise_if(is.numeric, var)
    object_df1000 <- df1000_boot %>% summarise_if(is.numeric, var)

    object_df3_vec <- as.numeric(object_df3[1, ])
    object_df5_vec <- as.numeric(object_df5[1, ])
    object_df10_vec <- as.numeric(object_df10[1, ])
    object_df100_vec <- as.numeric(object_df100[1, ])
    object_df1000_vec <- as.numeric(object_df1000[1, ])

    df3var <- var(object_df3_vec)
    df5var <- var(object_df5_vec)
    df10var <- var(object_df10_vec)
    df100var <- var(object_df100_vec)
    df1000var <- var(object_df1000_vec)

    return(c(df3var, df5var, df10var, df100var, df1000var))

}

variances_between_samples <- between_samples_bss_var(df3_boot,
                                                     df5_boot,
                                                     df10_boot,
                                                     df100_boot,
                                                     df1000_boot)


# Plot a bunch of boostrapped sampling variances together with
# the true sampling variance

plot_radicalization <- function(d_f, n_obs, ref_data, boot_df) {
    df_melt <- melt(boot_df)[, 1:2]
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

rad_df3 <- plot_radicalization(3, 100, df3_base, df3_boot)
rad_df5 <- plot_radicalization(5, 100, df5_base, df5_boot)
rad_df10 <- plot_radicalization(10, 100, df10_base, df10_boot)
rad_df100 <- plot_radicalization(100, 100, df100_base, df100_boot)
rad_df1000 <- plot_radicalization(1000, 100, df1000_base, df1000_boot)

pdf("bootstrap_variability.pdf")
plot_grid(rad_df3[[1]],
          rad_df5[[1]],
          rad_df10[[1]],
          rad_df100[[1]])
dev.off()


###########################
### MULTILEVEL APPROACH ###
###########################

multilevel_var_decomp <- function(df_fix) {
    df_ret <- melt(df_fix)
    colnames(df_ret) <- c("variable", "value")
    df_ret$variable <- as.factor(df_ret$variable)

    mlfit <- lmer(formula = value ~ 1 + (1|variable), 
              data = df_ret)

    frame_tmp <- data.frame(VarCorr(mlfit))
    decomp <- frame_tmp[1, 4] / (frame_tmp[1, 4] + frame_tmp[2, 4]) 

    return(decomp)
}

ml_df3 <- multilevel_var_decomp(df3_boot)
ml_df5 <- multilevel_var_decomp(df5_boot)
ml_df10 <- multilevel_var_decomp(df10_boot)
ml_df100 <- multilevel_var_decomp(df100_boot)
ml_df1000 <- multilevel_var_decomp(df1000_boot)

c(ml_df3, ml_df5, ml_df10, ml_df100, ml_df1000)