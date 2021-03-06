####################################
### ANALYSING THE BOOTSTRAP DATA ###
####################################


######################################
### DIFFERENT MEASURES OF VARIANCE ###
######################################

bss_summary <- matrix(NA, nrow = length(dfs), ncol = 4)

for (i in seq_len(length(dfs))) {

    bss_vars <- data.frame(dfs_boot[[i]][[1]]) %>% summarise_if(is.numeric, var)
    bss_vars <- as.numeric(bss_vars)
    bss_summary[i, 1] <- mean(bss_vars)
    bss_summary[i, 2] <- var(bss_vars)

    jkn_vars <- data.frame(dfs_boot[[i]][[2]]) %>% summarise_if(is.numeric, var)
    jkn_vars <- jkn_vars * (n_obs - 1)^2 / n_obs
    jkn_vars <- as.numeric(jkn_vars)
    bss_summary[i, 3] <- mean(jkn_vars)
    bss_summary[i, 4] <- var(jkn_vars)
}

reference_vars <- baseline_dat %>% summarise_if(is.numeric, var)
all_things <- cbind(bss_summary, t(reference_vars))
colnames(all_things) <- c("avg boot var", "var boot var", "avg jack var", "var jack var", "baseline var")
all_things <- round(all_things)

#######################
### VISUAL ANALYSIS ###
#######################

if (n_parents < 1001) {

    plot_radicalization <- function(ref_data, boot_df) {
        df_melt <- reshape2::melt(boot_df)[, 2:3]
        colnames(df_melt) <- c("variable", "value")
        df_melt$variable <- as.factor(df_melt$variable)
        ref_data <- data.frame(ref_data)
        colnames(ref_data) <- "log_BF"
        radical_plot <- ggplot() +
                        geom_density(data = df_melt,
                                     aes(x = value,
                                     color = variable)) +
                        geom_density(data = ref_data,
                                     aes(x = log_BF),
                                     color = "black",
                                     size = 1.5) +
                    theme_minimal() +
                    theme(legend.position = "none")
        return(radical_plot)
    }

    plots_boot <- list()
    for (i in seq_len(length(dfs))) {

        plots_boot[[i]] <- plot_radicalization(baseline_dat[, i], dfs_boot[[i]][[1]])

    }

}



###########################
### MULTILEVEL APPROACH ###
###########################

# Specifies a simple multilevel model with a global intercept
# and a random effect at the group level (parets) and at the
# individual level. Higher intraclass correlations means low
# variation between boostrap relicates from the same parent,
# and a low ICC can mean that all the estimates variances are
# basically the same (knowing which parent a boostrapped replicate
# comes from does not give a lot of information)

multilevel_var_decomp <- function(df_fix) {

    df_ret <- reshape2::melt(df_fix)[, 2:3]
    colnames(df_ret) <- c("variable", "value")
    df_ret$variable <- as.factor(df_ret$variable)

    mlfit <- lme4::lmer(formula = value ~ 1 + (1 | variable), data = df_ret)

    frame_tmp <- data.frame(lme4::VarCorr(mlfit))
    decomp <- frame_tmp[1, 4] / (frame_tmp[1, 4] + frame_tmp[2, 4])

    return(decomp)
}

intra_class_correlations <- rep(NA, length(dfs))
for (i in seq_len(length(dfs))) {
    intra_class_correlations[i] <- multilevel_var_decomp(dfs_boot[[i]][[1]])
}
icc <- data.frame(t(intra_class_correlations))
colnames(icc) <- names_vec
icc <- round(icc, digits = 3)

#####################################
### VISUALISATION OF THE BASELINE ###
#####################################

# Create a nice graph
df_all_baseline_melt <- reshape2::melt(baseline_dat)[, 2:3]
colnames(df_all_baseline_melt) <- c("variable", "value")
df_all_baseline_melt$variable <- as.factor(df_all_baseline_melt$variable)
plot_all <- ggplot(df_all_baseline_melt, aes(x = value, col = variable)) +
                geom_density()

################################
### COLLECT ALL THE EVIDENCE ###
################################

folder_name <- paste0("run_", seed_val)
dir.create(folder_name)
folder_name <- paste0(folder_name, "/")

baseline_variance <- paste0(folder_name, seed_val, "_baseline_variance.pdf")
pdf(baseline_variance)
plot_all
dev.off()

if (n_parents < 1001) {

    boot_analysis <- paste0(folder_name, seed_val, "_bootstrap_analysis.pdf")
    pdf(boot_analysis)
    do.call("grid.arrange", c(plots_boot, ncol = ceiling(sqrt(length(dfs)))))
    dev.off()

}

multilevel <- paste0(folder_name, seed_val, "_multilevel.txt")
write.table(icc, file = multilevel)

boot_analysis_num <- paste0(folder_name, seed_val, "_boot_analysis_num.pdf")
pdf(boot_analysis_num)
grid.table(all_things)
dev.off()

# Save the parameters of the simulation setup to a txt document
setup <- paste("seed:", seed_val, "n_obs:", n_obs, "n_parents:", n_parents,
            "n_bss:", n_bss, "sim_reps:", sim_reps, "sigma2:", sigma2)
write.table(setup, file = paste0(folder_name, seed_val, ".txt"))