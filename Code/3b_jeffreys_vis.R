# Dichotomize the data

# The bootstrap data
is_radical <- function(x) { abs(x) > 5}
df_vals <- length(dfs_boot)
data_shell <- matrix(NA, nrow = n_parents, ncol = df_vals)

for (i in seq_len(length(dfs_boot))) {
    data <- dfs_boot[[i]]
    data_jeff <- apply(data, c(1, 2), is_radical)
    summary_jeff <- apply(data_jeff, 2, sum) / n_bss
    data_shell[ , i] <- summary_jeff
}

shell_melt <- melt(data_shell)[, 2:3]
colnames(shell_melt) <- c("df", "radical")
shell_melt$df <- as.factor(shell_melt$df)

# The baseline data

baseline_jeff <- is_radical(baseline_dat)
summary_baseline_jeff <- colSums(baseline_jeff) / nrow(baseline_jeff)

truth <- rep(summary_baseline_jeff, each  = n_parents)
shell_melt <- cbind(shell_melt, truth)
# Plotting it all

# Calculate the mean for each value of df and add to data frame

setDT(shell_melt)[, mean_theta := mean(radical), by = df]

pdf("whatever_name_you_want.pdf")
ggplot(shell_melt, aes(x = radical)) +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    geom_density() + geom_point(aes(x = truth, y = 0.1), col = "red") +
    geom_point(aes(x = mean_theta, y = 0.1), col = "black") +
    facet_grid(rows = vars(df))
dev.off()