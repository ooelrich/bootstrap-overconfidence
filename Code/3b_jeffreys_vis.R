is_radical <- function(x) { abs(x) > 5}
df_vals <- length(dfs_boot)
data_shell <- matrix(NA, nrow = n_parents, ncol = df_vals)

for (i in seq_len(length(dfs_boot))) {
    data <- dfs_boot[[i]]
    data_jeff <- apply(data, c(1, 2), is_radical)
    summary_jeff <- apply(data_jeff, 2, sum) / n_bss
    data_shell[ , i] <- summary_jeff
    print(i)
}

shell_melt <- melt(data_shell)[, 2:3]
colnames(shell_melt) <- c("df", "radical")
shell_melt$df <- as.factor(shell_melt$df)

ggplot(shell_melt, aes(x = radical, color = df)) +
    geom_boxplot(outlier.shape = NA)

head(shell_melt)