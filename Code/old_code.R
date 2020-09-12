
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