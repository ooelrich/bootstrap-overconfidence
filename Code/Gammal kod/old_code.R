
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

sim_reps <- 1e3
samp_size <- 40

stromberg_repl <- matrix(rnorm(sim_reps * samp_size, mean = 0, sd = 2),
                         nrow = samp_size)

medians <- apply(stromberg_repl, 2, median)  # DT is my data.table


var <- c()
for (i in 1:100) {
    stromberg_repl <- matrix(rnorm(sim_reps * samp_size, mean = 0, sd = 2),
                         nrow = samp_size)
    medians <- apply(stromberg_repl, 2, median)
    var[i] <- var(medians)
}





# B = 100, n = 10, 10 %, probability of breakdown roughly 80%
first_prob <- 1 - (1-(10*(1-pbinom(4, 10, 0.1))))^100


# B = 200, n = 10, 10 %, probability of breakdown roughly 96%
1 - (1-(10*(1-pbinom(4, 10, 0.1))))^200


# B = 400, n = 10, 10 %, probability of breakdown roughly 99.86%
1 - (1-(10*(1-pbinom(4, 10, 0.1))))^400

dattaro <- rbinom(1000, 100, first_prob)

hist(dattaro)

sum(dattaro < 72.5) / length(dattaro)

# Try the same approach for 20 observations

1 - (1-(20*(1-pbinom(10, 20, 0.1))))^100
