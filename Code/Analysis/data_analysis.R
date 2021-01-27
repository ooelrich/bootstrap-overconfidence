library(data.table)

n100 <- data.frame(readRDS("Data sets/n100.Rds"))
n500 <- data.frame(readRDS("Data sets/n500.Rds"))
n1000 <- data.frame(readRDS("Data sets/n1000.Rds"))

varnames <- c("boot_reps", "n_obs", "deg_f", "lbf", "var_lbf", "p_radical")

colnames(n500) <- varnames
colnames(n1000) <- varnames
bdf <- rbind(n500, n1000, n100)

head(bdf)


bdf <- data.table(bdf)
ans <- bdf[, .(true_var = var(lbf), mean_var_lbf = mean(var_lbf),
            var_var_lfb = var(var_lbf),
            true_p_radical = tabulate(as.numeric(abs(lbf) > 5)) / .N,
            mean_p_radical =  mean(p_radical),
            var_p_radical = var(p_radical)),
            by = .(deg_f, n_obs)]
ans
