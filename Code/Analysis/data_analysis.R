n100 <- data.frame(readRDS("Data sets/n100.Rds"))
n500 <- data.frame(readRDS("Data sets/n500.Rds"))
n1000 <- data.frame(readRDS("Data sets/n1000.Rds"))

varnames <- c("boot_reps", "n_obs", "deg_f", "lbf", "var_lbf", "p_radical")

colnames(n500) <- varnames
colnames(n1000) <- varnames
bdf <- rbind(n500, n1000, all_data)