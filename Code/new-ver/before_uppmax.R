# Generate all the data

set.seed(861226)
design_mat_100_A <- dgp(100, c(1, 1), true_mean = TRUE)
design_mat_500_A <- dgp(500, c(1, 1), true_mean = TRUE)
design_mat_1000_A <- dgp(1000, c(1, 1), true_mean = TRUE)
