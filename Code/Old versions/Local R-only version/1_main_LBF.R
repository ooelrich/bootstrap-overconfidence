#########################
### PARAMETERS TO SET ###
#########################
rm(list = ls())
library(dgpsim)
library(qwraps2)
library(xtable)
library(mvtnorm)
library(parallel)

# Create input data and control that it's still the same
# since we want to use the same design matrix for all
# repetitions
set.seed(861226)
design_mat_100_a <- dgp(100, c(1, 1), true_mean = TRUE)
design_mat_500_a <- dgp(500, c(1, 1), true_mean = TRUE)
design_mat_1000_a <- dgp(1000, c(1, 1), true_mean = TRUE)


sigma_fn_gen <- function(sigma2) {
    if (sigma2 > 0) {
        sigma <- sqrt(sigma2)
        function(m) {
            sigma
        }
    } else {
        function(m) {
            summary(m)$sigma
        }
    }
}

log_bf_fun <- function(data, a_0, b_0, omega_0_1, omega_0_2) {

    n_obs <- nrow(data)
    scale_mat1 <- (b_0 / a_0) *
                  (diag(1, n_obs) +  data[, 2] %*% omega_0_1 %*% t(data[, 2]))
    scale_mat2 <- (b_0 / a_0) *
                  (diag(1, n_obs) + data[, 3] %*% omega_0_2 %*% t(data[, 3]))

    log_ml1 <- dmvt(data[, 1], rep(0, n_obs),
                    scale_mat1, df = 2 * a_0, log = TRUE)
    log_ml2 <- dmvt(data[, 1], rep(0, n_obs),
                    scale_mat2, df = 2 * a_0, log = TRUE)

    return(log_ml1 - log_ml2)
}


gen_one_line <- function(design_matrix, n_obs, deg_f, n_bss,
                              omega_0_1, omega_0_2, a_0, b_0) {

    log_bf <- rep(NA_real_, n_bss)
    index <- seq_len(n_obs)
    data_temp <- design_matrix
    data_temp[, 1] <- design_matrix[, 1] +
                      rt(n_obs, deg_f) * sqrt((deg_f - 2) / deg_f)

    for (i in seq_len(n_bss)) {
        bss_index <- sample(index, n_obs, replace = TRUE)
        bss <- data_temp[bss_index, ]
        log_bf[i] <- log_bf_fun(bss, a_0, b_0, omega_0_1, omega_0_2)
    }

    lbf <- log_bf_fun(data_temp, a_0, b_0, omega_0_1, omega_0_2)
    var_lbf <- var(log_bf)
    p_rad <- sum(as.numeric(abs(log_bf) > 5)) / n_bss
    return(c(n_bss, n_obs, deg_f, lbf, var_lbf, p_rad))
}

generate_new_rows <- function(deg_f, n_bss, runs, design_mat,
                              omega_0_1, omega_0_2, a_0, b_0) {

    n_obs <- nrow(design_mat)
    data_gen <- matrix(NA_real_, ncol = 6, nrow = runs)
    colnames(data_gen) <- c("boot_reps", "n_obs", "deg_f", "lbf",
                            "var_lbf", "p_radical")

    for (j in seq_len(runs)) {
        data_gen[j, ] <- gen_one_line(design_mat,
                                      n_obs,
                                      deg_f,
                                      n_bss,
                                      omega_0_1,
                                      omega_0_2,
                                      a_0,
                                      b_0)
        print(j)
    }

    return(data_gen)
}

initialize_all_data <- function() {
    all_data <- data.frame(boot_reps = integer(),
                           n_obs = integer(),
                           deg_f = double(),
                           lbf = double(),
                           var_lbf = double(),
                           p_radical = double())
    save(all_data, file = "all_data.RData")
}


# Only run this if you do not have a data_all, as it will wipe
# out the old one!!!
initialize_all_data()

# This function generates a new number of observations equal to
# runs. One observation is one parent sample from the given DGP
# and n_bss bootstrap replicates based on that parent. Each row
# contains the sim setup, the LBF of the parent, and the bootstrap
# estimate of var(LBF)




data_set <- design_mat_100_a
aaa <- Sys.time()
for (df in c(2.5, 5, 30)) {

    new_rows <- generate_new_rows(deg_f = df, n_bss = 1e3,
                              runs = 1e4, design_mat = data_set, omega_0_1 = matrix(1),
                              omega_0_2 = matrix(1), a_0 = 0.01, b_0 = 0.01)

    rm(all_data)
    load("all_data.RData")
    all_data <- rbind(all_data, new_rows)
    save(all_data, file = "all_data.RData")
    print("Current number of rows: ")
    print(nrow(all_data))
}
Sys.time() - aaa

all_data$radical <- as.numeric(abs(all_data$lbf) > 5)
all_data$setup <- paste0("n: ", all_data$n_obs, " df: ", all_data$deg_f)

levels_vec <- c("n: 100 df: 2.5",
                "n: 100 df: 5",
                "n: 100 df: 30",
                "n: 500 df: 2.5",
                "n: 500 df: 5",
                "n: 500 df: 30",
                "n: 1000 df: 2.5",
                "n: 1000 df: 5",
                "n: 1000 df: 30")

all_data$setup <- factor(all_data$setup, levels = levels_vec)


# Add some stuff to the data frame for summarizing
summary_appendix <-
    list("Simulation results" =
        list("truth"  = ~mean(.data$radical),
             "boot_approx"  = ~qwraps2::mean_sd(.data$p_radical, digits = 3))
        )

a <- t(summary_table(dplyr::group_by(all_data, setup), summary_appendix))
