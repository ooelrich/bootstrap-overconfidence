library(mvtnorm)
library(parallel)

# Helper functions


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
    save(all_data, file = "/proj/dennis/nobackup/all_data.RData")
}


load("/proj/dennis/nobackup/data100.RData")
load("/proj/dennis/nobackup/data500.RData")
load("/proj/dennis/nobackup/data1000.RData")

initialize_all_data()

ncl <- 20

aaa <- Sys.time()
for (i in 1:3) {

    if (i == 1) {
        data_set <- design_mat_100_a
    } else if (i == 2) {
        data_set <- design_mat_500_a
    } else if (i == 3) {
        data_set <- design_mat_1000_a
    }

    for (df in c(2.5, 5, 30)) {

        new_rows <- mclapply(rep(1, ncl), generate_new_rows, deg_f = df,
                             n_bss = 1e4, design_mat = data_set,
                             omega_0_1 = matrix(1), omega_0_2 = matrix(1),
                             a_0 = 0.001, b_0 = 0.001, mc.cores = ncl)

        new_rows <- do.call("rbind", new_rows)

        rm(all_data)
        load("/proj/dennis/nobackup/all_data.RData")
        all_data <- rbind(all_data, new_rows)
        save(all_data, file = "/proj/dennis/nobackup/all_data.RData")
    }

}
Sys.time() - aaa