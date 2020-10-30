check_data_sets <- function(){
    
    check_vec <- rep(NA_real_, 9)

    check_vec[1] <- (colMeans(design_mat_50_A)[1] == -0.09955195527523756637844)
    check_vec[2] <- (colMeans(design_mat_50_A)[2] == -0.09251772635616362094879)
    check_vec[3] <- (colMeans(design_mat_50_A)[3] == -0.00703422891907393935812)

    check_vec[4] <- (colMeans(design_mat_100_A)[1] == -0.02692726207431405879844)
    check_vec[5] <- (colMeans(design_mat_100_A)[2] == -0.07525495003443211095018)
    check_vec[6] <- (colMeans(design_mat_100_A)[3] == 0.04832768796011805562118)

    check_vec[7] <- (colMeans(design_mat_1000_A)[1] == -0.01641358056059107062286)
    check_vec[8] <- (colMeans(design_mat_1000_A)[2] == -0.02671633330173790385476)
    check_vec[9] <- (colMeans(design_mat_1000_A)[3] == 0.0103027527411468332319)

    if (sum (as.numeric(check_vec)) == 9) {
        data_status <- "pass"
    } else {
        data_status <- "fail"
    }

    return(data_status)
}


sigma_fn_gen <- function(sigma2) {
    if (sigma2 > 0) {
        sigma <- sqrt(sigma2)
        function(m) { sigma }
    } else {
        function(m) { summary(m)$sigma }
    } 
}

log_bf_fun <- function(data, sigma_fn) {
    m1 <- lm(data[, 1] ~ 0 + data[, 2])
    m2 <- lm(data[, 1] ~ 0 + data[, 3])
    log_ml1 <- sum(dnorm(data[, 1], fitted(m1), sigma_fn(m1), log = T))
    log_ml2 <- sum(dnorm(data[, 1], fitted(m2), sigma_fn(m2), log = T))
    return(log_ml1 - log_ml2)
}

log_bf_fun_2 <- function(data, sigma_fn, a_0, b_0, omega_0_1, omega_0_2) {

    scale_mat1 <- (b_0 / a_0) * (diag(1, nrow = nrow(data)) + omega_0_1 * data[, 2] %*% t(data[, 2]))
    scale_mat2 <- (b_0 / a_0) * (diag(1, nrow = nrow(data)) + omega_0_2 * data[, 3] %*% t(data[, 3]))
    
    log_ml1 <- dmvt(data[, 1], 0, scale_mat1, df = 2 * a_0, log = TRUE,
                    checkSymemtry = FALSE)
    log_ml2 <- dmvt(data[, 1], 0, scale_mat2, df = 2 * a_0, log = TRUE,
                    checkSymemtry = FALSE)
    
    return(log_ml1 - log_ml2)
}

generate_one_line <- function(design_matrix, n_obs, deg_f, n_bss, sigma2) {

    log_bf <- rep(NA_real_, n_bss)
    sigma_fn <- sigma_fn_gen(sigma2)
    index <- seq_len(n_obs)
    data_temp <- design_matrix
    data_temp[, 1] <- design_matrix[, 1] + rt(n_obs, deg_f) * sqrt((deg_f - 2) / deg_f)

    for (i in seq_len(n_bss)) {
        bss_index <- sample(index, n_obs, replace = TRUE)
        bss <- data_temp[bss_index, ]
        log_bf[i] <- log_bf_fun(bss, sigma_fn)
    }

    LBF <- log_bf_fun(data_temp, sigma_fn)
    var_LBF <- var(log_bf)
    p_rad <- sum(as.numeric(abs(log_bf) > 5)) / n_bss
    return(c(n_bss, n_obs, deg_f, LBF, var_LBF, p_rad))
}

generate_new_rows <- function(deg_f, n_bss, sigma_2, runs, design_mat) {

    n_obs <- nrow(design_mat)
    data_gen <- matrix(NA_real_, ncol = 6, nrow = runs)
    colnames(data_gen) <- c("boot_reps", "n_obs", "deg_f", "LBF",
                            "var_LBF", "p_radical")

    for (j in seq_len(runs)) {
        data_gen[j, ] <- generate_one_line(design_mat, n_obs, deg_f, n_bss, sigma_2)
        print(j)
    }

    return(data_gen)
}

initialize_all_data <- function() {
    all_data <- data.frame(boot_reps = integer(),
                           n_obs = integer(),
                           deg_f = double(),
                           LBF = double(),
                           var_LBF = double(),
                           p_radical = double())
    save(all_data, file = "all_data.RData")
}