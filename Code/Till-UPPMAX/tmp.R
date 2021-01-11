
# Timing some code

aaa <- Sys.time()
log_bf_fun(data = design_mat_1000_a, sigma_fn = 1, a_0 = 0.01, b_0 = 0.01, omega_0_1 = matrix(1), omega_0_2 = matrix(1))
Sys.time() - aaa

microbenchmark(log_bf_fun(data = design_mat_500_a, sigma_fn = 1, a_0 = 0.01, b_0 = 0.01, omega_0_1 = matrix(1), omega_0_2 = matrix(1)))


microbenchmark(gen_one_line(design_mat_100_a, 100, deg_f = 2.5, 1000, 1, matrix(1), matrix(1), 0.01, 0.01))

# Trying to calculate the ratio of densities quicker

log_bf_fun(data = design_mat_1000_a, sigma_fn = 1, a_0 = 0.01, b_0 = 0.01, omega_0_1 = matrix(1), omega_0_2 = matrix(1))

log_bf_fun_fast(data = design_mat_1000_a, a_0 = 0.01, b_0 = 0.01, omega_0_1 = matrix(1), omega_0_2 = matrix(1))

microbenchmark(log_bf_fun(data = design_mat_100_a, sigma_fn = 1, a_0 = 0.01, b_0 = 0.01, omega_0_1 = matrix(1), omega_0_2 = matrix(1)), log_bf_fun_fast(data = design_mat_100_a, a_0 = 0.01, b_0 = 0.01, omega_0_1 = matrix(1), omega_0_2 = matrix(1))
)
a <- rnorm(9, 5, 1)

b <- matrix(a, nrow = 3)
b <- t(b) %*% b

c <- chol(b)

t(c) %*% c == b


prod(diag(c))^2
det(b)



d <- solve(b)
solve(c) %*% solve(t(c))



log_bf_fun_fast <- function(data, a_0, b_0, omega_0_1, omega_0_2) {
    n_obs <- nrow(data)
    X <- data
    scale_mat1 <- (b_0 / a_0) *
                  (diag(1, n_obs) +  data[, 2] %*%  t(data[, 2]))
    scale_mat2 <- (b_0 / a_0) *
                  (diag(1, n_obs) +  data[, 3] %*%  t(data[, 3]))

    s1 <- chol(scale_mat1)
    s2 <- chol(scale_mat2)

    det1 <- prod(diag(s1))^2
    det2 <- prod(diag(s2))^2

    scale1_inv <- solve(s1) %*% solve(t(s1))
    scale2_inv <- solve(s2) %*% solve(t(s2))
    part1 <- sqrt(det1) / sqrt(det1)
    part2 <- (-(2*a_0 + n_obs)/2) * log((1 + (1/(2*a_0)) + (t(X[, 1]) %*% scale1_inv %*% (X[, 1]) )) / (1 + (1/(2*a_0)) * (t(X[, 1]) %*% scale2_inv %*% (X[, 1] ))))

    return(log(part1) - part2)
}


load("all_data.RData")
head(all_data)
nrow(all_data)
