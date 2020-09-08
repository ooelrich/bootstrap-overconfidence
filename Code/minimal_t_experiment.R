
# The data genarating process
# is y = X1 + X2 + e, where e
# is student's t with 1, 10 or
# 100 degrees of freedom

library(dgpsim)

sim_boostrap_t <- function(df) {

    sim_reps <- 1e6
    bf <- rep(NA, sim_reps)

    for (i in seq_len(sim_reps)) {

        n <- 100
        data <- dgp(n, 2, 2, TRUE)
        y <- data[, 1] + rt(n, df)
        mod1 <- lm(y ~ 0 + data[, 2])
        mod2 <- lm(y ~ 0 + data[, 3])

        ml1 <- sum(pnorm(y, fitted(mod1), summary(mod1)$sigma, log = T))
        ml2 <- sum(pnorm(y, fitted(mod2), summary(mod2)$sigma, log = T))
        bf[i] <- ml1 - ml2
    }

    return(bf)
}


df <- 10