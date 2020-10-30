b <- 3
r <- mclapply(1:8, function(i) { rnorm(i, mean = b) }, mc.cores = 5)



r <- mclapply(1:5, function(i) { rnorm(3) }, mc.cores = 4)