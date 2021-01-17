library(RcppArmadillo)
library(RcppDist)
library(dgpsim)
library(mvtnorm)

Rcpp::sourceCpp("Code/Rcpp-version/funsRcpp2.cpp")
 

Rcpp::sourceCpp("Code/Rcpp-version/funsRcpp.cpp")

S1  <- diag(100) + datta[, 2] %*% t(datta[,2])
S2  <- diag(100) + datta[, 3] %*% t(datta[,3])


logdmvt_Rcpp(datta[,1], S1) - logdmvt_Rcpp(datta[,1], S2)

microbenchmark(atomic_operation(2.5, 100, as.matrix(datta), 1000))


dim(datta)

class(datta)

dim(S1)

datta <- dgp(1000, c(1,1), TRUE)

aa <- Sys.time()
a <-  bootstrap_batch(2.5, 1000, as.matrix(datta), 1, 1000)
Sys.time() - aa



datta <- dgp(500, c(1,1), TRUE)

aa <- Sys.time()
a <-  bootstrap_batch(2.5, 500, as.matrix(datta), 1, 500)
Sys.time() - aa
