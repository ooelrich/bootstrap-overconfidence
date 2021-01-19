library(RcppArmadillo)
library(RcppDist)
library(dgpsim)
library(mvtnorm
library(microbenchmark)

Rcpp::sourceCpp("Code/Rcpp-version/funsRcpp2.cpp")

set.seed(861226)
datta <- dgp(100, c(1,1), TRUE)


microbenchmark(generate_rows(2.5, 100, as.matrix(datta), 1000, 1))



# arma::solve_opts::fast cuts about 20 % of the running time
# the question is if it might be not-so-wise