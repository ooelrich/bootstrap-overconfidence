#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadilloExtensions/sample.h>

// [[Rcpp::export]]
Rcpp::List lmRcpp(const arma::mat& X, const arma::colvec& y) {
    arma::uword n = X.n_rows;
    arma::uword k = X.n_cols;
    arma::mat XtX = X.t() * X;
    arma::vec Xty = X.t() * y;
    arma::vec b = arma::solve(XtX, Xty);
    arma::vec fit = X * b;
    arma::vec res = y - fit;
    double sigma = std::sqrt(arma::as_scalar(arma::sum(arma::pow(res, 2.0)))/(n-k-1));

    return Rcpp::List::create(fit, sigma);
}

// [[Rcpp::export]]
arma::vec dnormRcpp(const arma::vec& y, const arma::vec& mu, double sigma) {
    arma::vec x = -0.5*std::log(2*M_PI)-std::log(sigma)-0.5/std::pow(sigma, 2.0)*arma::pow(y-mu, 2.0);
    return x;
}

// [[Rcpp::export]]
Rcpp::NumericVector sim_baseline_t_Rcpp(double df, arma::uword n_obs,
    arma::uword sim_reps, arma::mat data, double sigma2) {

    arma::vec log_bf(sim_reps);
    Rcpp::List m1, m2;
    arma::vec y, m1Fit, m2Fit, t;
    double m1Sigma, m2Sigma, log_ml1, log_ml2;

    arma::mat ySim = arma::mat(n_obs, sim_reps, arma::fill::zeros);
    ySim.each_col() += data.col(0);

    double scale = std::sqrt((df-2)/df);
    Rcpp::NumericVector randomT = Rcpp::rt(n_obs*sim_reps, df);
    arma::mat randomTArma(randomT.begin(), n_obs, sim_reps, false);
    ySim += scale * randomTArma;
    if (sigma2 == 0) {
        for (arma::uword i = 0; i < sim_reps; ++i) {
            y = ySim.col(i);
            m1 = lmRcpp(data.col(1), y);
            m2 = lmRcpp(data.col(2), y);

            m1Fit = Rcpp::as<arma::vec>(m1[0]);
            m1Sigma = m1[1];
            m2Fit = Rcpp::as<arma::vec>(m2[0]);
            m2Sigma = m2[1];

            log_ml1 = arma::sum(dnormRcpp(y, m1Fit, m1Sigma));
            log_ml2 = arma::sum(dnormRcpp(y, m2Fit, m2Sigma));
            log_bf(i) = log_ml1 - log_ml2;
        }
    }
    Rcpp::NumericVector out = Rcpp::NumericVector(log_bf.begin(), log_bf.end());
    return out;
}

// [[Rcpp::export]]
arma::mat sim_baseline_t_boot_Rcpp(double df, arma::uword n_obs, const arma::mat& design_mat, arma::uword n_parents,
arma::uword n_bss, double sigma2) {
    arma::mat log_bf(n_bss, n_parents);

    // Generate dependent variable
    Rcpp::NumericVector randomT = Rcpp::rt(n_obs*n_parents, df);
    arma::mat ySim(randomT.begin(), n_obs, n_parents, true);
    ySim.each_col() += design_mat.col(0);

    // Prepare vectors
    arma::vec x2 = design_mat.col(1);
    arma::vec x3 = design_mat.col(2);
    arma::vec y_i;

    // Stuff for bootstrap
    arma::uvec idx = arma::linspace<arma::uvec>(0, n_obs-1, n_obs);
    arma::vec prob(n_obs, arma::fill::zeros);
    prob += 1.0/n_obs;

    // Intialization
    arma::vec y_b, x2_b, x3_b, m1Fit, m2Fit;
    arma::uvec bindx;
    double log_ml1, log_ml2;
    double m1Sigma = std::sqrt(sigma2);
    double m2Sigma = std::sqrt(sigma2);
    Rcpp::List m1, m2;
    bool sigmaFlag;
    if (std::fabs(sigma2)<1e-12) {
        sigmaFlag = true;
    } else {
        sigmaFlag = false;
    }
    
    for (arma::uword i = 0; i < n_parents; ++i) {
        y_i = ySim.col(i);
        for (arma::uword j = 0; j < n_bss; ++j) {
            bindx = Rcpp::RcppArmadillo::sample(idx, n_obs, true, prob);
            y_b = y_i(bindx);
            x2_b = x2(bindx);
            x3_b = x3(bindx);
            m1 = lmRcpp(x2_b, y_b);
            m2 = lmRcpp(x3_b, y_b);

            m1Fit = Rcpp::as<arma::vec>(m1[0]);
            m2Fit = Rcpp::as<arma::vec>(m2[0]);
            if (sigmaFlag) {
                m1Sigma = m1[1];
                m2Sigma = m2[1];
            }      
            log_ml1 = arma::sum(dnormRcpp(y_b, m1Fit, m1Sigma));
            log_ml2 = arma::sum(dnormRcpp(y_b, m2Fit, m2Sigma));
            log_bf(j, i) = log_ml1 - log_ml2;
        }
    }
    return Rcpp::List::create(log_bf);
}
