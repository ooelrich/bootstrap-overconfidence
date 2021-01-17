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
double logdmvt_Rcpp(const arma::vec& x, const arma::mat& S) {

    double twoa0 = 0.02;
    arma::uword n = x.n_elem;
    double det_S = arma::det( S );
    arma::mat S_inv = S.i();
    double result;
    double P = -0.5 * log(det_S);
    result = arma::as_scalar(P - ((twoa0 + n) * 0.5) * log(1.0 + (1.0 / twoa0) * x.t() * S_inv * x));
    return result;
    
}

// [[Rcpp::export]]
arma::vec dmvt(const arma::mat& x, const arma::vec& mu,
        const arma::mat& S, const double df, const bool log_p = false) {
    arma::uword n = x.n_rows, m = x.n_cols;
    double det_S = arma::det(S);
    arma::mat S_inv = S.i();
    arma::vec result(n);
    arma::rowvec X(m);
    arma::rowvec Mu = mu.t();
    if ( log_p ) {
        double P = R::lgammafn((df + m) * 0.5) - R::lgammafn(df * 0.5);
        P -= ( (m * 0.5) * (log(df) + log(M_PI)) + 0.5 * log(det_S) );
        for ( arma::uword i = 0; i < n; ++i ) {
            X = x.row(i) - Mu;
            result[i] = arma::as_scalar(P - ((df + m) * 0.5) * log(1.0 + (1.0 / df) * X * S_inv * X.t()));
        }
        return result;
    }
    double P = R::gammafn((df + m) * 0.5);
    P /= (R::gammafn(df*0.5) * pow(df, m*0.5) * pow(M_PI, m*0.5) * sqrt(det_S));
    for ( arma::uword i = 0; i < n; ++i ) {
        X = x.row(i) - Mu;
        result[i] = arma::as_scalar(P/pow(1.0+(1.0/df) * X * S_inv * X.t(), (df+m) * 0.5));
    }
    return result;
}



// [[Rcpp::export]]
arma::vec atomic_operation(double df, arma::uword n_obs, 
    const arma::mat& design_mat, arma::uword n_bss) {
    
    arma::vec log_bf(n_bss); // Vektor att spara resultaten från resp bss i

    // Generate dependent variable
    Rcpp::NumericVector randomT = Rcpp::rt(n_obs, df);
    double scale = std::sqrt( (df-2.0)/df );
    arma::vec ySim(n_obs);
    arma::vec randT = randomT * scale;
    ySim = design_mat.col(0) + randT; // y = mu + epsilon

    // Prepare vectors
    arma::vec x2 = design_mat.col(1); //kovariater för modell 1
    arma::vec x3 = design_mat.col(2); //kovariater för modell 2

    // Used to generate bootstrap samples
    arma::uvec idx = arma::linspace<arma::uvec>(0, n_obs-1, n_obs); 
    arma::vec prob(n_obs, arma::fill::zeros);
    prob += 1.0/n_obs;

    // Intialization
    arma::vec y_b, x2_b, x3_b;
    arma::uvec bindx;
    double log_ml1, log_ml2;

    Rcpp::List m1, m2;
    
    
    for (arma::uword j = 0; j < n_bss; ++j) {
        bindx = Rcpp::RcppArmadillo::sample(idx, n_obs, true, prob); // Bootstrap sample
        y_b = ySim(bindx); // Selecting
        x2_b = x2(bindx); // bootstrap
        x3_b = x3(bindx); // sample
        arma::mat UnityMatrix;
        arma::mat S1 = UnityMatrix.eye(n_obs, n_obs) + x2_b * x2_b.t();
        arma::mat S2 = UnityMatrix.eye(n_obs, n_obs) + x3_b * x3_b.t();
        log_ml1 = logdmvt_Rcpp(y_b, S1);
        log_ml2 = logdmvt_Rcpp(y_b, S2);

        log_bf(j) = log_ml1 - log_ml2;
    }
    
    return log_bf;
}

// [[Rcpp::export]]
arma::mat bootstrap_batch(double df, arma::uword n_obs,
    const arma::mat& design_mat, arma::uword runs, arma::uword n_bss){

        arma::vec tempres(n_bss);
        arma::mat resvec(runs,2, arma::fill::zeros);
        double all = n_bss;
        for (arma::uword i = 0; i < runs; ++i) {
            tempres = atomic_operation(df, n_obs, design_mat, n_bss);
            resvec(i, 0) = var(tempres);
            resvec(i, 1) = (sum(abs(tempres)>5.0)/all);
        }

        return resvec;

}
