#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadilloExtensions/sample.h>

//[[Rcpp::export("wild_boot_cpp")]]
arma::vec wild_boot_cpp(arma::mat x, arma::vec y, int nboot){
   int n = y.n_elem;
   arma::vec ones = arma::ones(n);
   arma::mat x_new = join_rows(ones, x);
   int p = x_new.n_cols;
   
   arma::vec beta_orig = arma::inv( x_new.t() * x_new) * x_new.t() * y;
   arma::vec resid = y - x_new * beta_orig;
   arma::mat coefs(p, nboot);
   arma::vec v = {-1.0, 1.0};
   for(int i = 0; i < nboot; i++){
      arma::vec w = Rcpp::RcppArmadillo::sample(v, n, TRUE, 0.5);
      arma::vec resid_boot = w % resid;
      arma::vec y_boot = x_new * beta_orig + resid_boot;
      coefs.col(i) = (arma::inv(x_new.t() * x_new) * x_new.t() * y_boot);
   }
   arma::vec coef_boot = arma::mean(coefs, 1);
   return coef_boot;
}