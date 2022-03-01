#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

//[[Rcpp::export("lin_mod_cpp")]]
arma::mat beta(arma::colvec& y, arma::mat& X){
   arma::colvec beta_est = arma::solve(X, y);
   return beta_est;
}