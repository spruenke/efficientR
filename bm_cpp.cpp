#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// [[Rcpp::export("rank_ind_cpp")]]
arma::vec rank_ind_cpp(arma::vec x){
   arma::vec r(x.n_elem);
   int n = x.n_elem;
   for(int i = 0; i < n; i++){
      arma::uvec a1 = arma::find(x < x(i));
      arma::uvec a2 = arma::find(x == x(i));
      r(i) = (a1.n_elem + 0.5 * a2.n_elem) + 0.5;
   }
   return r;
}

// [[Rcpp::export("var_cpp")]]
double var_cpp(arma::vec x){
   int n = x.n_elem;
   double x_bar = arma::mean(x);
   double v = 0;
   for(int i = 0; i < n; i++){
      v += pow(x(i) - x_bar, 2);
   }
   return v / (n-1);
}

// [[Rcpp::export("bmstat_cpp")]]
Rcpp::NumericVector bmstat_cpp(arma::vec x, arma::vec y){
   int n_x = x.n_elem;
   int n_y = y.n_elem;
   int N   = n_x + n_y;
   arma::vec xy = arma::join_cols(x, y);
   arma::vec rxy = rank_ind_cpp(xy);
   arma::vec rx  = rank_ind_cpp(x);
   arma::vec ry  = rank_ind_cpp(y);
   //arma::vec posx = arma::regspace(0, n_x - 1);
   //arma::vec posy = arma::regspace(n_x, N - 1);
   arma::vec rx_int = rxy.subvec(0, n_x - 1);//rxy(posx);
   arma::vec ry_int = rxy.subvec(n_x, N - 1); //rxy(posy);
   arma::vec plx = (rx_int - rx) / n_y;
   arma::vec ply = (ry_int - ry) / n_x;
   
   double vx = var_cpp(plx);
   double vy = var_cpp(ply);
   double vxy = N * (vx / n_x + vy/n_y);
   double pd = arma::mean(ply);
   
  //if(pd == 0){
   //   pd = (1 / (2 * n_x)) / n_y;
//   }
   // if(pd == 1){
   //    pd = (n_x - 1/(2 * n_y)) /n_x;
   // }
    if(vxy == 0){
      vxy =  N / (2 * n_x * n_y);
    }
   
   arma::vec cent = rxy - (N + 1) / 2;
   double nen = pow(n_x, (-1)) + pow(n_y, (-1));
   double VHF = arma::dot(cent, cent) / (N * (N - 1)) * nen;
   double df_sw = pow(vx / n_x + vy / n_y, 2) / (pow(vx,2) / (pow(n_x, 2) * (n_x - 1)) + pow(vy, 2) / (pow(n_y, 2) * (n_y - 1)));
   
   double Z_exact = arma::sum(rxy.subvec(n_x, N - 1));
   double T = sqrt(N) * (pd - 0.5) / sqrt(vxy);
   double se_pd = sqrt(vxy / N);
   double f_pd = pd;
   
   Rcpp::NumericVector res_list = Rcpp::NumericVector::create(pd, vx, vy, vxy, T, f_pd, se_pd, VHF, df_sw, se_pd, Z_exact);
   return res_list;
   
}

// [[Rcpp::export("bm_test_cpp")]]
Rcpp::NumericVector bm_test_cpp(arma::vec x, arma::vec y, double alpha){
   Rcpp::NumericVector bmstats = bmstat_cpp(x, y);
   double x_stat = bmstats[4];
   double df = bmstats[8];
   double p_val = 2 * Rcpp::min(Rcpp::NumericVector::create(R::pt(x_stat, df, TRUE, FALSE), 1 - R::pt(x_stat, df, TRUE, FALSE)));
   
   double crit1 = R::qt(1 - alpha/2, df, TRUE, FALSE);
   double crit2 = R::qt(alpha/2, df, TRUE, FALSE);
   
   Rcpp::NumericVector rel_eff = Rcpp::NumericVector::create(bmstats[0], bmstats[9], bmstats[4], p_val);
   return rel_eff;
}
