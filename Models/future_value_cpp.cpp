#include <Rcpp.h>
using namespace Rcpp;


// [[Rcpp::export]]
double get_npv_rcpp(double rate, NumericVector cashflows){
  int n = cashflows.size();
  NumericVector discount_factors(n);
  for(int i = 0; i < n; i++){
    discount_factors[i] = 1 / std::pow(1+rate, i+1);
  }
  double net_present_value = sum(cashflows * discount_factors);
  return(net_present_value);
}


// [[Rcpp::export]]
NumericVector lag_rcpp(NumericVector x, int k, double default_value) {
  int n = x.size();
  if (k >= n) {
    return rep(default_value, n);
  } else {
    NumericVector result(n);
    result[seq(k, n-1)] = NumericVector(x[seq(0, n-k-1)]);
    result[seq(0, k-1)] = NumericVector(k, default_value);
    return result;
  }
}

// [[Rcpp::export]]
NumericVector cumprodC(NumericVector x) {
  int n = x.size();
  NumericVector out(n);
  
  out[0] = x[0];
  for(int i = 1; i < n; ++i) {
    out[i]  = out[i - 1] * x[i];
  }
  return out;
}


// [[Rcpp::export]]
NumericVector opt_PVFB_rcpp(NumericVector sep_rate_vec, NumericVector interest, NumericVector value_vec) {
  int n = value_vec.size();
  NumericVector PVFB(n);
  for (int i = 0; i < n; i++) {
    NumericVector sep_rate_ = sep_rate_vec[seq(i, n-1)];
    // Rcpp::Rcout << "sep_rate_: " << sep_rate_ << std::endl;
    
    NumericVector sep_prob = cumprodC(1 - lag_rcpp(sep_rate_, 2, 0)) * lag_rcpp(sep_rate_, 1, 0);
    // Rcpp::Rcout << "sep_prob: " << sep_prob << std::endl;
    
    NumericVector value = value_vec[seq(i, n-1)];
    // Rcpp::Rcout << "value: " << value << std::endl;
    
    NumericVector value_adjusted = value * sep_prob;
    // Rcpp::Rcout << "value_adjusted: " << value_adjusted << std::endl;
    
    if (value_adjusted.size() > 1){
      PVFB[i] = get_npv_rcpp(interest[i], value_adjusted[seq(1, value_adjusted.size() - 1)]);
    } else{
      PVFB[i] = get_npv_rcpp(interest[i], value_adjusted);
    }
    
    // Rcpp::Rcout << "PVFB[" << i << "]: " << PVFB[i] << std::endl;
    
  }
  return PVFB;
}


// [[Rcpp::export]]
NumericVector opt_PVFS_rcpp(NumericVector remaining_prob_vec, NumericVector interest, NumericVector sal_vec){
  int n = sal_vec.size();
  NumericVector PVFS(n);
  for (int i = 0; i < n; i++){
    NumericVector remaining_prob_og = remaining_prob_vec[seq(i, n-1)];
    NumericVector remaining_prob = remaining_prob_og / remaining_prob_og[0];
    NumericVector sal = sal_vec[seq(i, n-1)];
    NumericVector sal_adjusted = sal * remaining_prob;
    PVFS[i] = get_npv_rcpp(interest[i], sal_adjusted);
  }
  return PVFS;
}


