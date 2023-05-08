#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector calc_mort_rate_cpp(NumericVector age, IntegerVector is_retirement, NumericVector male_employee, NumericVector male_healthy_retiree,
                                 NumericVector male_cumprod_mp_adj, NumericVector female_employee, NumericVector female_healthy_retiree,
                                 NumericVector female_cumprod_mp_adj, NumericVector male_mort_table_age, NumericVector male_mort_table_rate,
                                 NumericVector female_mort_table_age, NumericVector female_mort_table_rate) {
  
  // Initialize the output vector
  int n = age.size();
  NumericVector mort_rate(n);
  
  // Loop over the input vectors and compute the mortality rate for each element
  for (int i = 0; i < n; i++) {
    // Calculate male mortality rate
    double male_rate = 0.0;
    int nrow_male = male_mort_table_age.size();
    int male_index = std::lower_bound(male_mort_table_age.begin(), male_mort_table_age.end(), age[i]) - male_mort_table_age.begin();
    if (male_index < nrow_male) {
      male_rate = male_employee[i] * male_mort_table_rate[male_index];
    }
    double male_mort_rate = male_cumprod_mp_adj[i] * (is_retirement[i] * male_healthy_retiree[i] + (!is_retirement[i]) * male_rate);
    
    // Calculate female mortality rate
    double female_rate = 0.0;
    int nrow_female = female_mort_table_age.size();
    int female_index = std::lower_bound(female_mort_table_age.begin(), female_mort_table_age.end(), age[i]) - female_mort_table_age.begin();
    if (female_index < nrow_female) {
      female_rate = female_employee[i] * female_mort_table_rate[female_index];
    }
    double female_mort_rate = female_cumprod_mp_adj[i] * (is_retirement[i] * female_healthy_retiree[i] + (!is_retirement[i]) * female_rate);
    
    
    // Calculate the overall mortality rate as the average of male and female rates
    mort_rate[i] = (male_mort_rate + female_mort_rate) / 2.0;
  }
  
  // Return the vector of mortality rates
  return mort_rate;
}


