
s_fund <- Sys.time()
det_SQ_assumption <-  memoised_get_funding_data(
  curr_disc_rate = 0.0755,
  new_disc_rate = 0.0755,
  cola = 0.03,
  retire_refund_ratio = 0.6,
  funding_policy = "status quo",
  analysis_type = "Deterministic",
  roa_scenario = "Assumption"
)
e_fund <- Sys.time()

det_SQ_recur_recession <-  memoised_get_funding_data(
  curr_disc_rate = 0.0755,
  new_disc_rate = 0.0755,
  cola = 0.03,
  retire_refund_ratio = 0.6,
  funding_policy = "status quo",
  analysis_type = "Deterministic",
  roa_scenario = "Recurring Recession"
)

det_ADC_assumption <-  memoised_get_funding_data(
  curr_disc_rate = 0.0755,
  new_disc_rate = 0.0755,
  cola = 0.03,
  retire_refund_ratio = 0.6,
  funding_policy = "ADC",
  analysis_type = "Deterministic",
  roa_scenario = "Assumption"
)

det_ADC_recur_recession <-  memoised_get_funding_data(
  curr_disc_rate = 0.0755,
  new_disc_rate = 0.0755,
  cola = 0.03,
  retire_refund_ratio = 0.6,
  funding_policy = "ADC",
  analysis_type = "Deterministic",
  roa_scenario = "Recurring Recession"
)



file_path = "/Users/anhtu/Documents/Reason Org/Actuarial Modeling Projects/Mississippi PERS/Mississippi-PERS-modeling-Steve-Github/Outputs/"
write.xlsx(det_SQ_assumption, file=paste0(file_path, "det_SQ_assumption.xlsx"))
write.xlsx(det_SQ_recur_recession, file=paste0(file_path, "det_SQ_recur_recession.xlsx"))
write.xlsx(det_ADC_assumption, file=paste0(file_path, "det_ADC_assumption.xlsx"))
write.xlsx(det_ADC_recur_recession, file=paste0(file_path, "det_ADC_recur_recession.xlsx"))















