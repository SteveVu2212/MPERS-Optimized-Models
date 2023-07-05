
source("./Models/MasterModel.R")

s_fund <- Sys.time()
det_SQ_assumption_755pct <-  memoised_get_funding_data(
  curr_disc_rate = 0.06,
  new_disc_rate = 0.06,
  cola = 0.03,
  retire_refund_ratio = 0.6,
  funding_policy = "status quo",
  analysis_type = "Deterministic",
  roa_scenario = "Assumption"
)
e_fund <- Sys.time()

# s_fund <- Sys.time()
# det_SQ_assumption_7pct <-  memoised_get_funding_data(
#   curr_disc_rate = 0.07,
#   new_disc_rate = 0.07,
#   cola = 0.03,
#   retire_refund_ratio = 0.6,
#   funding_policy = "status quo",
#   analysis_type = "Deterministic",
#   roa_scenario = "Assumption"
# )
# e_fund <- Sys.time()


det_SQ_recur_recession_755pct <-  memoised_get_funding_data(
  curr_disc_rate = 0.06,
  new_disc_rate = 0.06,
  cola = 0.03,
  retire_refund_ratio = 0.6,
  funding_policy = "status quo",
  analysis_type = "Deterministic",
  roa_scenario = "Recurring Recession"
)

# det_SQ_recur_recession_7pct <-  memoised_get_funding_data(
#   curr_disc_rate = 0.07,
#   new_disc_rate = 0.07,
#   cola = 0.03,
#   retire_refund_ratio = 0.6,
#   funding_policy = "status quo",
#   analysis_type = "Deterministic",
#   roa_scenario = "Recurring Recession"
# )

det_ADC_assumption_755pct <-  memoised_get_funding_data(
  curr_disc_rate = 0.06,
  new_disc_rate = 0.06,
  cola = 0.03,
  retire_refund_ratio = 0.6,
  funding_policy = "ADC",
  analysis_type = "Deterministic",
  roa_scenario = "Assumption"
)

# det_ADC_assumption_7pct <-  memoised_get_funding_data(
#   curr_disc_rate = 0.07,
#   new_disc_rate = 0.07,
#   cola = 0.03,
#   retire_refund_ratio = 0.6,
#   funding_policy = "ADC",
#   analysis_type = "Deterministic",
#   roa_scenario = "Assumption"
# )


det_ADC_recur_recession_755pct <-  memoised_get_funding_data(
  curr_disc_rate = 0.06,
  new_disc_rate = 0.06,
  cola = 0.03,
  retire_refund_ratio = 0.6,
  funding_policy = "ADC",
  analysis_type = "Deterministic",
  roa_scenario = "Recurring Recession"
)

# det_ADC_recur_recession_7pct <-  memoised_get_funding_data(
#   curr_disc_rate = 0.07,
#   new_disc_rate = 0.07,
#   cola = 0.03,
#   retire_refund_ratio = 0.6,
#   funding_policy = "ADC",
#   analysis_type = "Deterministic",
#   roa_scenario = "Recurring Recession"
# )

file_path = "/Users/anhtu/Documents/Reason Org/Actuarial_Modeling_Projects/MS_PERS/MPERS-Optimized-Models-Steve-Github/Outputs/"
write.xlsx(det_SQ_assumption_755pct, file=paste0(file_path, "det_SQ_assumption_6pct.xlsx"))
write.xlsx(det_SQ_recur_recession_755pct, file=paste0(file_path, "det_SQ_recur_recession_6pct.xlsx"))
write.xlsx(det_ADC_assumption_755pct, file=paste0(file_path, "det_ADC_assumption_6pct.xlsx"))
write.xlsx(det_ADC_recur_recession_755pct, file=paste0(file_path, "det_ADC_recur_recession_6pct.xlsx"))

# write.xlsx(det_SQ_assumption_7pct, file=paste0(file_path, "det_SQ_assumption_7pct.xlsx"))
# write.xlsx(det_SQ_recur_recession_7pct, file=paste0(file_path, "det_SQ_recur_recession_7pct.xlsx"))
# write.xlsx(det_ADC_assumption_7pct, file=paste0(file_path, "det_ADC_assumption_7pct.xlsx"))
# write.xlsx(det_ADC_recur_recession_7pct, file=paste0(file_path, "det_ADC_recur_recession_7pct.xlsx"))















