####################### Memoization ####################
# opt_annuity_factor_func <- memoise(function(x) {
#   data.table(x)[, {
#     discount_rate <- ifelse(entry_year <= year_start, curr_discount_rate, new_discount_rate)
#     survival_rate <- cumprod(1 - shift(mort_rate, fill = 0, type = "lag"))
#     disc_survival_rate <- survival_rate / (1 + discount_rate) ^ (age - min_age)
#     disc_survival_rate_cola <- disc_survival_rate * (1 + cola) ^ (age - min_age)
#     annuity_factor <- rev(cumsum(rev(disc_survival_rate_cola))) / disc_survival_rate_cola
#     .(discount_rate, survival_rate, disc_survival_rate, disc_survival_rate_cola, annuity_factor)
#   }, by = .(entry_year, entry_age, yos)]
# })
# 
# opt_annuity_factor_tab <- opt_annuity_factor_func(opt_mortality_rate)

#################### Compilation #######################
# opt_annuity_factor_func <- cmpfun(function(x) {
#   data.table(x)[, {
#     discount_rate <- ifelse(entry_year <= year_start, curr_discount_rate, new_discount_rate)
#     survival_rate <- cumprod(1 - shift(mort_rate, fill = 0, type = "lag"))
#     disc_survival_rate <- survival_rate / (1 + discount_rate) ^ (age - min_age)
#     disc_survival_rate_cola <- disc_survival_rate * (1 + cola) ^ (age - min_age)
#     annuity_factor <- rev(cumsum(rev(disc_survival_rate_cola))) / disc_survival_rate_cola
#     .(discount_rate, survival_rate, disc_survival_rate, disc_survival_rate_cola, annuity_factor)
#   }, by = .(entry_year, entry_age, yos)]
# })
# 
# opt_annuity_factor_tab <- opt_annuity_factor_func(opt_mortality_rate)

# setDT(opt_mortality_rate)
# setkey(opt_mortality_rate, entry_year, entry_age, yos)
# 
# opt_mortality_rate[, min_age := cummin(age)]
# opt_annuity_factor_tab <- opt_mortality_rate[, {
#   discount_rate = ifelse(entry_year <= year_start, curr_discount_rate, new_discount_rate)
#   survival_rate_ = cumprod(1 - shift(mort_rate, fill = 0, type = "lag"))
#   disc_survival_rate = survival_rate_/(1+discount_rate)^(age - min_age)
#   disc_survival_rate_cola = disc_survival_rate * (1+cola)^(age - min_age)
#   annuity_factor = rev(cumsum(rev(disc_survival_rate_cola)))/disc_survival_rate_cola
#   .(discount_rate, survival_rate_, disc_survival_rate, disc_survival_rate_cola, annuity_factor)
# }, by = .(entry_year, entry_age, yos)]

################ Parallel processing 1 ##################
# setDT(opt_mortality_rate)
# opt_mortality_rate[, min_age := cummin(age)]
# n_cores <- parallel::detectCores() - 1
# future::plan(future::multisession(workers = n_cores))
# opt_annuity_factor_tab <- opt_mortality_rate[, {
#   discount_rate = ifelse(entry_year <= year_start, curr_discount_rate, new_discount_rate)
#   survival_rate_ = cumprod(1 - shift(mort_rate, fill = 0, type = "lag"))
#   disc_survival_rate = survival_rate_/(1+discount_rate)^(age - min_age)
#   disc_survival_rate_cola = disc_survival_rate * (1+cola)^(age - min_age)
#   annuity_factor = rev(cumsum(rev(disc_survival_rate_cola)))/disc_survival_rate_cola
#   .(discount_rate, survival_rate_, disc_survival_rate, disc_survival_rate_cola, annuity_factor)
# }, by = .(entry_year, entry_age, yos)]

################ Parallel processing 2 ##################
# # Set the number of workers
# plan(multisession, workers = detectCores())
# uni_comb <- unique(opt_mortality_rate[, .(entry_year, entry_age, yos)])
# start_time = Sys.time()
# # Split the data by unique combinations of entry_year, entry_age, and yos, and compute annuity factors in parallel
# opt_annuity_factor_tab <- future_lapply(seq_len(nrow(uni_comb)), function(i) {
#   info = as.list(uni_comb[i])
#   subset <- opt_mortality_rate[entry_year == info$entry_year & entry_age == info$entry_age & yos == info$yos]
#   subset[, {
#     discount_rate <- ifelse(entry_year <= year_start, curr_discount_rate, new_discount_rate)
#     survival_rate <- cumprod(1 - shift(mort_rate, fill = 0, type = "lag"))
#     disc_survival_rate <- survival_rate / (1 + discount_rate) ^ (age - min_age)
#     disc_survival_rate_cola <- disc_survival_rate * (1 + cola) ^ (age - min_age)
#     annuity_factor <- rev(cumsum(rev(disc_survival_rate_cola))) / disc_survival_rate_cola
#     .(discount_rate, survival_rate, disc_survival_rate, disc_survival_rate_cola, annuity_factor)
#   }]
# })
# # Combine the results into a single data table
# opt_annuity_factor_tab <- rbindlist(opt_annuity_factor_tab, fill = TRUE)
# end_time = Sys.time()
# print(end_time - start_time)

# setDT(opt_mortality_rate)
# setkey(opt_mortality_rate, entry_year, entry_age, yos)
# n_cores <- detectCores() - 1
# cl <- makeCluster(n_cores)
# clusterExport(cl, c("cola", "curr_discount_rate", "new_discount_rate", "year_start"))
# opt_annuity_factor_tab <- opt_mortality_rate[, {
#   discount_rate = ifelse(entry_year[1] <= year_start, curr_discount_rate, new_discount_rate)
#   survival_rate_ = cumprod(1 - shift(mort_rate, fill = 0, type = "lag"))
#   min_age = min(age)
#   disc_survival_rate = survival_rate_/(1+discount_rate)^(age - min_age)
#   disc_survival_rate_cola = disc_survival_rate * (1+cola)^(age - min_age)
#   annuity_factor = rev(cumsum(rev(disc_survival_rate_cola)))/disc_survival_rate_cola
#   .(discount_rate, survival_rate_, disc_survival_rate, disc_survival_rate_cola, annuity_factor)
# }, by = .(entry_year, entry_age, yos)][order(entry_year, entry_age, yos)]




# library(data.table)
# library(parallel)
# library(doFuture)
# library(future.apply)
# annuity_factor_tab_dt <- setDT(annuity_factor_tab)
# setorder(annuity_factor_tab_dt, entry_year, entry_age, yos)
# plan(multisession, workers = detectCores())
# uni_comb <- unique(annuity_factor_tab_dt[, .(entry_year, entry_age, yos)])
# opt_reduced_factor_tab_ <- future_lapply(seq_len(nrow(uni_comb)), function(i){
#   info = as.list(uni_comb[i])
#   subset <- annuity_factor_tab_dt[entry_year %chin% info$entry_year & entry_age == info$entry_age & yos == info$yos]
#   subset[, {
#     retirement_type = get_retirement_type(age, yos, entry_year)
#     reduced_factor = ifelse(
#         retirement_type == 'early', annuity_factor[age == 65] * (disc_survival_rate[age == 65] / disc_survival_rate) / annuity_factor,
#         ifelse(retirement_type == 'normal', 1, 0))
#     .(entry_year, entry_age, retire_age=age, yos, retirement_type, reduced_factor)
#   }]
# })
# opt_reduced_factor_tab <- rbindlist(opt_reduced_factor_tab_, fill=TRUE)


######################### Retirement Eligibility #########################
# init_check_normal_retirement_eligibility <- function(age, yos, entry_year){
#   check_ <- ifelse(
#     (entry_year <= 2007 & yos >= 25) |
#       (entry_year <= 2007 & age >= 60 & yos >= 4) |
#       (entry_year > 2007 & entry_year < 2011 & yos >= 25) |
#       (entry_year > 2007 & entry_year < 2011 & age >= 60 & yos >= 8) |
#       (entry_year >= 2011 & yos >= 30) |
#       (entry_year >= 2011 & age >= 65 & yos >= 8),
#     TRUE, FALSE)
#   return(check_)
# }
# 
# 
# # Define a lookup table for normal retirement eligibility conditions
# normal_retirement_eligibility <- function() {
#   data.table(
#     entry_year = c(2007, 2011, Inf),
#     yos_threshold = c(25, 25, 30),
#     age_threshold = c(60, 60, 65),
#     yos_offset = c(0, 4, 8)
#   )
# }
# 
# # Check normal retirement eligibility
# opt_check_normal_retirement_eligibility <- function(age, yos, entry_year) {
#   memo_table <- data.table(age = double(), yos = double(), entry_year = double(), check = logical()) # create an empty memoization table
#   if (nrow(memo_table[age == age & yos == yos & entry_year == entry_year, check]) > 0) { # check if the result is already memoized
#     return(memo_table[age == age & yos == yos & entry_year == entry_year, check])
#   } else {
#     elg <- normal_retirement_eligibility()
#     check <- (entry_year <= 2007 & yos >= 25) |
#       (entry_year <= 2007 & age >= 60 & yos >= 4) |
#       (entry_year > 2007 & entry_year < 2011 & yos >= 25) |
#       (entry_year > 2007 & entry_year < 2011 & age >= 60 & yos >= 8) |
#       (entry_year >= 2011 & yos >= 30) |
#       (entry_year >= 2011 & age >= 65 & yos >= 8)
#     memo_table <- rbind(memo_table, data.table(age = age, yos = yos, entry_year = entry_year, check = check)) # memoize the result
#     return(check)
#   }
# }


################################ Mortality rate ##################################
# dt_male_mp <- data.table(male_mp)
# setkey(dt_male_mp, age, year)
# dt_male_ultimate_mp <- data.table(male_ultimate_mp)
# setkey(dt_male_ultimate_mp, age)
# 
# opt_male_final_mp <- function(){
#   dt_male_final_mp <- CJ(age=age_range, year = 1951:max_year)
#   
#   setkey(dt_male_final_mp, age)
#   
#   df_join1 <- dt_male_final_mp[dt_male_mp, nomatch = 0, on = c("age", "year")]
#   
#   setkey(df_join1, age)
#   
#   dt_opt_male_final_mp <- df_join1[dt_male_ultimate_mp, nomatch = 0, on = "age"]
#   
#   male_final_mp <- dt_opt_male_final_mp %>%
#     mutate(
#       male_final_mp_rate = ifelse(year > max(male_mp$year), male_ultimate_mp_rate, male_mp_rate)
#       # the "ultimate rates" are used for all years
#       # male_final_mp_rate = male_ultimate_mp_rate
#     ) %>%
#     group_by(age) %>%
#     mutate(
#       male_cumprod_mp_raw = cumprod(1 - male_final_mp_rate),
#       male_cumprod_mp_adj = male_cumprod_mp_raw / male_cumprod_mp_raw[year == 2010]) %>%   #Adjust the mort improvement rates for the 2010 base year
#     ungroup()
# }


# # Load the parallel package
# library(parallel)
# 
# s_par <- Sys.time()
# # Create a cluster of workers
# cl <- makeCluster(4)
# 
# # Create a list of function names
# function_names <- c("wf_active_func", "wf_term_func", "wf_refund_func", "wf_retire_func")
# 
# # Use mclapply to run the functions in parallel
# results <- mclapply(function_names, function(f) {
#   f <- get(f)
#   f(wf_projection, benefit_data)
# }, mc.cores = length(cl))
# 
# # Stop the cluster
# stopCluster(cl)
# 
# # Print the results
# wf_active_df_final <- results[[1]]
# wf_term_df_final <- results[[2]]
# wf_refund_df_final <- results[[3]]
# wf_retire_df_final <- results[[4]]
# 
# e_par = Sys.time()



