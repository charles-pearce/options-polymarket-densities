source("comparing\\complete_pipeline.R")
source("comparing\\comparing probabilities.R")
source("PM\\logistic-recalibration-PM-normalized.R")
library(tidyverse)

# method = "1lognorm"
parquet_loc ="C:\\Users\\lars\\OneDrive\\Dokumente\\Uni\\Seminar Forecasting\\data collection\\Getting poly data\\polymarket_data\\all_prices.parquet"
data_loc = "C:\\Users\\lars\\OneDrive\\Dokumente\\Uni\\Seminar Forecasting\\data collection\\Getting poly data\\data"


coefs_per_stock_whole_timegroup = get_calibration_coefs(parquet_loc, pool_time = TRUE)
coefs_combined = get_calibration_coefs(parquet_loc, global = TRUE)

calibration_coeff = split(coefs_per_stock_whole_timegroup, coefs_per_stock_whole_timegroup[, "stock"]) |>
  lapply(function(df) unname(df[, "logit_prices"]))
names(calibration_coeff) = toupper(names(calibration_coeff))
calibration_coeff$"COMBINED" = coefs_combined$logit_prices
calibration_coeff$"NOT CALIBRATED" = c(1,1,1,1)


for(method in c( "nonparam", "3lognorm","1lognorm")){
  
  results = get_results(data_loc = data_loc,
                        nclusters = 8, 
                        method = method, 
                        calibration_coeff = calibration_coeff)
  
  print(paste("got results for ", method))
  density_result = results$density_comparison
  probabilities = results$probabilities
  # remove probabilities where the bracket is from 0-0
  probabilities = probabilities[probabilities$bracket_upper > 0,]
  
  skipped = results$skipped
  
  
  accuracy_result_dte = compare_accuracy_by_dte(probabilities) |>
    mutate(stock = "all")
  
  accuracy_result_dte_stock = probabilities |>
    nest_by(stock) |>
    mutate(result = list(compare_accuracy_by_dte(data))) |>
    unnest(result) |>
    select(-data)
  
  accuracy_result = rbind(accuracy_result_dte, accuracy_result_dte_stock)
  
  daily_loss_scores = probabilities |> 
    mutate(
      log_score_pm       = log_loss(pm_probability, outcome),
      log_score_option   = log_loss(option_probability, outcome),
      brier_score_pm     = brier_loss(pm_probability, outcome),
      brier_score_option = brier_loss(option_probability, outcome)
    ) |>
    nest_by(Date, DTE, stock, calibration_method) |>
    mutate(mean_log_score_pm = mean(data$log_score_pm),
           mean_log_score_option = mean(data$log_score_option),
           mean_brier_score_pm = mean(data$brier_score_pm),
           mean_brier_score_option = mean(data$brier_score_option)) |>
    select(-data)
  
  mz_results_by_DTE = probabilities |>
    nest_by(DTE)|>
    mutate(result = list(test_mz(lm(pm_probability ~ option_probability, data = data))),
           stock = "all") |>
    unnest(result) |>
    select(DTE, coefficient, estimate, F_stat, p_val, stock)
  
  mz_result_by_DTE_stock = probabilities |>
    nest_by(DTE, stock) |>
    mutate(result = list(test_mz(lm(pm_probability ~ option_probability, data = data)))) |>
    unnest(result) |>
    select(DTE, coefficient, estimate, F_stat, p_val, stock)
  
  mz_result = rbind(mz_results_by_DTE, mz_result_by_DTE_stock)
  
  # save results
  folder_name = paste0("results\\", method)
  dir.create(folder_name)
  
  write.csv(density_result, paste0(folder_name,"\\", method,"_", "density_result.csv"), row.names = FALSE)
  write.csv(accuracy_result,  paste0(folder_name,"\\", method,"_", "accuracy_result.csv"), row.names = FALSE)
  write.csv(daily_loss_scores, paste0(folder_name,"\\", method, "_daily_accuracy_result.csv"), row.names = FALSE)
  write.csv(mz_result,  paste0(folder_name,"\\", method,"_", "mz_result.csv"), row.names = FALSE)
  write.csv(skipped,  paste0(folder_name,"\\",method,"_", "skipped_dates.csv"), row.names = FALSE)
}


library(profvis)

profvis({combined_results(files[1], data_loc)})

combined_results(files[1], data_loc, method = "3lognorm",
                 calibration_coeff = calibration_coeff)

files[2]
length(files)

x = sample(1:137, 137)

for (i in x){
  print(i)
  a = combined_results(files[i], data_loc, method = "3lognorm", calibration_coeff = calibration_coeff)
}
