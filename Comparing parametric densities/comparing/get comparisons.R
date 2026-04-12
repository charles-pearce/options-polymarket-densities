source("comparing\\complete_pipeline.R")
source("comparing\\comparing probabilities.R")
source("PM\\logistic-recalibration-PM-parquet.R")
library(tidyverse)

# method = "1lognorm"
parquet_loc ="C:\\Users\\lars\\OneDrive\\Dokumente\\Uni\\Seminar Forecasting\\data collection\\Getting poly data\\polymarket_data\\all_prices.parquet"
data_loc = "C:\\Users\\lars\\OneDrive\\Dokumente\\Uni\\Seminar Forecasting\\data collection\\Getting poly data\\data"

calibration_coeff = get_calibration_coefficients(parquet_loc)

for (calibrated in c(TRUE, FALSE)){
  for(method in c("3lognorm", "nonparam", "1lognorm")){
    if (calibrated){
      results = get_results(data_loc = data_loc,
                            nclusters = 8, 
                            method = method, 
                            calibration_coeff = calibration_coeff)
    } else {
      results = get_results(data_loc = data_loc,
                            nclusters = 8, 
                            method = method, 
                            calibration_coeff = list("COMBINED" = c(1,1,1,1)))
    }
    print(paste("got results for ", method, calibrated))
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
    
    # create folder
    c = ifelse(calibrated, "calibrated", "")
    folder_name = paste("results\\", method,  c)
    dir.create(folder_name)
    
    write.csv(density_result, paste0(folder_name,"\\", method,"_", c, "density_result.csv"), row.names = FALSE)
    write.csv(accuracy_result,  paste0(folder_name,"\\", method,"_", c, "accuracy_result.csv"), row.names = FALSE)
    write.csv(mz_result,  paste0(folder_name,"\\", method,"_", c, "mz_result.csv"), row.names = FALSE)
    write.csv(skipped,  paste0(folder_name,"\\",method,"_", c, "skipped_dates.csv"), row.names = FALSE)
  }
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
