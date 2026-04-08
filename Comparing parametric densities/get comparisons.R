source("comparing\\complete_pipeline.R")
source("comparing\\comparing probabilities.R")
library(tidyverse)

data_loc = "C:\\Users\\lars\\OneDrive\\Dokumente\\Uni\\Seminar Forecasting\\data collection\\Getting poly data\\data"
results = get_results(data_loc = data_loc,
                      nclusters = 8, 
                      method = "1lognorm", 
                      calibration_coeff = c(1,1,1,1))
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

write.csv(density_result, "density_result_ci.csv", row.names = FALSE)
write.csv(accuracy_result, "accuracy_result_ci.csv", row.names = FALSE)
write.csv(mz_result, "mz_result_ci.csv", row.names = FALSE)
write.csv(skipped, "skipped_dates_ci.csv", row.names = FALSE)


library(profvis)

profvis({combined_results(files[1], data_loc)})

combined_results(files[103], data_loc, method = "1lognorm",
                 calibration_coeff = c(1,1,1,1))
