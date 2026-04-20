files = list.files(data_loc, pattern = "_td\\.csv$", 
                   full.names = FALSE, recursive = TRUE)
name = files[96]
cal_name = "NOT CALIBRATED"
dte = 2
method = "3lognorm"
dx = 0.1


source("comparing\\Comparing densities.R")
source("comparing\\comparing probabilities.R")
source("Options\\am_density.R")
source("PM\\am pm density.R")
source("Non_parametric\\non-parametric-approach-t-asymptotes.R")
source("Non_parametric\\shimko-density.R")
library(dplyr)

split = unlist(strsplit(name, "_"))
file_name = paste(head(split, length(split) - 1), collapse = "_")
stock = unlist(strsplit(name, "/"))[1]
date = regmatches(name, regexpr("\\d{4}-\\d{2}-\\d{2}", name))

option_data = read.csv(paste0(data_loc, "\\",file_name,"_td.csv"))
pm_data = read.csv(paste0(data_loc, "\\", file_name, "_pm.csv"))

max_bracket = max(unlist(sapply(
  (strsplit(gsub( "<|>", "",pm_data$bracket), "-")),
  function(x) as.numeric(x)
)))


sum_prices = sum((pm_data|>
                    filter(DTE == dte))$price_yes)
# if the sum of all yes prices is above 2, skip this dte as the prices
# are not reliable/informative
if (sum_prices > 2){
  skipped_list[[length(skipped_list)+1]] = data.frame(
    "stock" = stock, 
    "date" = date, 
    "dte" = dte, 
    "calibration_level" = cal_name,
    "reason" = "sum price > 2",
    "error" = "sum price > 2"
  )
  # stop(paste("sum prices:", sum_prices))
}

# calibrate and/or normalize the prices
cal = calibration_coeff[[cal_name]]
print(cal[dte])
if(cal[dte] == 1){
  this_pm_data = filter(pm_data, DTE == dte)|>
    mutate(probs = price_yes / sum_prices)
} else {
  b = cal[dte]
  this_pm_data = pm_data |>
    filter(DTE == dte) |>
    mutate(probs = 
             (price_yes / sum_prices)^b / (
               (price_yes / sum_prices)^b + 
                 (1- (price_yes / sum_prices))^b
             )
    )
  this_pm_data = this_pm_data |>
    mutate(probs = probs/sum(probs))
}


y_vals_pm    = list()
y_vals_options = list()
x_values = list()

for(method in c("3lognorm", "1lognorm", "nonparam")){
  
  attempt_option = try({
    option_density = get_option_density(data = option_data,
                                        dte = dte,
                                        method = method,
                                        dx = dx)
  }, silent = FALSE)
  
  # If an error happened, 'attempt' will be of class "try-error"
  if (inherits(attempt_option, "try-error")) {
    skipped_list[[length(skipped_list)+1]] = data.frame(
      "stock" = stock, 
      "date" = date, 
      "dte" = dte,
      "calibration_level" = cal_name,
      "reason" = "option density not found",
      "error" = attempt_option[1]
    )
    
    next
  }
  
  
  attempt_pm = try({
    x_range = if (method == "nonparam") range(option_density$rnd_K) else NULL
    pm_density = fit_pm_density(method = method, 
                                data = this_pm_data,
                                x_range = x_range,
                                dx = dx)
  }, silent = FALSE)
  
  # If an error happened, 'attempt' will be of class "try-error"
  if (inherits(attempt_pm, "try-error")) {
    skipped_list[[length(skipped_list)+1]] = data.frame(
      "stock" = stock, 
      "date" = date, 
      "dte" = dte,
      "calibration_level" = cal_name,
      "reason" = "pm density not found",
      "error" = attempt_pm[1]
    )
    
    next
  }
  
  if(method == "nonparam"){
    x_new = option_density$rnd_K
    y_options = option_density$rnd
    y_pm = pm_density$data$y
    
  } else {
    x_new = seq(0, max_bracket*2, by = dx)
    y_options = mixture_density_options(x = x_new,
                                        density_obj = option_density,
                                        method = method)
    
    y_pm = mixture_density_pm(x = x_new, 
                              params = pm_density, 
                              method = method)
    
  }
  
  y_vals_options[[method]] = y_options
  y_vals_pm[[method]] = y_pm
  x_values[[method]] = x_new
  
}


n = max(lengths(x_values)) 

pad = function(x) c(x, rep(NA, n - length(x)))


plot_data = data.frame("x_3log" = x_values[["3lognorm"]],
               "y_options_3log" = y_vals_options[["3lognorm"]],
               "y_pm_3log" = y_vals_pm[["3lognorm"]],
               "x_1log" = x_values[["1lognorm"]],
               "y_options_1log" = y_vals_options[["1lognorm"]],
               "y_pm_1log" = y_vals_pm[["1lognorm"]],
               "x_nonparam" = pad(x_values[["nonparam"]]),
               "y_options_nonparam" = pad(y_vals_options[["nonparam"]]),
               "y_pm_nonparam" = pad(y_vals_pm[["nonparam"]])
)

write.csv(plot_data, "nvda20260325.csv")




