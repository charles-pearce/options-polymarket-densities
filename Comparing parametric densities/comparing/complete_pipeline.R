# calibration_coeff must be of form 
# list("COMBINED = c(dte1, dte2, dte3, dte4), "AAPL" = c(dte1, dte2,...), ...)
 
get_results = function(data_loc, method, calibration_coeff = list("COMBINED" =c(1,1,1,1)), 
                       nclusters = 2, progress = TRUE){
  require(doParallel)
  require(foreach)
  require(purrr)
  require(dplyr)
  require(doSNOW)
  require(doRNG)
  
  cl <- makeCluster(nclusters)

  # only iterate over pm files
  files = list.files(data_loc, pattern = "_td\\.csv$", 
                                     full.names = FALSE, recursive = TRUE)
  
  on.exit(stopCluster(cl))
  
  # add progress bar
  registerDoSNOW(cl)
  pb <- txtProgressBar(max = length(files), style = 3)
  progress <- function(n) setTxtProgressBar(pb, n)
  opts <- list(progress = progress)
  
  # wd = getwd()
  # 
  # clusterExport(cl, c("data_loc", "method", "calibration_coeff", "wd"), envir = environment())
  
  clusterEvalQ(cl, {
    source("comparing\\Comparing densities.R")
    source("comparing\\comparing probabilities.R")
    source("Options\\am_density.R")
    source("PM\\am pm density.R")
    source("Non_parametric\\non-parametric-approach-t-asymptotes.R")
    source("Non_parametric\\shimko-density.R")
    library(dplyr)
  })
  
  set.seed(6969)
  result = foreach(name = files,
                   .export = c("combined_results", "get_comparison_density",
                               "get_both_probabilities"),
                    .options.snow = opts) %dorng%
    tryCatch(
            combined_results(name, data_loc, method, calibration_coeff),
            error = function(e) list(error = conditionMessage(e), file = name)
    )
  close(pb)
  density_df = bind_rows(map(result, "density"))
  rownames(density_df) = NULL
  probability_df = bind_rows(map(result, "probability"))
  skipped = bind_rows(map(result, "skipped"))
  num_skipped_strikes = mean(unlist((map(result, "skipped_option_strikes"))))

  return(list("density_comparison" = density_df,
              "probabilities" = probability_df,
              "skipped" = skipped,
              "num_skipped_strikes" = num_skipped_strikes))
}

# get the results (density and probability comparisons) for a specific 
# option file name, for dte 1:4
# 
combined_results = function(name, data_loc, method, calibration_coeff, dx = 0.1){
  # get corresponding pm file name
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

  density = list()
  probability = list()
  skipped_list = list()
  
  # loop over the combined and the stock specific calibration coefficients and dte
  wanted = c("COMBINED", "NOT CALIBRATED", stock)
  this_coeff = calibration_coeff[intersect(wanted, names(calibration_coeff))]
  
  for(cal_name in names(this_coeff)){
    
    density[[cal_name]]     = list()
    probability[[cal_name]] = list()
    
    for(dte in setdiff(unique(option_data$DTE), 0)){
      
      sum_prices = sum(filter(pm_data, DTE == dte)$price_yes)
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
        next
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
      

      attempt_option = try({
        option_density = get_option_density(data = option_data,
                                            dte = dte,
                                            method = method,
                                            dx = dx)
      }, silent = TRUE)
      
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
      
      # get probability results before a date might be skipped because pm density cannot be fit
      # as we use the raw pm prices
      probability[[cal_name]][[dte]] = get_both_probabilities(option_fit = option_density, 
                                                              this_pm_data = this_pm_data,
                                                              dte = dte,
                                                              stock = stock,
                                                              date = date,
                                                              method = method
      )
      probability[[cal_name]][[dte]][["calibration_method"]] = cal_name
      
      
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
        
      
      density[[cal_name]][[dte]] = get_comparison_density(option_density = option_density, 
                                                 pm_density = pm_density, 
                                                 dte = dte,
                                                 max_bracket = max_bracket,
                                                 date = date, 
                                                 stock = stock, 
                                                 method = method
                                                 )
      density[[cal_name]][[dte]][["calibration_method"]] = cal_name
    }
  }
  
  # have to call rbind twice as we now have a nested list 
  return(list(density = do.call(rbind, lapply(density, function(x) do.call(rbind, x))),
              probability = do.call(rbind, lapply(probability, function(x) do.call(rbind, x))),
              skipped = bind_rows(skipped_list),
              skipped_option_strikes = option_density$num_na)
         )
}
###########################################
get_comparison_density = function(option_density, pm_density, dte, 
                                  max_bracket, date, stock, method, dx = 0.1){

  # max_bracket = max(unlist(sapply(
  #   (strsplit(gsub( "<|>", "",pm_data$bracket), "-")),
  #   function(x) as.numeric(x)
  # )))
  
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
  
  wasserstein = get_wasserstein_dist(y_options = y_options,
                                     y_pm = y_pm)
  hellinger = get_hellinger_distance(y_options = y_options,
                                     y_pm = y_pm,
                                     dx = dx)
  ks = ks_test(y_options = y_options,
               x_new = x_new,
               pm_fit = pm_density,
               dx = dx,
               method = method)
  
  moments = get_moments(method = method, 
                        option_density = option_density,
                        pm_density = pm_density,
                        y_options = y_options,
                        x_new = x_new,
                        dx = dx)
  

    
  results = data.frame("Stock" = stock, 
                       "Date" = date,
                       "DTE" = dte,
                       "method" = method,
                       "Wasserstein Distance" = wasserstein,
                       "Hellinger Distance" = hellinger,
                       "KS statistic" = ks$ks_statistic,
                       "KS p-value" = ks$p_val,
                       "option_mean" = moments$option$mean,
                       "option_mean_halfwidth" = moments$option$mean_halfwidth,
                       "option_median" = moments$option$median,
                       "option_median_halfwidth" = moments$option$median_halfwidth,
                       "option_variance" = moments$option$variance,
                       "option_variance_halfwidth" = moments$option$variance_halfwidth,
                       "option_skewness" = moments$option$skewness,
                       "option_skewness_halfwidth" = moments$option$skewness_halfwidth,
                       "option_kurtosis" = moments$option$kurtosis,
                       "option_kurtosis_halfwidth" = moments$option$kurtosis_halfwidth,
                       "pm_mean" = moments$pm$mean,
                       "pm_mean_halfwidth" = moments$pm$mean_halfwidth,
                       "pm_median" = moments$pm$median,
                       "pm_median_halfwidth" = moments$pm$median_halfwidth,
                       "pm_variance" = moments$pm$variance,
                       "pm_variance_halfwidth" = moments$pm$variance_halfwidth,
                       "pm_skewness" = moments$pm$skewness,
                       "pm_skewness_halfwidth" = moments$pm$skewness_halfwidth,
                       "pm_kurtosis" = moments$pm$kurtosis,
                       "pm_kurtosis_halfwidth" = moments$pm$kurtosis_halfwidth
  )
    
  
  return(results)
}
##################################################

get_both_probabilities = function(option_fit, this_pm_data, dte, stock, date, method){
  
  brackets = sapply((strsplit(gsub( "<|>", "",this_pm_data$bracket), "-")),
                    function(x) as.numeric(x))
  
  # make sure the lower an upper brackets are are fitting
  # don't use Inf or 0 to save time when estimating integral, instead
  # find bounds such that the missing probability is < 0.00001
  brackets = sapply(1:length(brackets), function(i) {
    a = brackets[[i]]
    if(length(a) == 1){
      if(a[1] == min(unlist(brackets))){
        b = a[1] /1.05
        while(extract_option_prob(c(0, b), option_fit, method = method)> 0.00001){
          b = b / 1.05
        }
        c(b, a[1])
      } else if(a[1] == max(unlist(brackets))){
        iter = 0
        b = a[1] *1.05
        while(extract_option_prob(c(0,b), option_fit, method = method)< 0.99999){
          b = b * 1.05
        }
        c(a[1], b)
      } else {
        stop("Error: a bracket not at the tails only has only one value")
      } 
    } else {
      a
    }
  }, simplify = FALSE)
  
  list_output = list()
  for(i in 1:length(brackets)){
    bracket = brackets[[i]]
    pm_prob = this_pm_data$probs[i]
    option_prob = extract_option_prob(bracket, option_fit, method = method)
    
    temp = c(bracket_lower = bracket[1], bracket_upper = bracket[2],
             pm_probability = pm_prob, option_probability = option_prob)
    
    list_output[[i]] = temp
  }
  output = do.call(rbind, list_output)
  rownames(output) = NULL
  output = as.data.frame(output)
  output$stock = rep(stock, nrow(output))
  output$Date = rep(date, nrow(output))
  output$DTE = rep(dte, length(brackets))
  output$outcome = this_pm_data$outcome_yes
  # sort the brackets from lowest to highest and number them
  output = arrange(output, bracket_lower)
  output$bracket_nmbr = 1:length(brackets)
  output$method = method
  
  return(output)
}


