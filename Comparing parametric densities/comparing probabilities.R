# data_opt  = read.csv("Options\\NVDA_2026-03-27_day_full.csv")
# data_loc = "C:\\Users\\lars\\OneDrive\\Dokumente\\Uni\\Seminar Forecasting\\data collection\\Getting poly data\\"  
# data_pms = read_excel(paste0(data_loc, "nvda_weekly_market\\nvda-week-march-27-2026.xlsx"))
# 
# df = get_both_probabilities(data_opt, data_pms)
# reg = lm(pm_probability~option_probability+as.factor(DTE), data = df)
# test_mz(reg)
# compare_accuracy_by_dte(df)

# extract the option-implied probability that the stock will end in some interval
extract_option_prob = function(bracket, density_obj, method){
  if (method == "3lognorm"){
    w1 = density_obj$p.1 
    w2 = density_obj$p.2
    w3 = 1 - w1 - w2 
    
    prob_lower = w1 * plnorm(bracket[1], density_obj$u.1, density_obj$sigma.1) +
                 w2 * plnorm(bracket[1], density_obj$u.2, density_obj$sigma.2) +
                 w3 * plnorm(bracket[1], density_obj$u.3, density_obj$sigma.3)
    
    prob_upper = w1 * plnorm(bracket[2], density_obj$u.1, density_obj$sigma.1) +
                 w2 * plnorm(bracket[2], density_obj$u.2, density_obj$sigma.2) +
                 w3 * plnorm(bracket[2], density_obj$u.3, density_obj$sigma.3)
  } else if(method == "1lognorm"){
    prob_lower = plnorm(bracket[1], density_obj$mu, density_obj$zeta)
    prob_upper = plnorm(bracket[2], density_obj$mu, density_obj$zeta)
  }
    
  return(prob_upper - prob_lower)
}

# perform the mz tests, intercept = 0, beta = 1 separately and jointly
test_mz = function(reg){
  require(car)
  test_intercept = linearHypothesis(reg, c("(Intercept) = 0"))
  intercept = c(estimate = unname(reg$coefficients["(Intercept)"]),
                    F_stat = test_intercept$F[2], 
                    p_val = test_intercept$`Pr(>F)`[2])
  
  test_beta = linearHypothesis(reg, c(paste(names(reg$coefficients[2]), " = 1")))
  beta = c(estimate = unname(reg$coefficients[2]),
               F_stat = test_beta$F[2], 
               p_val = test_beta$`Pr(>F)`[2])
  test_joined = linearHypothesis(reg, c("(Intercept) = 0", 
                                          c(paste(names(reg$coefficients[2]), " = 1"))))
  joined = c(estimate = NA,
                       F_stat = test_joined$F[2], 
                       p_val = test_joined$`Pr(>F)`[2])
  # return a dataframe where the rownames are in a seperate column
  return(rbind(intercept, beta, joined) |>
           as.data.frame() |>
           tibble::rownames_to_column("coefficient")
         )
}

brier_loss = function(probs, outcomes){
  return((probs-outcomes)^2)
}

log_loss = function(probs, outcomes){
  result = outcomes * log10(probs) + (1-outcomes) * log10(1-probs)
  nas = which(is.na(result))
  result[nas] = .Machine$double.eps 
  return(result)
}

compare_accuracy_by_dte = function(df, dte_range = 1:4){
  require(forecast)
  results = list()
  for(dte in dte_range){
    this_df = filter(df, DTE == dte)
    pm_brier_loss = brier_loss(this_df$pm_probability, this_df$outcome)
    option_brier_loss = brier_loss(this_df$option_probability, this_df$outcome)
    brier_dm = dm.test(pm_brier_loss, 
                       option_brier_loss,
                       alternative = "two.sided", 
                       h = dte, 
                       power = 2)
    
    pm_log_loss = log_loss(this_df$pm_probability, this_df$outcome)
    option_log_loss = log_loss(this_df$option_probability, this_df$outcome)
    log_dm = dm.test(pm_log_loss,
                     option_log_loss,
                     alternative = "two.sided",
                     h = dte,
                     power = 1)
    
    temp = data.frame("DTE" = dte,
                      "brier_score_pm" = mean(pm_brier_loss),
                      "brier_score_options" = mean(option_brier_loss),
                      "brier_dm_stat" = brier_dm$statistic,
                      "brier_dm_p_val" = brier_dm$p.value,
                      "log_score_pm" = mean(pm_log_loss),
                      "log_score_options" = mean(option_log_loss),
                      "log_dm_stat" = log_dm$statistic,
                      "log_dm_p_val" = log_dm$p.value
                      )
    results[[length(results)+1]] = temp
  }
  return(bind_rows(results))
}



