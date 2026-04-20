library(readxl)
library(ggplot2)
library(dplyr)
library(arrow)
library(data.table)
parquet_loc = "C:\\Users\\melit\\OneDrive\\Documents\\Erasmus\\Year 3\\Seminar in Forecasting\\Data\\all weekly stock prices 31-03\\all_prices.parquet"



# Load and transform parquet
load_parquet = function(parquet_loc, weeks_per_group = 5) {
  
  parquet = read_parquet(parquet_loc)
  setDT(parquet)
  
  parquet[, `:=`(
    stock = sub("-week-.*", "", market),
    week  = sub(".*-week-", "", market)
  )]
  
  # Standardizing the prices
  setorder(parquet, market, timestamp)
  parquet = parquet[DTE %in% 1:4]
  parquet[, snapshot_id := seq_len(.N), by = .(market, bracket)]
  parquet[, price_yes   := price_yes / sum(price_yes), by = .(market, snapshot_id)]
  
  parquet = as_tibble(parquet)
  
  # Build chronological time groups
  unique_weeks = unique(sub(".*-week-", "", unique(parquet$market)))
  weeks_df = data.frame(week = unique_weeks) |>
    mutate(date = as.Date(week, format = "%B-%d-%Y")) |>
    arrange(date)
  week_groups = setNames(
    paste0("group_", ceiling(seq_along(weeks_df$week) / weeks_per_group)),
    weeks_df$week
  )
  parquet = parquet |> mutate(time_group = week_groups[week])
  
  return(parquet)
}



# Fit one logistic regression on a given subset
fit_calibration = function(parquet_subset, price_tolerance = 0.05) {
  if (nrow(parquet_subset) < 10) return(NULL)
  
  prices   = parquet_subset$price_yes
  outcomes = parquet_subset$outcome_yes
  
  # Remove extreme prices
  keep     = prices >= price_tolerance & prices <= 1 - price_tolerance
  prices   = prices[keep]
  outcomes = outcomes[keep]
  
  if (length(prices) < 10) return(NULL)
  
  if (length(unique(outcomes)) < 2) return(NULL)
  
  fit = tryCatch(
    withCallingHandlers(
      glm(outcomes ~ logit_prices,
          family = binomial(link = "logit"),
          data = data.frame(
            outcomes     = outcomes,
            logit_prices = log(prices / (1 - prices))
          )),
      warning = function(w) {
        message("Warning in stock=", parquet_subset$stock[1],
                " tg=", parquet_subset$time_group[1],
                " dte=", parquet_subset$DTE[1], ": ", conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) NULL
  )
  if (is.null(fit)) return(NULL)
  coef(fit)
}



# Wrapper: load+transform, then calibrate
get_calibration_coefs = function(parquet_loc,
                                 stocks          = NULL,
                                 time_groups     = NULL,
                                 dtes            = 1:4,
                                 price_tolerance = 0.05,
                                 weeks_per_group = 5,
                                 global          = FALSE,
                                 pool_time       = FALSE,
                                 pool_stocks     = FALSE) {
  
  parquet = load_parquet(parquet_loc, weeks_per_group)
  
  if (global | (pool_time & pool_stocks)) {
    parquet     = parquet |> mutate(time_group = "all", stock = "all")
    stocks      = "all"
    time_groups = "all"
  } else if (pool_time) {
    parquet     = parquet |> mutate(time_group = "all")
    time_groups = "all"
    if (is.null(stocks)) stocks = unique(parquet$stock)
  } else if (pool_stocks) {
    parquet     = parquet |> mutate(stock = "all")
    stocks      = "all"
    if (is.null(time_groups)) time_groups = unique(parquet$time_group)
  } else {
    if (is.null(stocks))      stocks      = unique(parquet$stock)
    if (is.null(time_groups)) time_groups = unique(parquet$time_group)
  }
  
  coefs = c()
  
  for (s in stocks) {
    for (tg in time_groups) {
      for (d in dtes) {
        
        parquet_sub = parquet |> filter(DTE == d, stock == s, time_group == tg)
        result      = fit_calibration(parquet_sub, price_tolerance)
        if (is.null(result)) next
        
        coefs = rbind(coefs, c(time_group = tg, stock = s, dte = d, result))
      }
    }
  }
  
  if (is.null(coefs) || length(coefs) == 0) {
    message("No results found for the given filters.")
    return(NULL)
  }
  
  as.data.frame(coefs) |>
    mutate(across(c(`(Intercept)`, logit_prices), ~ round(as.numeric(.), 3)),
           dte = as.integer(dte)) |>
    arrange(stock, dte, time_group)
}

