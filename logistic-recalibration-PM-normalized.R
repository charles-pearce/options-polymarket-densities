library(readxl)
library(ggplot2)
library(dplyr)
library(arrow)
library(data.table)

parquet_loc = "C:\\Users\\melit\\OneDrive\\Documents\\Erasmus\\Year 3\\Seminar in Forecasting\\Data\\all weekly stock prices 31-03\\all_prices.parquet"
parquet = read_parquet(parquet_loc)

parquet = parquet |> mutate(
  stock = sapply(strsplit(market, "-week-"), `[`, 1),
  week  = sapply(strsplit(market, "-week-"), `[`, 2)
)

parquet = parquet |>
  arrange(market, timestamp) |>
  group_by(market, bracket) |>
  mutate(snapshot_id = row_number()) |>
  ungroup() |>
  group_by(market, snapshot_id) |>
  mutate(price_yes = price_yes / sum(price_yes)) |>
  ungroup()

parquet |>
  group_by(market, snapshot_id) |>
  summarise(n = n()) |>
  pull(n) |>
  table()

parquet |>
  group_by(market) |>
  summarise(n_brackets = n_distinct(bracket)) |>
  pull(n_brackets) |>
  table()


weeks_df = data.frame(week = unique(parquet$week)) |>
  mutate(date = as.Date(paste(week), format = "%B-%d-%Y")) |>
  arrange(date)
weeks = weeks_df$week
week_groups = setNames(paste0("group_", ceiling(seq_along(weeks) / 5)), weeks)
parquet = parquet |> mutate(time_group = week_groups[week])

stocks = unique(parquet$stock)
time_groups = unique(parquet$time_group)

parquet_global = parquet |> mutate(time_group = "all", stock = "all")

price_tolerance = 0.05



# fit one logistic regression on a given subset
fit_calibration = function(parquet_subset, price_tolerance = 0.05) {
  
  if (nrow(parquet_subset) < 10) return(NULL)
  
  # daily normalization (for robustness)
  # parquet_subset = parquet_subset |>
  #   group_by(market) |>
  #   mutate(price_yes = price_yes * 24*60 / sum(price_yes)) |>
  #   ungroup()
  
  prices   = parquet_subset$price_yes
  outcomes = parquet_subset$outcome_yes
  
  # Remove extreme prices
  keep     = prices >= price_tolerance & prices <= 1 - price_tolerance
  prices   = prices[keep]
  outcomes = outcomes[keep]
  
  if (length(prices) < 10) return(NULL)
  
  fit = glm(outcomes ~ logit_prices,
            family = binomial(link = "logit"),
            data = data.frame(
              outcomes     = outcomes,
              logit_prices = log(prices / (1 - prices))
            ))
  
  coef(fit)
}


# Wrapper: filter and loop, then call fit_calibration
get_calibration_coefs = function(parquet,
                                 stocks     = NULL,   # NULL = all stocks
                                 time_groups = NULL,  # NULL = all time groups
                                 dtes       = 1:4,
                                 price_tolerance = 0.05) {
  
  # Default to all unique values if not specified
  if (is.null(stocks))      stocks      = unique(parquet$stock)
  if (is.null(time_groups)) time_groups = unique(parquet$time_group)
  
  coefs = c()
  
  for (stock in stocks) {
    for (tg in time_groups) {
      for (dte in dtes) {
        
        subset = parquet |> filter(DTE == dte, stock == !!stock, time_group == !!tg)
        
        result = fit_calibration(subset, price_tolerance)
        if (is.null(result)) next
        
        coefs = rbind(coefs, c(time_group = tg, stock = stock, dte = dte, result))
      }
    }
  }
  
  as.data.frame(coefs) |>
    mutate(across(c(`(Intercept)`, logit_prices), ~ round(as.numeric(.), 3)),
           dte = as.integer(dte)) |>
    arrange(stock, dte, time_group)
}



get_calibration_coefs(parquet, stocks = "aapl")
get_calibration_coefs(parquet, stocks = "aapl", time_groups = "group_1")
get_calibration_coefs(parquet, dtes = 2)
get_calibration_coefs(parquet_global)


