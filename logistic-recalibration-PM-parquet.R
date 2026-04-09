library(readxl)
library(ggplot2)
library(dplyr)
library(arrow)

parquet_loc = "C:\\Users\\melit\\OneDrive\\Documents\\Erasmus\\Year 3\\Seminar in Forecasting\\Data\\all weekly stock prices 31-03\\all_prices.parquet"
parquet = read_parquet(parquet_loc)

price_tolerance = 0.05
coefs = c()
dte = 3

parquet_markets = parquet$market
parquet_markets = unique(parquet_markets)

for (dte in c(1:10)) {
  prices = c()
  outcomes = c()
  
  parquet_dte = parquet |> filter(DTE == dte)
  
  # Standardize the prices so they sum up to 1
  parquet_dte = parquet_dte |> group_by(market) |>
    mutate(price_yes = price_yes * 24*60 / sum(price_yes)) |> ungroup()
  
  parquet_dte_prices = parquet_dte$price_yes
  parquet_dte_outcomes = parquet_dte$outcome_yes
  
  # Remove extreme prices as they explode under logit
  keep = parquet_dte_prices >= price_tolerance &
    parquet_dte_prices <= 1 -price_tolerance
  parquet_dte_prices = parquet_dte_prices[keep]
  parquet_dte_outcomes = parquet_dte_outcomes[keep]
  
  prices = append(prices, parquet_dte_prices)
  outcomes = append(outcomes, parquet_dte_outcomes)
  
  fit = glm(outcomes ~ logit_prices,
            family = binomial(link = "logit"),
            data = data.frame(
              outcomes     = outcomes,
              logit_prices = log(prices / (1 - prices))   # logit transform
            ))
  
  coefs = rbind(coefs, coef(fit))
}

dte



