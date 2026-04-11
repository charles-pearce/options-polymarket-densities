library(readxl)
library(ggplot2)
library(dplyr)
library(arrow)

parquet_loc = "C:\\Users\\melit\\OneDrive\\Documents\\Erasmus\\Year 3\\Seminar in Forecasting\\Data\\all weekly stock prices 31-03\\all_prices.parquet"
parquet = read_parquet(parquet_loc)

parquet = parquet |> mutate(
  stock = sapply(strsplit(market, "-week-"), `[`, 1),
  week  = sapply(strsplit(market, "-week-"), `[`, 2)
)


price_tolerance = 0.05

stocks = unique(parquet$stock)
weeks_df = data.frame(week = unique(parquet$week)) |>
  mutate(date = as.Date(paste(week), format = "%B-%d-%Y")) |>
  arrange(date)
weeks = weeks_df$week
week_groups = setNames(paste0("group_", ceiling(seq_along(weeks) / 5)), weeks)
parquet = parquet |> mutate(time_group = week_groups[week])

time_groups = unique(parquet$time_group)


coefs = c()

for (stock in stocks) {
  for (tg in time_groups) {
    for (dte in c(1:4)) {
      
      parquet_dte = parquet |>
        filter(DTE == dte,
               stock == !!stock,
               time_group == !!tg)
      
      # Skip if not enough data
      if (nrow(parquet_dte) < 10) next
      
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
      
      if (length(parquet_dte_prices) < 10) next
      
      fit = glm(outcomes ~ logit_prices,
                family = binomial(link = "logit"),
                data = data.frame(
                  outcomes     = parquet_dte_outcomes,
                  logit_prices = log(parquet_dte_prices / (1 - parquet_dte_prices))
                ))
      
      coefs = rbind(coefs, c(time_group = tg,
                             stock = stock,
                             dte = dte, coef(fit)))
    }
  }
}


coefs = as.data.frame(coefs)
coefs$`(Intercept)` = round(as.numeric(coefs$`(Intercept)`), 3)
coefs$logit_prices = round(as.numeric(coefs$logit_prices), 3)
coefs$dte = as.integer(coefs$dte)
coefs = coefs |> arrange(stock, dte, time_group)
coefs


# For AAPL, plotting how each DTE's coefficient varies over time
plot(c(1:4), coefs$logit_prices[1:4], ylim = c(0,5), col = 2) # DTE 1
points(c(1:4), coefs$logit_prices[5:8], col = 3)              # DTE 2
points(c(1:4), coefs$logit_prices[9:12], col = 4)             # DTE 3
points(c(1:4), coefs$logit_prices[13:16], col = 5)            # DTE 4
