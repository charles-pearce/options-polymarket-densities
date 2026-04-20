library(RND)
library(dplyr)
library(readr)

# Load Data
td_path<- "data/NFLX/NFLX_2026-01-16_td.csv"
pm_path <- "data/NFLX/NFLX_2026-01-16_pm.csv"

td <- read_csv(td_path, show_col_types = FALSE)
pm <- read_csv(pm_path, show_col_types = FALSE)

# Get shared trade days
trade_dates <- td %>%
  filter(Tau > 0) %>%
  pull(Date) %>%
  unique() %>%
  sort() %>%
  head(4)

# Bracket parser
parse_bracket <- function(b) {
  if (grepl("^<", b)) {
    return(c(NA, as.numeric(gsub("<", "", b))))
  } else if (grepl("^>", b)) {
    return(c(as.numeric(gsub(">", "", b)), NA))
  } else {
    parts <- as.numeric(strsplit(b, "-")[[1]])
    return(c(parts[1], parts[2]))
  }
}

# Fit lognormal to Polymarket using CDF matching
fit_pm_lognormal_cdf <- function(brackets_df) {
  
  brackets_sorted <- brackets_df %>% arrange(mid)
  n        <- nrow(brackets_sorted)
  cum_prob <- cumsum(brackets_sorted$prob)
  x_vals   <- brackets_sorted$upper[-n]   # upper boundaries, drop last
  cdf_vals <- cum_prob[-n]                # empirical CDF, drop last
  
  obj <- function(theta) {
    sum((plnorm(x_vals, meanlog = theta[1], sdlog = exp(theta[2])) - cdf_vals)^2)
  }
  
  # Moment-matched warm start
  EX    <- sum(brackets_df$mid * brackets_df$prob)
  VX    <- sum(brackets_df$mid^2 * brackets_df$prob) - EX^2
  zeta0 <- sqrt(log(1 + VX / EX^2))
  mu0   <- log(EX) - 0.5 * zeta0^2
  
  fit <- optim(c(mu0, log(zeta0)), obj, method = "Nelder-Mead")
  
  list(mu = fit$par[1], zeta = exp(fit$par[2]), converged = fit$convergence == 0)
}

# Loop over days
results <- list()

for (d in seq_along(trade_dates)) {
  date <- trade_dates[d]
  
  # Options side
  day_td <- td %>% filter(Date == date)
  S  <- day_td$SpotClose[1]
  r  <- day_td$Rate[1]
  te <- day_td$Tau[1]
  
  calls <- day_td %>% filter(OptionType == "CALL", !is.na(Mid), Mid > 0) %>% arrange(Strike)
  puts  <- day_td %>% filter(OptionType == "PUT",  !is.na(Mid), Mid > 0) %>% arrange(Strike)
  
  rnd_td <- extract.bsm.density(
    r = r, y = 0, te = te, s0 = S,
    market.calls = calls$Mid, call.strikes = calls$Strike,
    market.puts  = puts$Mid,  put.strikes  = puts$Strike,
    lambda = 1, hessian.flag = FALSE
  )
  
  # Polymarket side
  day_pm <- pm %>%
    mutate(date_only = as.Date(target_time)) %>%
    filter(date_only == date, market_open == TRUE) %>%
    arrange(target_time) %>%
    group_by(bracket) %>%
    slice_tail(n = 1) %>%
    ungroup()
  
  # Tail bounds from options strike range
  bounds <- do.call(rbind, lapply(day_pm$bracket, parse_bracket))
  
  day_pm <- day_pm %>%
    mutate(
      lower = bounds[, 1],
      upper = bounds[, 2],
      lower = ifelse(is.na(lower), min(calls$Strike), lower),
      upper = ifelse(is.na(upper), max(calls$Strike), upper),
      mid   = (lower + upper) / 2,
      prob  = (price / sum(price))^1.105/((price/sum(price))^1.105 + (1-price/sum(price))^1.105)
      
    ) %>%
    arrange(mid)
  
  
  rnd_pm <- fit_pm_lognormal_cdf(day_pm)
  
  # Shared x grid: union of +-4 SD from both
  # x_min <- min(exp(rnd_td$mu - 4 * rnd_td$zeta), exp(rnd_pm$mu - 4 * rnd_pm$zeta))
  # x_max <- max(exp(rnd_td$mu + 4 * rnd_td$zeta), exp(rnd_pm$mu + 4 * rnd_pm$zeta))
  x_min = 75
  x_max = 100
  grid  <- seq(x_min, x_max, length.out = 500)
  
  results[[d]] <- list(
    day      = d,
    date     = as.character(date),
    spot     = S,
    te       = te,
    grid     = grid,
    dens_td  = dlnorm(grid, meanlog = rnd_td$mu, sdlog = rnd_td$zeta),
    dens_pm  = dlnorm(grid, meanlog = rnd_pm$mu, sdlog = rnd_pm$zeta),
    iv_td    = rnd_td$zeta / sqrt(te) * 100,
    iv_pm    = rnd_pm$zeta / sqrt(te) * 100,
    brackets = day_pm
  )
}

# Plot: 2x2 panels
par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

for (res in results) {
  
  y_max <- max(res$dens_td, res$dens_pm) * 1.15
  
  plot(NA,
       xlim = range(res$grid), ylim = c(0, y_max),
       main = sprintf("Day %d — %s", res$day, res$date),
       xlab = "Stock Price at Expiry", ylab = "Density",
       las  = 1)
  
  grid(col = "grey88", lty = 1)
  
  lines(res$grid, res$dens_td, col = "steelblue",  lwd = 2)
  lines(res$grid, res$dens_pm, col = "darkorange", lwd = 2)
  abline(v = res$spot,         col = "grey40",     lwd = 1, lty = 3)
  
  legend("topright", bty = "n", cex = 0.72,
         legend = c("Options",
                    "Polymarket",
                    sprintf("Spot = $%.2f",          res$spot)),
         col = c("steelblue", "darkorange", "grey40"),
         lty = c(1, 1, 3), lwd = c(2, 2, 1))
}

par(mfrow = c(1, 1))

