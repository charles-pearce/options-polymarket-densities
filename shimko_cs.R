library(RND)
library(dplyr)

shimkoCS.extract <- function(data,
                             S, #underlying price
                             q, #div yield
                             spar, #smoothing param, use > 0.8
                             callsOnly = FALSE,
                             moneyness_filter) #filter for deep contracts 
  {
  
  .extract_single <- function(data, S, r, q, tau, spar, moneyness_filter, callsOnly) {
    
    if (tau == 0) {
      cat("Skipping tau = 0, BSM is undefined at expiration\n")
      return(NULL)
    }
    
    df <- data.frame(
      K     = as.numeric(data$Strike),
      price = as.numeric(data$Mid)
    )
    
    #use OTM options, following Shimko (1993)
    
    if(callsOnly){
      df$type <- toupper(data$OptionType)
      df <- df %>% filter(type == "CALL", price > 0)
      
    } else {
    df$type <- toupper(data$OptionType)
    df_calls <- df %>% filter(type == "CALL", K >= S)
    df_puts  <- df %>% filter(type == "PUT",  K < S)
    df <- bind_rows(df_calls, df_puts) %>%
      filter(price > 0)
    }
    # moneyness filtration
    df <- df %>%
      filter(abs(K - S) / S < moneyness_filter)
    
    
    # arbitrage bounds
    lower_bound <- ifelse(
      df$type == "CALL",
      pmax(S * exp(-q * tau) - df$K * exp(-r * tau), 0),
      pmax(df$K * exp(-r * tau) - S * exp(-q * tau), 0)
    )
    upper_bound <- ifelse(
      df$type == "CALL",
      S * exp(-q * tau),
      df$K * exp(-r * tau)
    )
    df <- df %>%
      filter(price >= lower_bound, price <= upper_bound)
    
    # implied vol
    iv <- function(K, price, type) {
      tryCatch({
        
        call_price <- ifelse(
          type == "CALL",
          price,
          price + S * exp(-q * tau) - K * exp(-r * tau)
        )
        
        compute.implied.volatility(
          r          = r,
          te         = tau,
          s0         = S,
          k          = K,
          y          = q,
          call.price = call_price,
          lower      = 0.001,
          upper      = 3
        )
        
      }, error = function(e) NA)
    }
    
    df$sigma_iv <- mapply(iv, df$K, df$price, df$type)
    
    cat(sprintf("  NA count: %d\n", sum(is.na(df$sigma_iv))))
    cat(sprintf("  IV range: %.3f to %.3f\n",
                min(df$sigma_iv, na.rm=TRUE),
                max(df$sigma_iv, na.rm=TRUE)))
    
    df <- df %>%
      filter(!is.na(sigma_iv), sigma_iv > 0.1, sigma_iv < 1.5) #IV filter
    
    
    # strike aggregation
    df <- df %>%
      group_by(K) %>%
      summarise(sigma_iv = median(sigma_iv), .groups = "drop") %>%
      arrange(K)
    
    # spline guard
    if (length(unique(df$K)) < 4) {
      warning(sprintf("Skipping tau = %.4f — only %d unique strike(s) after filtering.",
                      tau, length(unique(df$K))))
      return(NULL)
    }
    
    # log moneyness smoothing
    x          <- log(df$K / S)
    w          <- df$sigma_iv^2 * tau
    spline_fit <- smooth.spline(x, w, spar = spar)
    
    # original grid
    K_min      <- min(df$K)
    K_max      <- max(df$K)
    K_grid     <- seq(K_min, K_max, length.out = 500)
    x_grid     <- log(K_grid / S)
    w_grid     <- predict(spline_fit, x_grid)$y
    sigma_grid <- sqrt(pmax(w_grid, 0) / tau)
    
    call_prices <- price.bsm.option(
      r     = r,
      te    = tau,
      s0    = S,
      k     = K_grid,
      sigma = sigma_grid,
      y     = q
    )$call
    
    # Breeden-Litzenberger
    dK    <- K_grid[2] - K_grid[1]
    d2C   <- diff(call_prices, differences = 2) / (dK^2)
    K_mid <- K_grid[2:(length(K_grid) - 1)]
    rnd   <- exp(r * tau) * d2C
    rnd   <- pmax(rnd, 0)
    rnd   <- rnd / sum(rnd * dK)
    
    cat("Area under RND:", round(sum(rnd * dK), 4), "\n")
    if (all(rnd >= 0)) {
      cat("The RND is non-negative\n")
    } else {
      cat("Warning: the RND contains negative values\n")
    }
    
    list(
      tau        = tau,
      strikes    = df$K,
      sigma_iv   = df$sigma_iv,
      K_grid     = K_grid,
      sigma_grid = sigma_grid,
      rnd_K      = K_mid,
      rnd        = rnd,
      area       = sum(rnd * dK)
    )
  }
  
  # split by date, apply extractor
  dates   <- sort(unique(data$Date))
  n_dates <- length(dates)
  
  cat(sprintf("Extracting RND for %d date(s)...\n", n_dates))
  
  results <- data %>%
    group_by(Date) %>%
    group_split() %>%
    setNames(as.character(dates)) %>%
    lapply(function(group) {
      tau <- unique(group$Tau)
      r   <- unique(group$Rate)
      cat(sprintf("Date: %s | tau = %.4f | r = %.4f\n",
                  as.character(unique(group$Date)), tau, r))
      .extract_single(group, S, r, q, tau, spar, moneyness_filter, callsOnly)
    })
  
  # remove skipped days
  results <- results[!sapply(results, is.null)]
  results <- results[sapply(results, is.list)]
  
  if (length(results) == 0) {
    stop("No valid dates remaining after filtering — try relaxing moneyness_filter or spar.")
  }
  
  # plots
  x_range       <- range(sapply(results, function(res) range(res$rnd_K)))
  y_range_smile <- range(sapply(results, function(res) res$sigma_iv)) * 1.1
  y_range_rnd   <- c(0, max(sapply(results, function(res) max(res$rnd))) * 1.1)
  
  pal <- rainbow(length(results))
  par(mfrow = c(length(results), 2),
      mar   = c(4, 4, 3, 1))
  
  for (i in seq_along(results)) {
    res  <- results[[i]]
    date <- names(results)[i]
    
    # smile
    plot(res$strikes, res$sigma_iv, pch = 16, col = pal[i],
         main = paste("Implied Vol Smile —", date),
         xlab = "Strike", ylab = "Implied Volatility",
         xlim = x_range,
         ylim = y_range_smile)
    lines(res$K_grid, res$sigma_grid, col = pal[i], lwd = 2)
    
    # RND
    plot(res$rnd_K, res$rnd, type = "l", lwd = 2, col = pal[i],
         main = paste("Risk-Neutral Density —", date),
         xlab = "Strike", ylab = "Density",
         xlim = x_range,
         ylim = y_range_rnd)
  }
  
  par(mfrow = c(1, 1))
  invisible(results)
}

# function call
results <- shimkoCS.extract(
  data             = AAPL_2025.11.21_td,
  S                = 271.49,
  q                = 0.0041,
  spar             = 0.9,
  moneyness_filter = 0.45,
  callsOnly = FALSE
)



