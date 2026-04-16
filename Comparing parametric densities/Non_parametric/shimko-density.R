# retuns the cdf saved as a function
extract_shimko <- function(data,
                           dx = 0.1, #number of points on the density function to return
                           q = 0,  #div yield
                           spar = 0.9,  #smoothing param, use > 0.8
                           moneyness_filter = 0.45, #filter for deep contracts 
                           callsOnly = FALSE) {
  
  S = unique(data$SpotClose)
  r = unique(data$Rate)
  tau = unique(data$Tau)
  
  if (length(S) > 1){
    stop("more than one spot price given")
  }
  
  if (tau == 0) {
    stop("Skipping tau = 0, BSM is undefined at expiration\n")
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
  
  
  df <- df %>%
    filter(!is.na(sigma_iv), sigma_iv > 0.1, sigma_iv < 1.5)
  
  
  # strike aggregation
  df <- df %>%
    group_by(K) %>%
    summarise(sigma_iv = median(sigma_iv), .groups = "drop") %>%
    arrange(K)
  
  # spline guard
  if (length(unique(df$K)) < 4) {
    stop(sprintf("Skipping tau = %.4f — only %d unique strike(s) after filtering.",
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
  K_grid     <- seq(K_min, K_max, by = dx)
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
  
  if (!all(rnd >= 0)) {
    stop("Warning: the RND contains negative values\n")
  }
  
  return(list(
              rnd_K      = K_mid,
              rnd        = rnd,
              cdf = approxfun(x = K_mid, 
                              y = cumsum(rnd) * dx, 
                              yleft = 0, 
                              yright = 1),
              area       = sum(rnd * dK)
              )
  )
}

