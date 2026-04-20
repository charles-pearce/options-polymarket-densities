library(splines)
library(readxl)
library(ggplot2)
library(dplyr)


dte = 2
data_loc = "C:\\Users\\melit\\OneDrive\\Documents\\Erasmus\\Year 3\\Seminar in Forecasting\\Data\\all weekly stock prices 31-03\\"  

stock = "nvda"
date = "march-27-2026"
aapl_3_27 = read_excel(paste0(data_loc, stock,"_weekly_market\\",stock,"-week-", date, ".xlsx"))
dte4 = aapl_3_27|> filter(DTE == dte)
brackets = sapply((strsplit(gsub( "<|>", "",dte4$bracket), "-")), function(x) as.numeric(x))
probs = dte4$price_yes
approx_interval = (range(brackets)[2]+5)- (range(brackets)[1] -5)

data = non_param_approx(brackets, probs)
plot_non_param(brackets, probs, data, "woof")

for (stock in c("aapl", "googl", "nvda", "meta")){
  for(dte in 1:4){
    file_location = paste0(data_loc, stock,"_weekly_market\\",stock,"-week-", date, ".xlsx")
    plot_param_non_param(file_location = file_location, dte = dte, title = paste(stock, dte))
  }
}

stock = "nvda"
dte = 4
file_location = paste0(data_loc, stock,"_weekly_market\\",stock,"-week-", date, ".xlsx")
plot_param_non_param(file_location = file_location, dte = dte, title = paste(stock, dte))



# approximate the pdf by fitting a spline to the empirical cdf
non_param_approx = function(brackets, probs){
  require(stats)
  cum_probs = cumsum(probs)/(sum(probs))
  knot_x = sapply(brackets, FUN = max)
  knot_x[length(knot_x)] = knot_x[length(knot_x)] + 5

  # Fit spline only to the real data
  fit_cdf = smooth.spline(knot_x, cum_probs, df = 10)
  
  # Determine plot range
  x_min = range(brackets)[1] * 0.9
  x_max = range(brackets)[2] * 1.1
  x_new = seq(x_min, x_max, by = 0.1)
  
  
  # Boundary info for tail fitting
  x_left  = knot_x[2]
  F_left  = predict(fit_cdf, x_left)$y
  dF_left = predict(fit_cdf, x_left,  deriv = 1)$y      # slope at left knot
  d2F_left  <- predict(fit_cdf, x_left,  deriv = 2)$y   # curvature
  
  x_right = knot_x[length(knot_x) - 1]
  F_right = predict(fit_cdf, x_right)$y
  dF_right = predict(fit_cdf, x_right, deriv = 1)$y     # slope at right knot
  d2F_right <- predict(fit_cdf, x_right, deriv = 2)$y   # curvature
  
  # F'(x) = (1/σ) * φ((x-μ)/σ)  where φ is standard normal pdf
  # From F_left = Φ(z):  z = qnorm(F_left),  μ = x_left - z*σ
  # From dF_left = φ(z)/σ:  σ = φ(z) / dF_left
  z_left  = qnorm(F_left)
  sigma_left = dnorm(z_left) / dF_left
  mu_left = x_left - z_left * sigma_left
  left_tail_cdf = function(x) pnorm(x, mu_left, sigma_left)
  left_tail_pdf = function(x) dnorm(x, mu_left, sigma_left)
  
  # Right tail:  F(x) = 1 - c * exp(-d * x)
  # Constraints: F(x_right) = F_right  and  F'(x_right) = dF_right
  # => d = dF_right / (1 - F_right),   c = (1 - F_right) * exp(d * x_right)
  z_right = qnorm(F_right)
  sigma_right = dnorm(z_right) / dF_right
  mu_right = x_right - z_right * sigma_right
  right_tail_cdf = function(x) pnorm(x, mu_right, sigma_right)
  right_tail_pdf = function(x) dnorm(x, mu_right, sigma_right)
  
  
  # Assemble CDF and PDF piece-wise
  cdf_vals = ifelse(
    x_new < x_left,  left_tail_cdf(x_new),
    ifelse(x_new > x_right, right_tail_cdf(x_new),
           predict(fit_cdf, x_new)$y)
  )
  
  pdf_vals = ifelse(
    x_new < x_left,  left_tail_pdf(x_new),
    ifelse(x_new > x_right, right_tail_pdf(x_new),
           predict(fit_cdf, x_new, deriv = 1)$y)
  )
  
  # Force CDF to [0,1] as a safety net (probably overkill)
  cdf_vals = pmax(0, pmin(1, cdf_vals))
  
  pdf_data = data.frame(x = x_new, y = pdf_vals)
  cdf_data = data.frame(x = x_new, y = cdf_vals)
  
  return(list(data = pdf_data, area = sum(pdf_vals * 0.1)))
}

plot_non_param = function(brackets, prices, pdf_data, title){
  require(ggplot2)
  start_bracket = sapply(brackets, function(x) x[1])
  start_bracket[1] = start_bracket[1]-5
  approx_interval = (range(brackets)[2]+5)- (range(brackets)[1] -5)
  poly_df = data.frame(start_bracket = start_bracket, probs_df = 
                         prices/(mean(prices) * approx_interval)
  )
  
  plot = ggplot() +
    geom_line(data = pdf_data, aes(x = x, y = y), linewidth = 1.5)+
    geom_step(data = poly_df, aes(x = start_bracket, y = probs_df), 
              linewidth = 1.5, color = "red") +
    labs(title = title,
         x = "Strike",
         y = "Density")
  print(plot)
}



plot_param_non_param = function(file_location, dte, title){
  data = read_excel(file_location)
  dte_data = data|> filter(DTE == dte)
  brackets = sapply((strsplit(gsub( "<|>", "",dte_data$bracket), "-")), function(x) as.numeric(x))
  prices = dte_data$price_yes
  
  x_min = range(brackets)[1]*0.9
  x_max = range(brackets)[2]*1.1
  
  params = fit_distributions(brackets, prices)
  pdf_data_non_param = non_param_approx(brackets, prices)[[1]]
  require(ggplot2)
  mixture_density = function(x, params) {
    w1 = 1 - params[1] - params[2]
    w2 = params[1]
    w3 = params[2]
    
    w1 * dlnorm(x, params[3], params[6]) +
      w2 * dlnorm(x, params[4], params[7]) +
      w3 * dlnorm(x, params[5], params[8])
  }
  
  # plot it
  x = seq(x_min, x_max, length.out = 10000)
  y = mixture_density(x, params)
  
  density_df = data.frame(x = x, y = y)
  
  start_bracket = sapply(brackets, function(x) x[1])
  start_bracket[1] = start_bracket[1]-5
  approx_interval = (range(brackets)[2]+5)- (range(brackets)[1] -5)
  poly_df = data.frame(start_bracket = start_bracket, poly_probs = 
                         prices/(mean(prices) * approx_interval)
  )
  plot = ggplot() +
    geom_line(data = pdf_data_non_param, aes(x = x, y = y, color = "Non-Parametric"), 
              linewidth = 1.5)+
    # mixture density line
    geom_line(data = density_df, aes(x = x, y = y, color = "mixture log normal"), 
              linewidth = 1.5) +
    # step plot from poly
    geom_step(data = poly_df, aes(x = start_bracket, y = poly_probs, color = "empirical"), 
              linewidth = 1.5) +
    labs(title = title,
         x = "Strike",
         y = "Density") +
    scale_color_manual(values = c("Non-Parametric" = "blue", 
                                  "mixture log normal" = "black", 
                                  "empirical" = "red"))
  print(plot)
}
