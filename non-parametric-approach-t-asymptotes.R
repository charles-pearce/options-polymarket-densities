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



fit_t_tail = function(F_join, dF_join, d2F_join, x_join, nu_init = 5) {
  
  # For a given nu, solve for z, sigma, mu using the 3 constraints
  # Constraint 1: pt(z, nu) = F_join  =>  z = qt(F_join, nu)
  # Constraint 2: dt(z, nu) / sigma = dF_join  =>  sigma = dt(z,nu) / dF_join
  # Constraint 3: F''= dt'(z,nu)/sigma^2 = d2F_join
  #   dt'(z, nu) = -dt(z,nu) * (nu+1) * z / (nu + z^2)  [derivative of t pdf]
  #   so d2F_join = -dt(z,nu)*(nu+1)*z / ((nu+z^2)*sigma^2)
  
  # Residual: given nu, compute implied d2F and compare to actual d2F
  residual = function(nu) {
    if (nu <= 2) return(Inf)   # need nu > 2 for finite variance
    z     = qt(F_join, df = nu)
    sigma = dt(z, df = nu) / dF_join
    # derivative of t pdf: d/dz dt(z,nu) = -dt(z,nu) * (nu+1)*z / (nu + z^2)
    dt_prime = -dt(z, df = nu) * (nu + 1) * z / (nu + z^2)
    d2F_implied = dt_prime / sigma^2
    return(d2F_implied - d2F_join)
  }
  
  # Solve for nu numerically
  nu_solution = tryCatch(
    uniroot(residual, interval = c(2 + 1e-4, 200))$root,
    error = function(e) nu_init    # fall back to default if no solution found
  )
  
  z     = qt(F_join, df = nu_solution)
  sigma = dt(z, df = nu_solution) / dF_join
  mu    = x_join - z * sigma       # note: x_join must be in scope
  
  list(nu = nu_solution, mu = mu, sigma = sigma)
}



# approximate the pdf by fitting a spline to the empirical cdf
non_param_approx = function(brackets, probs){
  require(stats)
  cum_probs = cumsum(probs)/(sum(probs))
  knot_x = sapply(brackets, FUN = max)
  bracket_size = knot_x[2]
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
  
  # Left tail, t distribution
  left  = fit_t_tail(F_left,  dF_left,  d2F_left,  x_join = x_left)
  left_tail_cdf = function(x) pt((x - left$mu)  / left$sigma,  df = left$nu)
  left_tail_pdf = function(x) dt((x - left$mu)  / left$sigma,  df = left$nu) / left$sigma
  
  # Right tail, t distribution
  right = fit_t_tail(F_right, dF_right, d2F_right, x_join = x_right)
  right_tail_cdf = function(x) pt((x - right$mu) / right$sigma, df = right$nu)
  right_tail_pdf = function(x) dt((x - right$mu) / right$sigma, df = right$nu) / right$sigma
  
  
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
