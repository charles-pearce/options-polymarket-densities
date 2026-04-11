# library(readxl)
# data_opt  = read.csv("Options\\NVDA_2026-03-27_day_full.csv")
# data_loc = "C:\\Users\\lars\\OneDrive\\Dokumente\\Uni\\Seminar Forecasting\\data collection\\Getting poly data\\"  
# data_pms = read_excel(paste0(data_loc, "nvda_weekly_market\\nvda-week-march-27-2026.xlsx"))
# fit_options = get_am_density(data_opt, dte = 4)
# fit_pm = fit_am_pm(data_pms, dte = 4)
# plot_x = c(130, 260)
# hell_x = c(0, 400)
# plot_over_time = get_plot_over_time(data_opt, data_pms, 1:4, plot_x, hell_x, 0.1)
# plot_over_time

get_wasserstein_dist = function(y_options, y_pm, p = 1){
  require(transport)
  wasserstein1d(y_options, y_pm, p = p)
}

# get the density at a certain value x, for a am.density object
mixture_density_options = function(x, density_obj, method) {
  if(method == "3lognorm"){
    w1 = density_obj$p.1 
    w2 = density_obj$p.2
    w3 = 1 - w1 - w2 
    
    return(
      w1 * dlnorm(x, density_obj$u.1, density_obj$sigma.1) +
      w2 * dlnorm(x, density_obj$u.2, density_obj$sigma.2) +
      w3 * dlnorm(x, density_obj$u.3, density_obj$sigma.3)
    )
  } else if (method == "1lognorm"){
    return(
      dlnorm(x, density_obj$mu, density_obj$zeta)
    )
  }
}

# get the density at a certain value x, for a PM mixture model
mixture_density_pm = function(x, params, method) {
  if (method == "3lognorm"){
    w2 = params$w.2
    w3 = params$w.3
    w1 = 1 - w2 -w3
    
    return(
      w1 * dlnorm(x, params$mu.1, params$sd.1) +
      w2 * dlnorm(x, params$mu.2, params$sd.2) +
      w3 * dlnorm(x, params$mu.3, params$sd.3)
    )
  } else if (method == "1lognorm"){
    return(
      dlnorm(x, params$mu, params$sd)
    )
  }
}

# get the hellinger distance between two fitted distributions
# get an approximation of the hellinger distance, where y_options, y_pm
# are vectors of density values over a large range of x
get_hellinger_distance = function(y_options, y_pm, dx){
  return(sqrt(0.5 * sum((sqrt(y_options)-sqrt(y_pm))^2 * dx)))
}

ks_test = function(y_options, x_new, pm_fit, dx, method, n = 10000){
  require(stats)
  if (method == "3lognorm"){
    # get random sample from the pm fit distribution
    w_2 = pm_fit$w.2
    w_3 = pm_fit$w.3
    w_1 = 1-w_2-w_3
    
    component = sample(c(1,2,3), size = n, replace = TRUE, prob = c(w_1, w_2, w_3))
    n1 = sum(component == 1)
    n2 = sum(component == 2)  
    n3 = n - n1 - n2
    pm_rnd = c(rlnorm(n1, pm_fit$mu.1, pm_fit$sd.1),
               rlnorm(n2, pm_fit$mu.2, pm_fit$sd.2),
               rlnorm(n3, pm_fit$mu.3, pm_fit$sd.3))
  } else if (method == "1lognorm"){
    pm_rnd = rlnorm(n, meanlog = pm_fit$mu, sdlog = pm_fit$sd)
  } else if (method == "nonparam"){
    rnd_unif = runif(n)
    
    pm_cdf = cumsum(pm_fit$data$y)*dx
    pm_rnd = approx(pm_cdf, x_new, xout = rnd_unif)$y
  }
    
  # get random sample from the option implied distribution
  # draw form cdf as there can be one negative weight for the log normals
  # so cdf ensures that we get a valid output
  cdf = cumsum(y_options)*dx
  # draw form uniform and compute corresponding sample
  rnd_unif = runif(n)
  opt_rnd = approx(cdf, x_new, xout = rnd_unif)$y
  
  test = ks.test(pm_rnd, opt_rnd, alternative = "two.sided")
  return(list("ks_statistic" = test$statistic, p_val = test$p.value))
}

# compute the mean and variance of the pm and option densities
# get_moments = function(method, option_density, pm_density, ci = TRUE, n = 10000,
#                        B = 1000, y_options = NULL, dx = NULL, x_new = NULL){
#   require(matrixStats)
#   if (method == "3lognorm"){
#     p.1 = option_density$p.1
#     p.2 = option_density$p.2
#     p.3 = 1 - p.1 - p.2
#     option_mean = p.1 * exp(option_density$u.1 + option_density$sigma.1^2 /2) + 
#                   p.2 * exp(option_density$u.2 + option_density$sigma.2^2 /2) +
#                   p.3 * exp(option_density$u.3 + option_density$sigma.3^2 /2)
#     
#     w.2 = pm_density$w.2
#     w.3 = pm_density$w.3
#     w.1 = 1- w.2 - w.3
#     pm_mean = w.1 * exp(pm_density$mu.1 + pm_density$sd.1^2 /2) +
#               w.2 * exp(pm_density$mu.2 + pm_density$sd.2^2 /2) +
#               w.3 * exp(pm_density$mu.3 + pm_density$sd.3^2 /2)
#     
#     # var = sum(w_i(sigma_i^2 + mu_i^2))- mu^2 
#     # source: https://en.wikipedia.org/wiki/Mixture_distribution#Moments 
#     pm_sd = sqrt(
#                   w.1 * (lognorm_var(mu = pm_density$mu.1, sd = pm_density$sd.1)
#                   + exp(pm_density$mu.1 + pm_density$sd.1^2 /2)^2) +
#                   w.2 * (lognorm_var(mu = pm_density$mu.2, sd = pm_density$sd.2)
#                   + exp(pm_density$mu.2 + pm_density$sd.2^2 /2)^2) +
#                   w.3 * (lognorm_var(mu = pm_density$mu.3, sd = pm_density$sd.3)
#                   + exp(pm_density$mu.3 + pm_density$sd.3^2 /2)^2) -
#                   pm_mean^2
#     )
#     # print(pm_sd)
#     
#     option_sd = sqrt(
#         p.1 * (lognorm_var(mu = option_density$u.1, sd = option_density$sigma.1) +
#                  exp(option_density$u.1 + option_density$sigma.1^2 / 2)^2) +
#         p.2 * (lognorm_var(mu = option_density$u.2, sd = option_density$sigma.2) +
#                  exp(option_density$u.2 + option_density$sigma.2^2 / 2)^2) +
#         p.3 * (lognorm_var(mu = option_density$u.3, sd = option_density$sigma.3) +
#                  exp(option_density$u.3 + option_density$sigma.3^2 / 2)^2) -
#         option_mean^2
#     )
#     
#     if (ci){
#       
#       mu.1 = pm_density$mu.1
#       mu.2 = pm_density$mu.2
#       mu.3 = pm_density$mu.3
#       sd.1 = pm_density$sd.1
#       sd.2 = pm_density$sd.2
#       sd.3 = pm_density$sd.3
#         
#       # PM CI
#       all_components = matrix(
#         sample(c(1,2,3), size = n*B, replace = TRUE, prob = c(w.1, w.2, w.3)),
#         nrow = n  
#       )
#       all_rnd = matrix(0, nrow = n, ncol = B)
#       mask1 = all_components == 1
#       mask2 = all_components == 2
#       mask3 = all_components == 3
#       all_rnd[mask1] = rlnorm(sum(mask1), mu.1, sd.1)
#       all_rnd[mask2] = rlnorm(sum(mask2), mu.2, sd.2)
#       all_rnd[mask3] = rlnorm(sum(mask3), mu.3, sd.3)
#       
#       pm_means = colMeans(all_rnd)
#       half_width_pm = (quantile(pm_means, 0.975) - quantile(pm_means, 0.025)) / 2
#       ci_pm = pm_mean + c(-1, 1) * half_width_pm
#       
#       # pm median
#       pm_medians = colMedians(all_rnd)
#       pm_median = mean(pm_medians)
#       pm_median_ci = (quantile(pm_medians, 0.975) - quantile(pm_medians, 0.025)) / 2
#       
#       #pm kurosis
#       
#       
#       # Option CI
#       cdf = cumsum(y_options) * dx
#       all_unif = runif(n * B)
#       all_opt = matrix(
#         approx(cdf, x_new, xout = all_unif)$y,
#         nrow = n
#       )
#       option_means = colMeans(all_opt)
#       # note that if all_unif[i] is very close to 0 or 1, all_opt[i] 
#       # can be NA. This happens very rarely, so just ignore it
#       half_width_opt = (quantile(option_means, 0.975, na.rm = TRUE) - 
#                           quantile(option_means, 0.025, na.rm = TRUE)) / 2
#       ci_option = option_mean + c(-1, 1) * half_width_opt
#     }
#   } else if (method == "1lognorm"){
#     option_mean = exp(option_density$mu + option_density$zeta^2 /2)
#     pm_mean =  exp(pm_density$mu + pm_density$sd^2 /2)
#     option_sd = sqrt(lognorm_var(mu = option_density$mu, sd = option_density$zeta))
#     pm_sd = sqrt(lognorm_var(mu = pm_density$mu, sd = pm_density$sd))
#     
#     if(ci){
#       all_rnd = matrix(rlnorm(n * B, pm_density$mu, pm_density$sd), nrow = n, ncol = B)
#       pm_means = colMeans(all_rnd)
#       half_width_pm = (quantile(pm_means, 0.975) - quantile(pm_means, 0.025)) / 2
#       ci_pm = pm_mean + c(-1, 1) * half_width_pm
#       
#       # Option CI
#       cdf = cumsum(y_options) * dx
#       all_unif = runif(n * B)
#       all_opt = matrix(
#         approx(cdf, x_new, xout = all_unif)$y,
#         nrow = n
#       )
#       option_means = colMeans(all_opt)
#       # note that if all_unif[i] is very close to 0 or 1, all_opt[i] 
#       # can be NA. This happens very rarely, so just ignore it
#       half_width_opt = (quantile(option_means, 0.975, na.rm = TRUE) - 
#                           quantile(option_means, 0.025, na.rm = TRUE)) / 2
#       ci_option = option_mean + c(-1, 1) * half_width_opt
#     }
#   }
#   
#   if (ci){
#     return(list("option_mean" = option_mean,
#                 "pm_mean" = pm_mean,
#                 "option_sd" = option_sd,
#                 "pm_sd" = pm_sd,
#                 "pm_ci_lower" = ci_pm[1],
#                 "pm_ci_upper" = ci_pm[2],
#                 "option_ci_lower" = ci_option[1],
#                 "option_ci_upper" = ci_option[2]))
#   }else{
#     return(list("option_mean" = option_mean,
#                 "pm_mean" = pm_mean,
#                 "option_sd" = option_sd,
#                 "pm_sd" = pm_sd))
#   }
# }

# get moments through bootstrapping

get_moments = function(method, option_density, pm_density, ci = TRUE, n = 10000,
                       B = 100, y_options = NULL, dx = NULL, x_new = NULL){
  if (method == "3lognorm"){
    mu.1 = pm_density$mu.1
          mu.2 = pm_density$mu.2
          mu.3 = pm_density$mu.3
          sd.1 = pm_density$sd.1
          sd.2 = pm_density$sd.2
          sd.3 = pm_density$sd.3

          # PM CI
          all_components = matrix(
            sample(c(1,2,3), size = n*B, replace = TRUE, prob = c(w.1, w.2, w.3)),
            nrow = n
          )
          all_rnd = matrix(0, nrow = n, ncol = B)
          mask1 = all_components == 1
          mask2 = all_components == 2
          mask3 = all_components == 3
          all_rnd[mask1] = rlnorm(sum(mask1), mu.1, sd.1)
          all_rnd[mask2] = rlnorm(sum(mask2), mu.2, sd.2)
          all_rnd[mask3] = rlnorm(sum(mask3), mu.3, sd.3)
          
          pm_moments = get_all_moments(all_rnd)
          
          
          
          cdf = cumsum(y_options) * dx
          all_unif = runif(n * B)
          all_rnd_opt = matrix(
            approx(cdf, x_new, xout = all_unif)$y,
            nrow = n
          )
          
          option_moments = get_all_moments(all_rnd_opt)
          
  } else if ( method == "1lognorm"){
    all_rnd = matrix(rlnorm(n * B, pm_density$mu, pm_density$sd), nrow = n, ncol = B)
    pm_moments = get_all_moments(all_rnd)
    
    
    cdf = cumsum(y_options) * dx
    all_unif = runif(n * B)
    all_rnd_opt = matrix(
      approx(cdf, x_new, xout = all_unif)$y,
      nrow = n
    )
    option_moments = get_all_moments(all_rnd_opt)
  } else if(method == "nonparam"){
    
    # find boostrap sample using cdf method
    cdf = cumsum(option_density$rnd) * dx
    all_unif = runif(n * B)
    all_rnd = matrix(
      approx(cdf, x_new, xout = all_unif)$y,
      nrow = n
    )
    option_moments = get_all_moments(all_rnd)
    
    
    cdf = cumsum(pm_density$data$y) * dx
    all_unif = runif(n * B)
    all_rnd = matrix(
      approx(cdf, x_new, xout = all_unif)$y,
      nrow = n
    )
    
    pm_moments = get_all_moments(all_rnd)
  }
  
  return(c(pm_moments, option_moments))
}
# compute the variance of a lognormal
lognorm_var = function(mu, sd){
  return((exp(sd^2) - 1) * exp(2 * mu + sd^2))
}

get_all_moments = function(mat) {
  require(matrixStats)
  n = ncol(mat)
  mu = colMeans(mat, na.rm = TRUE)
  
  centered = sweep(mat, 2, mu, FUN = "-")
  
  m2 = colMeans(centered^2, na.rm = TRUE)
  m3 = colMeans(centered^3, na.rm = TRUE)
  m4 = colMeans(centered^4, na.rm = TRUE)
  
  medians = colMedians(mat, na.rm = TRUE)
  median = mean(medians)
  median_ci = unname((quantile(medians, 0.975, na.rm = TRUE) - 
                 quantile(medians, 0.025, na.rm = TRUE))) / 2
  
  mean     = mean(mu)
  mean_ci = unname((quantile(mu, 0.975, na.rm = TRUE) - 
    quantile(mu, 0.025, na.rm = TRUE))) / 2
  variance =mean(m2)   
  variance_ci = unname((quantile(m2, 0.975, na.rm = TRUE) - 
                   quantile(m2, 0.025, na.rm = TRUE))) / 2
  skewness = mean(m3 / m2^(3/2))
  skewness_ci = unname((quantile(m3 / m2^(3/2), 0.975, na.rm = TRUE) - 
                   quantile(m3 / m2^(3/2), 0.025, na.rm = TRUE))) / 2
  kurtosis = mean(m4 / m2^2)
  kurtosis_ci = unname((quantile(m4 / m2^2, 0.975, na.rm = TRUE) - 
                   quantile(m4 / m2^2, 0.025, na.rm = TRUE))) / 2

    
  list(
    mean = mean,
    mean_halfwidth = mean_ci,
    median = median,
    median_halfwidth = median_ci,
    variance = variance,
    variance_halfwidth = variance_ci,
    skewness = skewness,
    skewness_halfwidth = skewness_ci,
    kurtosis = kurtosis,
    kurtosis_halfwidth = kurtosis_ci
  )
}




# plot the option and pm distributions over time
# get_plot_over_time = function(data_options, data_pm, dte, plot_x, hell_x, dx, method){
#   require(ggplot2)
#   require(patchwork)
#   opt_cols = colorRampPalette(c("grey80", "black"))(length(dte))
#   pm_cols = colorRampPalette(c("lightblue", "darkblue"))(length(dte))
#   
#   # Build combined data first
#   all_data = data.frame()
#   hellinger_distances = c()
#   x_plot = seq(plot_x[1], plot_x[2], by = dx)
#   
#   for(i in dte){
#     fit_options = get_am_density(data_options, dte = dte[i])
#     fit_pm = fit_am_pm(data_pm, dte = dte[i])
#     
#     hellinger_distances = c(hellinger_distances, 
#                             get_hellinger_distance(fit_options, fit_pm, hell_x))
#     
#     all_data = rbind(all_data, data.frame(
#       x     = x_plot,
#       y     = mixture_density_options(x_plot, fit_options, method = method),
#       dte   = as.factor(dte[i]),
#       type  = "Options"
#     ))
#     all_data = rbind(all_data, data.frame(
#       x     = x_plot,
#       y     = mixture_density_pm(x_plot, fit_pm),
#       dte   = as.factor(dte[i]),
#       type  = "PM"
#     ))
#   }
#   
#   # Named color vector matching interaction(type, dte) levels
#   color_values = c(
#     setNames(opt_cols, paste("Options", dte, sep = ".")),
#     setNames(pm_cols,  paste("PM", dte, sep = "."))
#   )
#   
#   # Plot
#   hellinger_plot = ggplot() +
#     geom_line(data = data.frame(x = dte, y = hellinger_distances),
#               aes(x = x, y = y, color = "Hellinger Distance"), linewidth = 1.5) +
#     labs(title = "", x = "DTE", y = "Distances")
#   this_plot = ggplot(all_data, aes(x = x, y = y, color = interaction(type, dte))) +
#     geom_line(linewidth = 1.5) +
#     scale_color_manual(values = color_values, name = "Legend") +
#     theme(legend.position = "right")
#   return(this_plot /hellinger_plot + plot_layout(heights = c(3, 1)))
# }