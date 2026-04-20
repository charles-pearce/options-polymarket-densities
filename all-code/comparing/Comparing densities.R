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
  return(sqrt(0.5 * sum((sqrt(y_options)-sqrt(y_pm))^2 * dx, na.rm = TRUE)))
}

ks_test = function(y_options, x_new, pm_fit, dx, method, n = 10000){
  require(stats)
  if (method == "3lognorm"){
    # get random sample from the pm fit distribution
    w_2 = pm_fit$w.2
    w_3 = pm_fit$w.3
    w_1 = 1-w_2-w_3
    
    # pmax to ensure that weights are positive (should be the case due to restriction in numerical
    # optimization, but seems like very rarely it isn't)
    component = sample(c(1,2,3), size = n, replace = TRUE, prob = pmax(c(w_1, w_2, w_3),0))
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
    pm_rnd = approx(pm_cdf, x_new, xout = rnd_unif, rule = 2)$y
  }
    
  # get random sample from the option implied distribution
  # draw form cdf as there can be one negative weight for the log normals
  # so cdf ensures that we get a valid output
  cdf = cumsum(y_options)*dx
  # draw form uniform and compute corresponding sample
  rnd_unif = runif(n)
  opt_rnd = approx(cdf, x_new, xout = rnd_unif, rule = 2)$y
  
  test = ks.test(pm_rnd, opt_rnd, alternative = "two.sided")
  return(list("ks_statistic" = test$statistic, p_val = test$p.value))
}

# get moments through bootstrapping

get_moments = function(method, option_density, pm_density, n = 10000,
                       B = 100, y_options = NULL, dx = NULL, x_new = NULL){
  if (method == "3lognorm"){
    require(DescTools)
    
    mu.1 = pm_density$mu.1
    mu.2 = pm_density$mu.2
    mu.3 = pm_density$mu.3
    sd.1 = pm_density$sd.1
    sd.2 = pm_density$sd.2
    sd.3 = pm_density$sd.3
    w.2 = pm_density$w.2
    w.3 = pm_density$w.3
    w.1 = 1 - w.2 - w.3

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
    
    # winsorize highest 5% of random draws for robustness
    all_rnd = sapply(1:ncol(all_rnd), function(i) Winsorize(all_rnd[,i], val = quantile(all_rnd[,i], probs = c(0, 0.95))))
    
    pm_moments = get_all_moments(all_rnd)
    
    cdf = cumsum(y_options) * dx
    all_unif = runif(n * B)
    all_rnd_opt = matrix(
      approx(cdf, x_new, xout = all_unif, rule = 2)$y,
      nrow = n
    )
    
    option_moments = get_all_moments(all_rnd_opt)
          
  } else if ( method == "1lognorm"){
    all_rnd = matrix(rlnorm(n * B, pm_density$mu, pm_density$sd), nrow = n, ncol = B)
    pm_moments = get_all_moments(all_rnd)
    
    
    cdf = cumsum(y_options) * dx
    all_unif = runif(n * B)
    all_rnd_opt = matrix(
      approx(cdf, x_new, xout = all_unif, rule = 2)$y,
      nrow = n
    )
    option_moments = get_all_moments(all_rnd_opt)
  } else if(method == "nonparam"){
    
    # find boostrap sample using cdf method
    cdf = cumsum(option_density$rnd) * dx
    all_unif = runif(n * B)
    all_rnd = matrix(
      approx(cdf, x_new, xout = all_unif, rule = 2)$y,
      nrow = n
    )
    option_moments = get_all_moments(all_rnd)
    
    
    cdf = cumsum(pm_density$data$y) * dx
    all_unif = runif(n * B)
    all_rnd = matrix(
      approx(cdf, x_new, xout = all_unif, rule = 2)$y,
      nrow = n
    )
    
    pm_moments = get_all_moments(all_rnd)
  }
  
  return(list(pm = pm_moments, option = option_moments))
}
# compute the variance of a lognormal
lognorm_var = function(mu, sd){
  return((exp(sd^2) - 1) * exp(2 * mu + sd^2))
}

get_all_moments = function(mat) {
  require(matrixStats)
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
  variance = mean(m2)   
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