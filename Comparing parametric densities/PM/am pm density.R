library(stats)

# get prob (log-normal) of being in an interval
prob_interval = function(start, end, mean, sd){
  return(plnorm(end, meanlog = mean, sdlog = sd) - 
           plnorm(start, meanlog = mean, sdlog = sd))
}

# get the probability from a mixture of 3 log normals, 
# params is a list of parameters list(w_2,w_3,mu_1,...,mu_3,sd_1,...sd_3)
mixture_prob = function(bracket, params){
  
  (1-params$w.2-params$w.3)*prob_interval(bracket[1], bracket[2], params$mu.1, params$sd.1) +
                params$w.2*prob_interval(bracket[1], bracket[2], params$mu.2, params$sd.2) +
                params$w.3 * prob_interval(bracket[1], bracket[2], params$mu.3, params$sd.3)
}

# idea: use this to find the parameters that best fit the observed probabilities on the interval

# brackets must be lists of vectors: c(lower, upper)
# params is a list of parameters
objective = function(params, brackets, probs, method){
  
  if (method == "3lognorm"){
    if (length(params) != 8){
      # print(params)
      stop(paste("there should be 8 parameters in the objective function but there are",
                 length(params), "you are using method:", method))
    }
    ob = sum(abs(sapply(1:length(brackets), function(i) 
        (mixture_prob(brackets[[i]], params) - probs[i])^2)))
  } else if (method == "1lognorm"){
    
    if(length(params) != 2){
      stop(paste("there should be 2 parameters in the objective function but there are",
                 length(params), "you are using method:", method))
    }
    ob = sum(abs(sapply(1:length(brackets), function(i) 
      (prob_interval(start = brackets[[i]][1], end = brackets[[i]][2],
                     mean = params$mu, sd = params$sd) - probs[i])^2)))
  }
  return(ob)
}

fit_pm_density = function(method, data, educated_guess = NULL, x_range = NULL, dx = NULL){
  require(stats)

  brackets = sapply((strsplit(gsub( "<|>", "",data$bracket), "-")), function(x) as.numeric(x))
  probs = data[["probs"]]
  
  # make sure the lower an upper brackets are (0,x) and (x,Inf)
  brackets = sapply(1:length(brackets), function(i) {
              a = brackets[[i]]
              if(length(a) == 1){
                if(a[1] == min(unlist(brackets))){
                  c(0, a[1])
                } else if(a[1] == max(unlist(brackets))){
                  c(a[1], Inf)
                } else {
                  stop("Error: a bracket not at the tails only has one value")
                } 
              } else {
                a
              }
            }, simplify = FALSE)
  
  # ensure probs and brackets are sorted
  corr_order = order(sapply(brackets, FUN = min))
  brackets = brackets[corr_order]
  probs = probs[corr_order]
  
  if(method == "nonparam"){
    brackets[[1]][1] = x_range[1]
    brackets[[length(brackets)]][2] = x_range[2]
    return(non_param_approx(brackets = brackets, probs = probs, x_range = x_range, dx = dx))
  }
  
  # get starting values. start with equal weights and estimate mu and sigma
  # but then give one higher mu and sd and one lower
  if (is.null(educated_guess)){
    # function to handle the Inf in the last bracket
    interval_mean = function(a) {
      if (is.infinite(a[2])) return(a[1])
      if (is.infinite(a[1])) return(a[2])
      mean(a)
    }
    # sort the brackets and probabilities
    ord = order(sapply(brackets, function(x) x[1]))
    brackets = brackets[ord]
    probs = probs[ord]
    
    mean = sum(sapply(1:length(brackets), function(i) interval_mean(brackets[[i]]) * probs[i]))
    cum_prob = 0
    median = 1
    for(i in 1:(length(brackets)-1)){
      cum_prob = cum_prob + probs[i]
      if (cum_prob < 0.5 && cum_prob + probs[i+1] >= 0.5){
        median = mean(brackets[[i+1]])
      }
    }
    mu = log(median)
    sd = sqrt(2*max(log(mean) - mu, 1e-3))
    if(method == "3lognorm"){
      educated_guess = c(0.33, 0.33, mu, mu*0.8, mu*1.2, sd, 0.8*sd, 1.2*sd)
    } else if (method == "1lognorm") {
      educated_guess = c(mu, sd)
    }
  }
  
  
  this_objective = function(prms) {
    if(method == "3lognorm"){
      params = list("w.2" = prms[1], "w.3" = prms[2], "mu.1" = prms[3],
                    "mu.2" = prms[4], "mu.3" = prms[5], "sd.1" = prms[6],
                    "sd.2" = prms[7], "sd.3" = prms[8])
      # avoid weigths below 0.05
      if (params$w.2 < 0.05 || params$w.3 < 0.05 || params$w.2 + params$w.3 > 0.95) return(1e10)
      if (any(params[c("sd.1", "sd.2", "sd.3")] <= 1e-3)) return(1e10)
      if (any(params[c("sd.1", "sd.2", "sd.3")] > 5)) return(1e10)
    } else if (method == "1lognorm") {
      params = list("mu" = prms[1], "sd" = prms[2])
      if(params$sd < 1e-3) return(1e10)
    }
    return(objective(params = params,
              brackets = brackets,
              probs = probs,
              method = method))
    }
  
  # try 100 different starting values
  initial_values = list()
  for (i in 1:100){
    if (i == 1){
      initial_values[[i]] = educated_guess
    } else if(method == "3lognorm") {
    initial_values[[i]] = c(
      runif(2, 0.05, 0.48), runif(3, 0, log(2*tail(sort(unlist(brackets)),2)[1])),
      runif(3, 1e-3, 4)
    )
    } else if(method == "1lognorm"){
      initial_values[[i]] = c(
        runif(1, 0, log(2*tail(sort(unlist(brackets)),2)[1])), 
        runif(1, 1e-3, 4)
      )
    }
  }
  # only optimize very roughly
  coarse = lapply(initial_values, function(start) {
    tryCatch(
      optim(start, fn = this_objective, method = "Nelder-Mead",
            control = list(maxit = 200)),
      error = function(e) list(value = 1e10)
    )
  })
  # for the best 5, optimize fully
  top5 <- order(sapply(coarse, function(r) r$value))[1:5]
  refined = lapply(top5, function(i) {
    optim(coarse[[i]]$par, fn = this_objective, method = "Nelder-Mead",
          control = list(maxit = 10000))
  })
  
  best <- refined[[which.min(sapply(refined, function(r) r$value))]]
  
  if (method == "3lognorm"){
    
    return(list("w.2" = best$par[1], "w.3" = best$par[2],
                "mu.1" = best$par[3], "mu.2" = best$par[4],
                "mu.3" = best$par[5], "sd.1" = best$par[6],
                "sd.2" = best$par[7], "sd.3" = best$par[8]))
    
  } else if(method == "1lognorm"){
    
    return(list("mu" = best$par[1],
                "sd" = best$par[2]))
  }
}

################################################################################
# #plot the distribution
# plot_log_mixture = function(params, poly_probs, x_min, x_max, title){
#   require(ggplot2)
#   mixture_density = function(x, params) {
#     w2 = params$w.2
#     w3 = params$w.3
#     w1 = 1 - w2 - w3
#     
#     w1 * dlnorm(x, params$mu.1, params$sd.1) +
#       w2 * dlnorm(x, params$mu.2, params$sd.2) +
#       w3 * dlnorm(x, params$mu.3, params$sd.3)
#   }
# 
#   # plot it
#   x = seq(x_min, x_max, length.out = 10000)
#   y = mixture_density(x, params)
#   
#   density_df = data.frame(x = x, y = y)
#   
#   start_bracket = sapply(brackets, function(x) x[1])
#   start_bracket[1] = start_bracket[1]-5
#   poly_df = data.frame(start_bracket = start_bracket, poly_probs)
#   
#   plot = ggplot() +
#     # mixture density line
#     geom_line(data = density_df, aes(x = x, y = y), linewidth = 1.5) +
#     # step plot from poly
#     geom_step(data = poly_df, aes(x = start_bracket, y = poly_probs), 
#               linewidth = 1.5, color = "red") +
#     labs(title = title,
#          x = "Strike",
#          y = "Density")
#   print(plot)
# }


# library(readxl)
# library(dplyr)
# dte = 3
# data_loc = "C:\\Users\\lars\\OneDrive\\Dokumente\\Uni\\Seminar Forecasting\\data collection\\Getting poly data\\"   
# aapl_3_27 = read_excel(paste0(data_loc, "aapl_weekly_market\\aapl-week-november-28-2025.xlsx"))
# dte4 = aapl_3_27|> filter(DTE == dte)
# brackets = sapply((strsplit(gsub( "<|>", "",dte4$bracket), "-")), function(x) as.numeric(x))
# probs = dte4$price_yes
# approx_interval = (range(brackets)[2]+5)- (range(brackets)[1] -5)
# probs_2 = dte4$price_yes /(mean(probs) * approx_interval)
# 
# mean(probs_2) * approx_interval
# 
# params = fit_am_pm(aapl_3_27, 4)
# 
# plot_log_mixture(params, poly_probs = probs_2, x_min = range(brackets)[1]*0.9, x_max = range(brackets)[2]*1.1, title = dte)
# 



