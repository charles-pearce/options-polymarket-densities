# data = read.csv("Options\\NVDA_2026-03-27_day_full.csv")
# a = get_am_density(data, dte = 4)
# plot_am_density(a, x_min = 100, x_max = 300)

get_option_density = function(data, dte, method){
  require(RND)
  require(dplyr)
  data = data |>
    filter(DTE == dte)
  
  calls = arrange(filter(data, OptionType == "call"), Strike)
  puts = arrange(filter(data, OptionType == "put"), Strike)
  r = unique(data$Rate)
  te = unique(data$Tau)
  s0 = unique(data$SpotClose)
  
  if(method == "3lognorm"){
    fit = extract.am.density(r = r, 
                             te = te, 
                             s0 = s0, 
                             market.calls = calls$Mid,
                             market.puts = puts$Mid,
                             strikes = calls$Strike)
    
  } else if (method == "1lognorm"){
    fit = extract.bsm.density(r = r, 
                              y = 0,
                              te = te, 
                              s0 = s0, 
                              market.calls = calls$Mid,
                              market.puts = puts$Mid,
                              call.strikes = calls$Strike,
                              put.strikes = puts$Strike)
  }
  return(fit)
}

# plot_am_density = function(density_obj, x_min, x_max){
#   require(dplyr)
#   mixture_density_options = function(x, density_obj) {
#     w1 = density_obj$p.1 
#     w2 = density_obj$p.2
#     w3 = 1 - w1 - w2 
#     
#     w1 * dlnorm(x, density_obj$u.1, density_obj$sigma.1) +
#       w2 * dlnorm(x, density_obj$u.2, density_obj$sigma.2) +
#       w3 * dlnorm(x, density_obj$u.3, density_obj$sigma.3)
#   }
#   
#   
#   # plot it
#   x = seq(x_min, x_max, length.out = 10000)
#   y = mixture_density_options(x, a)
#   
#   density_df = data.frame(x = x, y = y)
#   
#   
#   plot = ggplot() +
#     # mixture density line
#     geom_line(data = density_df, aes(x = x, y = y), linewidth = 1.5) +
#     # step plot from poly
#     labs(title = "miauw",
#          x = "Strike",
#          y = "Density")
#   print(plot)
#   
# }


