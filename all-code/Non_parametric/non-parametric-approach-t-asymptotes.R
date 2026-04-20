# approximate the pdf by fitting a spline to the empirical cdf
non_param_approx = function(brackets, probs, dx = 0.1, x_range = NULL){
  require(splines)
  require(dplyr)
  require(stats)
  
  if (length(probs) < 10) {
    stop("Too few brackets to fit spline with df = 8")
  }
  
  cum_probs = cumsum(probs)/(sum(probs))
  knot_x = sapply(brackets, FUN = max)
  
  knot_x[length(knot_x)] = knot_x[length(knot_x)] +
    (knot_x[length(knot_x)-1] - knot_x[length(knot_x)-2])
  
  # Check spline inputs are valid
  if (any(is.na(knot_x)) || any(diff(knot_x) <= 0)) {
    stop("knot_x is non-monotone or contains NAs")
  }
  
  
  # Wrap smooth.spline in tryCatch
  fit_cdf = tryCatch(
    smooth.spline(knot_x, cum_probs, df = 8),
    error = function(e) { stop("smooth.spline failed: ", e$message)}
  )
  if (is.null(fit_cdf)) stop("fit cdf is null")
  
  # if no x range is given determine it by looking at the brackets
  if (is.null(x_range)){
    # make sure brackets are ordered
    brackets = brackets[order(sapply(brackets, function(a) a[2]))]
    x_min = brackets[[1]][2] * 0.8
    x_max = brackets[[length(brackets)]][1] * 1.2
  } else {
    x_min = x_range[1]
    x_max = x_range[2]
  }
  x_new = seq(x_min, x_max, by = dx)
  
  # Boundary info for tail fitting
  x_left  = knot_x[2]
  F_left  = predict(fit_cdf, x_left)$y
  dF_left = predict(fit_cdf, x_left,  deriv = 1)$y      # slope at left knot
  d2F_left  <- predict(fit_cdf, x_left,  deriv = 2)$y   # curvature
  
  F_left  = pmax(0.001, pmin(0.999, F_left))
  if (dF_left < 1e-6) dF_left = 1e-6   # avoid division by zero in sigma
  
  if (dF_left <= 0 || F_left <= 0 || F_left >= 1) {
    stop("Invalid left join point: F_left = ", F_left, ", dF_left = ", dF_left)
  }
  
  x_right = knot_x[length(knot_x) - 1]
  F_right = predict(fit_cdf, x_right)$y
  dF_right = predict(fit_cdf, x_right, deriv = 1)$y     # slope at right knot
  d2F_right <- predict(fit_cdf, x_right, deriv = 2)$y   # curvature
  
  F_right  = pmax(0.001, pmin(0.999, F_right))
  if (dF_right < 1e-6) dF_right = 1e-6
  
  if (dF_right <= 0 || F_right <= 0 || F_right >= 1) {
    stop("Invalid right join point: F_right = ", F_right, ", dF_right = ", dF_right)
  }
  
  # Left tail, t distribution
  left  = fit_t_tail(F_left,  dF_left,  d2F_left,  x_join = x_left)
  left_tail_pdf = function(x) dt((x - left$mu)  / left$sigma,  df = left$nu) / left$sigma
  
  # Right tail, t distribution
  right = fit_t_tail(F_right, dF_right, d2F_right, x_join = x_right)
  right_tail_pdf = function(x) dt((x - right$mu) / right$sigma, df = right$nu) / right$sigma
  
  
  # Assemble PDF piece-wise making sure that we only get positive values
  pdf_vals = pmax(ifelse(
    x_new < x_left,  left_tail_pdf(x_new),
    ifelse(x_new > x_right, right_tail_pdf(x_new),
           predict(fit_cdf, x_new, deriv = 1)$y)),
    0
  )
  
 
  pdf_data = data.frame(x = x_new, y = pdf_vals)
  
  return(list(data = pdf_data, area = sum(pdf_vals)*dx))
}

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
    uniroot(residual, interval = c(2 + 1e-4, 30))$root,
    error = function(e) nu_init    # fall back to default if no solution found
  )
  
  z     = qt(F_join, df = nu_solution)
  sigma = dt(z, df = nu_solution) / dF_join
  mu    = x_join - z * sigma       # note: x_join must be in scope
  
  list(nu = nu_solution, mu = mu, sigma = sigma)
}


