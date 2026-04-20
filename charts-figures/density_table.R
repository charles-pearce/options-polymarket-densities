library(tidyverse)

# Load data
den_np <- read_csv("nonparam_density_result.csv")
den_1l <- read_csv("1lognorm_density_result.csv")
den_3l <- read_csv("3lognorm_density_result.csv")

# Compute summary statistics per model, NOT CALIBRATED only
get_dist_summary <- function(df, model_label) {
  df |>
    filter(calibration_method == "NOT CALIBRATED") |>
    group_by(DTE) |>
    summarise(
      model     = model_label,
      n         = n(),
      wass_med  = median(Wasserstein.Distance, na.rm = TRUE),
      hell_med  = median(Hellinger.Distance,   na.rm = TRUE),
      ks_med    = median(KS.statistic,         na.rm = TRUE),
      ks_p_mean = mean(KS.p.value,             na.rm = TRUE),
      .groups   = "drop"
    )
}

summary_df <- bind_rows(
  get_dist_summary(den_np, "Nonparametric"),
  get_dist_summary(den_1l, "Single Lognormal"),
  get_dist_summary(den_3l, "Lognormal Mixture")
) |>
  mutate(model = factor(model, levels = c(
    "Nonparametric", "Single Lognormal", "Lognormal Mixture"
  ))) |>
  arrange(model, DTE)

# Format one data row
make_row <- function(dte, n, wm, hm, km, kp) {
  sprintf("& %d & %d & %.4f & %.4f & %.4f & %.2e \\\\",
          dte, n, wm, hm, km, kp)
}

rows <- mapply(make_row,
               summary_df$DTE,
               summary_df$n,
               summary_df$wass_med,
               summary_df$hell_med,
               summary_df$ks_med,
               summary_df$ks_p_mean)

get_block <- function(model_name) {
  rows[summary_df$model == model_name]
}

latex_table <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Distributional Distance Between Prediction Market and Options-Implied Densities}",
  "\\label{tab:density_distance}",
  "\\small",
  "\\begin{threeparttable}",
  "\\begin{tabular}{llcccccc}",
  "\\toprule",
  "& DTE & $N$ & Median Wasserstein & Median Hellinger & Median KS & Mean KS $p$-value \\\\",
  "\\midrule",
  
  "\\multicolumn{8}{l}{\\textit{Nonparametric}} \\\\",
  get_block("Nonparametric"),
  "\\addlinespace",
  
  "\\multicolumn{8}{l}{\\textit{Single Lognormal}} \\\\",
  get_block("Single Lognormal"),
  "\\addlinespace",
  
  "\\multicolumn{8}{l}{\\textit{Lognormal Mixture}} \\\\",
  get_block("Lognormal Mixture"),
  
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\small",
  "\\item \\textit{Notes}: This table reports distributional distance metrics between",
  "prediction market and options-implied risk-neutral densities across three density",
  "extraction methods using raw uncalibrated prediction market prices. $N$ denotes",
  "the number of stock-date observations. Wasserstein distance measures the total",
  "transport cost between the two densities. Hellinger distance ranges from zero",
  "(identical densities) to one (disjoint support). The Kolmogorov-Smirnov (KS)",
  "statistic is the maximum absolute difference between the two cumulative",
  "distribution functions. Mean KS $p$-value reports the average $p$-value from",
  "two-sample KS tests of density equality; values close to zero indicate",
  "statistically distinct densities. The three density extraction methods are",
  "described in Sections~\\ref{Nonparametric Method}, \\ref{single lognorm methodology},",
  "and~\\ref{Mixture of Lognormals}. Metrics are computed across ten stocks",
  "(AAPL, AMZN, GOOGL, META, MSFT, NFLX, NVDA, OPEN, PLTR, TSLA)",
  "over November 2025 to March 2026.",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(latex_table, "table_density.tex")
cat("Done — table written to table_density.tex\n")