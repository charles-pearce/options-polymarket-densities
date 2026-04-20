# Accuracy table
library(tidyverse)

acc_np <- read_csv("nonparam_accuracy_result.csv")
acc_1l <- read_csv("1lognorm_accuracy_result.csv")
acc_3l <- read_csv("3lognorm_accuracy_result.csv")

fmt_p <- function(p, stat) {
  stars <- case_when(
    p < 0.001 ~ "$^{***}$",
    p < 0.01  ~ "$^{**}$",
    p < 0.05  ~ "$^{*}$",
    TRUE      ~ ""
  )
  paste0(formatC(p, format = "f", digits = 3), stars)
}

get_agg <- function(df, model_label) {
  df |>
    filter(is.na(stock), calibration_method == "NOT CALIBRATED") |>
    arrange(DTE) |>
    transmute(
      model         = model_label,
      DTE,
      brier_pm      = round(brier_score_pm,      4),
      brier_opt     = round(brier_score_options,  4),
      brier_dm_stat = round(brier_dm_stat,        3),
      brier_p       = brier_dm_p_val,
      log_pm        = round(log_score_pm,         4),
      log_opt       = round(log_score_options,    4),
      log_dm_stat   = round(log_dm_stat,          3),
      log_p         = log_dm_p_val
    )
}

table_df <- bind_rows(
  get_agg(acc_np, "Nonparametric"),
  get_agg(acc_1l, "Single Lognormal"),
  get_agg(acc_3l, "Lognormal Mixture")
)

make_row <- function(dte, bp, bo, bd, bpp, lp, lo, ld, lpp) {
  sprintf(
    "& %d & %.4f & %.4f & %.3f & %s & %.4f & %.4f & %.3f & %s \\\\",
    dte, bp, bo, bd, fmt_p(bpp, bd), lp, lo, ld, fmt_p(lpp, ld)
  )
}

rows <- mapply(make_row,
               table_df$DTE,
               table_df$brier_pm,   table_df$brier_opt,
               table_df$brier_dm_stat, table_df$brier_p,
               table_df$log_pm,     table_df$log_opt,
               table_df$log_dm_stat,   table_df$log_p)

get_block <- function(model_name) {
  rows[table_df$model == model_name]
}

latex_table <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Aggregate Forecast Accuracy of Prediction Market and Options-Implied Densities}",
  "\\footnotesize",
  "\\label{tab:accuracy}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{llccccccccc}",
  "\\toprule",
  "& & \\multicolumn{4}{c}{\\textbf{Brier Score}} & \\multicolumn{4}{c}{\\textbf{Log Score}} \\\\",
  "\\cmidrule(lr){3-6} \\cmidrule(lr){7-10}",
  "& DTE & PM & Options & DM Stat & $p$-value & PM & Options & DM Stat & $p$-value \\\\",
  "\\midrule",
  "\\multicolumn{10}{l}{\\textit{Nonparametric}} \\\\",
  get_block("Nonparametric"),
  "\\addlinespace",
  "\\multicolumn{10}{l}{\\textit{Single Lognormal}} \\\\",
  get_block("Single Lognormal"),
  "\\addlinespace",
  "\\multicolumn{10}{l}{\\textit{Lognormal Mixture}} \\\\",
  get_block("Lognormal Mixture"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item \\textit{This table presents aggregate forecast accuracy of prediction",
  "market (PM) and options-implied densities. Brier Score is lower for better",
  "forecasts. Log Score is lower for better forecasts. DM Stat is the",
  "Diebold-Mariano test statistic; a negative value indicates PM outperforms",
  "options. The three density extraction methods are described in",
  "Sections~\\ref{Nonparametric Method}, \\ref{single lognorm methodology},",
  "and~\\ref{Mixture of Lognormals}. Scores are pooled across ten stocks",
  "over the full sample period using raw normalised PM prices without",
  "recalibration. $^{*}$, $^{**}$, and $^{***}$ denote significance at the",
  "10\\%, 5\\%, and 1\\% levels respectively.}",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(latex_table, "table_accuracy3.0.tex")
cat("Done — written to table_accuracy3.0.tex\n")
