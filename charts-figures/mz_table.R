# MZ table
library(tidyverse)

mz_np <- read_csv("nonparam_mz_result.csv")
mz_1l <- read_csv("1lognorm_mz_result.csv")
mz_3l <- read_csv("3lognorm_mz_result.csv")

fmt_p <- function(p) {
  case_when(
    p < 0.001 ~ "$<$0.001$^{***}$",
    p < 0.01  ~ paste0(formatC(p, format="f", digits=3), "$^{**}$"),
    p < 0.05  ~ paste0(formatC(p, format="f", digits=3), "$^{*}$"),
    TRUE      ~ formatC(p, format="f", digits=3)
  )
}

get_mz <- function(df, model_label) {
  df |>
    filter(stock == "all", calibration_method == "NOT CALIBRATED") |>
    select(DTE, coefficient, estimate, p_val) |>
    pivot_wider(names_from = coefficient,
                values_from = c(estimate, p_val)) |>
    transmute(
      model          = model_label,
      DTE,
      int_est        = round(estimate_intercept, 4),
      int_p          = p_val_intercept,
      beta_est       = round(estimate_beta,      4),
      beta_p         = p_val_beta,
      joint_p        = p_val_joined
    ) |>
    arrange(DTE)
}

table_df <- bind_rows(
  get_mz(mz_np, "Nonparametric"),
  get_mz(mz_1l, "Single Lognormal"),
  get_mz(mz_3l, "Lognormal Mixture")
)

make_row <- function(dte, ie, ip, be, bp, jp) {
  sprintf(
    "& %d & %.4f & %s & %.4f & %s & %s \\\\",
    dte, ie, fmt_p(ip), be, fmt_p(bp), fmt_p(jp)
  )
}

rows <- mapply(make_row,
               table_df$DTE,
               table_df$int_est, table_df$int_p,
               table_df$beta_est, table_df$beta_p,
               table_df$joint_p)

get_block <- function(model_name) {
  rows[table_df$model == model_name]
}

latex_table <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Mincer-Zarnowitz Regression Results --- Pooled Across All Stocks}",
  "\\footnotesize",
  "\\label{tab:mz}",
  "\\begin{threeparttable}",
  "\\begin{tabular}{llccccc}",
  "\\toprule",
  "& & \\multicolumn{2}{c}{\\textbf{Intercept}} & \\multicolumn{2}{c}{\\textbf{Beta}} & \\textbf{Joint} \\\\",
  "\\cmidrule(lr){3-4} \\cmidrule(lr){5-6}",
  "& DTE & Estimate & $p$-value & Estimate & $p$-value & $p$-value \\\\",
  "\\midrule",
  "\\multicolumn{7}{l}{\\textit{Nonparametric}} \\\\",
  get_block("Nonparametric"),
  "\\addlinespace",
  "\\multicolumn{7}{l}{\\textit{Single Lognormal}} \\\\",
  get_block("Single Lognormal"),
  "\\addlinespace",
  "\\multicolumn{7}{l}{\\textit{Lognormal Mixture}} \\\\",
  get_block("Lognormal Mixture"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{tablenotes}[flushleft]",
  "\\footnotesize",
  "\\item \\textit{This table reports Mincer-Zarnowitz regression estimates",
  "from regressing prediction market probabilities on options-implied",
  "probabilities, pooled across ten stocks. The intercept and beta columns",
  "report OLS coefficient estimates with associated $p$-values. The joint",
  "$p$-value tests the null hypothesis $H_0: \\alpha = 0, \\beta = 1$",
  "simultaneously using an $F$-test. Under perfect consistency between the",
  "two markets, the intercept equals zero and the beta equals one. Results",
  "are reported for raw uncalibrated prediction market prices. The three",
  "density extraction methods are described in",
  "Sections~\\ref{Nonparametric Method}, \\ref{single lognorm methodology},",
  "and~\\ref{Mixture of Lognormals}. $^{*}$, $^{**}$, and $^{***}$ denote",
  "significance at the 10\\%, 5\\%, and 1\\% levels respectively.}",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(latex_table, "table_mz2.0.tex")
cat("Done — written to table_mz.tex\n")
