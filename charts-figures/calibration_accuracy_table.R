# Table calibration for accuracy with and without calibration
library(tidyverse)

acc_np <- read_csv("nonparam_accuracy_result.csv")
acc_1l <- read_csv("1lognorm_accuracy_result.csv")
acc_3l <- read_csv("3lognorm_accuracy_result.csv")

# Build table data
get_cal_table <- function(df, model_label) {
  agg <- df[is.na(df$stock), ]
  
  nc <- agg |>
    filter(calibration_method == "NOT CALIBRATED") |>
    arrange(DTE) |>
    transmute(DTE,
              brier_nc = brier_score_pm,
              log_nc   = log_score_pm)
  
  gl <- agg |>
    filter(calibration_method == "COMBINED") |>
    arrange(DTE) |>
    transmute(DTE,
              brier_gl = brier_score_pm,
              log_gl   = log_score_pm)
  
  ss <- df |>
    filter(!is.na(stock), calibration_method == stock) |>
    group_by(DTE) |>
    summarise(
      brier_ss = mean(brier_score_pm, na.rm = TRUE),
      log_ss   = mean(log_score_pm,   na.rm = TRUE),
      .groups  = "drop"
    )
  
  nc |>
    left_join(gl, by = "DTE") |>
    left_join(ss, by = "DTE") |>
    mutate(model = model_label)
}

table_df <- bind_rows(
  get_cal_table(acc_np, "Nonparametric"),
  get_cal_table(acc_1l, "Single Lognormal"),
  get_cal_table(acc_3l, "Lognormal Mixture")
) |>
  mutate(model = factor(model, levels = c(
    "Nonparametric", "Single Lognormal", "Lognormal Mixture"
  )))

# Bold the minimum value in each row for each score type
fmt <- function(x) formatC(x, format = "f", digits = 4)

bold_min <- function(a, b, c) {
  vals  <- c(a, b, c)
  m     <- min(vals)
  ifelse(vals == m,
         paste0("\\textbf{", fmt(vals), "}"),
         fmt(vals))
}

table_fmt <- table_df |>
  rowwise() |>
  mutate(
    b_nc = bold_min(brier_nc, brier_gl, brier_ss)[1],
    b_gl = bold_min(brier_nc, brier_gl, brier_ss)[2],
    b_ss = bold_min(brier_nc, brier_gl, brier_ss)[3],
    l_nc = bold_min(log_nc,   log_gl,   log_ss)[1],
    l_gl = bold_min(log_nc,   log_gl,   log_ss)[2],
    l_ss = bold_min(log_nc,   log_gl,   log_ss)[3]
  ) |>
  ungroup()

# Build one data row
make_row <- function(dte, bnc, bgl, bss, lnc, lgl, lss) {
  sprintf("& %d & %s & %s & %s & %s & %s & %s \\\\",
          dte, bnc, bgl, bss, lnc, lgl, lss)
}

rows <- mapply(make_row,
               table_fmt$DTE,
               table_fmt$b_nc, table_fmt$b_gl, table_fmt$b_ss,
               table_fmt$l_nc, table_fmt$l_gl, table_fmt$l_ss)

get_block <- function(model_name) {
  rows[table_fmt$model == model_name]
}

latex_table <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Effect of PM Calibration Method on Forecast Accuracy}",
  "\\label{tab:calibration_comparison}",
  "\\small",
  "\\begin{threeparttable}",
  "\\begin{tabular}{llcccccc}",
  "\\toprule",
  "& & \\multicolumn{3}{c}{\\textbf{Brier Score}} & \\multicolumn{3}{c}{\\textbf{Log Score}} \\\\",
  "\\cmidrule(lr){3-5} \\cmidrule(lr){6-8}",
  "& DTE & NC & G & SS & NC & G & SS \\\\",
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
  "\\item \\textit{Notes}:",
  "\\end{tablenotes}",
  "\\end{threeparttable}",
  "\\end{table}"
)

writeLines(latex_table, "calibration3.0.tex")