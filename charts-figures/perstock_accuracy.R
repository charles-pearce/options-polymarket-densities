# Bar chart for the per stock accuracy scores
library(tidyverse)
library(patchwork)

# Load data
acc_np <- read_csv("nonparam_accuracy_result.csv")
acc_1l <- read_csv("1lognorm_accuracy_result.csv")
acc_3l <- read_csv("3lognorm_accuracy_result.csv")

# Per-stock rows, NOT CALIBRATED, averaged over DTE
get_stock <- function(df, model_label) {
  df |>
    filter(!is.na(stock), calibration_method == "NOT CALIBRATED") |>
    group_by(stock) |>
    summarise(
      model     = model_label,
      brier_pm  = mean(brier_score_pm,      na.rm = TRUE),
      brier_opt = mean(brier_score_options,  na.rm = TRUE),
      log_pm    = mean(log_score_pm,         na.rm = TRUE),
      log_opt   = mean(log_score_options,    na.rm = TRUE),
      .groups   = "drop"
    )
}

plot_df <- bind_rows(
  get_stock(acc_np, "Nonparametric"),
  get_stock(acc_1l, "Single Lognormal"),
  get_stock(acc_3l, "Lognormal Mixture")
) |>
  mutate(
    model = factor(model, levels = c(
      "Nonparametric", "Single Lognormal", "Lognormal Mixture"
    )),
    stock = factor(stock, levels = c(
      "AAPL","AMZN","GOOGL","META","MSFT","NFLX","NVDA","OPEN","PLTR","TSLA"
    ))
  ) |>
  pivot_longer(c(brier_pm, brier_opt, log_pm, log_opt),
               names_to  = "series",
               values_to = "score") |>
  mutate(
    score_type = if_else(str_starts(series, "brier"),
                         "Brier Score", "Log Score"),
    source     = if_else(str_ends(series, "pm"),
                         "Prediction Market", "Options-Implied"),
    source     = factor(source, levels = c(
      "Prediction Market", "Options-Implied"
    ))
  )

# JoF theme
theme_jof <- theme_classic(base_size = 10, base_family = "Times New Roman") +
  theme(
    strip.background   = element_blank(),
    strip.text         = element_text(face = "bold", size = 10,
                                      family = "Times New Roman"),
    panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
    axis.ticks.length  = unit(-3, "pt"),
    axis.text          = element_text(family = "Times New Roman"),
    axis.title         = element_text(family = "Times New Roman"),
    legend.title       = element_blank(),
    legend.key         = element_blank(),
    legend.key.width   = unit(1.2, "cm"),
    legend.background  = element_blank(),
    plot.background    = element_rect(fill = "white", colour = NA),
    plot.tag           = element_text(family = "Times New Roman", face = "bold")
  )

fill_vals <- c("Prediction Market" = "black",
               "Options-Implied"   = "white")
col_vals  <- c("Prediction Market" = "black",
               "Options-Implied"   = "black")

# Panel A: Brier score
p_brier <- plot_df |>
  filter(score_type == "Brier Score") |>
  ggplot(aes(x = stock, y = score,
             fill = source, colour = source, group = source)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6,
           linewidth = 0.4) +
  scale_fill_manual(values   = fill_vals) +
  scale_colour_manual(values = col_vals) +
  facet_wrap(~model, nrow = 1) +
  labs(x = NULL, y = "Brier Score", tag = "A") +
  theme_jof +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "none"
  )

# Panel B: Log score
p_log <- plot_df |>
  filter(score_type == "Log Score") |>
  ggplot(aes(x = stock, y = score,
             fill = source, colour = source, group = source)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6,
           linewidth = 0.4) +
  scale_fill_manual(values   = fill_vals) +
  scale_colour_manual(values = col_vals) +
  facet_wrap(~model, nrow = 1) +
  labs(x = NULL, y = "Log Score", tag = "B") +
  theme_jof +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "bottom"
  )

fig <- p_brier / p_log
ggsave("perstock_accuracy3.0.png", fig,
       width = 6.5, height = 6.2, dpi = 300)
