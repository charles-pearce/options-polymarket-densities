# Accuracy of uncalibrated pm vs options accross dte
library(tidyverse)
library(patchwork)

# Load data
acc_np <- read_csv("nonparam_accuracy_result.csv")
acc_1l <- read_csv("1lognorm_accuracy_result.csv")
acc_3l <- read_csv("3lognorm_accuracy_result.csv")

# Prepare tidy data — NOT CALIBRATED only
get_agg <- function(df, model_label) {
  df |>
    filter(is.na(stock), calibration_method == "NOT CALIBRATED") |>
    transmute(
      DTE,
      model     = model_label,
      brier_pm  = brier_score_pm,
      brier_opt = brier_score_options,
      log_pm    = log_score_pm,
      log_opt   = log_score_options
    )
}

raw <- bind_rows(
  get_agg(acc_np, "Nonparametric"),
  get_agg(acc_1l, "Single Lognormal"),
  get_agg(acc_3l, "Lognormal Mixture")
)

plot_df <- bind_rows(
  raw |> transmute(DTE, model,
                   series = "Options-Implied",
                   brier  = brier_opt,
                   log    = log_opt),
  raw |> transmute(DTE, model,
                   series = "Prediction Market",
                   brier  = brier_pm,
                   log    = log_pm)
) |>
  mutate(
    model  = factor(model, levels = c(
      "Nonparametric", "Single Lognormal", "Lognormal Mixture"
    )),
    series = factor(series, levels = c(
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
    legend.key.width   = unit(1.5, "cm"),
    legend.text        = element_text(family = "Times New Roman"),
    legend.background  = element_blank(),
    plot.background    = element_rect(fill = "white", colour = NA),
    plot.tag           = element_text(family = "Times New Roman", face = "bold")
  )

lt_vals <- c("Prediction Market" = "solid",  "Options-Implied" = "dashed")
sh_vals <- c("Prediction Market" = 16,        "Options-Implied" = 17)

# Panel A: Brier
p_brier <- ggplot(plot_df, aes(x = DTE, y = brier,
                               linetype = series,
                               shape    = series,
                               group    = series)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 4:1, labels = c("4","3","2","1"),
                     trans = "reverse") +
  scale_linetype_manual(values = lt_vals) +
  scale_shape_manual(values   = sh_vals) +
  facet_wrap(~model, nrow = 1) +
  labs(x = "Days Before Expiry", y = "Brier Score", tag = "A") +
  theme_jof +
  theme(legend.position = "none")

# Panel B: Log
p_log <- ggplot(plot_df, aes(x = DTE, y = log,
                             linetype = series,
                             shape    = series,
                             group    = series)) +
  geom_line(linewidth = 0.6) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 4:1, labels = c("4","3","2","1"),
                     trans = "reverse") +
  scale_linetype_manual(values = lt_vals) +
  scale_shape_manual(values   = sh_vals) +
  facet_wrap(~model, nrow = 1) +
  labs(x = "Days Before Expiry", y = "Log Score", tag = "B") +
  theme_jof +
  theme(legend.position = "bottom")

fig1 <- p_brier / p_log
ggsave("accuracy_uncalibrated3.0.png", fig1, width = 6.5, height = 6.2, dpi = 300)
