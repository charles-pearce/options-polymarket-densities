library(tidyverse)
library(patchwork)

acc_np <- read_csv("nonparam_daily_accuracy_result.csv")
acc_1l <- read_csv("1lognorm_daily_accuracy_result.csv")
acc_3l <- read_csv("3lognorm_daily_accuracy_result.csv")

get_series <- function(df, model_label) {
  df |>
    filter(calibration_method == "NOT CALIBRATED") |>
    mutate(Date = as.Date(Date)) |>
    group_by(Date, DTE) |>
    summarise(
      model     = model_label,
      brier_pm  = mean(mean_brier_score_pm,    na.rm = TRUE),
      brier_opt = mean(mean_brier_score_option, na.rm = TRUE),
      .groups   = "drop"
    ) |>
    arrange(Date, desc(DTE)) |>
    mutate(t = row_number())
}

plot_df <- bind_rows(
  get_series(acc_np, "Nonparametric"),
  get_series(acc_1l, "Single Lognormal"),
  get_series(acc_3l, "Lognormal Mixture")
) |>
  mutate(model = factor(model, levels = c(
    "Nonparametric", "Single Lognormal", "Lognormal Mixture"
  )))

week_labels <- plot_df |>
  filter(model == "Nonparametric", DTE == 4) |>
  mutate(label = format(Date, "%d %b")) |>
  select(t, label)

plot_long <- plot_df |>
  pivot_longer(c(brier_pm, brier_opt),
               names_to  = "source",
               values_to = "brier") |>
  mutate(
    source = if_else(source == "brier_pm",
                     "Prediction Market", "Options-Implied"),
    source = factor(source, levels = c(
      "Prediction Market", "Options-Implied"
    ))
  )

theme_jof <- theme_classic(base_size = 10, base_family = "Times New Roman") +
  theme(
    strip.background   = element_blank(),
    strip.text         = element_text(face = "bold", size = 10,
                                      family = "Times New Roman"),
    panel.grid.major.y = element_line(colour = "grey90", linewidth = 0.3),
    panel.grid.major.x = element_line(colour = "grey95", linewidth = 0.3),
    axis.ticks.length  = unit(-3, "pt"),
    axis.text          = element_text(family = "Times New Roman"),
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 7),
    axis.title         = element_text(family = "Times New Roman"),
    legend.title       = element_blank(),
    legend.key         = element_blank(),
    legend.key.width   = unit(1.5, "cm"),
    legend.text        = element_text(family = "Times New Roman"),
    legend.background  = element_blank(),
    plot.background    = element_rect(fill = "white", colour = NA),
    plot.tag           = element_text(family = "Times New Roman", face = "bold")
  )

col_vals <- c("Prediction Market" = "#111111",
              "Options-Implied"   = "#999999")

fig <- ggplot(plot_long, aes(x     = t,
                             y     = brier,
                             colour = source,
                             group  = source)) +
  geom_line(linewidth = 0.5) +
  geom_vline(
    data      = week_labels,
    aes(xintercept = t),
    colour    = "grey80",
    linewidth = 0.3,
    linetype  = "dotted"
  ) +
  scale_x_continuous(
    breaks = week_labels$t,
    labels = week_labels$label
  ) +
  scale_y_continuous(
    limits = c(0, 0.1),
    breaks = seq(0, 0.1, 0.05)
  ) +
  scale_colour_manual(values = col_vals) +
  facet_wrap(~model, nrow = 3) +
  labs(x = NULL, y = "Brier Score") +
  theme_jof +
  theme(legend.position = "bottom")

ggsave("fig_ts_brier.png", fig, width = 6.5, height = 7.5, dpi = 300)