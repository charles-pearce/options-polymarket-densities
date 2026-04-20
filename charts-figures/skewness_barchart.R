library(tidyverse)
library(patchwork)

# Load density files
den_np <- read_csv("nonparam_density_result.csv")
den_1l <- read_csv("1lognorm_density_result.csv")
den_3l <- read_csv("3lognorm_density_result.csv")

# Get median skewness by DTE, NOT CALIBRATED
get_skew <- function(df, model_label) {
  df |>
    filter(calibration_method == "NOT CALIBRATED") |>
    group_by(DTE) |>
    summarise(
      model       = model_label,
      skew_opt    = median(option_skewness, na.rm = TRUE),
      skew_pm     = median(pm_skewness,     na.rm = TRUE),
      .groups     = "drop"
    )
}

skew_df <- bind_rows(
  get_skew(den_np, "Nonparametric"),
  get_skew(den_1l, "Single Lognormal"),
  get_skew(den_3l, "Lognormal Mixture")
) |>
  mutate(model = factor(model, levels = c(
    "Nonparametric", "Single Lognormal", "Lognormal Mixture"
  ))) |>
  pivot_longer(c(skew_opt, skew_pm),
               names_to  = "source",
               values_to = "skewness") |>
  mutate(
    source = if_else(source == "skew_pm",
                     "Prediction Market", "Options-Implied"),
    source = factor(source, levels = c(
      "Prediction Market", "Options-Implied"
    )),
    DTE = factor(DTE, levels = c("4", "3", "2", "1"))
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

fill_vals <- c("Prediction Market" = "grey40",
               "Options-Implied"   = "white")
col_vals  <- c("Prediction Market" = "black",
               "Options-Implied"   = "black")

fig <- ggplot(skew_df,
              aes(x     = DTE,
                  y     = skewness,
                  fill  = source,
                  colour = source,
                  group = source)) +
  geom_col(position  = position_dodge(width = 0.7),
           width      = 0.6,
           linewidth  = 0.4) +
  geom_hline(yintercept = 0, linetype = "solid",
             colour = "grey30", linewidth = 0.4) +
  scale_fill_manual(values   = fill_vals) +
  scale_colour_manual(values = col_vals) +
  scale_x_discrete(limits = c("4","3","2","1")) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  facet_wrap(~model, nrow = 1, scales = "free_y") +
  labs(
    x = "Days Before Expiry",
    y = "Median Skewness"
  ) +
  theme_jof +
  theme(legend.position = "bottom")

ggsave("fig_skewness.png", fig, width = 6.5, height = 3.5, dpi = 300)