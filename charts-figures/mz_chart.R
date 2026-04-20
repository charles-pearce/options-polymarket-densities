# MZ bar chart dtes
library(tidyverse)
library(patchwork)

# Load MZ files
mz_np <- read_csv("nonparam_mz_result.csv")
mz_1l <- read_csv("1lognorm_mz_result.csv")
mz_3l <- read_csv("3lognorm_mz_result.csv")

# Extract aggregate rows, NOT CALIBRATED only
get_mz <- function(df, model_label) {
  df |>
    filter(stock == "all",
           calibration_method == "NOT CALIBRATED",
           coefficient %in% c("intercept", "beta")) |>
    transmute(
      model       = model_label,
      DTE         = factor(DTE),
      coefficient,
      estimate
    )
}

mz_df <- bind_rows(
  get_mz(mz_np, "Nonparametric"),
  get_mz(mz_1l, "Single Lognormal"),
  get_mz(mz_3l, "Lognormal Mixture")
) |>
  mutate(model = factor(model, levels = c(
    "Nonparametric", "Single Lognormal", "Lognormal Mixture"
  )))

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
    legend.background  = element_blank(),
    plot.background    = element_rect(fill = "white", colour = NA),
    plot.tag           = element_text(family = "Times New Roman", face = "bold")
  )

# Panel A: Beta
p_beta <- mz_df |>
  filter(coefficient == "beta") |>
  ggplot(aes(x = DTE, y = estimate)) +
  geom_col(width = 0.6, fill = "black", alpha = 0.85, colour = "black",
           linewidth = 0.4) +
  geom_hline(yintercept = 1, linetype = "dashed", linewidth = 0.5,
             colour = "grey40") +
  scale_x_discrete(limits = c("4","3","2","1")) +
  scale_y_continuous(limits = c(0, 1.5), breaks = seq(0, 1.5, 0.25)) +
  facet_wrap(~model, nrow = 1) +
  labs(x = "Days Before Expiry", y = "Beta", tag = "A") +
  theme_jof +
  theme(legend.position = "none")

# Panel B: Intercept
p_intercept <- mz_df |>
  filter(coefficient == "intercept") |>
  ggplot(aes(x = DTE, y = estimate)) +
  geom_col(width = 0.6, fill = "black", alpha = 0.85, colour = "black",
           linewidth = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.5,
             colour = "grey40") +
  scale_x_discrete(limits = c("4","3","2","1")) +
  facet_wrap(~model, nrow = 1) +
  labs(x = "Days Before Expiry", y = "Intercept", tag = "B") +
  theme_jof +
  theme(legend.position = "none")

fig <- p_beta / p_intercept
ggsave("mz2.0.png", fig, width = 6.5, height = 6.2, dpi = 300)

