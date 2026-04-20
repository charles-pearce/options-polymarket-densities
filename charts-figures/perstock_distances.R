library(tidyverse)
library(patchwork)

# Load density files
den_np <- read_csv("nonparam_density_result.csv")
den_1l <- read_csv("1lognorm_density_result.csv")
den_3l <- read_csv("3lognorm_density_result.csv")

# Per-stock median Hellinger and Wasserstein, averaged over DTE
get_stock <- function(df, model_label) {
  df |>
    filter(calibration_method == "NOT CALIBRATED") |>
    group_by(Stock) |>
    summarise(
      model       = model_label,
      hellinger   = median(Hellinger.Distance,   na.rm = TRUE),
      wasserstein = median(Wasserstein.Distance,  na.rm = TRUE),
      .groups     = "drop"
    ) |>
    rename(stock = Stock)
}

plot_df <- bind_rows(
  get_stock(den_np, "Nonparametric"),
  get_stock(den_1l, "Single Lognormal"),
  get_stock(den_3l, "Lognormal Mixture")
) |>
  mutate(
    model = factor(model, levels = c(
      "Nonparametric", "Single Lognormal", "Lognormal Mixture"
    )),
    stock = factor(stock, levels = c(
      "AAPL","AMZN","GOOGL","META","MSFT","NFLX","NVDA","OPEN","PLTR","TSLA"
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

# Panel A: Hellinger
p_hell <- plot_df |>
  ggplot(aes(x = stock, y = hellinger)) +
  geom_col(width = 0.6, fill = "grey40", colour = "black",
           linewidth = 0.4) +
  scale_y_continuous(limits = c(0, 1),
                     breaks = seq(0, 1, 0.25)) +
  facet_wrap(~model, nrow = 1) +
  labs(x = NULL, y = "Median Hellinger Distance", tag = "A") +
  theme_jof +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "none"
  )

# Panel B: Wasserstein
p_wass <- plot_df |>
  ggplot(aes(x = stock, y = wasserstein)) +
  geom_col(width = 0.6, fill = "grey40", colour = "black",
           linewidth = 0.4) +
  scale_y_continuous(limits = c(0, NA),
                     labels = scales::number_format(accuracy = 0.001)) +
  facet_wrap(~model, nrow = 1, scales = "free_y") +
  labs(x = NULL, y = "Median Wasserstein Distance", tag = "B") +
  theme_jof +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "none"
  )

fig <- p_hell / p_wass
ggsave("perstock_distances.png", fig,
       width = 6.5, height = 6.2, dpi = 300)