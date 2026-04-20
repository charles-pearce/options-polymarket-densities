library(tidyverse)
library(patchwork)

den_np <- read_csv("nonparam_density_result.csv")
den_1l <- read_csv("1lognorm_density_result.csv")
den_3l <- read_csv("3lognorm_density_result.csv")

get_stock <- function(df, model_label) {
  df |>
    filter(calibration_method == "NOT CALIBRATED") |>
    transmute(
      model       = model_label,
      stock       = Stock,
      hellinger   = Hellinger.Distance,
      wasserstein = Wasserstein.Distance
    )
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

# Panel A: Hellinger
p_hell <- plot_df |>
  ggplot(aes(x = stock, y = hellinger)) +
  geom_boxplot(
    fill          = "grey40",
    colour        = "black",
    linewidth     = 0.4,
    outlier.size  = 0.8,
    outlier.shape = 16,
    outlier.alpha = 0.5,
    width         = 0.6
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.25)
  ) +
  facet_wrap(~model, nrow = 1) +
  labs(x = NULL, y = "Hellinger Distance", tag = "A") +
  theme_jof +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "none"
  )

# Panel B: Wasserstein — log scale to handle OPEN outlier
# Add a small constant to avoid log(0) issues
p_wass <- plot_df |>
  mutate(wasserstein = wasserstein + 1e-6) |>
  ggplot(aes(x = stock, y = wasserstein)) +
  geom_boxplot(
    fill          = "grey40",
    colour        = "black",
    linewidth     = 0.4,
    outlier.size  = 0.8,
    outlier.shape = 16,
    outlier.alpha = 0.5,
    width         = 0.6
  ) +
  scale_y_log10(
    labels = scales::label_number(accuracy = 0.0001)
  ) +
  facet_wrap(~model, nrow = 1) +
  labs(x = NULL, y = "Wasserstein Distance (log scale)", tag = "B") +
  theme_jof +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 8),
    legend.position = "none"
  )

fig <- p_hell / p_wass
ggsave("perstock_box_log.png", fig,
       width = 6.5, height = 6.2, dpi = 300)