library(tidyverse)

den_np <- read_csv("1776526643675_nonparam_density_result.csv")
den_1l <- read_csv("1776526643673_1lognorm_density_result.csv")
den_3l <- read_csv("1776526643676_3lognorm_density_result.csv")

nc_np <- den_np |> filter(calibration_method == "NOT CALIBRATED")
nc_1l <- den_1l |> filter(calibration_method == "NOT CALIBRATED")
nc_3l <- den_3l |> filter(calibration_method == "NOT CALIBRATED")

den_df <- bind_rows(
  nc_np |> transmute(model = "Nonparametric",     DTE = factor(DTE), wasserstein = Wasserstein.Distance),
  nc_1l |> transmute(model = "Single Lognormal",  DTE = factor(DTE), wasserstein = Wasserstein.Distance),
  nc_3l |> transmute(model = "Lognormal Mixture", DTE = factor(DTE), wasserstein = Wasserstein.Distance)
) |>
  mutate(model = factor(model, levels = c(
    "Nonparametric", "Single Lognormal", "Lognormal Mixture"
  )))

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

fig <- ggplot(den_df, aes(x = DTE, y = wasserstein)) +
  geom_boxplot(
    fill          = "grey40",
    colour        = "black",
    linewidth     = 0.4,
    outlier.shape = NA,
    width         = 0.6
  ) +
  scale_x_discrete(limits = c("4","3","2","1")) +
  scale_y_continuous(
    limits = c(0, 0.005),
    breaks = seq(0, 0.005, 0.001),
    labels = scales::number_format(accuracy = 0.001)
  ) +
  facet_wrap(~model, nrow = 1) +
  labs(x = "Days Before Expiry", y = "Wasserstein Distance") +
  theme_jof +
  theme(legend.position = "none")

ggsave("fig_wasserstein.png", fig, width = 6.5, height = 3.5, dpi = 300)
