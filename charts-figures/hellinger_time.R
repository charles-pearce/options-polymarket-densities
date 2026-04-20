library(tidyverse)
library(patchwork)

# Load density files
den_np <- read_csv("nonparam_density_result.csv")
den_1l <- read_csv("1lognorm_density_result.csv")
den_3l <- read_csv("3lognorm_density_result.csv")

# Filter NOT CALIBRATED only
nc_np <- den_np |> filter(calibration_method == "NOT CALIBRATED")
nc_1l <- den_1l |> filter(calibration_method == "NOT CALIBRATED")
nc_3l <- den_3l |> filter(calibration_method == "NOT CALIBRATED")

# Weekly median Hellinger by DTE
get_weekly <- function(df, model_label) {
  df |>
    mutate(
      Date = as.Date(Date),
      week = floor_date(Date, "week", week_start = 1)
    ) |>
    group_by(week, DTE) |>
    summarise(
      model     = model_label,
      hellinger = median(Hellinger.Distance, na.rm = TRUE),
      .groups   = "drop"
    )
}

plot_df <- bind_rows(
  get_weekly(nc_np, "Nonparametric"),
  get_weekly(nc_1l, "Single Lognormal"),
  get_weekly(nc_3l, "Lognormal Mixture")
) |>
  mutate(
    model = factor(model, levels = c(
      "Nonparametric", "Single Lognormal", "Lognormal Mixture"
    )),
    DTE = factor(DTE, levels = c(1, 2, 3, 4),
                 labels = c("DTE 1", "DTE 2", "DTE 3", "DTE 4"))
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
    axis.text.x        = element_text(angle = 45, hjust = 1, size = 7),
    axis.title         = element_text(family = "Times New Roman"),
    legend.title       = element_blank(),
    legend.key         = element_blank(),
    legend.key.width   = unit(1.2, "cm"),
    legend.text        = element_text(family = "Times New Roman"),
    legend.background  = element_blank(),
    plot.background    = element_rect(fill = "white", colour = NA),
    plot.tag           = element_text(family = "Times New Roman", face = "bold")
  )

lt_vals  <- c("DTE 1" = "solid",
              "DTE 2" = "solid",
              "DTE 3" = "solid",
              "DTE 4" = "solid")
sh_vals  <- c("DTE 1" = 16, "DTE 2" = 17, "DTE 3" = 15, "DTE 4" = 18)
col_vals <- c("DTE 1" = "#111111", "DTE 2" = "#444444",
              "DTE 3" = "#777777", "DTE 4" = "#AAAAAA")

# Build one panel per model
make_panel <- function(data, model_name, tag, show_x = FALSE, show_legend = FALSE) {
  ggplot(data |> filter(model == model_name),
         aes(x = week, y = hellinger,
             colour   = DTE, linetype = DTE,
             shape    = DTE, group    = DTE)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1.8) +
    scale_x_date(date_breaks = "4 weeks",
                 labels = if (show_x) scales::date_format("%Y-%m-%d") else function(x) rep("", length(x))) +
    scale_colour_manual(values   = col_vals) +
    scale_linetype_manual(values = lt_vals) +
    scale_shape_manual(values    = sh_vals) +
    labs(x = NULL, y = "Median Hellinger", tag = tag,
         title = model_name) +
    theme_jof +
    theme(
      plot.title      = element_text(face = "bold", size = 10,
                                     family = "Times New Roman",
                                     hjust = 0.5),
      legend.position = if (show_legend) "right" else "none",
      axis.text.x     = if (show_x) element_text(angle = 45, hjust = 1, size = 7)
      else element_blank(),
      axis.ticks.x    = if (show_x) element_line() else element_blank()
    )
}

p1 <- make_panel(plot_df, "Nonparametric",    "A", show_x = FALSE, show_legend = FALSE)
p2 <- make_panel(plot_df, "Single Lognormal", "B", show_x = FALSE, show_legend = FALSE)
p3 <- make_panel(plot_df, "Lognormal Mixture","C", show_x = TRUE,  show_legend = TRUE)

fig <- p1 / p2 / p3

ggsave("fig_hellinger_time.png", fig,
       width = 7, height = 8, dpi = 300)
