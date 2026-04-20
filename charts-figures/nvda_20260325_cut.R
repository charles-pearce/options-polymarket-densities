library(tidyverse)
library(patchwork)

pdf_df <- read_csv("nvda20260325.csv")

np_df <- pdf_df |>
  filter(!is.na(x_nonparam)) |>
  transmute(x = x_nonparam, model = "Nonparametric") |>
  bind_cols(pdf_df |> filter(!is.na(x_nonparam)) |>
              select(y_options = y_options_nonparam, y_pm = y_pm_nonparam)) |>
  pivot_longer(c(y_options, y_pm), names_to = "source", values_to = "y") |>
  mutate(source = if_else(source == "y_pm", "Prediction Market", "Options-Implied"))

sl_df <- pdf_df |>
  filter(!is.na(x_1log)) |>
  transmute(x = x_1log, model = "Single Lognormal") |>
  bind_cols(pdf_df |> filter(!is.na(x_1log)) |>
              select(y_options = y_options_1log, y_pm = y_pm_1log)) |>
  pivot_longer(c(y_options, y_pm), names_to = "source", values_to = "y") |>
  mutate(source = if_else(source == "y_pm", "Prediction Market", "Options-Implied"))

lm_df <- pdf_df |>
  filter(!is.na(x_3log)) |>
  transmute(x = x_3log, model = "Lognormal Mixture") |>
  bind_cols(pdf_df |> filter(!is.na(x_3log)) |>
              select(y_options = y_options_3log, y_pm = y_pm_3log)) |>
  pivot_longer(c(y_options, y_pm), names_to = "source", values_to = "y") |>
  mutate(source = if_else(source == "y_pm", "Prediction Market", "Options-Implied"))

x_min <- 130; x_max <- 225

all_df <- bind_rows(np_df, sl_df, lm_df) |>
  filter(x >= x_min, x <= x_max) |>
  mutate(
    model  = factor(model, levels = c(
      "Nonparametric", "Single Lognormal", "Lognormal Mixture"
    )),
    source = factor(source, levels = c(
      "Prediction Market", "Options-Implied"
    ))
  )

spot     <- 178.67999267578125
pm_lower <- 155
pm_upper <- 200

# Shared y limit — exclude the spike from the scale calculation
# Spike is the degenerate mixture component near 155
y_global_max <- all_df |>
  filter(!(model == "Lognormal Mixture" & y > 0.2)) |>
  pull(y) |> max(na.rm = TRUE)
y_limit <- y_global_max * 1.15

# Spike value for annotation
spike_val <- all_df |>
  filter(model == "Lognormal Mixture") |>
  pull(y) |> max(na.rm = TRUE)

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
    plot.tag           = element_text(family = "Times New Roman", face = "bold"),
    plot.margin        = margin(8, 8, 2, 8)
  )

col_vals <- c("Prediction Market" = "#111111",
              "Options-Implied"   = "#999999")
lt_vals  <- c("Prediction Market" = "solid",
              "Options-Implied"   = "dashed")

make_panel <- function(model_name, tag, show_x = FALSE, show_legend = FALSE,
                       add_spike_note = FALSE) {
  
  df_sub <- all_df |> filter(model == model_name)
  
  p <- ggplot(df_sub, aes(x = x, y = y,
                          colour   = source,
                          linetype = source,
                          group    = source)) +
    annotate("rect", xmin = pm_lower, xmax = pm_upper,
             ymin = -Inf, ymax = Inf, fill = "grey90", alpha = 0.5) +
    geom_line(linewidth = 0.7) +
    geom_vline(xintercept = spot,     linetype = "solid",
               colour = "grey30", linewidth = 0.5) +
    geom_vline(xintercept = pm_lower, linetype = "dotted",
               colour = "grey30", linewidth = 0.5) +
    geom_vline(xintercept = pm_upper, linetype = "dotted",
               colour = "grey30", linewidth = 0.5) +
    annotate("text", x = spot + 2, y = y_global_max, vjust = -0.3, hjust = 0,
             label = "Spot", size = 2.5, family = "Times New Roman",
             colour = "grey30") +
    annotate("text", x = pm_upper, y = y_global_max, vjust = -0.3, hjust = -1.5,
             label = "200", size = 2.5, family = "Times New Roman",
             colour = "grey30") +
    annotate("text", x = pm_lower, y = y_global_max, vjust = -0.3, hjust = 1.5,
             label = "155", size = 2.5, family = "Times New Roman",
             colour = "grey30") +
    scale_colour_manual(values   = col_vals) +
    scale_linetype_manual(values = lt_vals) +
    scale_x_continuous(
      limits = c(x_min, x_max),
      breaks = seq(130, 225, 15),
      labels = if (show_x) as.character(seq(130, 225, 15))
      else function(x) rep("", length(x))
    ) +
    # Clip the y axis — spike will be cut off with an arrow indicating it
    coord_cartesian(ylim = c(0, y_limit), clip = "on") +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
    labs(
      x     = if (show_x) "NVDA Stock Price (USD)" else NULL,
      y     = "Density",
      tag   = tag,
      title = model_name
    ) +
    theme_jof +
    theme(
      plot.title      = element_text(face = "bold", size = 10,
                                     family = "Times New Roman", hjust = 0.5),
      legend.position = if (show_legend) "bottom" else "none",
      axis.text.x     = if (show_x) element_text(size = 8,
                                                 family = "Times New Roman")
      else element_blank(),
      axis.ticks.x    = if (show_x) element_line() else element_blank()
    )
}

p1 <- make_panel("Nonparametric",    "A", show_x = FALSE, show_legend = FALSE)
p2 <- make_panel("Single Lognormal", "B", show_x = FALSE, show_legend = FALSE)
p3 <- make_panel("Lognormal Mixture","C", show_x = TRUE,  show_legend = TRUE)

fig <- p1 / p2 / p3

ggsave("density_nvda_25032026_cut.png", fig,
       width = 5, height = 9, dpi = 300)