library(tidyverse)
library(patchwork)

# ── Parameters ────────────────────────────────────────────────────────────────
stock_name  <- "NVDA"
target_date <- "2025-12-05"
spot_close  <- 182.40013122558594

# ── Load moments ──────────────────────────────────────────────────────────────
den_1l <- read_csv("1lognorm_density_result.csv")

params <- den_1l |>
  filter(Stock              == stock_name,
         Date               == target_date,
         calibration_method == "NOT CALIBRATED") |>
  select(DTE, option_mean, option_variance, pm_mean, pm_variance,
         Hellinger.Distance) |>
  arrange(desc(DTE))

# ── Build lognormal curve from mean and variance ───────────────────────────────
make_curve <- function(mean_val, var_val, source_label, dte_val,
                       x_min, x_max, n = 2000) {
  sd_val <- sqrt(max(var_val, 1e-6))
  sigma2 <- log(1 + (sd_val / mean_val)^2)
  mu     <- log(mean_val) - sigma2 / 2
  sigma  <- sqrt(sigma2)
  x      <- seq(x_min, x_max, length.out = n)
  y      <- dlnorm(x, meanlog = mu, sdlog = sigma)
  tibble(x = x, y = y, source = source_label, DTE = dte_val)
}

x_min <- 160; x_max <- 210

all_df <- pmap_dfr(params, function(DTE, option_mean, option_variance,
                                    pm_mean, pm_variance,
                                    Hellinger.Distance) {
  bind_rows(
    make_curve(option_mean, option_variance, "Options-Implied",   DTE, x_min, x_max),
    make_curve(pm_mean,     pm_variance,     "Prediction Market", DTE, x_min, x_max)
  )
}) |>
  mutate(
    source = factor(source, levels = c("Prediction Market", "Options-Implied")),
    DTE    = factor(DTE,    levels = c(4, 3, 2, 1))
  )

# Hellinger labels for each panel
hell_labels <- params |>
  mutate(label = paste0("Hellinger = ", round(Hellinger.Distance, 3))) |>
  select(DTE, label) |>
  mutate(DTE = factor(DTE, levels = c(4, 3, 2, 1)))

# ── JoF theme ─────────────────────────────────────────────────────────────────
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

col_vals <- c("Prediction Market" = "#111111",
              "Options-Implied"   = "#999999")
lt_vals  <- c("Prediction Market" = "solid",
              "Options-Implied"   = "dashed")

y_max   <- max(all_df$y, na.rm = TRUE)
y_limit <- y_max * 1.20

# ── Make one panel ────────────────────────────────────────────────────────────
make_panel <- function(dte_val, tag, show_x = FALSE, show_legend = FALSE) {
  
  df_sub <- all_df     |> filter(DTE == dte_val)
  hl_lab <- hell_labels |> filter(DTE == dte_val) |> pull(label)
  
  ggplot(df_sub, aes(x = x, y = y,
                     colour   = source,
                     linetype = source,
                     group    = source)) +
    geom_vline(xintercept = spot_close,
               linetype   = "dotted",
               colour     = "grey50",
               linewidth  = 0.5) +
    annotate("text",
             x      = spot_close + 0.5,
             y      = y_limit,
             label  = "Week Close",
             hjust  = 1.1, vjust = 1.2,
             size   = 2.5,
             family = "Times New Roman",
             colour = "grey50") +
    geom_line(linewidth = 0.7) +
    annotate("text",
             x      = x_max,
             y      = y_limit,
             label  = hl_lab,
             hjust  = 1, vjust = 1.2,
             size   = 2.5,
             family = "Times New Roman",
             colour = "grey30") +
    scale_colour_manual(values   = col_vals) +
    scale_linetype_manual(values = lt_vals) +
    scale_x_continuous(
      limits = c(x_min, x_max),
      breaks = seq(160, 210, 10),
      labels = if (show_x) as.character(seq(160, 210, 10))
      else function(x) rep("", length(x))
    ) +
    scale_y_continuous(
      limits = c(0, y_limit),
      labels = scales::number_format(accuracy = 0.001)
    ) +
    labs(
      x     = if (show_x) "NVDA Stock Price (USD)" else NULL,
      y     = "Density",
      tag   = tag,
      title = paste0("DTE ", dte_val)
    ) +
    theme_jof +
    theme(
      plot.title      = element_text(face = "bold", size = 10,
                                     hjust = 0.5,
                                     family = "Times New Roman"),
      legend.position = if (show_legend) "bottom" else "none",
      axis.text.x     = if (show_x) element_text(size = 8)
      else element_blank(),
      axis.ticks.x    = if (show_x) element_line() else element_blank()
    )
}

p4 <- make_panel(4, "A", show_x = FALSE, show_legend = FALSE)
p3 <- make_panel(3, "B", show_x = FALSE, show_legend = FALSE)
p2 <- make_panel(2, "C", show_x = TRUE,  show_legend = TRUE)
p1 <- make_panel(1, "D", show_x = TRUE,  show_legend = FALSE)

fig <- (p4 | p3) / (p2 | p1)

ggsave("fig_density_convergence.png", fig,
       width = 6.5, height = 6.2, dpi = 300)
