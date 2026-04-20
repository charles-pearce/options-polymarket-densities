library(tidyverse)
library(patchwork)

# ── Load data ─────────────────────────────────────────────────────────────────
acc_np <- read_csv("nonparam_daily_accuracy_result.csv")
acc_1l <- read_csv("1lognorm_daily_accuracy_result.csv")
acc_3l <- read_csv("3lognorm_daily_accuracy_result.csv")

# ── Earnings info ──────────────────────────────────────────────────────────────
# after_close = TRUE means the earnings affect the NEXT expiry week
earnings_info <- tribble(
  ~stock,  ~earnings_date,       ~after_close,  ~label,
  "META",  as.Date("2026-01-27"), FALSE,         "META Earnings\n27 Jan",
  "MSFT",  as.Date("2026-01-27"), TRUE,          "MSFT Earnings\n27 Jan (AC)",
  "TSLA",  as.Date("2026-01-28"), FALSE,         "TSLA Earnings\n28 Jan",
  "AAPL",  as.Date("2026-01-29"), TRUE,          "AAPL Earnings\n29 Jan (AC)",
  "AMZN",  as.Date("2026-02-05"), FALSE,         "AMZN Earnings\n5 Feb",
  "GOOGL", as.Date("2026-02-04"), FALSE,         "GOOGL Earnings\n4 Feb",
  "PLTR",  as.Date("2026-02-02"), FALSE,         "PLTR Earnings\n2 Feb",
  "NFLX",  as.Date("2026-01-20"), FALSE,         "NFLX Earnings\n20 Jan"
)

# ── Helper: get stock data ────────────────────────────────────────────────────
get_stock <- function(df, stock_name, model_label) {
  df |>
    filter(stock == stock_name, calibration_method == "NOT CALIBRATED") |>
    transmute(
      model   = model_label,
      date    = as.Date(Date),
      DTE     = DTE,
      pm      = mean_brier_score_pm,
      options = mean_brier_score_option
    ) |>
    arrange(date, desc(DTE))
}

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
    legend.key.width   = unit(1.2, "cm"),
    legend.background  = element_blank(),
    plot.background    = element_rect(fill = "white", colour = NA),
    plot.tag           = element_text(family = "Times New Roman", face = "bold")
  )

col_vals   <- c("Prediction Market" = "#111111",
                "Options-Implied"   = "#999999")
lt_vals    <- c("Prediction Market" = "solid",
                "Options-Implied"   = "dashed")
shape_vals <- c("Prediction Market" = 16,
                "Options-Implied"   = 17)

to_long <- function(df) {
  df |>
    pivot_longer(c(pm, options),
                 names_to  = "source",
                 values_to = "brier") |>
    mutate(source = if_else(source == "pm",
                            "Prediction Market", "Options-Implied"),
           source = factor(source, levels = c(
             "Prediction Market", "Options-Implied"
           )))
}

# ── Function: find expiry week containing a given date ───────────────────────
# Returns the Friday expiry date whose week contains the given date
find_expiry_week <- function(date, available_dates) {
  # For after-close earnings, the effect shows in the NEXT week
  # so we look for the next Friday expiry after the earnings date
  fridays <- sort(unique(available_dates))
  # find the first Friday expiry >= the earnings date
  idx <- which(fridays >= date)
  if (length(idx) == 0) return(NA)
  fridays[idx[1]]
}

# ── Main loop ─────────────────────────────────────────────────────────────────
for (i in seq_len(nrow(earnings_info))) {
  
  stock_name    <- earnings_info$stock[i]
  earn_date     <- earnings_info$earnings_date[i]
  after_close   <- earnings_info$after_close[i]
  earn_label    <- earnings_info$label[i]
  
  # Build combined data for this stock
  stock_df <- bind_rows(
    get_stock(acc_np, stock_name, "Nonparametric"),
    get_stock(acc_1l, stock_name, "Single Lognormal"),
    get_stock(acc_3l, stock_name, "Lognormal Mixture")
  ) |>
    mutate(model = factor(model, levels = c(
      "Nonparametric", "Single Lognormal", "Lognormal Mixture"
    )))
  
  if (nrow(stock_df) == 0) next
  
  # Add consecutive obs index within each model
  stock_df <- stock_df |>
    group_by(model) |>
    mutate(obs = row_number()) |>
    ungroup()
  
  # Find available Friday expiry dates
  available_expiries <- stock_df |>
    filter(DTE == 1) |>
    pull(date) |>
    unique() |>
    sort()
  
  # Determine which expiry week the earnings fall in
  # After close: effect in NEXT expiry week
  earn_expiry <- find_expiry_week(
    if (after_close) earn_date + 1 else earn_date,
    available_expiries
  )
  
  # Earnings week data for zoom panel
  earn_week_df <- stock_df |>
    filter(date == earn_expiry)
  
  # x-axis ticks: show expiry date at DTE 1
  tick_df <- stock_df |>
    filter(model == "Nonparametric", DTE == 1) |>
    select(obs, date)
  
  # Convert earnings date to x position (obs index)
  # Find obs values for earnings week
  earn_obs_range <- stock_df |>
    filter(model == "Nonparametric", date == earn_expiry) |>
    pull(obs)
  
  full_long <- to_long(stock_df)
  
  # ── Make time series panel ────────────────────────────────────────────────
  make_ts <- function(model_name, tag, show_legend = FALSE) {
    
    df  <- full_long |> filter(model == model_name)
    
    # Earnings vertical line x position: find obs closest to earnings date
    earn_obs_model <- stock_df |>
      filter(model == model_name, date == earn_expiry) |>
      arrange(desc(DTE)) |>
      pull(obs)
    
    # Shade the earnings week
    x_shade_min <- if (length(earn_obs_model) > 0)
      min(earn_obs_model) - 0.5 else NA
    x_shade_max <- if (length(earn_obs_model) > 0)
      max(earn_obs_model) + 0.5 else NA
    
    # Earnings date vertical line position:
    # find obs of the DTE that straddles the earnings date within that week
    earn_dte_row <- stock_df |>
      filter(model == model_name, date == earn_expiry) |>
      mutate(dist = abs(as.numeric(date - DTE) - as.numeric(earn_date))) |>
      arrange(dist)
    
    # Simpler: draw vertical line between the obs just before and after earnings
    # Use earnings date mapped to obs scale:
    # earnings is between DTE obs in the week
    earn_x <- stock_df |>
      filter(model == model_name, date == earn_expiry) |>
      arrange(desc(DTE)) |>
      mutate(obs_date = date - (DTE - 1)) |>  # approx calendar date of each obs
      mutate(dist = abs(as.numeric(obs_date - earn_date))) |>
      slice_min(dist, n = 1) |>
      pull(obs)
    
    p <- ggplot(df, aes(x        = obs,
                        y        = brier,
                        colour   = source,
                        linetype = source,
                        shape    = source,
                        group    = source)) +
      # Shade earnings week
      {if (!is.na(x_shade_min))
        annotate("rect",
                 xmin  = x_shade_min, xmax = x_shade_max,
                 ymin  = -Inf,        ymax = Inf,
                 fill  = "grey85",    alpha = 0.5)} +
      geom_line(linewidth = 0.5) +
      geom_point(size = 1.2) +
      # Earnings date vertical line
      {if (length(earn_x) > 0)
        geom_vline(xintercept = earn_x,
                   linetype   = "dotted",
                   colour     = "grey30",
                   linewidth  = 0.6)} +
      {if (length(earn_x) > 0)
        annotate("text",
                 x      = earn_x,
                 y      = Inf,
                 label  = earn_label,
                 vjust  = 1.3,
                 hjust  = -0.05,
                 size   = 2.2,
                 family = "Times New Roman",
                 colour = "grey30")} +
      scale_colour_manual(values   = col_vals) +
      scale_linetype_manual(values = lt_vals) +
      scale_shape_manual(values    = shape_vals) +
      scale_x_continuous(
        breaks = tick_df$obs,
        labels = format(tick_df$date, "%d %b")
      ) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
      labs(x     = NULL,
           y     = "Brier Score",
           tag   = tag,
           title = model_name) +
      theme_jof +
      theme(
        plot.title      = element_text(face  = "bold", size = 10,
                                       hjust = 0.5,
                                       family = "Times New Roman"),
        axis.text.x     = element_text(angle = 45, hjust = 1, size = 7),
        legend.position = if (show_legend) "bottom" else "none"
      )
    p
  }
  
  # ── Make earnings zoom panel ──────────────────────────────────────────────
  make_earn <- function(model_name, tag) {
    
    df <- earn_week_df |>
      filter(model == model_name) |>
      arrange(desc(DTE))
    
    if (nrow(df) == 0) return(ggplot() + theme_void())
    
    df_long <- to_long(df)
    dte_levels <- as.character(sort(unique(df$DTE), decreasing = TRUE))
    
    ggplot(df_long,
           aes(x        = factor(DTE, levels = dte_levels),
               y        = brier,
               colour   = source,
               linetype = source,
               shape    = source,
               group    = source)) +
      geom_line(linewidth = 0.5) +
      geom_point(size = 1.8) +
      # Mark the earnings day within this week
      {
        # Which DTE corresponds to the earnings date?
        earn_dte <- df |>
          mutate(obs_date = date - (DTE - 1)) |>
          mutate(dist = abs(as.numeric(obs_date - earn_date))) |>
          slice_min(dist, n = 1) |>
          pull(DTE)
        if (length(earn_dte) > 0)
          geom_vline(xintercept = as.character(earn_dte),
                     linetype   = "dotted",
                     colour     = "grey30",
                     linewidth  = 0.6)
      } +
      scale_colour_manual(values   = col_vals) +
      scale_linetype_manual(values = lt_vals) +
      scale_shape_manual(values    = shape_vals) +
      scale_y_continuous(labels = scales::number_format(accuracy = 0.001)) +
      labs(x     = "DTE",
           y     = NULL,
           tag   = tag,
           title = paste0("Earnings Week\n(",
                          format(earn_date, "%d %b %Y"), ")")) +
      theme_jof +
      theme(
        plot.title      = element_text(face  = "bold", size = 9,
                                       hjust = 0.5,
                                       family = "Times New Roman"),
        legend.position = "none"
      )
  }
  
  # ── Assemble figure ───────────────────────────────────────────────────────
  tags <- list(
    list(ts = "A", earn = "B"),
    list(ts = "C", earn = "D"),
    list(ts = "E", earn = "F")
  )
  
  models <- c("Nonparametric", "Single Lognormal", "Lognormal Mixture")
  
  rows <- lapply(seq_along(models), function(j) {
    show_leg <- j == length(models)
    ts_p   <- make_ts(models[j],   tags[[j]]$ts,   show_legend = show_leg)
    earn_p <- make_earn(models[j], tags[[j]]$earn)
    ts_p + earn_p + plot_layout(widths = c(3, 1))
  })
  
  fig <- rows[[1]] / rows[[2]] / rows[[3]] +
    plot_annotation(
      title = paste0(stock_name, " — Brier Score Around Earnings"),
      theme = theme(
        plot.title = element_text(family = "Times New Roman",
                                  face   = "bold",
                                  size   = 12,
                                  hjust  = 0.5)
      )
    )
  
  fname <- paste0("earnings_", tolower(stock_name), ".png")
  ggsave(fname, fig, width = 8, height = 9, dpi = 300)
  cat("Saved:", fname, "\n")
}