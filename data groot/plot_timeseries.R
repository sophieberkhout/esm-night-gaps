dat <- read.csv("data groot/data/data_uncorrected.csv")

library("ggplot2")
library("dplyr")
library("patchwork")
source("data groot/utils.R")

plotTimeSeries <- function(dat, items, names, y_breaks,
                           filename, width, height, plot_widths = c(3, 7, 1)) {

  dat_plot <- dat[, c("datetime", "date", items)]
  
  dat_long <- tidyr::pivot_longer(dat_plot, 3:ncol(dat_plot))
  
  dat_long$name <- factor(dat_long$name, levels = items)
  levels(dat_long$name) <- names
  
  dat_long$date <- as.POSIXct(dat_long$date, tz = "UTC")
  dat_long$datetime <- as.POSIXct(dat_long$datetime, tz = "UTC")
  
  last_date_plus_one <- as.POSIXct(lubridate::ymd(max(dat_long$date)) + 1,
                                   tz = "UTC")
  date_long <- c(dat_long$date, last_date_plus_one)
  
  x_breaks <- pretty(date_long)
  x_labels <- attr(x_breaks, "labels") # english?
  
  y_lim <- c(min(y_breaks), max(y_breaks))
  tick_labels <- y_breaks
  tick_labels[seq(0, length(tick_labels), 2)] <- " "
  axis_line_width <- 0.3
  
  p_ts <- ggplot(dat_long) + 
    geom_vline(xintercept = date_long,
               alpha = 0.1, linewidth = axis_line_width) +
    geom_line(aes(x = datetime, y = value, group = date)) +
    geom_point(aes(x = datetime, y = value), size = 0.5) +
    scale_y_continuous(breaks = y_breaks, labels = tick_labels) +
    coord_cartesian(ylim = y_lim) +
    scale_x_datetime(expand = c(0.01, 0.01)) +
    facet_wrap(~ name, ncol = 1) +
    theme_void() +
    labs(x = "All Data (Phase 4 & 5)", y = "") + 
    theme(text = element_text(family = "sans", size = 16),
          axis.text.x = element_text(margin = margin(5, 5, 5, 5)),
          axis.title = element_text(margin = margin(5, 5, 5, 5)),
          axis.ticks = element_line(lineend = "butt",
                                    linewidth = axis_line_width),
          axis.ticks.length = unit(2.5, "pt"),
          strip.text = element_text(margin = margin(5, 5, 5, 5), size = 16),
          panel.spacing = unit(5, units = "pt"),
          plot.margin = margin(0, 5, 0, 0),
          legend.position = "none") +
    geom_segment(x = -Inf, xend = -Inf, y = y_breaks[1], yend = 7,
                 linewidth = axis_line_width, lineend = "square") +
    geom_segment(y = -Inf, yend = -Inf, x = min(date_long, na.rm = TRUE),
                 xend = max(date_long, na.rm = TRUE),
                 linewidth = axis_line_width,
                 lineend = "square") 
  
  p_hist <- ggplot(dat_long) + 
    geom_bar(aes(y = value)) +
    scale_y_continuous(breaks = y_breaks, labels = tick_labels) +
    coord_cartesian(ylim = y_lim) +
    facet_wrap(~ name, ncol = 1, labeller = label_bquote(rows = "")) +
    theme_void() +
    labs(x = "", y = "") +
    theme(text = element_text(family = "sans", size = 16),
          axis.title = element_text(margin = margin(5, 5, 5, 5)),
          strip.text = element_text(margin = margin(5, 5, 5, 5), size = 16),
          panel.spacing = unit(5, units = "pt"),
          plot.margin = margin(0, 5, 0, 0),
          legend.position = "none")
  
  dat_zoom <- dat_long %>%
    filter(between(date, unique(dat_long$date)[1], unique(dat_long$date)[7]))
  last_zoom_plus_one <- as.POSIXct(lubridate::ymd(max(dat_zoom$date)) + 1,
                                   tz = "UTC")
  date_zoom <- c(dat_zoom$date, last_zoom_plus_one)
  
  p_ts_zoom <- ggplot(dat_zoom) + 
    geom_vline(xintercept = date_zoom,
               alpha = 0.1, linewidth = axis_line_width) +
    geom_line(aes(x = datetime, y = value, group = date)) +
    geom_point(aes(x = datetime, y = value), size = 0.5) +
    scale_y_continuous(breaks = y_breaks, labels = tick_labels) +
    coord_cartesian(ylim = y_lim) +
    scale_x_datetime(limits = c(min(date_zoom, na.rm = TRUE),
                                max(date_zoom, na.rm = TRUE))) +
    facet_wrap(~ name, ncol = 1, labeller = label_bquote(rows = "")) +
    theme_void() +
    labs(x = "First Week", y = "Score") +
    theme(text = element_text(family = "sans", size = 16),
          axis.title.y = element_text(margin = margin(5, 5, 5, 5), angle = 90),
          axis.text = element_text(margin = margin(5, 5, 5, 5)),
          axis.text.y = element_text(hjust = 0.95),
          axis.title = element_text(margin = margin(5, 5, 5, 5)),
          axis.ticks = element_line(lineend = "butt",
                                    linewidth = axis_line_width),
          axis.ticks.length = unit(2.5, "pt"),
          strip.text = element_text(margin = margin(5, 5, 5, 5), size = 16),
          panel.spacing = unit(5, units = "pt"),
          plot.margin = margin(0, 5, 0, 0),
          legend.position = "none") +
    geom_segment(x = -Inf, xend = -Inf, y = y_breaks[1], yend = 7,
                 linewidth = axis_line_width, lineend = "square") +
    geom_segment(y = -Inf, yend = -Inf, x = min(date_zoom, na.rm = TRUE),
                 xend = max(date_zoom, na.rm = TRUE),
                 linewidth = axis_line_width,
                 lineend = "square") 
  
  p <- p_ts_zoom + p_ts + p_hist + plot_layout(widths = plot_widths)
  
  ggsave(filename, plot = p, width = width, height = height)

}

items <- names(dat)[grepl("mood_|pat_|phy_|se_", names(dat))]

plotTimeSeries(dat, items = items[grepl("se_", items)],
               names = prettyNames(grepl("se_", items)), y_breaks = 1:7,
               filename = "data groot/plots/timeseries_se.pdf", 15, 8)

plotTimeSeries(dat, items = items[grepl("pat_", items)],
               names = prettyNames(grepl("pat_", items)), y_breaks = 1:7,
               filename = "data groot/plots/timeseries_pat.pdf", 15, 8)

plotTimeSeries(dat, items = items[grepl("phy_", items)],
               names = prettyNames(grepl("phy_", items)), y_breaks = 1:7,
               filename = "data groot/plots/timeseries_phy.pdf", 15, 15)

plotTimeSeries(dat, items = items[c(1, 4, 7, 9, 12)],
               names = prettyNames(c(1, 4, 7, 9, 12)), y_breaks = 1:7,
               filename = "data groot/plots/timeseries_mood_pos.pdf", 15, 10)

plotTimeSeries(dat, items = items[c(2, 3, 5, 6, 8, 10, 11)],
               names = prettyNames(c(2, 3, 5, 6, 8, 10, 11)), y_breaks = -3:7,
               filename = "data groot/plots/timeseries_mood_neg.pdf", 15, 14)

plotTimeSeries(dat, items = items[3],
               names = "", y_breaks = 1:7,
               filename = "data groot/plots/timeseries_irritat.pdf", 15, 3)
