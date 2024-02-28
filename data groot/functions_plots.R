prettyNames <- function (idx = 1:29) {
  names <- c("relaxed", "down", "irritated", "satisfied", "lonely", "anxious",
             "enthusiastic", "suspicious", "cheerful", "guilty", "indecisive",
             "strong", "restless", "agitated", "worry", "concentrate well",
             "like myself", "ashamed", "doubt myself", "handle anything", 
             "hungry", "tired", "in pain", "dizzy", "dry mouth", "nauseous",
             "headache", "sleepy", "physically active")
  return(names[idx])
}

itemCategory <- function (idx = 1:29) {
  names <- c("PA", "NA", "NA", "PA", "NA", "NA",
             "PA", "NA", "PA", "NA", "NA",
             "PA", "unrest", "unrest", "unrest", "unrest",
             "self-esteem", "self-esteem", "self-esteem", "self-esteem",
             "physical", "physical", "physical", "physical", "physical",
             "physical", "physical", "physical", "physical")
  return(names[idx])
}

facetLabels <- c(
  `NA` = "Negative Affect",
  `PA` = "Positive Affect",
  `unrest` = "Mental Unrest",
  `self-esteem` = "Self-Esteem",
  `physical` = "Physical"
)

# descriptive plots

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

plotScatter <- function(dat_unc, dat_c, items, names, breaks,
                        filename, width, height) {
  
  # names <- gsub(" ", "\n", names)
  
  dat_plot <- dat_unc[, c("beepno", items)]
  
  dat_long <- tidyr::pivot_longer(dat_plot, cols = tidyr::all_of(items),
                                  cols_vary = "slowest")
  
  dat_long$x <- c(NA, dat_long$value[-nrow(dat_long)])
  
  dat_long$g <- "unequal"
  
  dat_split_long <- dat_long
  dat_split_long$g <- ifelse(
    dat_split_long$beepno != 1 | is.na(dat_split_long$beepno), "day", "night"
  )
  
  dat_c_plot <- dat_c[, c("firstbeep", items)]
  dat_c_long <- tidyr::pivot_longer(dat_c_plot, cols = tidyr::all_of(items),
                                    cols_vary = "slowest")
  
  dat_c_long$x <- c(NA, dat_c_long$value[-nrow(dat_c_long)])
  dat_c_long$x[which(dat_c_long$firstbeep == 1)] <- NA
  
  dat_c_long$g <- "corrected"
  
  # no lag for the first observation
  for (i in items) {
    dat_long$x[min(which(dat_long$name == i))] <- NA
    dat_split_long$x[min(which(dat_split_long$name == i))] <- NA
    dat_c_long$x[min(which(dat_c_long$name == i))] <- NA
  }
  
  df <- rbind(dat_long[, names(dat_long) != "beepno"],
              dat_split_long[, names(dat_split_long) != "beepno"],
              dat_c_long[, names(dat_c_long) != "firstbeep"])
  
  df$g <- factor(df$g, levels = c("unequal", "corrected", "day", "night"))
  levels(df$g) <- c("Unequal Intervals", "Corrected Intervals",
                    "Day Only", "Night Only")
  df$g <- factor(df$g, levels = c("Unequal Intervals", "Corrected Intervals",
                                  "Day Only",  "Night Only"))
  df$name <- factor(df$name, levels = items)
  
  levels(df$name) <- names
  
  tick_labels <- breaks
  y_lim <- c(min(breaks) - 0.25, max(breaks) + 0.25)
  
  if (length(breaks) > 7) tick_labels[seq(1, length(tick_labels), 2)] <- " "
  
  p <- ggplot(aes(x = x, y = value), data = df) +
    facet_grid(name ~ g) +
    stat_smooth(method = lm, se = FALSE, fullrange = TRUE, col = "#22A884FF") +
    geom_count() +
    scale_size(breaks = c(1, 5, 10, 25, 50, 100, 200, 400),
               name = "Number of observations") +
    theme_void() +
    scale_y_continuous(limits = y_lim, labels = tick_labels, breaks = breaks) +
    scale_x_continuous(limits = y_lim, labels = tick_labels, breaks = breaks) +
    theme(panel.border = element_rect(fill = NA),
          text = element_text(family = "sans"),
          axis.text = element_text(margin = margin(5, 5, 5, 5)),
          axis.text.y = element_text(hjust = 0.95),
          axis.title = element_text(margin = margin(5, 5, 5, 5)),
          axis.ticks = element_line(),
          axis.ticks.length = unit(2.5, "pt"),
          strip.text = element_text(margin = margin(5, 5, 5, 5), size = 12),
          strip.text.y = element_text(angle = 90),
          panel.spacing = unit(7.5, units = "pt"),
          plot.margin = margin(0, 5, 0, 0),
          legend.position = "top") +
    labs(x = "t - 1", y = "t", ) +
    coord_fixed()        
  
  ggsave(p, filename = filename, width = width, height = height)
  
}
