dat_unc <- read.csv("data groot/data/data_uncorrected.csv")
dat_c <- read.csv("data groot/data/data.csv")

dat_c$firstbeep <- 0
for (i in 1:nrow(dat_c)) {
  dat_c$firstbeep[i] <- ifelse(min(which(dat_c$day == dat_c$day[i])) == i, 1, 0)
}

library("ggplot2")
source("data groot/utils.R")

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

items <- names(dat_c)[grepl("mood_|pat_|phy_|se_", names(dat_c))]

plotScatter(dat_unc, dat_c, items = items[grepl("se_", items)], breaks = 1:7,
            names = prettyNames(grepl("se_", items)),
            filename = "data groot/plots/scatterplot_se.pdf", width = 7, height = 8)

plotScatter(dat_unc, dat_c, items = items[grepl("pat_", items)], breaks = 1:7,
            names = prettyNames(grepl("pat_", items)),
            filename = "data groot/plots/scatterplot_pat.pdf",
            width = 7, height = 8)

phy_items <- items[grepl("phy_", items)]
phy_names <- prettyNames(grepl("phy_", items))

plotScatter(dat_unc, dat_c, items = items[grepl("phy_", items)], breaks = 1:7,
            names = prettyNames(grepl("phy_", items)),
            filename = "data groot/plots/scatterplot_phy.pdf",
            width = 7, height = 16)

plotScatter(dat_unc, dat_c, items = phy_items[-c(4, 5, 6)], breaks = 1:7,
            names = phy_names[-c(4, 5, 6)],
            filename = "data groot/plots/scatterplot_phy_good.pdf",
            width = 7, height = 11)

plotScatter(dat_unc, dat_c, items = phy_items[c(4, 5, 6)], breaks = 1:7,
            names = phy_names[c(4, 5, 6)],
            filename = "data groot/plots/scatterplot_phy_bad.pdf",
            width = 7, height = 6)

plotScatter(dat_unc, dat_c, items = items[c(1, 4, 7, 9, 12)], breaks = 1:7,
            names = prettyNames(c(1, 4, 7, 9, 12)),
            filename = "data groot/plots/scatterplot_mood_pos.pdf",
            width = 7, height = 9)

neg_items <- items[c(2, 3, 5, 6, 8, 10, 11)]
neg_names <- prettyNames(c(2, 3, 5, 6, 8, 10, 11))

plotScatter(dat_unc, dat_c, items = items[c(2, 3, 5, 6, 8, 10, 11)],
            breaks = -3:7, names = prettyNames(c(2, 3, 5, 6, 8, 10, 11)),
            filename = "data groot/plots/scatterplot_mood_neg.pdf",
            width = 7, height = 12)

plotScatter(dat_unc, dat_c, items = neg_items[-c(2, 5, 7)],
            breaks = -3:3, names = neg_names[-c(2, 5, 7)],
            filename = "data groot/plots/scatterplot_mood_neg_minus.pdf",
            width = 7, height = 8)

plotScatter(dat_unc, dat_c, items = neg_items[c(2, 5, 7)],
            breaks = 1:7, names = neg_names[c(2, 5, 7)],
            filename = "data groot/plots/scatterplot_mood_neg_normal.pdf",
            width = 7, height = 6)

plotScatter(dat_unc, dat_c, items = items[3], breaks = 1:7,
            names = "",
            filename = "data groot/plots/scatterplot_irritat.pdf",
            width = 7, height = 3)

