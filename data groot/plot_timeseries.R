dat <- read.csv("data groot/data/data_uncorrected.csv")

library("ggplot2")
library("dplyr")
library("patchwork")
source("data groot/functions_plots.R")

items <- names(dat)[grepl("mood_|pat_|phy_|se_", names(dat))]

plotTimeSeries(dat, items = items[grepl("se_", items)],
               names = prettyNames(grepl("se_", items)), y_breaks = 1:7,
               filename = "data groot/plots/timeseries_se.pdf", 15, 8)

plotTimeSeries(dat, items = items[grepl("pat_", items)],
               names = prettyNames(grepl("pat_", items)), y_breaks = 1:7,
               filename = "data groot/plots/timeseries_pat.pdf", 15, 8)

phy_items <- items[grepl("phy_", items)]
phy_names <- prettyNames(grepl("phy_", items))
plotTimeSeries(dat, items = phy_items,
               names = phy_names, y_breaks = 1:7,
               filename = "data groot/plots/timeseries_phy.pdf", 15, 18)

plotTimeSeries(dat, items = phy_items[-c(6, 9, 8, 2)],
               names = phy_names[-c(6, 9, 8, 2)], y_breaks = 1:7,
               filename = "data groot/plots/timeseries_phy_first.pdf", 15, 10)

plotTimeSeries(dat, items = phy_items[c(6, 9, 8, 2)],
               names = phy_names[c(6, 9, 8, 2)], y_breaks = 1:7,
               filename = "data groot/plots/timeseries_phy_second.pdf", 15, 8)

plotTimeSeries(dat, items = items[c(1, 4, 7, 9, 12)],
               names = prettyNames(c(1, 4, 7, 9, 12)), y_breaks = 1:7,
               filename = "data groot/plots/timeseries_mood_pos.pdf", 15, 10)

plotTimeSeries(dat, items = items[c(2, 3, 5, 6, 8, 10, 11)],
               names = prettyNames(c(2, 3, 5, 6, 8, 10, 11)), y_breaks = -3:7,
               filename = "data groot/plots/timeseries_mood_neg.pdf", 15, 14)

plotTimeSeries(dat, items = items[3],
               names = "", y_breaks = 1:7,
               filename = "data groot/plots/timeseries_irritat.pdf", 15, 3)
