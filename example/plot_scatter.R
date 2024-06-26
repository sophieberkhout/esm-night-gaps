dat_unc <- read.csv("example/data/data_uncorrected.csv")
dat_c <- read.csv("example/data/data.csv")

dat_c$firstbeep <- 0
for (i in 1:nrow(dat_c)) {
  dat_c$firstbeep[i] <- ifelse(min(which(dat_c$day == dat_c$day[i])) == i, 1, 0)
}

library("ggplot2")
source("example/functions_plots.R")

items <- names(dat_c)[grepl("mood_|pat_|phy_|se_", names(dat_c))]

plotScatter(dat_unc, dat_c, items = items[grepl("se_", items)], breaks = 1:7,
            names = prettyNames(grepl("se_", items)),
            filename = "example/plots/scatterplot_se.pdf",
            width = 7, height = 8)

plotScatter(dat_unc, dat_c, items = items[grepl("pat_", items)], breaks = 1:7,
            names = prettyNames(grepl("pat_", items)),
            filename = "example/plots/scatterplot_pat.pdf",
            width = 7, height = 8)

phy_items <- items[grepl("phy_", items)]
phy_names <- prettyNames(grepl("phy_", items))

plotScatter(dat_unc, dat_c, items = items[grepl("phy_", items)], breaks = 1:7,
            names = prettyNames(grepl("phy_", items)),
            filename = "example/plots/scatterplot_phy.pdf",
            width = 7, height = 16)

plotScatter(dat_unc, dat_c, items = phy_items[-c(6, 9, 8, 2)], breaks = 1:7,
            names = phy_names[-c(6, 9, 8, 2)],
            filename = "example/plots/scatterplot_phy_first.pdf",
            width = 7, height = 9)

plotScatter(dat_unc, dat_c, items = phy_items[c(6, 9, 8, 2)], breaks = 1:7,
            names = phy_names[c(6, 9, 8, 2)],
            filename = "example/plots/scatterplot_phy_second.pdf",
            width = 7, height = 8)

plotScatter(dat_unc, dat_c, items = items[c(1, 4, 7, 9, 12)], breaks = 1:7,
            names = prettyNames(c(1, 4, 7, 9, 12)),
            filename = "example/plots/scatterplot_mood_pos.pdf",
            width = 7, height = 9)

neg_items <- items[c(2, 3, 5, 6, 8, 10, 11)]
neg_names <- prettyNames(c(2, 3, 5, 6, 8, 10, 11))

plotScatter(dat_unc, dat_c, items = items[c(2, 3, 5, 6, 8, 10, 11)],
            breaks = -3:7, names = prettyNames(c(2, 3, 5, 6, 8, 10, 11)),
            filename = "example/plots/scatterplot_mood_neg.pdf",
            width = 7, height = 12)

plotScatter(dat_unc, dat_c, items = neg_items[-c(2, 5, 7)],
            breaks = -3:3, names = neg_names[-c(2, 5, 7)],
            filename = "example/plots/scatterplot_mood_neg_minus.pdf",
            width = 7, height = 8)

plotScatter(dat_unc, dat_c, items = neg_items[c(2, 5, 7)],
            breaks = 1:7, names = neg_names[c(2, 5, 7)],
            filename = "example/plots/scatterplot_mood_neg_normal.pdf",
            width = 7, height = 6)

plotScatter(dat_unc, dat_c, items = items[3], breaks = 1:7,
            names = "",
            filename = "example/plots/scatterplot_irritat.pdf",
            width = 7, height = 3)

