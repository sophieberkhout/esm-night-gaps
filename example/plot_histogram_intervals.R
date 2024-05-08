# function to compute interval lengths for plotting
intervalLengths <- function (dat, corrected = F) {
  if (!corrected) {
    # get datetimes of first and last beep
    t_b1 <- dat[dat$beepno == 1, "datetime"]
    t_b10 <- dat[dat$beepno == 10, "datetime"]
    
    # get difference in time between first beep of day and
    # last beep of previous day
    dt_night <- difftime(t_b1[2:length(t_b1)], t_b10[1:(length(t_b1) - 1)],
                         units = "hours")
    dt_night <- as.numeric(dt_night)
    
    # create lagged datetime variable
    dat$lag <- c(NA, dat$datetime[1:(nrow(dat) - 1)])
    
    # get all datetimes that are not the first beep so night lag is excluded
    t_bi <- dat[dat$beepno != 1, "datetime"]
    t_bi_lag <- dat[dat$beepno != 1, "lag"]
    
    # get difference in time for daytime intervals
    dt_beep <- difftime(t_bi, t_bi_lag, units = "hours")
    dt_beep <- as.numeric(dt_beep)
    
    # combine in date frame
    dt_df <- data.frame(dt = c(dt_night, dt_beep),
                        Interval = c(rep("Nighttime", length(dt_night)),
                                     rep("Daytime", length(dt_beep))))
  } else {
    # create variable that indicates what row represents the first beeps of the day
    firstbeep <- 0
    for (i in 1:nrow(dat)) {
      firstbeep[i] <- ifelse(min(which(dat$day == dat$day[i])) == i, 1, 0)
    }
    
    # create variable that indicates what row represents the last beeps of the day
    lastbeep <- 0
    for (i in 1:nrow(dat)) {
      lastbeep[i] <- ifelse(max(which(dat$day == dat$day[i])) == i, 1, 0)
    }
    
    # get datetime for first and last beeps
    t_b1 <- dat[as.logical(firstbeep), "datetime"]
    t_b10 <- dat[as.logical(lastbeep), "datetime"]
    
    # compute difference in time for night intervals
    dt_night <- difftime(t_b1[2:length(t_b1)], t_b10[1:(length(t_b1) - 1)],
                         units = "hours")
    dt_night <- as.numeric(dt_night)
    
    # create lagged datetime variable
    dat$lag <- c(NA, dat$datetime[1:(nrow(dat) - 1)])
    
    # get all datetimes except for first beep to exclude night intervals
    t_bi <- dat[firstbeep != 1, "datetime"]
    t_bi_lag <- dat[firstbeep != 1, "lag"]
    
    # compute difference in time for daytime intervals
    dt_beep <- difftime(t_bi, t_bi_lag, units = "hours")
    dt_beep <- as.numeric(dt_beep)
    
    # combine in data frame
    dt_df <- data.frame(dt = c(dt_night, dt_beep),
                        Interval = c(rep("Nighttime", length(dt_night)),
                                     rep("Daytime", length(dt_beep))))
  }
  return(dt_df)
}

# plot day and nighttime intervals histogram of uncorrected intervals data
dat_unc <- read.csv("example/data/data_uncorrected.csv")

dt_unc <- intervalLengths(dat_unc)

library(ggplot2)

x_breaks <- pretty(dt_unc$dt)
x_breaks <- 0:12
ggplot(dt_unc) +
  geom_histogram(aes(x = dt, fill = Interval, color = Interval)) +
  labs(x = "Time interval in hours") +
  scale_x_continuous(breaks = x_breaks) +
  scale_y_continuous(breaks = c(0, 25, 50, 75, 100, 125, 150),
                     limits = c(0, 150)) +
  scale_color_manual(values = c("grey", "black")) +
  scale_fill_manual(values = alpha(c("grey", "black"), 0.5)) +
  theme_void() +
  theme(text = element_text(family = "sans", size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(margin = margin(5, 5, 5, 5)),
        axis.title = element_text(margin = margin(5, 5, 5, 5)),
        axis.ticks = element_line(lineend = "butt",
                                  linewidth = 0.3),
        axis.ticks.length = unit(2.5, "pt"),
        legend.position = c(.8, .8)) +
  geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 150,
               linewidth = 0.3, lineend = "square") +
  geom_segment(y = -Inf, yend = -Inf, x = min(x_breaks),
               xend = max(x_breaks),
               linewidth = 0.3,
               lineend = "square") 

ggsave("example/plots/histogram_intervals.pdf", height = 3, width = 5)

# plot histograms of both corrected and uncorrected data
dat <- read.csv("example/data/data.csv")

dt_cor <- intervalLengths(dat, corrected = TRUE)

# combine corrected and uncorrected interval differences of night only
dt_cor_night <- subset(dt_cor, Interval == "Nighttime", select = dt)
dt_unc_night <- subset(dt_unc, Interval == "Nighttime", select = dt)
dt_cor_night$data <- "Corrected"
dt_unc_night$data <- "True"

df_night <- rbind(dt_cor_night, dt_unc_night)

x_breaks <- 9:12
ggplot(df_night) +
  geom_histogram(aes(x = dt, fill = data, colour = data), bins = 12) +
  labs(x = "Nighttime interval in hours") +
  scale_x_continuous(breaks = x_breaks, limits = c(min(x_breaks),
                                                   max(x_breaks))) +
  scale_y_continuous(breaks = c(0, 25, 50),
                     limits = c(0, 50)) +
  scale_color_manual(values = c("black", "grey")) +
  scale_fill_manual(values = alpha(c("black", "grey"), 0.5)) +
  theme_void() +
  theme(text = element_text(family = "sans", size = 16),
        axis.title.y = element_blank(),
        axis.text = element_text(margin = margin(5, 5, 5, 5)),
        axis.title = element_text(margin = margin(5, 5, 5, 5)),
        axis.ticks = element_line(lineend = "butt",
                                  linewidth = 0.3),
        axis.ticks.length = unit(2.5, "pt"),
        legend.position = "none") +
  geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 50,
               linewidth = 0.3, lineend = "square") +
  geom_segment(y = -Inf, yend = -Inf, x = min(x_breaks),
               xend = max(x_breaks),
               linewidth = 0.3,
               lineend = "square") 

ggsave("example/plots/histogram_intervals_corrected_night.pdf",
       height = 3, width = 5)

# combine corrected and uncorrected interval differences of day only
dt_cor_day <- subset(dt_cor, Interval == "Daytime", select = dt)
dt_unc_day <- subset(dt_unc, Interval == "Daytime", select = dt)
dt_cor_day$data <- "Corrected"
dt_unc_day$data <- "True"

df_day <- rbind(dt_cor_day, dt_unc_day)

x_breaks <- 0:3
ggplot(df_day) +
  geom_histogram(aes(x = dt, colour = data, fill = data), bins = 40) +
  labs(x = "Daytime interval in hours") +
  scale_x_continuous(breaks = x_breaks, limits = c(min(x_breaks), max(x_breaks))) +
  scale_y_continuous(breaks = seq(0, 2000, 500),
                     limits = c(0, 2000)) +
  scale_color_manual(values = c("black", "grey")) +
  scale_fill_manual(values = alpha(c("black", "grey"), 0.5)) +
  theme_void() +
  theme(text = element_text(family = "sans", size = 16),
        axis.title.y = element_blank(),
        axis.text = element_text(margin = margin(5, 5, 5, 5)),
        axis.title = element_text(margin = margin(5, 5, 5, 5)),
        axis.ticks = element_line(lineend = "butt",
                                  linewidth = 0.3),
        axis.ticks.length = unit(2.5, "pt"),
        legend.position = c(.8, .8),
        legend.title = element_blank()) +
  geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 2000,
               linewidth = 0.3, lineend = "square") +
  geom_segment(y = -Inf, yend = -Inf, x = min(x_breaks),
               xend = max(x_breaks),
               linewidth = 0.3,
               lineend = "square")

ggsave("example/plots/histogram_intervals_corrected_day.pdf",
       height = 3, width = 5)
