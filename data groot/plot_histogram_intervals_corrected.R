dat <- read.csv("data groot/data/data.csv")

firstbeep <- 0
for (i in 1:nrow(dat)) {
  firstbeep[i] <- ifelse(min(which(dat$day == dat$day[i])) == i, 1, 0)
}
lastbeep <- 0
for (i in 1:nrow(dat)) {
  lastbeep[i] <- ifelse(max(which(dat$day == dat$day[i])) == i, 1, 0)
}
t_b1 <- dat[as.logical(firstbeep), "datetime"]
t_b10 <- dat[as.logical(lastbeep), "datetime"]

dt_night <- difftime(t_b1[2:length(t_b1)], t_b10[1:(length(t_b1) - 1)],
                     units = "hours")
dt_night <- as.numeric(dt_night)

dat$lag <- c(NA, dat$datetime[1:(nrow(dat) - 1)])
t_bi <- dat[firstbeep != 1, "datetime"]
t_bi_lag <- dat[firstbeep != 1, "lag"]
dt_beep <- difftime(t_bi, t_bi_lag, units = "mins")
dt_beep <- difftime(t_bi, t_bi_lag, units = "hours")

dt_beep <- as.numeric(dt_beep)

dt_df <- data.frame(dt = c(dt_night, dt_beep),
                    Interval = c(rep("Nighttime", length(dt_night)),
                                 rep("Daytime", length(dt_beep))))

dt_df_night <- data.frame(dt = dt_night)
# got dt_df_uncorrected from plot_histogram_intervals.R
dt_df_night_uncorrected <- subset(dt_df_uncorrected, dt_df_uncorrected$Interval == "Nighttime", select = dt)
df_night <- data.frame(dt = c(dt_df_night_uncorrected$dt, dt_df_night$dt))
df_night$data <- c(rep("True", nrow(dt_df_night_uncorrected)), rep("Corrected", nrow(dt_df_night)))


library(ggplot2)

x_breaks <- pretty(dt_df$dt)
x_breaks <- 9:12
ggplot(df_night) +
  geom_histogram(aes(x = dt, fill = data, colour = data), bins = 12) +
  # geom_histogram(aes(x = dt), fill = alpha("black", 0.5), color = "black", bins = 12) +
  # geom_histogram(data = dt_df_night_uncorrected, aes(x = dt), fill = alpha("grey", 0.5), color = "grey", bins = 12) +
  labs(x = "Nighttime interval in hours") +
  scale_x_continuous(breaks = x_breaks, limits = c(min(x_breaks), max(x_breaks))) +
  scale_y_continuous(breaks = c(0, 25, 50),
                     limits = c(0, 50)) +
  scale_color_manual(values = c("black", "grey")) +
  scale_fill_manual(values = alpha(c("black", "grey"), 0.5)) +
  # scale_color_manual(values = c("grey", "black")) +
  # scale_fill_manual(values = alpha(c("grey", "black"), 0.5)) +
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

ggsave("data groot/plots/histogram_intervals_corrected_night_unc.pdf", height = 3, width = 5)


dt_df_day <- data.frame(dt = dt_beep)
dt_df_day_uncorrected <- subset(dt_df_uncorrected, dt_df_uncorrected$Interval == "Daytime", select = dt)
df_day <- data.frame(dt = c(dt_df_day_uncorrected$dt, dt_df_day$dt))
df_day$data <- c(rep("Original", nrow(dt_df_day_uncorrected)), rep("Corrected", nrow(dt_df_day)))

x_breaks <- seq(0, 60, 15)
x_breaks <- 0:3
ggplot(df_day) +
  geom_histogram(aes(x = dt, colour = data, fill = data), bins = 40) +
  # geom_histogram(aes(x = dt), fill = alpha("black", 0.5), color = "black", bins = 40) +
  # geom_histogram(data = dt_df_day_uncorrected, aes(x = dt), fill = alpha("grey", 0.5), color = "grey", bins = 12) +
  labs(x = "Daytime interval in hours") +
  scale_x_continuous(breaks = x_breaks, limits = c(min(x_breaks), max(x_breaks))) +
  scale_y_continuous(breaks = seq(0, 2000, 500),
                     limits = c(0, 2000)) +
  scale_color_manual(values = c("black", "grey")) +
  scale_fill_manual(values = alpha(c("black", "grey"), 0.5)) +
  # scale_color_manual(values = c("grey", "black")) +
  # scale_fill_manual(values = alpha(c("grey", "black"), 0.5)) +
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

ggsave("data groot/plots/histogram_intervals_corrected_day_unc.pdf", height = 3, width = 5)
