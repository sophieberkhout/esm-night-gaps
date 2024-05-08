dat <- read.csv("example/data/data_uncorrected.csv")

t_b1 <- dat[dat$beepno == 1, "datetime"]
t_b10 <- dat[dat$beepno == 10, "datetime"]

dt_night <- difftime(t_b1[2:length(t_b1)], t_b10[1:(length(t_b1) - 1)],
                     units = "hours")
dt_night <- as.numeric(dt_night)

dat$lag <- c(NA, dat$datetime[1:(nrow(dat) - 1)])
t_bi <- dat[dat$beepno != 1, "datetime"]
t_bi_lag <- dat[dat$beepno != 1, "lag"]
dt_beep <- difftime(t_bi, t_bi_lag, units = "hours")
dt_beep <- as.numeric(dt_beep)

dt_df <- data.frame(dt = c(dt_night, dt_beep),
                    Interval = c(rep("Nighttime", length(dt_night)),
                                 rep("Daytime", length(dt_beep))))
library(ggplot2)

x_breaks <- pretty(dt_df$dt)
x_breaks <- 0:12
ggplot(dt_df) +
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
