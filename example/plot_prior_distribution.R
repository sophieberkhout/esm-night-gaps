df <- data.frame(y = dnorm(seq(-3, 3, 0.01), 0, sqrt(0.5)),
                 x = seq(-3, 3, 0.01))

x_breaks <- seq(-3, 3, 1)
ggplot(df) +
  geom_line(aes(x = x, y = y)) +
  labs(x = expression(italic(p(gamma)))) +
  scale_x_continuous(breaks = x_breaks, limits = c(min(x_breaks), max(x_breaks))) +
  scale_y_continuous(breaks = seq(0, 0.6, 0.2),
                     limits = c(0, 0.6)) +
  theme_void() +
  theme(text = element_text(family = "sans", size = 12),
        axis.title.y = element_blank(),
        axis.text = element_text(margin = margin(5, 5, 5, 5)),
        axis.title = element_text(margin = margin(5, 5, 5, 5)),
        axis.ticks = element_line(lineend = "butt",
                                  linewidth = 0.3),
        axis.ticks.length = unit(2.5, "pt"),
        legend.position = c(.8, .8)) +
  geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 0.6,
               linewidth = 0.3, lineend = "square") +
  geom_segment(y = -Inf, yend = -Inf, x = min(x_breaks),
               xend = max(x_breaks),
               linewidth = 0.3,
               lineend = "square") 

ggsave("example/plots/prior_distribution.pdf", width = 4, height = 3)
