# load("data groot/results/interval hour/objects.Rdata")
# 
# df_bf_hour <- df_bayes_factors[, c(".id", "BF_pd", "BF_sd", "BF_cd")]
# df_bf_hour <- tidyr::pivot_longer(df_bf_hour, cols = tidyr::starts_with("BF"))
# df_bf_hour$set <- "hour"
# 
# df_par_hour <- subset(df_pars, parameter %in% c("phi", "gamma"))
# df_par_hour$set <- "hour"
# 
# df_pmp_hour <- df_pmps
# df_pmp_hour$set <- "hour"

.getDFs <- function (r, file, set) {
  load(file[r])
  
  par <- subset(df_pars, parameter %in% c("phi", "gamma"))
  par$set <- set[r]
  
  bf <- df_bayes_factors[, c(".id", "BF_pd", "BF_sd", "BF_cd")]
  bf <- tidyr::pivot_longer(bf, cols = tidyr::starts_with("BF"))
  bf$set <- set[r]
  
  pmp <- df_pmps
  pmp$set <- set[r]
  
  return(list(par = par, bf = bf, pmp = pmp))
}

# test <- .getDFs("data groot/results/interval hour/objects.Rdata", "hour")

set <- c("main", "prior narrow", "prior wide", "prior nonzero",
         "interval hour", "interval hour and half")

fileNames <- sprintf("data groot/results/%s/objects.Rdata", set)

res <- sapply(1:length(fileNames), .getDFs, file = fileNames, set = set)

all_bfs <- data.table::rbindlist(res["bf", ])
all_pars <- data.table::rbindlist(res["par", ])
all_pmps <- data.table::rbindlist(res["pmp", ])

library(ggplot2)
library(plyr)

# all_pars$set <- factor(all_pars$set, levels = set[c(2, 1, 3:6)])
pars_interval <- subset(all_pars, set %in% c("main", "interval hour",
                                             "interval hour and half"))
pars_interval$set <- factor(pars_interval$set, levels = set[c(1, 5, 6)],
                            labels = c("30", "60", "90"))
# levels(pars_interval$set) <- c("30", "60", "90")
# pars_interval$parameter <- factor(pars_interval$parameter, levels = c("phi", "gamma"))
pars_interval <- pars_interval[order(parameter), ]
ggplot(pars_interval) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_line(aes(x = set, y = median, group = parameter),
            linewidth = 1, position = position_dodge(width = 0.5)) +
  geom_pointrange(aes(x = set, y = median, ymin = lower, ymax = upper,
                      fill = parameter, shape = parameter),
                  linewidth = 1, size = 1, position = position_dodge(width = 0.5)) +
  facet_wrap(~ .id, ncol = 5) +
  coord_cartesian(ylim = c(-0.25, 1)) +
  scale_y_continuous(breaks = seq(-0.25, 1, 0.25)) +
  labs(y = "Estimates", x = "Daytime interval length in minutes",
       colour = "Parameter") +
  scale_shape_manual(values = c(22, 21), labels = c(expression(gamma), expression(phi))) +
  viridis::scale_fill_viridis(discrete = TRUE, labels = c(expression(gamma), expression(phi))) +
  theme_void() +
  theme(
    text = element_text(family = "sans", size = 16),
    axis.title.y = element_text(angle = 90),
    axis.text = element_text(margin = margin(5, 5, 5, 5)),
    axis.text.y = element_text(hjust = 0.95),
    axis.title = element_text(margin = margin(5, 5, 5, 5)),
    axis.ticks = element_line(lineend = "butt", linewidth = 0.3),
    axis.ticks.length = unit(2.5, "pt"),
    strip.text = element_text(margin = margin(5, 5, 5, 5), size = 16),
    panel.spacing = unit(7.5, units = "pt"),
    plot.margin = margin(0, 5, 0, 0),
    legend.position = c(.9, .07),
    legend.title = element_blank(),
    legend.text = element_text(size = 16),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "grey85")
  ) +
  geom_segment(x = -Inf, xend = -Inf, y = -0.25, yend = 1,
               linewidth = 0.3, lineend = "square") +
  geom_segment(y = -Inf, yend = -Inf, x = 1, xend = 3,
               linewidth = 0.3, lineend = "square")

ggsave("data groot/results/sensitivity_estimates.pdf", width = 12, height = 12)

all_bfs$set <- factor(all_bfs$set, levels = set[c(2, 1, 3:6)])
ggplot(all_bfs) +
  geom_point(aes(x = set, y = value, colour = name)) +
  facet_wrap(~ .id, ncol = 5, scales = "free") +
  scale_y_continuous(trans = "log10")

all_pmps$name <- factor(all_pmps$name,
                        levels = c("pause", "stop", "continue", "different"))
all_pmps$set <- factor(all_pmps$set, levels = set[c(2, 1, 3:6)])
ggplot(all_pmps) +
  geom_point(aes(x = set, y = value, colour = name)) +
  facet_wrap(~ item, ncol = 5)

pmps_interval <- subset(all_pmps, set %in% c("main", "interval hour",
                                             "interval hour and half"))
pmps_interval$set <- factor(pmps_interval$set, levels = set[c(1, 5, 6)],
                            labels = c("30", "60", "90"))
pmps_interval$name <- factor(pmps_interval$name,
                             levels = c("pause", "stop",
                                        "continue", "different"))

ggplot(pmps_interval) +
  facet_wrap(~ item, ncol = 5) +
  geom_bar(aes(x = set, y = value, fill = name), stat = "identity") +
  viridis::scale_fill_viridis(discrete = TRUE, name = "Method") +
  theme_void() +
  theme(
    text = element_text(family = "sans", size = 14),
    axis.title.y = element_text(angle = 90, size = 14),
    legend.position = c(.9, .07),
    legend.direction = "vertical",
    legend.text = element_text(family = "sans", size = 14),
    legend.box.margin = margin(-10, 0, 0, 0),
    axis.text = element_text(margin = margin(5, 5, 5, 5)),
    axis.text.y = element_text(hjust = 0.95),
    axis.ticks.y = element_line(lineend = "butt",
                                linewidth = 0.3),
    axis.ticks.length = unit(2.5, "pt"),
    strip.text = element_text(margin = margin(5, 5, 5, 5), size = 14),
    panel.spacing = unit(7.5, units = "pt"),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  labs(y = "Posterior Model Probabilities",
       x = "Daytime interval length in minutes")

ggsave("data groot/results/sensitivity.pdf", width = 12, height = 12)


pmps_prior <- subset(all_pmps, set %in% c("main", "prior wide",
                                          "prior narrow", "prior nonzero"))
pmps_prior$set <- factor(pmps_prior$set, levels = set[c(3, 1, 2, 4)],
                         labels = c("N(0, 2.5)", "N(0, 0.5)",
                                    "N(0, 0.1)", "N(0.3, 0.5)"))
pmps_prior$name <- factor(pmps_prior$name,
                             levels = c("pause", "stop",
                                        "continue", "different"))

ggplot(pmps_prior) +
  facet_wrap(~ item, ncol = 5) +
  geom_bar(aes(x = set, y = value, fill = name), stat = "identity") +
  viridis::scale_fill_viridis(discrete = TRUE, name = "Method") +
  theme_void() +
  theme(
    text = element_text(family = "sans", size = 14),
    axis.title.y = element_text(angle = 90, size = 14),
    legend.position = c(.9, 0),
    legend.direction = "vertical",
    legend.text = element_text(family = "sans", size = 14),
    legend.box.margin = margin(-10, 0, 0, 0),
    axis.text = element_text(margin = margin(5, 5, 5, 5)),
    axis.text.y = element_text(hjust = 0.95),
    axis.text.x = element_text(angle = 60, hjust = 1.1, vjust = 1.2),
    axis.ticks.y = element_line(lineend = "butt",
                                linewidth = 0.3),
    axis.ticks.length = unit(2.5, "pt"),
    strip.text = element_text(margin = margin(5, 5, 5, 5), size = 14),
    panel.spacing = unit(7.5, units = "pt"),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  labs(y = "Posterior Model Probabilities",
       x = "Prior")

ggsave("data groot/results/sensitivity_prior.pdf", width = 12, height = 12)
