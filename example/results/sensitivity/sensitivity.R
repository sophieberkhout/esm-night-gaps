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

# combine results of all analyses
set <- c("main", "prior narrow", "prior wide", "prior nonzero",
         "interval hour", "interval hour and half")

fileNames <- sprintf("example/results/%s/objects.Rdata", set)

res <- sapply(1:length(fileNames), .getDFs, file = fileNames, set = set)

all_bfs <- data.table::rbindlist(res["bf", ])
all_pars <- data.table::rbindlist(res["par", ])
all_pmps <- data.table::rbindlist(res["pmp", ])

library(ggplot2)
library(plyr)

# sensitivity results estimates intervals
set_interval <- c("main", "interval hour", "interval hour and half")
pars_interval <- subset(all_pars, set %in% set_interval)
pars_interval$set <- factor(pars_interval$set, levels = set_interval,
                            labels = c("30", "60", "90"))
pars_interval <- pars_interval[order(parameter), ]

## all results
p_sens_int_pars <- ggplot(pars_interval) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_line(aes(x = set, y = median, group = parameter),
            linewidth = 1, position = position_dodge(width = 0.5)) +
  geom_pointrange(aes(x = set, y = median, ymin = lower, ymax = upper,
                      fill = parameter, shape = parameter),
                  linewidth = 1, size = 1,
                  position = position_dodge(width = 0.5)) +
  facet_wrap(~ .id, ncol = 5) +
  coord_cartesian(ylim = c(-0.25, 1)) +
  scale_y_continuous(breaks = seq(-0.25, 1, 0.25)) +
  scale_x_discrete(expand = expansion(add = 0.4)) +
  labs(y = "Estimates", x = "Daytime interval length in minutes",
       fill = "Parameter", shape = "Parameter") +
  scale_shape_manual(values = c(22, 21), labels = c(expression(gamma),
                                                    expression(phi))) +
  viridis::scale_fill_viridis(discrete = TRUE, labels = c(expression(gamma),
                                                          expression(phi))) +
  theme_void() +
  theme(
    text = element_text(family = "sans", size = 16),
    axis.title.x = element_text(margin = margin(5, 5, 5, 5)),
    axis.title.y = element_text(angle = 90, margin = margin(0, -5, 0, 0)),
    axis.text = element_text(margin = margin(5, 5, 5, 5)),
    axis.text.y = element_text(hjust = 0.95),
    axis.ticks = element_line(lineend = "butt", linewidth = 0.3),
    axis.ticks.length = unit(2.5, "pt"),
    strip.text = element_text(margin = margin(5, 5, 5, 5), size = 16),
    panel.spacing = unit(20, units = "pt"),
    plot.margin = margin(5, 15, 5, 5),
    legend.position = c(.9, .07),
    legend.text = element_text(size = 16),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "grey85")
  ) +
  geom_segment(x = -Inf, xend = -Inf, y = -0.25, yend = 1,
               linewidth = 0.3, lineend = "square") +
  geom_segment(y = -Inf, yend = -Inf, x = 1, xend = 3,
               linewidth = 0.3, lineend = "square")

ggsave("example/results/sensitivity/sensitivity_interval_estimates.pdf",
       p_sens_int_pars, width = 12, height = 12)

## only 5 examples variables
example_variables <- c("agitated", "indecisive", "like myself",
                       "hungry", "satisfied")

pars_interval_example <- subset(pars_interval,  .id %in% example_variables)
pars_interval_example$.id <- factor(pars_interval_example$.id, 
                                    levels = example_variables)

p_sens_int_pars_ex <- ggplot(pars_interval_example) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_line(aes(x = set, y = median, group = parameter),
            linewidth = 1, position = position_dodge(width = 0.5)) +
  geom_pointrange(aes(x = set, y = median, ymin = lower, ymax = upper,
                      fill = parameter, shape = parameter),
                  linewidth = 1, size = 1,
                  position = position_dodge(width = 0.5)) +
  facet_wrap(~ .id, ncol = 5) +
  coord_cartesian(ylim = c(-0.25, 1)) +
  scale_y_continuous(breaks = seq(-0.25, 1, 0.25)) +
  scale_x_discrete(expand = expansion(add = 0.4)) +
  labs(y = "Estimates", x = "Daytime interval length in minutes",
       fill = "Parameter", shape = "Parameter") +
  scale_shape_manual(values = c(22, 21), labels = c(expression(gamma),
                                                    expression(phi))) +
  viridis::scale_fill_viridis(discrete = TRUE, labels = c(expression(gamma),
                                                          expression(phi))) +
  theme_void() +
  theme(
    text = element_text(family = "sans", size = 16),
    axis.title.y = element_text(angle = 90, margin = margin(0, -5, 0, 0)),
    axis.title.x = element_blank(),
    axis.text = element_text(margin = margin(5, 5, 5, 5)),
    axis.text.y = element_text(hjust = 0.95),
    axis.ticks = element_line(lineend = "butt", linewidth = 0.3),
    axis.ticks.length = unit(2.5, "pt"),
    strip.text = element_text(margin = margin(5, 5, 5, 5), size = 16),
    panel.spacing = unit(20, units = "pt"),
    plot.margin = margin(5, 15, 5, 5),
    legend.position = "bottom",
    legend.text = element_text(size = 16),
    panel.grid.major.y = element_line(linewidth = 0.3, color = "grey85")
  ) +
  geom_segment(x = -Inf, xend = -Inf, y = -0.25, yend = 1,
               linewidth = 0.3, lineend = "square") +
  geom_segment(y = -Inf, yend = -Inf, x = 1, xend = 3,
               linewidth = 0.3, lineend = "square")


# sensitivity results posterior model probabilities intervals
pmps_interval <- subset(all_pmps, set %in% set_interval)
pmps_interval$set <- factor(pmps_interval$set, levels = set_interval,
                            labels = c("30", "60", "90"))

p_sens_int_pmp <- ggplot(pmps_interval) +
  facet_wrap(~ item, ncol = 5) +
  geom_bar(aes(x = set, y = value, fill = name), stat = "identity") +
  viridis::scale_fill_viridis(discrete = TRUE, name = "Method") +
  labs(y = "Posterior Model Probabilities",
       x = "Daytime interval length in minutes") +
  theme_void() +
  theme(
    text = element_text(family = "sans", size = 16),
    axis.title.y = element_text(angle = 90),
    axis.title.x = element_text(),
    legend.position = c(.9, .065),
    legend.direction = "vertical",
    legend.text = element_text(family = "sans", size = 16),
    legend.box.margin = margin(-10, 0, 0, 0),
    axis.text.y = element_text(margin = margin(5, 5, 5, 5), hjust = 0.95),
    axis.text.x = element_text(margin = margin(0, 5, 5, 5)),
    axis.ticks.y = element_line(lineend = "butt",
                                linewidth = 0.3),
    axis.ticks.length = unit(2.5, "pt"),
    strip.text = element_text(margin = margin(5, 5, 5, 5), size = 16),
    panel.spacing = unit(7.5, units = "pt"),
    plot.margin = margin(5, 5, 5, 5)
)

ggsave("example/results/sensitivity/sensitivity_interval_probabilities.pdf",
       p_sens_int_pmp, width = 12, height = 12)

## only 5 example variables
pmps_interval_example <- subset(pmps_interval, item %in% example_variables)
pmps_interval_example$item <- factor(pmps_interval_example$item, 
                                     levels = example_variables)

p_sens_int_pmp_ex <- ggplot(pmps_interval_example) +
  facet_wrap(~ item, ncol = 5) +
  geom_bar(aes(x = set, y = value, fill = name), stat = "identity") +
  viridis::scale_fill_viridis(discrete = TRUE, name = "Method") +
  labs(y = "Posterior Model Probabilities",
       x = "Daytime interval length in minutes") +
  theme_void() +
  theme(
    text = element_text(family = "sans", size = 16),
    axis.title.y = element_text(angle = 90),
    axis.title.x = element_text(),
    legend.position = "bottom",
    legend.text = element_text(family = "sans", size = 16),
    axis.text.y = element_text(margin = margin(5, 5, 5, 5), hjust = 0.95),
    axis.text.x = element_text(margin = margin(0, 5, 5, 5)),
    axis.ticks.y = element_line(lineend = "butt",
                                linewidth = 0.3),
    axis.ticks.length = unit(2.5, "pt"),
    strip.text = element_blank(),
    panel.spacing = unit(7.5, units = "pt"),
    plot.margin = margin(5, 5, 5, 5)
)

## combine estimate and pmp example plots
library(ggpubr)
p_both <- ggarrange(p_sens_int_pars_ex, p_sens_int_pmp_ex, ncol = 1)
ggsave("example/results/sensitivity/sensitivity_interval_examples.pdf",
       width = 12, height = 6)

# sensitivity results posterior model probabilities prios
set_prior <- c("prior wide", "main", "prior narrow", "prior nonzero")

pmps_prior <- subset(all_pmps, set %in% set_prior)
pmps_prior$set <- factor(pmps_prior$set, levels = set_prior,
                         labels = c("N(0, 2.5)", "N(0, 0.5)",
                                    "N(0, 0.1)", "N(0.3, 0.5)"))

p_sens_prior <- ggplot(pmps_prior) +
  facet_wrap(~ item, ncol = 5) +
  geom_bar(aes(x = set, y = value, fill = name), stat = "identity") +
  viridis::scale_fill_viridis(discrete = TRUE, name = "Method") +
  theme_void() +
  theme(
    text = element_text(family = "sans", size = 16),
    axis.title.y = element_text(angle = 90),
    axis.title.x = element_text(margin = margin(-5, 5, 5, 5)),
    legend.position = c(.9, 0),
    legend.direction = "vertical",
    legend.text = element_text(family = "sans", size = 16),
    legend.box.margin = margin(-10, 0, 0, 0),
    axis.text = element_text(margin = margin(5, 5, 5, 5)),
    axis.text.y = element_text(hjust = 0.95),
    axis.text.x = element_text(angle = 60, hjust = 1.1, vjust = 1.2, size = 14),
    axis.ticks.y = element_line(lineend = "butt",
                                linewidth = 0.3),
    axis.ticks.length = unit(2.5, "pt"),
    strip.text = element_text(margin = margin(5, 5, 5, 5), size = 16),
    panel.spacing = unit(7.5, units = "pt"),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  labs(y = "Posterior Model Probabilities",
       x = "Prior distribution")

ggsave("example/results/sensitivity/sensitivity_prior_probabilities.pdf",
       p_sens_prior, width = 12, height = 12)
