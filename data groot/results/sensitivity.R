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

set_interval <- c("main", "prior narrow", "prior wide", "prior nonzero",
                  "interval hour", "interval uncorrected")

fileNames <- sprintf("data groot/results/%s/objects.Rdata", set)

res <- sapply(1:length(fileNames), .getDFs, file = fileNames, set = set)

all_bfs <- data.table::rbindlist(res["bf", ])
all_pars <- data.table::rbindlist(res["par", ])
all_pmps <- data.table::rbindlist(res["pmp", ])

library(ggplot2)

all_pars$set <- factor(all_pars$set, levels = set[c(2, 1, 3:6)])
df <- subset(all_pars, set %in% c("main", "interval hour", "interval uncorrected"))
levels(df$set) <- c("NA", "30", "NA", "NA", "60", "90")
ggplot(df) +
  geom_pointrange(aes(x = set, y = median, ymin = lower, ymax = upper,
                      colour = parameter)) +
  geom_line(aes(x = set, y = median, colour = parameter, group = parameter)) +
  facet_wrap(~ .id, ncol = 5) +
  coord_cartesian(ylim = c(-0.1, 1))

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

pmps_interval <- subset(all_pmps, set %in% c("main", "interval hour", "interval uncorrected"))
ggplot(pmps_interval) +
  facet_wrap(~ item, ncol = 5) +
  geom_bar(aes(x = set, y = value, fill = name), stat = "identity") +
  viridis::scale_fill_viridis(discrete = TRUE, name = "Method") +
  theme_void() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, size = 12),
    axis.title.y = element_text(angle = 90, size = 14),
    legend.position = "bottom",
    legend.text = element_text(family = "sans", size = 12),
    legend.box.margin = margin(-10, 0, 0, 0),
    text = element_text(family = "sans", size = 12),
    axis.text = element_text(margin = margin(5, 5, 5, 5)),
    axis.text.y = element_text(hjust = 0.95),
    axis.ticks.y = element_line(lineend = "butt",
                                linewidth = 0.3),
    axis.ticks.length = unit(2.5, "pt"),
    strip.text = element_text(margin = margin(5, 5, 5, 5), size = 14),
    panel.spacing = unit(7.5, units = "pt"),
    plot.margin = margin(5, 5, 5, 5)
  ) +
  labs(y = "Posterior Model Probabilities")

ggsave("data groot/results/sensitivity.pdf", width = 12, height = 12)
