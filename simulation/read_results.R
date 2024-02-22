source("simulation/utils.R")
# set up parallelization
n_threads <- 25
clus <- parallel::makeCluster(n_threads)
evq <- parallel::clusterEvalQ(clus, source("simulation/utils.R"))

# track duration
t_total <- numeric()

# get simulation settings (days, beeps, mu, phi, diff, resvar_i)
load("simulation/simulation_settings_phi_0.3.RData")
sigma_2 <- resvar_i

df_diagnostics <- data.frame(matrix(NA, nrow = 0, ncol = 15))
t_total <- system.time(
for (days_i in days) {
  for (diff_i in diff) {
    # set parameter values
    pars <- getPars(mu = mu, phi = phi,
                    diff = diff_i, sigma_2 = sigma_2)
    
    modelout <- sprintf("simulation/stan/modelout/phi0.3_iter10000/fit_days_%s_diff_%s",
                        days_i, diff_i)

    # read results
    out <- parallel::parLapplyLB(cl = clus, 1:reps,
                                 readStanResults,
                                 modelout = modelout)
    
    # res <- parallel::parLapplyLB(cl = clus, 1:reps,
    #                              getResults, out, pars)
    res <- lapply(1:reps, getResults, out, pars)
    
    # diags <- diagnostics(clus, reps, res, out, pars)
    diags <- diagnostics(reps, res, out, pars)
    diags$days <- days_i
    diags$diff <- diff_i
    df_diagnostics <- rbind(df_diagnostics, diags)
  }
}
)
save(df_diagnostics, file = "simulation/stan/results/results_phi_0.3_iter_10000.Rdata")
save(out, file = "simulation/stan/results/out_phi_0.3_iter_10000.Rdata")

parallel::stopCluster(clus)

load("simulation/stan/results/results_phi_0.3_iter_10000.Rdata")

library(ggplot2)

p <- ggplot(df_diagnostics[df_diagnostics$parameter == "diff", ]) +
  geom_hline(yintercept = 0.8) +
  geom_line(aes(x = diff, y = power, colour = as.factor(days))) +
  labs(x = expression(gamma - phi), y = "Power", colour = "Days") +
  theme_classic() +
  scale_x_continuous(limits = c(-0.3, 0.3), breaks = diff) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  viridis::scale_colour_viridis(discrete = TRUE, direction = -1)
ggsave("simulation/power_diff.png")

p <- ggplot(df_diagnostics[df_diagnostics$parameter == "gamma", ]) +
  geom_hline(yintercept = 0.8) +
  geom_line(aes(x = true, y = power_bf_3, colour = as.factor(days))) +
  labs(x = expression(gamma), y = "Power", colour = "Days") +
  theme_classic() +
  scale_x_continuous(limits = c(0.0, 0.6), breaks = seq(0, 0.6, 0.1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  viridis::scale_colour_viridis(discrete = TRUE, direction = -1) +
  theme_void() +
  theme(axis.text = element_text(size = 12, margin = margin(5, 5, 5, 5)),
        axis.title = element_text(margin = margin(5, 5, 5, 5)),
        axis.ticks = element_line(linewidth = 0.3),
        legend.position = c(.9, .3),
        plot.margin = margin(0, 5, 0, 0)) +
  geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 1,
               linewidth = 0.3, lineend = "square") +
  geom_segment(y = -Inf, yend = -Inf, x = 0, xend = 0.6,
               linewidth = 0.3, lineend = "square")

ggsave("power_gamma_0.pdf", height = 3, width = 4)

ggplot(df_diagnostics[df_diagnostics$parameter == "gamma", ]) +
  geom_line(aes(x = as.factor(days), y = absolute_bias, colour = as.factor(true), group = as.factor(true))) +
  labs(x = "Days", y = "Absolute Bias", colour = "Gamma") +
  theme_classic() +
  # scale_x_continuous(limits = c(0.0, 0.6), breaks = seq(0, 0.6, 0.1)) +
  scale_y_continuous(limits = c(0, 0.3), breaks = seq(0, 1, 0.2)) +
  viridis::scale_colour_viridis(discrete = TRUE, direction = -1)

ggplot(df_diagnostics[df_diagnostics$parameter == c("phi", "mu", "sigma_2", 'psi_2'), ]) +
  geom_bar(aes(x = as.factor(days), y = absolute_bias, fill = parameter), stat = "identity", position = "dodge") +
  labs(x = "Days", y = "Absolute Bias") +
  theme_classic()
  # scale_x_continuous(limits = c(0.0, 0.6), breaks = seq(0, 0.6, 0.1)) +
  # scale_y_continuous(limits = c(0, 0.3), breaks = seq(0, 1, 0.2))

################################################################################
## PLOT GAMMA SIMULATION

df_plot <- df_diagnostics[df_diagnostics$parameter %in% c("gamma", "diff", "diff_ct"), ]
# df_plot$estimate <- factor(df_plot$parameter, levels = c("gamma - phi", "gamma - phi ^ 7", "gamma"))
# labelPars <- c("gamma - phi", "gamma - phi ^ 7", "gamma")
labelPars <- as_labeller(c(gamma = "gamma", diff = "gamma - phi", diff_ct = "gamma - phi ^ 7"), label_parsed)
xBreaks <- function(x) {
  if (max(x) > 0.4) seq(0, 0.6, 0.1) else seq(-3, 3, 1) / 10
}
df_axis <- data.frame(parameter = unique(df_plot$parameter),
                      xmin = c(0, -0.3, 0), xmax = c(0.6, 0.3, 0.6))

df_plot$days <- factor(df_plot$days, levels = c(200, 100, 50, 25))
ggplot(df_plot) +
  facet_wrap(~ parameter, scales = "free", strip.position = "bottom", labeller = labelPars) + 
  geom_hline(yintercept = 0.8, linewidth = 0.3) +
  # geom_line(aes(x = true, y = power, colour = as.factor(days))) +
  geom_line(aes(x = true, y = power_bf_3, colour = days), linewidth = 1) +
  geom_point(aes(x = true, y = power_bf_3, colour = days, shape = days), size = 2, stroke = 1.5, fill = "white") +
  # geom_line(aes(x = true, y = power_bf_5, colour = as.factor(days)), linetype = "dashed", linewidth = 1) +
  # geom_line(aes(x = true, y = power_bf_10, colour = as.factor(days)), linetype = "dotted", linewidth = 1) +
  labs(x = "True", y = "Power", colour = "Days") +
  scale_x_continuous(breaks = xBreaks) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_shape_manual(values = 21:24, labels = c(200, 100, 50, 25), name = "legend") +
  viridis::scale_colour_viridis(discrete = TRUE, labels = c(200, 100, 50, 25), name = "legend") +
  theme_void() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(family = "sans", size = 12),
        # axis.title.y = element_text(angle = 90),
        axis.text = element_text(margin = margin(5, 5, 0, 5)),
        axis.text.y = element_text(hjust = 0.95),
        axis.title = element_text(margin = margin(5, 5, 5, 5)),
        axis.title.y = element_text(angle = 90, size = 12),
        axis.ticks = element_line(lineend = "butt",
                                  linewidth = 0.3),
        axis.ticks.length = unit(2.5, "pt"),
        strip.text = element_text(margin = margin(0, 5, 2, 5), size = 12),
        strip.placement = "outside",
        panel.spacing = unit(7.5, units = "pt"),
        plot.margin = margin(5, 5, 0, 0)) +
  geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 1,
               linewidth = 0.3, lineend = "square") +
  geom_segment(data = df_axis, y = -Inf, yend = -Inf,
               aes(x = xmin, xend = xmax), linewidth = 0.3, lineend = "square") 
ggsave("power_gamma.pdf", width = 10, height = 3)

df_phi <- df_diagnostics[df_diagnostics$parameter %in% "phi", ]
df_phi$days <- factor(df_phi$days, levels = c(200, 100, 50, 25))
ggplot(df_phi) +
  geom_hline(yintercept = 0.8, linewidth = 0.3) +
  # geom_line(aes(x = true, y = power, colour = as.factor(days))) +
  geom_line(aes(x = diff, y = coverage, colour = days), linewidth = 1) +
  geom_point(aes(x = diff, y = coverage, colour = days, shape = days), size = 2, fill = "white") +
  # geom_line(aes(x = true, y = power_bf_5, colour = as.factor(days)), linetype = "dashed", linewidth = 1) +
  # geom_line(aes(x = true, y = power_bf_10, colour = as.factor(days)), linetype = "dotted", linewidth = 1) +
  labs(x = "True", y = "Power", colour = "Days") +
  scale_x_continuous(breaks = xBreaks) +
  scale_y_continuous(limits = c(0.8, 1), breaks = seq(0, 1, 0.2)) +
  scale_shape_manual(values = 21:24, labels = c(200, 100, 50, 25), name = "legend") +
  viridis::scale_colour_viridis(discrete = TRUE, labels = c(200, 100, 50, 25), name = "legend") +
  theme_void() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(family = "sans", size = 12),
        # axis.title.y = element_text(angle = 90),
        axis.text = element_text(margin = margin(5, 5, 0, 5)),
        axis.text.y = element_text(hjust = 0.95),
        axis.title = element_text(margin = margin(5, 5, 5, 5)),
        axis.title.y = element_text(angle = 90, size = 12),
        axis.ticks = element_line(lineend = "butt",
                                  linewidth = 0.3),
        axis.ticks.length = unit(2.5, "pt"),
        strip.text = element_text(margin = margin(0, 5, 2, 5), size = 12),
        strip.placement = "outside",
        panel.spacing = unit(7.5, units = "pt"),
        plot.margin = margin(5, 5, 0, 0)) +
  geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 1,
               linewidth = 0.3, lineend = "square") +
  geom_segment(x = -0.3, xend = 0.3, y = -Inf, yend = -Inf,
               linewidth = 0.3, lineend = "square") 
ggsave("power_phi.pdf", width = 10, height = 3)


df_plot_long <- tidyr::pivot_longer(df_plot, cols = tidyr::starts_with("power_"), names_to = "criteria", values_to = "power_value")
ggplot(df_plot_long) +
  facet_wrap(~ parameter, scales = "free", strip.position = "bottom", labeller = labelPars) + 
  geom_hline(yintercept = 0.8, linewidth = 0.3) +
  # geom_line(aes(x = true, y = power, colour = as.factor(days))) +
  geom_line(aes(x = true, y = power_value, colour = as.factor(days), linetype = criteria), linewidth = 1) +
  geom_point(aes(x = true, y = power_value, colour = as.factor(days)), shape = 21, size = 2, fill = "white") +
  # geom_line(aes(x = true, y = power_bf_5, colour = as.factor(days)), linetype = "dashed", linewidth = 1) +
  # geom_line(aes(x = true, y = power_bf_10, colour = as.factor(days)), linetype = "dotted", linewidth = 1) +
  labs(x = "True", y = "Power", colour = "Days") +
  scale_x_continuous(breaks = xBreaks) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  viridis::scale_colour_viridis(discrete = TRUE, direction = -1, breaks = c(200, 100, 50, 25)) +
  theme_void() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(family = "sans", size = 12),
        # axis.title.y = element_text(angle = 90),
        axis.text = element_text(margin = margin(5, 5, 0, 5)),
        axis.text.y = element_text(hjust = 0.95),
        axis.title = element_text(margin = margin(5, 5, 5, 5)),
        axis.title.y = element_text(angle = 90, size = 12),
        axis.ticks = element_line(lineend = "butt",
                                  linewidth = 0.3),
        axis.ticks.length = unit(2.5, "pt"),
        strip.text = element_text(margin = margin(0, 5, 2, 5), size = 12),
        strip.placement = "outside",
        panel.spacing = unit(7.5, units = "pt"),
        plot.margin = margin(5, 5, 0, 0)) +
  geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 1,
               linewidth = 0.3, lineend = "square") +
  geom_segment(data = df_axis, y = -Inf, yend = -Inf,
               aes(x = xmin, xend = xmax), linewidth = 0.3, lineend = "square") 

ggplot(df_plot) +
  facet_wrap(~ parameter, scales = "free", strip.position = "bottom", labeller = labelPars) + 
  geom_hline(yintercept = 0.8, linewidth = 0.3) +
  geom_line(aes(x = true, y = power, colour = days, linetype = "CI"), linewidth = 1) +
  geom_point(aes(x = true, y = power, colour = days, shape = days), size = 2, stroke = 1.5, fill = "white") +
  geom_line(aes(x = true, y = power_bf_3, colour = days, linetype = "BF"), linewidth = 1) +
  geom_point(aes(x = true, y = power_bf_3, colour = days, shape = days), size = 2, stroke = 1.5, fill = "white") +
  # geom_line(aes(x = true, y = power_bf_5, colour = as.factor(days)), linetype = "dashed", linewidth = 1) +
  # geom_line(aes(x = true, y = power_bf_10, colour = as.factor(days)), linetype = "dotted", linewidth = 1) +
  labs(x = "True", y = "Power", colour = "Days") +
  scale_x_continuous(breaks = xBreaks) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_shape_manual(values = 21:24, labels = c(200, 100, 50, 25), name = "legend") +
  viridis::scale_colour_viridis(discrete = TRUE, labels = c(200, 100, 50, 25), name = "legend") +
  scale_linetype_manual("Line", breaks = c("CI", "BF"), values = c("CI" = "dotted", "BF" = "solid")) +
  theme_void() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(family = "sans", size = 12),
        # axis.title.y = element_text(angle = 90),
        axis.text = element_text(margin = margin(5, 5, 0, 5)),
        axis.text.y = element_text(hjust = 0.95),
        axis.title = element_text(margin = margin(5, 5, 5, 5)),
        axis.title.y = element_text(angle = 90, size = 12),
        axis.ticks = element_line(lineend = "butt",
                                  linewidth = 0.3),
        axis.ticks.length = unit(2.5, "pt"),
        strip.text = element_text(margin = margin(0, 5, 2, 5), size = 12),
        strip.placement = "outside",
        panel.spacing = unit(7.5, units = "pt"),
        plot.margin = margin(5, 5, 0, 0)) +
  geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 1,
               linewidth = 0.3, lineend = "square") +
  geom_segment(data = df_axis, y = -Inf, yend = -Inf,
               aes(x = xmin, xend = xmax), linewidth = 0.3, lineend = "square") 

ggsave("power_gamma_ci.pdf", width = 10, height = 3)

ggplot(df_plot) +
  facet_wrap(~ parameter, scales = "free", strip.position = "bottom", labeller = labelPars) + 
  geom_hline(yintercept = 0.8, linewidth = 0.3) +
  # geom_line(aes(x = true, y = power, colour = days, linetype = "CI"), linewidth = 1) +
  # geom_point(aes(x = true, y = power, colour = days, shape = days), size = 2, fill = "white") +
  geom_line(aes(x = true, y = power_bf_3, colour = days, linetype = "3"), linewidth = 1) +
  geom_point(aes(x = true, y = power_bf_3, colour = days, shape = days), size = 2, stroke = 1.5, fill = "white") +
  geom_line(aes(x = true, y = power_bf_5, colour = days, linetype = "5"), linewidth = 1) +
  geom_point(aes(x = true, y = power_bf_5, colour = days, shape = days), size = 2, stroke = 1.5, fill = "white") +
  geom_line(aes(x = true, y = power_bf_10, colour = days, linetype = "10"), linewidth = 1) +
  geom_point(aes(x = true, y = power_bf_10, colour = days, shape = days), size = 2, stroke = 1.5, fill = "white") +
  labs(x = "True", y = "Power", colour = "Days") +
  scale_x_continuous(breaks = xBreaks) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_shape_manual(values = 21:24, labels = c(200, 100, 50, 25), name = "legend") +
  viridis::scale_colour_viridis(discrete = TRUE, labels = c(200, 100, 50, 25), name = "legend") +
  scale_linetype_manual("Line", breaks = c("3", "5", "10"), values = c("3" = "solid", "5" = "dashed", "10" = "dotted")) +
  guides(linetype = guide_legend(override.aes = list(linewidth = 0.5))) +
  theme_void() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title.x = element_blank(),
        text = element_text(family = "sans", size = 12),
        # axis.title.y = element_text(angle = 90),
        axis.text = element_text(margin = margin(5, 5, 0, 5)),
        axis.text.y = element_text(hjust = 0.95),
        axis.title = element_text(margin = margin(5, 5, 5, 5)),
        axis.title.y = element_text(angle = 90, size = 12),
        axis.ticks = element_line(lineend = "butt",
                                  linewidth = 0.3),
        axis.ticks.length = unit(2.5, "pt"),
        strip.text = element_text(margin = margin(0, 5, 2, 5), size = 12),
        strip.placement = "outside",
        panel.spacing = unit(7.5, units = "pt"),
        plot.margin = margin(5, 5, 0, 0)) +
  geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 1,
               linewidth = 0.3, lineend = "square") +
  geom_segment(data = df_axis, y = -Inf, yend = -Inf,
               aes(x = xmin, xend = xmax), linewidth = 0.3, lineend = "square") 

ggsave("power_gamma_bfs.pdf", width = 10, height = 3)


ggplot(df_diagnostics[df_diagnostics$parameter == "gamma", ]) +
  geom_hline(yintercept = 0.8) +
  geom_line(aes(x = true, y = power, colour = as.factor(days))) +
  geom_line(aes(x = true, y = power_bf_3, colour = as.factor(days)), linetype = "dashed") +
  geom_line(aes(x = true, y = power_bf_10, colour = as.factor(days)), linetype = "dotted") +
  labs(x = expression(gamma), y = "Power", colour = "Days") +
  theme_classic() +
  scale_x_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, 0.1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  viridis::scale_colour_viridis(discrete = TRUE, direction = -1)
ggsave("power_gamma.pdf", width = 4, height = 3)

ggplot(df_diagnostics[df_diagnostics$parameter == "diff", ]) +
  geom_hline(yintercept = 0.8) +
  geom_line(aes(x = true, y = power, colour = as.factor(days))) +
  geom_line(aes(x = true, y = power_bf_3, colour = as.factor(days)), linetype = "dashed") +
  geom_line(aes(x = true, y = power_bf_10, colour = as.factor(days)), linetype = "dotted") +
  labs(x = expression(gamma - phi), y = "Power", colour = "Days") +
  theme_classic() +
  scale_x_continuous(limits = c(-0.3, 0.3), breaks = seq(-0.3, 0.3, 0.1), labels = seq(-3, 3, 1) / 10) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  viridis::scale_colour_viridis(discrete = TRUE, direction = -1)

ggplot(df_diagnostics[df_diagnostics$parameter == "diff_ct", ]) +
  geom_hline(yintercept = 0.8) +
  geom_line(aes(x = true, y = power, colour = as.factor(days))) +
  geom_line(aes(x = true, y = power_bf_3, colour = as.factor(days)), linetype = "dashed") +
  geom_line(aes(x = true, y = power_bf_10, colour = as.factor(days)), linetype = "dotted") +
  labs(x = expression(gamma - phi ^ 7), y = "Power", colour = "Days") +
  theme_classic() +
  scale_x_continuous(limits = c(-0.01, 0.6), breaks = seq(0, 0.6, 0.1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  viridis::scale_colour_viridis(discrete = TRUE, direction = -1)

ggplot(df_diagnostics[df_diagnostics$parameter == "diff_ct", ]) +
  geom_hline(yintercept = 0.8) +
  geom_line(aes(x = true, y = power, colour = as.factor(days))) +
  labs(x = expression(gamma - phi^6), y = "Power", colour = "Days") +
  theme_classic() +
  # scale_x_continuous(limits = c(-0.8, -0.1), breaks = seq(-0.8, -0.1, 0.1)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  viridis::scale_colour_viridis(discrete = TRUE, direction = -1)

ggplot(df_diagnostics[df_diagnostics$parameter == "phi", ]) +
  geom_hline(yintercept = 0.8) +
  ylim(c(0, 1)) +
  geom_line(aes(x = diff, y = separate_intervals, colour = as.factor(days)))

ggplot(df_diagnostics[df_diagnostics$parameter == "gamma", ]) +
  geom_hline(yintercept = 0.8) +
  geom_line(aes(x = true, y = power, colour = as.factor(days)))

ggplot(df_diagnostics[df_diagnostics$parameter == "diff", ]) +
  geom_line(aes(x = diff, y = coverage, colour = as.factor(days)))

ggplot(df_diagnostics[df_diagnostics$parameter == "diff", ]) +
  geom_line(aes(x = diff, y = bias, colour = as.factor(days)))

ggplot(df_diagnostics[df_diagnostics$parameter == "diff", ]) +
  geom_line(aes(x = diff, y = absolute_bias, colour = as.factor(days)))


################################################################################
## PMPs plot

# dfProbs$item <- names(bftest)

# dfLong <- tidyr::pivot_longer(dfProbs, cols = 1:4)

df_plot <- df_diagnostics[df_diagnostics$parameter %in% c("gamma", "diff", "diff_ct"), ]
df_bar <- df_plot[, c("parameter", "pref_model", "days", "diff")]

for (i in days) {
  for (j in diff) {
    pref <- 1 - sum(subset(df_bar, df_bar$days == i & df_bar$diff == j, "pref_model"))
  df_bar[nrow(df_bar) + 1, ] <- data.frame("other", pref, i, j)
  }
}

df_bar$days <- factor(df_bar$days)
df_bar$model <- factor(df_bar$parameter)
levels(df_bar$model) <- c("pS", "pC", "pP", "pD")
df_bar$model <- factor(df_bar$model, levels = c("pP", "pS", "pC", "pD"))
# levels(df_bar$model) <-  c("pP", "pS", "pC", "pD")

# dfLong$item2 <- factor(dfLong$item, levels = orderItems)
# dfLong$category <- rep(itemCategory(), each = 4)
# dfLong$category <- factor(dfLong$category, levels = c("NA", "PA", "unrest", "self-esteem", "physical"))
# dfLong$item2 <- factor(dfLong$item2, levels = df$item[order(df$category, df$gamma, df$item, decreasing = TRUE)])
# dfLong$preferred <- rep(bestModel, each = 4)
# dfLong$preferred <- factor(dfLong$preferred, labels = c("Pause", "Stop / Continue", "Stop / Continue", "Different"))

df_bar$label <- paste("gamma ==", df_bar$diff + 0.3)

ggplot(df_bar) +
  facet_grid(~ label, scales = "free_x", space = "free", labeller = label_parsed) +
  geom_bar(aes(x = days, y = pref_model, fill = model), stat = "identity") +
  viridis::scale_fill_viridis(discrete = TRUE, labels = c("Hp", "Hs", "Hc", "Hd")) +
  theme_void() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1, vjust = 2, size = 12),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.title = element_blank(),
        text = element_text(family = "sans", size = 12),
        # axis.title.y = element_text(angle = 90),
        axis.text = element_text(margin = margin(5, 5, 5, 5)),
        axis.text.y = element_text(hjust = 0.95),
        axis.ticks.y = element_line(lineend = "butt",
                                    linewidth = 0.3),
        axis.ticks.length = unit(2.5, "pt"),
        strip.text = element_text(margin = margin(5, 5, 5, 5), size = 12),
        panel.spacing = unit(7.5, units = "pt"),
        plot.margin = margin(0, 5, 0, 0))
ggsave("posterior_bar_simulation.pdf", height = 4, width = 10)

