library(ggplot2)

source("simulation/utils.R")

load("simulation/results/results_phi_0.3.Rdata")
load("simulation/results/results_phi_0.5.Rdata")

p_recovery <- plotParameterRecovery(df_diagnostics_0.3)
ggsave("simulation/results/plots/simulation_estimates.pdf", p_recovery,
       width = 9, height = 4)

df_both <- getDataFrameBoth(df_diagnostics_0.3, df_diagnostics_0.5)

p_power <- plotPower(df_both)
ggsave("simulation/results/plots/power_both.pdf", p_power,
       width = 8, height = 4)

p_power_ci <- plotPower(df_both, method = "CI")
ggsave("simulation/results/plots/power_gamma_ci.pdf", p_power_ci,
       width = 8, height = 4)

p_power_bfs <- plotPower(df_both, method = "BF_10")
ggsave("simulation/results/plots/power_gamma_bfs.pdf", p_power_bfs,
       width = 8, height = 4)

p_pmps <- plotProbabilities(df_both)
ggsave("simulation/results/plotsposterior_bar_simulation_both.pdf", p_pmps,
       height = 4, width = 8)
