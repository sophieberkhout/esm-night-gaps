library(ggplot2)

source("simulation/utils.R")

# load simulation results of both phi = 0.3 and phi = 0.5
load("simulation/results/results_phi_0.3.Rdata")
load("simulation/results/results_phi_0.5.Rdata")

# parameter recovery diagnostics for phi = 0.3 only
p_recovery <- plotParameterRecovery(df_diagnostics_0.3)
ggsave("simulation/results/plots/parameter_recovery.pdf", p_recovery,
       width = 9, height = 4)

# combine results
df_both <- getDataFrameBoth(df_diagnostics_0.3, df_diagnostics_0.5)

# plot proportion of selecting the different model for all values of gamma
# and per sample size

## based on BFmd < 1/3
p_bfs_3 <- plotBayesFactors(df_both)
ggsave("simulation/results/plots/model_selection_bf_3.pdf", p_bfs_3,
       width = 8, height = 4)

## based on BFmd < 1/10
p_bfs_10 <- plotBayesFactors(df_both, method = "BF_10")
ggsave("simulation/results/plots/model_selection_bf_10.pdf", p_bfs_10,
       width = 8, height = 4)

## based on point of interest being outside of credible interval
p_ci <- plotBayesFactors(df_both, method = "CI")
ggsave("simulation/results/plots/model_selection_ci.pdf", p_ci,
       width = 8, height = 4)

# plot proportion of model with most probability for all values of gamma
# and per sample size
p_pmps <- plotProbabilities(df_both)
ggsave("simulation/results/plots/model_selection_pmp.pdf", p_pmps,
       height = 4, width = 8)

# compute ranges of posterior model probabilities for
## pauses model when gamma equals phi
range(subset(df_both, parameter == "gamma" & true == phi, pref_model))

## stops model when gamma equals zero
range(subset(df_both, parameter == "diff" & diff + phi == 0, pref_model))

## continues model when gamma equals zero
range(subset(df_both, parameter == "diff_ct" & diff + phi == 0, pref_model))
