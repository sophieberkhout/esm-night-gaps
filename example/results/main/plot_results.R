source("example/results/functions_results.R")
load("example/results/main/objects.Rdata")

library(ggplot2)

p_gammas <- plotGammas(df_pars)
ggsave("example/results/main/plots/estimated_gammas.pdf", p_gammas,
       width = 12, height = 3)

p_pars <- plotPars(df_pars)
ggsave("example/results/main/plots/parameter_uncertainty.pdf", p_pars,
       width = 12, height = 12)

p_phi_gamma <- plotScatterPhiGamma(df_pars)
ggsave("example/results/main/plots/estphigamma.pdf", p_phi_gamma,
       width = 4, height = 3)

p_pmps <- plotPMPs(df_pmps)
ggsave("example/results/main/plots/posterior_bar.pdf",
       p_pmps, height = 4, width = 12)

p_prior_posterior <- plotPriorPosterior(df_posteriors, df_bayes_factors)
ggsave("example/results/main/plots/bayesfactors.pdf", p_prior_posterior,
       height = 12, width = 12)
