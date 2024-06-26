source("example/results/functions_results.R")
load("example/results/interval hour/objects.Rdata")

library(ggplot2)

delta_t <- 9

p_gammas <- plotGammas(df_pars)
ggsave("example/results/interval hour/plots/estimated_gammas.pdf",
       p_gammas, width = 12, height = 3)

p_pars <- plotPars(df_pars, delta_t)
ggsave("example/results/interval hour/plots/parameter_uncertainty.pdf",
       p_pars, width = 12, height = 12)

p_phi_gamma <- plotScatterPhiGamma(df_pars, delta_t)
ggsave("example/results/interval hour/plots/estphigamma.pdf",
       p_phi_gamma, width = 4, height = 3)

p_pmps <- plotPMPs(df_pmps)
ggsave("example/results/interval hour/plots/posterior_bar.pdf",
       p_pmps, height = 4, width = 12)

p_prior_posterior <- plotPriorPosterior(df_posteriors, df_bayes_factors, delta_t)
ggsave("example/results/interval hour/plots/bayesfactors.pdf",
       p_prior_posterior, height = 12, width = 12)
