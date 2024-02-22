source("data groot/utils.R")
load("data groot/results/prior wide/objects.Rdata")

library(ggplot2)

p_gammas <- plotGammas(df_pars)
ggsave("data groot/results/prior wide/plots/estimated_gammas.pdf",
       p_gammas, width = 12, height = 3)

p_pars <- plotPars(df_pars)
ggsave("data groot/results/prior wide/plots/parameter_uncertainty.pdf",
       p_pars, width = 12, height = 12)

p_phi_gamma <- plotScatterPhiGamma(df_pars)
ggsave("data groot/results/prior wide/plots/estphigamma.pdf",
       p_phi_gamma, width = 4, height = 3)

p_pmps <- plotPMPs(df_pmps)
ggsave("data groot/results/prior wide/plots/posterior_bar.pdf",
       p_pmps, height = 4, width = 12)

p_prior_posterior <- plotPriorPosterior(df_posteriors, df_bayes_factors)
ggsave("data groot/results/prior wide/plots/bayesfactors.pdf",
       p_prior_posterior, height = 12, width = 12)
