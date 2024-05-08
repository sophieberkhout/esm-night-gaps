source("example/results/functions_results.R")

prior <- c(0, 0.5)

dat <- read.csv("example/data/data_uncorrected.csv")

items <- names(dat)[grepl("mood_|pat_|phy_|se_", names(dat))]

out <- lapply(1:length(items), readStanResults,
              modelout = "example/results/interval uncorrected/out/",
              items = items)

pars <- c("mu", "phi", "gamma", "sigma_2", "psi_2", "diff_phi", "diff_phi_ct")
res <- lapply(1:length(items), getResults, out = out, pars = pars)
names(res) <- prettyNames()

logSpline <- lapply(1:length(items), fitLogSpline, out)
posterior_density <- lapply(1:length(items), getDensity, logSpline)
bfs  <- lapply(1:length(items), getBF, out, prior, logSpline, delta_t = 7)
pmps <- lapply(1:length(items), calcPostModProbs, bfs)

df_pars <- dfPars(res)
df_posteriors <- dfDensity(posterior_density, prior, res)
df_bayes_factors <- dfBayesFactors(bfs)
df_pmps <- dfPMPs(pmps)

save(df_pars, df_posteriors, df_bayes_factors, df_pmps,
     file = "example/results/interval uncorrected/objects.Rdata")
