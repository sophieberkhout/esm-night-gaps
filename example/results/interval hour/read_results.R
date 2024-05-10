source("example/results/functions_results.R")

# prior for bf computation
prior <- c(0, 0.5)

# get item names
dat <- read.csv("example/data/data_60min.csv")
items <- names(dat)[grepl("mood_|pat_|phy_|se_", names(dat))]

# read results
out <- lapply(1:length(items), readStanResults,
              modelout = "example/results/interval hour/out/", items = items)

# pars of interest
pars <- c("mu", "phi", "gamma", "sigma_2", "psi_2", "diff_phi", "diff_phi_ct")

# put results in nice format
res <- lapply(1:length(items), getResults, out = out, pars = pars)
names(res) <- prettyNames()

# compute Savage-Dickey density ratio and posterior model probabilities
logSpline <- lapply(1:length(items), fitLogSpline, out)
posterior_density <- lapply(1:length(items), getDensity, logSpline)
bfs  <- lapply(1:length(items), getBF, out, prior, logSpline, delta_t = 9)
pmps <- lapply(1:length(items), calcPostModProbs, bfs)

# put results in data frames for plotting
df_pars <- dfPars(res)
df_posteriors <- dfDensity(posterior_density, prior, res)
df_bayes_factors <- dfBayesFactors(bfs)
df_pmps <- dfPMPs(pmps)

# save dfs
save(df_pars, df_posteriors, df_bayes_factors, df_pmps,
     file = "example/results/interval hour/objects.Rdata")
