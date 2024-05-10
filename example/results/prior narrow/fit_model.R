source("example/results/functions_results.R")

dat <- read.csv("example/data/data.csv")

# only items of interest
items <- names(dat)[grepl("mood_|pat_|phy_|se_", names(dat))]

# parallelize
n_threads <- length(items)
clus <- parallel::makeCluster(n_threads)
exp <- parallel::clusterExport(clus, "items")

# compile Stan model
stan_code <- getStanCode(prior_gamma = "normal(0, sqrt(0.1))")
mod <- rstan::stan_model(model_code = stan_code)

# put data in right format
dat_stan <-  parallel::parLapplyLB(cl = clus, 1:length(items),
                                   stanData, dat, items)

# save pars
pars <- c("mu", "phi", "gamma", "sigma_2", "psi_2", "diff_phi", "diff_phi_ct")

# fit model
modelout <- "example/results/prior narrow/out/"
t_fit <- system.time(
  parallel::parLapplyLB(cl = clus, 1:length(items), fitModel,
                        mod, modelout, dat_stan, pars, items, seed = 1)
)

parallel::stopCluster(clus)
