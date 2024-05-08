source("example/results/functions_results.R")

dat <- read.csv("example/data/data.csv")

items <- names(dat)[grepl("mood_|pat_|phy_|se_", names(dat))]

n_threads <- length(items)
clus <- parallel::makeCluster(n_threads)
exp <- parallel::clusterExport(clus, "items")

# mean of gamma in main result is 0.29
stan_code <- getStanCode(prior_gamma = "normal(0.3, sqrt(0.5))")
mod <- rstan::stan_model(model_code = stan_code)

dat_stan <-  parallel::parLapplyLB(cl = clus, 1:length(items),
                                   stanData, dat, items)

pars <- c("mu", "phi", "gamma", "sigma_2", "psi_2", "diff_phi", "diff_phi_ct")

modelout <- "example/results/prior nonzero/out/"
t_fit <- system.time(
  parallel::parLapplyLB(cl = clus, 1:length(items), fitModel,
                        mod, modelout, dat_stan, pars, items, seed = 1)
)

parallel::stopCluster(clus)
