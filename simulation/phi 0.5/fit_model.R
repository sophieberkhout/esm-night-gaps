source("simulation/utils.R")
# set up parallelization
n_threads <- 25
clus <- parallel::makeCluster(n_threads)
evq <- parallel::clusterEvalQ(clus, source("simulation/utils.R"))

# compile stan model
mod <- rstan::stan_model("simulation/stan/different_model.stan")

# track duration
t_total <- numeric()

# get simulation settings (days, beeps, mu, phi, diff, resvar_i)
load("simulation/phi 0.5/simulation_settings_phi_0.5.RData")

for (days_i in days) {
  for (diff_i in diff) {
    stan_dat <- parallel::parLapplyLB(clus, 1:reps, stanData,
                                      file = "simulation/data/phi_0.5/",
                                      D = days_i, B = beeps, diff = diff_i)
    
    # where to save results
    modelout <- sprintf("simulation/stan/modelout/phi_0.5/fit_days_%s_diff_%s",
                        days_i, diff_i)

    # fit model 
    t_fit <- system.time(
      parallel::parLapplyLB(cl = clus, 1:reps, fitModel,
                            mod, modelout, stan_dat, seed = 1844,
                            iter = 11000, warmup = 1000)
    )
    
    t_total <- c(t_total, t_fit["elapsed"])
  }
}

parallel::stopCluster(clus)
