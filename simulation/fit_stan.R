# set up parallelization
n_threads <- 25
clus <- parallel::makeCluster(n_threads)
evq <- parallel::clusterEvalQ(clus, source("simulation/utils.R"))
exp <- parallel::clusterExport(clus, "mod")

# compile stan model
mod <- rstan::stan_model("simulation/stan/ar_beep_night.stan")

# track duration
t_total <- numeric()

# get simulation settings (days, beeps, mu, phi, delta, resvar_i)
load("simulation/simulation_settings.RData")

for (days_i in days) {
  for (delta_i in delta) {
    stan_dat <- parallel::parLapplyLB(clus, 1:reps, stanData,
                                      file = "simulation/data/",
                                      D = days_i, B = beeps, delta = delta_i)
    
    # where to save results
    modelout <- sprintf("simulation/stan/modelout/fit_days_%s_delta_%s",
                        days_i, delta_i)

    # fit model
    t_fit <- system.time(
      parallel::parLapplyLB(cl = clus, 1:reps, fitModel,
                            mod, modelout, stan_dat, seed = 1844)
    )
    
    t_total <- c(t_total, t_fit["elapsed"])
  }
}

parallel::stopCluster(clus)
