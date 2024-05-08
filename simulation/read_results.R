source("simulation/utils.R")
# set up parallelization
n_threads <- 25
clus <- parallel::makeCluster(n_threads)
evq <- parallel::clusterEvalQ(clus, source("simulation/utils.R"))

# track duration
t_total <- numeric()

# get simulation settings (days, beeps, mu, phi, diff, sigma_2)
load("simulation/simulation_settings_phi_0.3.RData")

df_diagnostics_0.3 <- data.frame(matrix(NA, nrow = 0, ncol = 15))
t_total <- system.time(
  for (days_i in days) {
    for (diff_i in diff) {
      # set parameter values
      pars <- getPars(mu = mu, phi = phi,
                      diff = diff_i, sigma_2 = sigma_2)
      
      # where results are saved
      modelout <- sprintf(
        "simulation/stan/modelout/phi_0.3/fit_days_%s_diff_%s", days_i, diff_i
      )
      
      # read results
      out <- parallel::parLapplyLB(cl = clus, 1:reps,
                                   readStanResults,
                                   modelout = modelout)
      
      # get results in nice format
      res <- lapply(1:reps, getResults, out, pars)
      
      # compute diagnostics
      diags <- diagnostics(reps, res, out, pars)
      diags$days <- days_i
      diags$diff <- diff_i
      
      # combine results
      df_diagnostics_0.3 <- rbind(df_diagnostics_0.3, diags)
    }
  }
)
save(df_diagnostics_0.3, file = "simulation/results/results_phi_0.3.Rdata")

parallel::stopCluster(clus)
