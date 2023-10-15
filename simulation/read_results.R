# set up parallelization
n_threads <- 3
clus <- parallel::makeCluster(n_threads)
evq <- parallel::clusterEvalQ(clus, source("simulation/utils.R"))

# track duration
t_total <- numeric()

# get simulation settings (days, beeps, mu, phi, delta, resvar_i)
load("simulation/simulation_settings.RData")

df_diagnostics <- data.frame(matrix(NA, nrow = 0, ncol = 12))
for (days_i in days) {
  for (delta_i in delta) {
    # set parameter values
    pars <- getPars(mu = mu, phi = phi,
                    delta = delta_i, resvar_i = resvar_i)
    
    modelout <- sprintf("simulation/stan/modelout/fit_days_%s_delta_%s",
                        days_i, delta_i)
    
    # read results
    res <- parallel::parLapplyLB(cl = clus, 1:reps,
                                 readStanResults,
                                 modelout = modelout, pars = pars)
    
    diags <- diagnostics(reps, res, pars)
    diags$days <- days_i
    diags$delta <- delta_i
    df_diagnostics <- rbind(df_diagnostics, diags)
  }
}

library(ggplot2)

ggplot(df_diagnostics[df_diagnostics$parameter == "delta", ]) +
  geom_hline(yintercept = 0.8) +
  ylim(c(0, 1)) +
  geom_line(aes(x = delta, y = power, colour = as.factor(days)))

ggplot(df_diagnostics[df_diagnostics$parameter == "delta_2", ]) +
  geom_hline(yintercept = 0.8) +
  ylim(c(0, 1)) +
  geom_line(aes(x = delta, y = power, colour = as.factor(days)))

ggplot(df_diagnostics[df_diagnostics$parameter == "phi", ]) +
  geom_hline(yintercept = 0.8) +
  ylim(c(0, 1)) +
  geom_line(aes(x = delta, y = separate_intervals, colour = as.factor(days)))

ggplot(df_diagnostics[df_diagnostics$parameter == "gamma", ]) +
  geom_hline(yintercept = 0.8) +
  geom_line(aes(x = true, y = power, colour = as.factor(days)))

ggplot(df_diagnostics[df_diagnostics$parameter == "delta", ]) +
  geom_line(aes(x = delta, y = coverage, colour = as.factor(days)))

ggplot(df_diagnostics[df_diagnostics$parameter == "delta", ]) +
  geom_line(aes(x = delta, y = bias, colour = as.factor(days)))

ggplot(df_diagnostics[df_diagnostics$parameter == "delta", ]) +
  geom_line(aes(x = delta, y = absolute_bias, colour = as.factor(days)))
