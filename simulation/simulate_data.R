source("simulation/utils.R")

# set up parallelization
n_threads <- 25
clus <- parallel::makeCluster(n_threads)
evq <- parallel::clusterEvalQ(clus, source("simulation/utils.R"))

# track duration
t_total <- numeric()

# number of replications
reps <- 1000
days <- c(25, 50, 100, 200)
beeps <- 10
burnin <- 50

mu <- 4
phi <- 0.3
delta <- seq(-3, 3, 1) / 10
resvar_i <- 1

save(reps, days, beeps, mu, phi, delta, resvar_i,
     file = "simulation/simulation_settings.RData")

set.seed(1844)
for (days_i in days) {
  for (delta_i in delta) {
    # set parameter values
    pars <- getPars(mu = mu, phi = phi,
                    delta = delta_i, resvar_i = resvar_i)
    
    # simulate data
    t_fit <- system.time(
      parallel::parLapplyLB(cl = clus, 1:reps,
                            simulateData,
                            D = days_i, B = beeps, burnin = burnin, pars = pars,
                            file = "simulation/data/")
    )
    
    t_total <- c(t_total, t_fit["elapsed"])
  }
}

parallel::stopCluster(clus)
