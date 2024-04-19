simulateData <- function (r, D, B, burnin, pars, file) {
  out <- tryCatch({
    # create empty y matrix
    y <- matrix(0, nrow = D + burnin, ncol = B)
    
    # create innovation matrix, with the different residual variances
    e <- y
    e[, 1] <- rnorm(D + burnin, 0, sqrt(pars$psi_2))
    e[, 2:B] <- rnorm((D + burnin) * (B - 1), 0, sqrt(pars$sigma_2))
    
    # simulate data
    for(i in 2:(D + burnin)) {
      y[i, 1] <- pars$mu + pars$gamma * (y[i - 1, B] - pars$mu) + e[i, 1]
      for(j in 2:B) {
        y[i, j] <- pars$mu + pars$phi * (y[i, j - 1] - pars$mu) + e[i, j]
      }
    }
    
    # remove burnin
    y <- y[-(1:burnin), ]
    
    # matrix to long format
    y_long <- tidyr::pivot_longer(as.data.frame(y), cols = everything(),
                                  names_to = "beep", names_prefix = "V",
                                  values_to = "y")
    cbind(day = rep(1:D, each = B), y_long)
  }, error = function(e) e)
  
  saveRDS(
    out,
    file = sprintf("%sdat_days_%s_diff_%s_r_%s.rds", file, D, pars$diff, r)
  )
}

stanData <- function(r, file, D, B, diff) {
  out <- tryCatch({
    dat <- readRDS(sprintf("%sdat_days_%s_diff_%s_r_%s.rds",
                    file, D, diff, r))
    # get indices for the beeps and first beep
    N <- B * D
    idx <- 1:N
    d1 <- seq(B + 1, N, B)
    db <- idx[-c(1, d1)]
  
    # put data in stan format
    stan_dat <- list(
      N = N, D = D, B = length(db),
      db1 = d1, dbi = db,
      y = dat$y
    )
    stan_dat
  }, error = function(e) return(NULL))
  return(out)
}

getPars <- function (mu, phi, diff, sigma_2) {
  df <- data.frame(mu = mu, phi = phi,
                   gamma = diff + phi, sigma_2 = sigma_2)
  var_i <- sigma_2 / (1 - phi ^ 2)
  df$psi_2 <- var_i * (1 - df$gamma ^ 2)
  df$diff <- df$gamma - phi
  df$diff_ct <- df$gamma - phi ^ 7
  return(df)
}

fitModel <- function (r, mod, modelout = "stan/modelout/", dat, seed, iter, warmup) {
  out <- tryCatch({
    rstan::sampling(mod, data = dat[[r]], seed = seed,
                    iter = iter, warmup = warmup, save_warmup = FALSE)
  }, error = function(e) e)
  
  saveRDS(
    out,
    file = sprintf("%s_r_%s.rds", modelout, r)
  )
}

readStanResults <- function (r, modelout = "stan/modelout/") {
  out <- tryCatch({
    readRDS(sprintf("%s_r_%s.rds", modelout, r))
  }, error = function(e) return(NULL))
  
  return(out)
}

getResults <- function (r, out, pars) {
  res <- tryCatch({
    # out <- readRDS(sprintf("%s_r_%s.rds", modelout, r))
    
    sfit <- rstan::summary(out[[r]])$summary
    est <- sfit[names(pars), c("50%", "sd", "2.5%", "97.5%")]
    est <- as.data.frame(est)
    names(est) <- c("median", "sd", "lower", "upper")
    est
  }, error = function(e) return(NULL))
  
  return(res)
}

# diagnostics <- function (clus, reps, res, out, pars) {
#   true <- as.numeric(pars)
#   deviance <- parallel::parSapply(cl = clus, 1:reps, .deviance,
#                                   res = res, true = true)
#   
#   bias <- parallel::parApply(cl = clus, deviance, 1, mean)
#   absolute_bias <- parallel::parApply(cl = clus, abs(deviance), 1, mean)
#   relative_bias <- numeric(length(true))
#   for (i in 1:length(true)) {
#     if (true[i] != 0) {
#       relative_bias[i] <- bias[i] / true[i]
#     } else {
#       relative_bias[i] <- NA
#     }
#   }
#   
#   mse <- parallel::parApply(cl = clus, deviance ^ 2, 1, mean)
#   
#   medians <- parallel::parSapply(cl = clus, 1:reps, .median, res = res)
#   empirical_SE <- parallel::parApply(cl = clus, medians, 1, sd)
#   
#   coverage <- parallel::parSapply(cl = clus, 1:reps, .coverage,
#                                   res = res, true = true)
#   
#   coverage_rate <- parallel::parApply(cl = clus, coverage, 1, .proportion)
#   
#   nonzero <- parallel::parSapply(cl = clus, 1:reps, .nonZeroInterval, res = res)
#   power <- parallel::parApply(cl = clus, nonzero, 1, .proportion)
#   
#   separate_intervals <- parallel::parSapply(cl = clus, 1:reps,
#                                             .differenceIntervals, res = res)
#   separate_intervals <- parallel::parApply(cl = clus, separate_intervals, 1,
#                                            .proportion)
#   
#   log_spline <- parallel::parLapplyLB(cl = clus, 1:reps, fitLogSpline, out)
#   bfs <- parallel::parLapplyLB(cl = clus, 1:reps, getBF, res, log_spline)
#   
#   min_bf_3    <- parallel::parSapply(cl = clus, 1:reps, minBF, bfs, min = 3)
#   power_bf_3  <- parallel::parApply(cl = clus, min_bf_3, 1, .proportion)
#   min_bf_5    <- parallel::parSapply(cl = clus, 1:reps, minBF, bfs, min = 5)
#   power_bf_5  <- parallel::parApply(cl = clus, min_bf_5, 1, .proportion)
#   min_bf_10   <- parallel::parSapply(cl = clus, 1:reps, minBF, bfs, min = 10)
#   power_bf_10 <- parallel::parApply(cl = clus, min_bf_10, 1, .proportion)
#   
#   return(
#     data.frame(parameter = names(pars),
#                true = true,
#                bias = bias,
#                absolute_bias = absolute_bias, relative_bias = relative_bias,
#                empirical_SE = empirical_SE, mse = mse,
#                separate_intervals = separate_intervals,
#                power_bf_3 = power_bf_3,
#                power_bf_5 = power_bf_5,
#                power_bf_10 = power_bf_10,
#                coverage = coverage_rate, power = power)
#   )
# }

diagnostics <- function (reps, res, out, pars) {
  true <- as.numeric(pars)
  deviance <- sapply(1:reps, .deviance, res = res, true = true)

  bias <- apply(deviance, 1, mean)
  absolute_bias <- apply(abs(deviance), 1, mean)
  relative_bias <- numeric(length(true))
  for (i in 1:length(true)) {
    if (true[i] != 0) {
      relative_bias[i] <- bias[i] / true[i]
    } else {
      relative_bias[i] <- NA
    }
  }

  mse <- apply(deviance ^ 2, 1, mean)
  mae <- absolute_bias / reps
  
  medians <- sapply(1:reps, .median, res = res)
  empirical_SE <- apply(medians, 1, sd)

  coverage <- sapply(1:reps, .coverage, res = res, true = true)

  coverage_rate <- apply(coverage, 1, .proportion)

  nonzero <- sapply(1:reps, .nonZeroInterval, res = res)
  power <- apply(nonzero, 1, .proportion)

  separate_intervals <- sapply(1:reps, .differenceIntervals, res = res)
  separate_intervals <- apply(separate_intervals, 1, .proportion)

  log_spline <- lapply(1:reps, fitLogSpline, out)
  bfs <- lapply(1:reps, getBF, res, log_spline)
  min_bf_3 <- sapply(1:reps, minBF, bfs, min = 3)
  power_bf_3 <- apply(min_bf_3, 1, .proportion)
  min_bf_5 <- sapply(1:reps, minBF, bfs, min = 5)
  power_bf_5 <- apply(min_bf_5, 1, .proportion)
  min_bf_10 <- sapply(1:reps, minBF, bfs, min = 10)
  power_bf_10 <- apply(min_bf_10, 1, .proportion)
  
  best_model <- sapply(1:reps, calcPostModProbs, bfs)
  pref_model <- apply(best_model, 1, .proportion)

  return(
    data.frame(parameter = names(pars),
               true = true,
               bias = bias,
               absolute_bias = absolute_bias, relative_bias = relative_bias,
               empirical_SE = empirical_SE, mse = mse, mae = mae,
               separate_intervals = separate_intervals,
               power_bf_3 = power_bf_3,
               power_bf_5 = power_bf_5,
               power_bf_10 = power_bf_10,
               pref_model = pref_model,
               coverage = coverage_rate, power = power)
  )
}

.deviance <- function (r, res, true) {
  out <- res[[r]]$median - true
  return(out)
}

.median <- function (r, res) {
  out <- res[[r]]$median
  return(out)
}

.coverage <- function (r, res, true) {
  out <- res[[r]]$lower <= true & true <= res[[r]]$upper
  return(out)
}

.nonZeroInterval <- function (r, res) {
  out <- (res[[r]]$lower < 0 & res[[r]]$upper < 0) | (res[[r]]$lower > 0 & res[[r]]$upper > 0)
  return(out)
}

.proportion <- function (x) {
  out <- sum(x) / length(x)
  return(out)
}

.differenceIntervals <- function(r, res) {
  out <- res[[r]]["phi", "lower"] > res[[r]]["gamma", "upper"] | res[[r]]["phi", "upper"] < res[[r]]["gamma", "lower"]
  out <- c(NA, out, rep(NA, 5))
  return(out)
}


getBF <- function(r, res, fit) {
  out <- tryCatch({
    posterior_d_0 <- logspline::dlogspline(0, fit[[r]])
    BF_0 <- posterior_d_0 / dnorm(0, 0, sqrt(0.5))
    
    phi <- res[[r]][row.names(res[[r]]) == "phi", "median"]
    
    posterior_d_phi <- logspline::dlogspline(phi, fit[[r]])
    BF_phi <- posterior_d_phi / dnorm(phi, 0, sqrt(0.5))
    
    posterior_d_phict <- logspline::dlogspline(phi ^ 7, fit[[r]])
    BF_phict <- posterior_d_phict / dnorm(phi ^ 7, 0, sqrt(0.5))
    
    data.frame(phi = phi, phict = phi ^ 7,
               BF_0 = BF_0, BF_phi = BF_phi, BF_phict = BF_phict)
  }, error = function(e) return(NULL))
  
  return(out)
}

fitLogSpline <- function(r, out) {
  out <- tryCatch({
    posterior_samples <- rstan::extract(out[[r]], "gamma")$gamma
    fit <- logspline::logspline(posterior_samples)
    fit
  }, error = function(e) return(NULL))
  
  return(out)
}

minBF <- function(r, bfs, min) {
  min_BF_0 <- 1 / bfs[[r]]$BF_0 > min
  min_BF_phi <- 1 / bfs[[r]]$BF_phi > min
  min_BF_phict <- 1 / bfs[[r]]$BF_phict > min
  
  out <- c(NA, NA, min_BF_0, NA, NA, min_BF_phi, min_BF_phict)
}

postModelProb <- function (BFpd, BFsd, BFcd) {
  # pause, stop, continue, different
  pD <- 0.25 / (BFpd * 0.25 + BFsd * 0.25 + BFcd * 0.25 + 0.25)
  pS <- BFsd * 0.25 / (BFpd * 0.25 + BFsd * 0.25 + BFcd * 0.25 + 0.25)
  pP <- BFpd * 0.25 / (BFpd * 0.25 + BFsd * 0.25 + BFcd * 0.25 + 0.25)
  pC <- BFcd * 0.25 / (BFpd * 0.25 + BFsd * 0.25 + BFcd * 0.25 + 0.25)
  
  df <- data.frame(pP, pS, pC, pD)
  return(df)
}

calcPostModProbs <- function (r, bfs) {
  pmps <- postModelProb(bfs[[r]]$BF_phi, bfs[[r]]$BF_0, bfs[[r]]$BF_phict)
  pref_paus <- which.max(pmps) == 1
  pref_stop <- which.max(pmps) == 2
  pref_cont <- which.max(pmps) == 3
  # pref_diff <- which.max(pmps) == 4
  # should probably change order
  out <- c(NA, NA, pref_paus, NA, NA, pref_stop, pref_cont)
}
