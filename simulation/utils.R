simulateData <- function (r, D, B, burnin, pars, file) {
  out <- tryCatch({
    # create empty y matrix
    y <- matrix(0, nrow = D + burnin, ncol = B)
    
    # create innovation matrix, with the different residual variances
    e <- y
    e[, 1] <- rnorm(D + burnin, 0, sqrt(pars$resvar_1))
    e[, 2:B] <- rnorm((D + burnin) * (B - 1), 0, sqrt(pars$resvar_i))
    
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
    file = sprintf("%sdat_days_%s_delta_%s_r_%s.rds", file, D, pars$delta, r)
  )
}

stanData <- function(r, file, D, B, delta) {
  out <- tryCatch({
    dat <- readRDS(sprintf("%sdat_days_%s_delta_%s_r_%s.rds",
                    file, D, delta, r))
    # get indices for the beeps and first beep
    N <- B * D
    idx <- 1:N
    b1 <- seq(B + 1, N, B)
    bi <- idx[-c(1, b1)]
  
    # put data in stan format
    stan_dat <- list(
      N = N, D = D, y = dat$y,
      bi = bi, bi_lag = bi - 1,
      b1 = b1, b1_lag = b1 - 1
    )
    stan_dat
  }, error = function(e) return(NULL))
  return(out)
}

getPars <- function (mu, phi, delta, resvar_i) {
  df <- data.frame(mu = mu, phi = phi,
                   gamma = phi - delta, resvar_i = resvar_i)
  var_i <- resvar_i / (1 - phi ^ 2)
  df$resvar_1 <- var_i * (1 - df$gamma ^ 2)
  df$delta <- phi - df$gamma
  df$delta_2 <- phi ^ 6 - df$gamma
  return(df)
}

fitModel <- function (r, mod, modelout = "stan/modelout/", dat, seed) {
  out <- tryCatch({
    rstan::sampling(mod, data = dat[[r]], seed = seed)
  }, error = function(e) e)
  
  saveRDS(
    out,
    file = sprintf("%s_r_%s.rds", modelout, r)
  )
}

readStanResults <- function (r, modelout = "stan/modelout/", pars) {
  out <- tryCatch({
    out <- readRDS(sprintf("%s_r_%s.rds", modelout, r))
    
    sfit <- rstan::summary(out)$summary
    est <- sfit[names(pars), c("50%", "sd", "2.5%", "97.5%")]
    est <- as.data.frame(est)
    names(est) <- c("median", "sd", "lower", "upper")
    est
  }, error = function(e) return(NULL))
  
  return(out)
}

diagnostics <- function (reps, res, pars) {
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
  
  medians <- sapply(1:reps, .median, res = res)
  empirical_SE <- apply(medians, 1, sd)
  
  coverage <- sapply(1:reps, .coverage, res = res, true = true)

  coverage_rate <- apply(coverage, 1, .proportion)
  
  nonzero <- sapply(1:reps, .nonZeroInterval, res = res)
  power <- apply(nonzero, 1, .proportion)
  
  separate_intervals <- sapply(1:reps, .differenceIntervals, res = res)
  separate_intervals <- apply(separate_intervals, 1, .proportion)
  
  return(data.frame(parameter = names(pars),
                    true = true,
                    bias = bias,
                    absolute_bias = absolute_bias, relative_bias = relative_bias,
                    empirical_SE = empirical_SE, mse = mse,
                    separate_intervals = separate_intervals,
                    coverage = coverage_rate, power = power))
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
  out <- res[[r]]$lower < 0 & res[[r]]$upper < 0 | res[[r]]$lower > 0 & res[[r]]$upper > 0
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
