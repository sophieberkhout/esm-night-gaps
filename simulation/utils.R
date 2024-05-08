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

  relative_bias <- numeric(length(true))
  for (i in 1:length(true)) {
    if (true[i] != 0) {
      relative_bias[i] <- bias[i] / true[i]
    } else {
      relative_bias[i] <- NA
    }
  }

  mse <- apply(deviance ^ 2, 1, mean)
  mae <- apply(abs(deviance), 1, mean)

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
  min_bf_5 <- sapply(1:reps, minBF, bfs, min = 5)
  min_bf_10 <- sapply(1:reps, minBF, bfs, min = 10)
  
  power_bf_3 <- apply(min_bf_3, 1, .proportion)
  power_bf_5 <- apply(min_bf_5, 1, .proportion)
  power_bf_10 <- apply(min_bf_10, 1, .proportion)
  
  best_model <- sapply(1:reps, calcPostModProbs, bfs)
  pref_model <- apply(best_model, 1, .proportion)

  return(
    data.frame(parameter = names(pars),
               true = true,
               bias = bias, relative_bias = relative_bias,
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

# plots

getDataFrameBoth <- function(df_0.3, df_0.5) {
  # combine data sets
  df_0.3$phi <- 0.3
  df_0.5$phi <- 0.5
  df_both <- rbind(df_0.3, df_0.5)
  
  # only get relevant sets
  df_plot <- df_both[df_both$parameter %in% c("gamma", "diff", "diff_ct"), ]
  
  # make factors
  df_plot$parameter <- factor(df_plot$parameter,
                              levels = c("diff", "gamma", "diff_ct"))
  df_plot$days <- factor(df_plot$days, levels = c(200, 100, 50, 25))
  
  return(df_plot)
}

plotParameterRecovery <- function(df) {
  # make df long format
  df_long <- tidyr::pivot_longer(df, cols = c("bias", "mae", "coverage"),
                                 names_to = "diagnostic", values_to = "value")
  
  # make factors
  df_long$days <- factor(df_long$days)
  df_long$diagnostic <- factor(df_long$diagnostic,
                               levels = c("bias", "mae", "coverage"),
                               labels = c("Bias", "MAE", "Coverage"))
  df_long$parameter <- factor(df_long$parameter,
                              levels = c("gamma", "phi", "diff", 
                                         "diff_ct", "mu", 
                                         "sigma_2", "psi_2"),
                              labels = expression(gamma, phi, gamma - phi,
                                                  gamma - phi ^ 7, mu,
                                                  sigma ^ 2, psi ^ 2))
  df_long$diff <- factor(df_long$diff, labels = seq(0, 0.6, 0.1))
  
  # control separate y-axes
  df_axis <- data.frame(diagnostic = unique(df_long$diagnostic),
                        ymin = c(-0.04, 0, .9), ymax = c(0.04, 0.25, 1),
                        hline = c(0, 0, 1))
  
  yBreaks <- function(x) {
    if (max(x) < 0.2) {
      seq(-0.04, 0.04, 0.02)
    } else if (max(x) > 0.9) {
      seq(.9, 1, 0.02)
    } else {
      seq(0, 0.25, 0.05)
    }
  }
  
  p <- ggplot(df_long) +
    facet_grid(rows = vars(diagnostic), cols = vars(parameter),
               scales = "free", labeller = label_parsed) +
    geom_hline(data = df_axis, aes(yintercept = hline), linewidth = 0.3) +
    geom_line(aes(x = days, y = value, group = diff, colour = diff),
              linewidth = 0.75) +
    scale_y_continuous(breaks = yBreaks) +
    viridis::scale_colour_viridis(discrete = TRUE) +
    labs(colour = expression(gamma)) +
    theme_void() +
    guides(colour = guide_legend(nrow = 1)) +
    theme(
      text = element_text(family = "sans", size = 12),
      axis.title = element_blank(),
      axis.text = element_text(margin = margin(5, 5, 5, 5)),
      axis.text.y = element_text(hjust = 0.95),
      axis.text.x = element_text(angle = 30, hjust = 1, vjust = 0.95, size = 11,
                                 margin = margin(0, 0, -5, 0)),
      axis.ticks = element_line(lineend = "butt",
                                linewidth = 0.3),
      axis.ticks.length = unit(2.5, "pt"),
      strip.text = element_text(margin = margin(0, 5, 0, 5), size = 14),
      strip.text.y = element_text(angle = 270),
      legend.position = "bottom",
      panel.spacing = unit(7.5, units = "pt")
    ) +
    geom_segment(data = df_axis,
                 aes(x = -Inf, xend = -Inf, y = ymin, yend = ymax),
                 linewidth = 0.3, lineend = "square") +
    geom_segment(x = 1, xend = 4, y = -Inf, yend = -Inf,
                 linewidth = 0.3, lineend = "square")
  
  return(p)
}

plotBayesFactors <- function(df, method = "BF_3") {
  # labels for facets
  labelPars <- as_labeller(
    c(gamma = "BF[sd]", diff = "BF[pd]", diff_ct = "BF[cd]",
      `0.3` = "phi == 0.3", `0.5` = "phi == 0.5"),
    label_parsed
  )
  
  p <- ggplot(df) +
    facet_grid(cols = vars(parameter), rows = vars(phi),
               scales = "free", labeller = labelPars) +
    geom_hline(yintercept = 0.5, linewidth = 0.3) +
    geom_line(aes(x = diff + phi, y = power_bf_3,
                  colour = days, linetype = "BF > 3"), linewidth = 1) +
    geom_point(aes(x = diff + phi, y = power_bf_3, colour = days),
               size = 2, stroke = 1.5, fill = "white", shape = 21) +
    labs(x = expression(gamma), y = bquote(Proportion~H[d]~selected),
         colour = "Days") +
    scale_x_continuous(breaks = seq(0, 0.6, 0.1)) +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
    viridis::scale_colour_viridis(discrete = TRUE,
                                  labels = c(200, 100, 50, 25)) +
    scale_linetype(guide = "none") +
    theme_void() +
    theme(
      text = element_text(family = "sans", size = 12),
      axis.title = element_text(margin = margin(5, 5, 5, 5)),
      axis.title.y = element_text(angle = 90, size = 14),
      axis.text = element_text(margin = margin(5, 5, 0, 5)),
      axis.text.y = element_text(hjust = 0.95),
      axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 12),
      axis.ticks = element_line(lineend = "butt",
                                linewidth = 0.3),
      axis.ticks.length = unit(2.5, "pt"),
      legend.text = element_text(size = 12),
      legend.position = "bottom",
      legend.margin = margin(0, 10, 0, 0),
      strip.text = element_text(margin = margin(0, 5, 2, 5), size = 14),
      strip.text.y = element_text(angle = 270),
      strip.placement = "outside",
      panel.spacing = unit(7.5, units = "pt"),
      plot.margin = margin(5, 5, 0, 0)
    ) +
    geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 1,
                 linewidth = 0.3, lineend = "square") +
    geom_segment(y = -Inf, yend = -Inf, x = 0, xend = 0.6,
                 linewidth = 0.3, lineend = "square")
  
  # add dotted line for CI or BF > 10
  if (method == "CI") {
    p <- p +
      geom_line(aes(x = diff + phi, y = power,
                    colour = days, linetype = "CI"), linewidth = 1) +
      geom_point(aes(x = diff + phi, y = power, colour = days),
                 size = 2, stroke = 1.5, fill = "white", shape = 21) +
      scale_linetype_manual("Method", breaks = c("CI", "BF > 3"),
                            values = c("CI" = "dotted", "BF > 3" = "solid"))
  }
  if (method == "BF_10") {
    p <- p +
      geom_line(aes(x = diff + phi, y = power_bf_10,
                    colour = days, linetype = "BF > 10"), linewidth = 1) +
      geom_point(aes(x = diff + phi, y = power_bf_10, colour = days),
                 size = 2, stroke = 1.5, fill = "white", shape = 21) +
      scale_linetype_manual("Method", breaks = c("BF > 3", "BF > 10"),
                            values = c("BF > 3" = "solid",
                                       "BF > 10" = "dotted"))
  }
  
  return(p)
}

plotProbabilities <- function(df) {
  # get only relevant columns
  df_bar <- df[, c("parameter", "pref_model", "days", "diff", "phi")]
  
  # compute gamma
  df_bar$gamma <- round(df_bar$diff + df_bar$phi, 1)
  
  # model indicator
  df_bar$model <- ifelse(df_bar$parameter == "gamma", "pauses",
                         ifelse(df_bar$parameter == "diff", "stops",
                                "continues"))
  
  # get posterior model probability for different model (1 - sum(3 other PMPs))
  for (i in unique(df_bar$days)) {
    for (j in unique(df_bar$gamma)) {
      for (h in unique(df_bar$phi)) {
        pref <- 1 - sum(subset(df_bar, days == i & gamma == j & phi == h,
                               "pref_model"))
        df_bar[nrow(df_bar) + 1, ] <- data.frame(NA, pref, i, j - h, h, j,
                                                 model = "different")
      }
    }
  }
  
  # labels to parse
  df_bar$label_x <- paste("gamma ==", df_bar$gamma)
  df_bar$label_y <- paste("phi ==", df_bar$phi)
  
  # make factors in correct order
  df_bar$days <- factor(df_bar$days, levels = c("25", "50", "100", "200"))
  df_bar$model <- factor(df_bar$model, levels = c("pauses", "stops",
                                                  "continues", "different"))
  
  p <- ggplot(df_bar) +
    facet_grid(cols = vars(label_x), rows = vars(label_y),
               scales = "free_x", space = "free", labeller = label_parsed) +
    geom_bar(aes(x = days, y = pref_model, fill = model), stat = "identity") +
    viridis::scale_fill_viridis(name = "Method", discrete = TRUE) +
    theme_void() +
    theme(
      text = element_text(family = "sans", size = 12),
      axis.title = element_blank(),
      axis.text = element_text(margin = margin(5, 5, 5, 5)),
      axis.text.y = element_text(hjust = 0.95),
      axis.text.x = element_text(angle = 45, hjust = 1.3, vjust = 2, size = 12,
                                 margin = margin(10, 0, -10, 0)),
      axis.ticks.y = element_line(lineend = "butt",
                                  linewidth = 0.3),
      axis.ticks.length = unit(2.5, "pt"),
      strip.text = element_text(margin = margin(5, 5, 5, 5), size = 12),
      strip.text.y = element_text(angle = 270),
      legend.position = "bottom",
      panel.spacing = unit(7.5, units = "pt")
    ) +
    labs(y = "Proportion selected models", x = "Number of days")
  
  return(p)
}

