getStanCode <- function (prior_gamma = "normal(0, sqrt(0.5))") {
  sprintf("
    data {
      int<lower=1> N;                          // no. of occasions
      int<lower=0, upper=N> n_mis;             // no. of missings
      int<lower=1, upper=N> ii_obs[N - n_mis]; // indices observations
      int<lower=1, upper=N> ii_mis[n_mis];     // indices missings
      vector[N - n_mis] y_obs;                 // observations
      int<lower=1, upper=N> D;                 // no. of db1 indices
      int<lower=1, upper=N> B;                 // no. of dbi indices
      int<lower=2, upper=N> db1[D - 1];        // indices first beeps minus first day
      int<lower=2, upper=N> dbi[B];            // indices other beeps
      real<lower=0> delta_t;                   // span night interval
    }
    transformed data {
      int<lower=1, upper=N - 1> db1_lag[D - 1]; // indices last beeps
      int<lower=1, upper=N - 1> dbi_lag[B];     // indices lagged beeps
      // subtract one from the db1 and dbi indices
      // to create lagged indices
      for (i in 1:(D - 1)) {
        db1_lag[i] = db1[i] - 1;
      }
      for (i in 1:B) {
        dbi_lag[i] = dbi[i] - 1;
      }
    }
    parameters {
      real mu;                // mean of y
      real phi;               // daytime autoregression
      real gamma;             // nighttime autoregression
      real<lower=0> sigma_2;  // daytime innovation variance
      real<lower=0> psi_2;    // nighttime innovation variance
      vector[n_mis] y_mis;    // estimated values for missings
    }
    transformed parameters {
      vector[N] y;            // combination of missing and observed
      y[ii_obs] = y_obs;      // fill observed
      y[ii_mis] = y_mis;      // fill missings
    }
    model {
      // priors
      mu ~ normal(0, 100);
      phi ~ normal(0, 100);
      gamma ~ %s;
      sigma_2 ~ inv_gamma(0.001, 0.001);
      psi_2 ~ inv_gamma(0.001, 0.001);
      // prior first observation if missing
      y[1] ~ normal(0, 100);
    
      // likelihood
      // the predictor is centered by subtracting mu
      y[dbi] ~ normal(mu + phi * (y[dbi_lag] - mu), sqrt(sigma_2));
      y[db1] ~ normal(mu + gamma * (y[db1_lag] - mu), sqrt(psi_2));
    }
    generated quantities {
      real diff_phi;    // difference between gamma and phi
      real diff_phi_ct; // difference between gamma and
                        // phi to the power of delta_t
      diff_phi = gamma - phi;
      diff_phi_ct = gamma - phi ^ delta_t;
    }
  ", prior_gamma
  )
}

stanData <- function(r, dat, items, delta_t = 18) {
  out <- tryCatch({
    dat$firstbeep <- FALSE
    for (i in 1:nrow(dat)) {
      dat$firstbeep[i] <- ifelse(min(which(dat$day == dat$day[i])) == i,
                                 TRUE, FALSE)
    }
    db1 <- which(dat$firstbeep)[-1]
    dbi <- which(!dat$firstbeep)
    
    y <- dat[, items[r]]
    ii_mis <- which(is.na(y))
    ii_obs <- which(!is.na(y))
    
    y_obs <- y[ii_obs]
    
    dat_stan <- list(
      N = length(y), D = length(db1) + 1, B = length(dbi),
      db1 = db1, dbi = dbi,
      n_mis = length(ii_mis), ii_obs = ii_obs, ii_mis = ii_mis,
      y_obs = y_obs, delta_t = delta_t
    )
    dat_stan
  }, error = function(e) return(NULL))
  return(out)
}

fitModel <- function (r, mod, modelout, dat, pars, items, seed) {
  out <- tryCatch({
    rstan::sampling(mod, data = dat[[r]], seed = seed,
                    iter = 11000, warmup = 1000, save_warmup = FALSE,
                    pars = pars)
  }, error = function(e) e)
  
  saveRDS(
    out,
    file = sprintf("%s%s.rds", modelout, items[r])
  )
}

readStanResults <- function (r, items, modelout) {
  out <- tryCatch({
    out <- readRDS(sprintf("%s%s.rds", modelout, items[r]))
    out
  }, error = function(e) return(NULL))
  
  return(out)
}

getResults <- function (r, out, pars) {
  out <- tryCatch({
    sfit <- rstan::summary(out[[r]], pars = pars, use_cache = FALSE)$summary
    est <- sfit[pars, c("50%", "sd", "2.5%", "97.5%", "Rhat")]
    est <- as.data.frame(est)
    est <- cbind(pars, est)
    names(est) <- c("parameter", "median", "sd", "lower", "upper", "rhat")
    est
  }, error = function(e) return(NULL))
  
  return(out)
}

fitLogSpline <- function (r, out) {
  out <- tryCatch({
    posterior_samples <- rstan::extract(out[[r]], "gamma")$gamma
    fit <- logspline::logspline(posterior_samples)
    fit
  }, error = function(e) return(NULL))
  
  return(out)
}

getDensity <- function (r, fit, step = 0.001) {
  out <- tryCatch({
    x <- seq(-1, 1, step)
    posterior_density <- logspline::dlogspline(x, fit[[r]])
    data.frame(density = posterior_density, x = x)
  }, error = function(e) return(NULL))
  
  return(out)
}

dfDensity <- function (posterior_density, prior = c(0, 0.5), res) {
  for (i in 1:length(res)) {
    int <- as.numeric(res[[i]]["gamma", c("lower", "upper")])
    areaInterval <- ifelse(
      posterior_density[[i]]$x > int[1] & posterior_density[[i]]$x < int[2],
      posterior_density[[i]]$density, 0)
    posterior_density[[i]]$area <- areaInterval
  }
  
  names(posterior_density) <- prettyNames()
  df <- data.table::rbindlist(posterior_density, idcol = TRUE)
  df$density_prior <- dnorm(df$x, prior[1], sqrt(prior[2]))
  df_long <- tidyr::pivot_longer(df, starts_with("dens"),
                                 names_to = "line", values_to = "density")
  
  return(df_long)
}

getBF <- function (r, out, prior = c(0, 0.5), fit, delta_t = 18) {
  out <- tryCatch({
    phi <- rstan::summary(out[[r]])$summary["phi", "50%"]
    
    post_dens_p <- logspline::dlogspline(phi, fit[[r]])
    BF_pd <- post_dens_p / dnorm(phi, prior[1], sqrt(prior[2]))
    
    post_dens_s <- logspline::dlogspline(0, fit[[r]])
    BF_sd <- post_dens_s / dnorm(0, prior[1], sqrt(prior[2]))
    
    post_dens_c <- logspline::dlogspline(phi ^ delta_t, fit[[r]])
    BF_cd <- post_dens_c / dnorm(phi ^ delta_t, prior[1], sqrt(prior[2]))
    
    data.frame(phi = phi, phi_ct = phi ^ delta_t,
               BF_pd = BF_pd, BF_sd = BF_sd, BF_cd = BF_cd)
  }, error = function(e) return(NULL))
  
  return(out)
}

dfBayesFactors <- function (bfs) {
  names(bfs) <- prettyNames()
  df_long <- data.table::rbindlist(bfs, idcol = TRUE)
  df_long$zero <- 0
  
  df_long$text_s <- ifelse(
    df_long$BF_sd < 0.01, paste("BF[s][d] < 0.01"),
    ifelse(df_long$BF_sd > 100,
           paste("BF[s][d] > 100"),
           ifelse(df_long$BF_sd > 10,
                  paste0("BF[s][d] == \"",
                         sprintf(df_long$BF_sd, fmt = "%#.1f"), "\""),
                  paste0("BF[s][d] == \"",
                         sprintf(df_long$BF_sd, fmt = "%#.2f"), "\""))))
  
  df_long$text_p <- ifelse(
    df_long$BF_pd < 0.01, paste("BF[p][d] < 0.01"),
    ifelse(df_long$BF_pd > 100,
           paste("BF[p][d] > 100"),
           ifelse(df_long$BF_pd > 10,
                  paste0("BF[p][d] == \"",
                         sprintf(df_long$BF_pd, fmt = "%#.1f"), "\""),
                  paste0("BF[p][d] == \"",
                         sprintf(df_long$BF_pd, fmt = "%#.2f"), "\""))))
  
  df_long$text_c <- ifelse(
    df_long$BF_cd < 0.01, paste("BF[c][d] < 0.01"),
    ifelse(df_long$BF_cd > 100,
           paste("BF[c][d] > 100"),
           ifelse(df_long$BF_cd > 10,
                  paste0("BF[c][d] == \"",
                         sprintf(df_long$BF_cd, fmt = "%#.1f"), "\""),
                  paste0("BF[c][d] == \"",
                         sprintf(df_long$BF_cd, fmt = "%#.2f"), "\""))))
  return(df_long)
}

dfPars <- function (res) {
  df_long <- data.table::rbindlist(res, idcol = TRUE)
  
  plot_pars <- c("gamma", "phi", "diff_phi", "diff_phi_ct")
  df_pars   <- subset(df_long, parameter %in% plot_pars)
  
  df_pars$parameter <- factor(df_pars$parameter, levels = plot_pars)
  df_pars$category  <- rep(itemCategory(), each = 4)
  orderCategories   <- c("NA", "PA", "unrest", "self-esteem", "physical")
  df_pars$category  <- factor(df_pars$category, levels = orderCategories)
  
  return(df_pars)
}

postModelProb <- function (BFpd, BFsd, BFcd) {
  # pause, stop, continue, different
  pD <- 0.25 / (BFpd * 0.25 + BFsd * 0.25 + BFcd * 0.25 + 0.25)
  pS <- BFsd * 0.25 / (BFpd * 0.25 + BFsd * 0.25 + BFcd * 0.25 + 0.25)
  pP <- BFpd * 0.25 / (BFpd * 0.25 + BFsd * 0.25 + BFcd * 0.25 + 0.25)
  pC <- BFcd * 0.25 / (BFpd * 0.25 + BFsd * 0.25 + BFcd * 0.25 + 0.25)
  
  df <- data.frame(pause = pP, stop = pS, continue = pC, different = pD)
  return(df)
}

calcPostModProbs <- function (r, bfs) {
  postModelProb(bfs[[r]]$BF_pd, bfs[[r]]$BF_sd, bfs[[r]]$BF_cd)
}

dfPMPs <- function (df) {
  df <- data.table::rbindlist(df)
  df$item <- prettyNames()
  df$category <- itemCategory()
  
  df_long <- tidyr::pivot_longer(df, cols = c("pause", "stop",
                                              "continue", "different"))
  df_long$name <- factor(df_long$name)
  
  return(df_long)
}

prettyNames <- function (idx = 1:29) {
  names <- c("relaxed", "down", "irritated", "satisfied", "lonely", "anxious",
             "enthusiastic", "suspicious", "cheerful", "guilty", "indecisive",
             "strong", "restless", "agitated", "worry", "concentrate well",
             "like myself", "ashamed", "doubt myself", "handle anything", 
             "hungry", "tired", "in pain", "dizzy", "dry mouth", "nauseous",
             "headache", "sleepy", "physically active")
  return(names[idx])
}

itemCategory <- function (idx = 1:29) {
  names <- c("PA", "NA", "NA", "PA", "NA", "NA",
             "PA", "NA", "PA", "NA", "NA",
             "PA", "unrest", "unrest", "unrest", "unrest",
             "self-esteem", "self-esteem", "self-esteem", "self-esteem",
             "physical", "physical", "physical", "physical", "physical",
             "physical", "physical", "physical", "physical")
  return(names[idx])
}

facetLabels <- c(
  `NA` = "Negative Affect",
  `PA` = "Positive Affect",
  `unrest` = "Mental Unrest",
  `self-esteem` = "Self-Esteem",
  `physical` = "Physical"
)

# results plots

plotGammas <- function (df) {
  df <- subset(df, parameter == "gamma")
  orderDecreasing <- order(df$category, df$median, df$.id, decreasing = TRUE)
  
  df$.id <- factor(df$.id, levels = df$.id[orderDecreasing])
  
  df_axis <- data.frame(category = unique(df$category),
                        xmin = 1, xmax = c(5, 7, 4, 4, 9))
  df_axis$category <- factor(df_axis$category)
  levels(df_axis) <- levels(df$category)
  
  p <- ggplot(df) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    facet_grid(~ category, scales = "free_x", space = "free",
               labeller = as_labeller(facetLabels)) +
    geom_pointrange(aes(x = .id, y = median,
                        ymin = lower, ymax = upper,
                        fill = category, shape = category),
                    linewidth = 0.75, size = 0.75) +
    coord_cartesian(ylim = c(-1, 1)) + 
    viridis::scale_fill_viridis(discrete = T) +
    scale_shape_manual(values = 21:25) +
    scale_y_continuous(breaks = seq(-1, 1, 0.25),
                       labels = c("-1.0", "", 0.5, "", "0.0",
                                  "", 0.5, "", "1.0")) +
    theme_void() +
    theme(
      text = element_text(family = "sans", size = 16),
      axis.text.x = element_text(angle = 30, hjust = 1, vjust = 1, size = 14),
      legend.position = "none",
      # legend.title = element_blank(),
      axis.title.x = element_text(margin = margin(0, 0, 5, 5)),
      axis.text = element_text(margin = margin(5, 5, 5, 5)),
      axis.text.y = element_text(hjust = 0.95),
      axis.title.y = element_text(margin = margin(5, 0, 5, 5)),
      axis.ticks = element_line(lineend = "butt",
                                linewidth = 0.3),
      axis.ticks.length = unit(2.5, "pt"),
      strip.text = element_text(margin = margin(5, 5, 5, 5), size = 16),
      panel.spacing = unit(7.5, units = "pt"),
      plot.margin = margin(0, 5, 0, 0),
      panel.grid.major.y = element_line(linewidth = 0.3, color = "grey85")
    ) +
    geom_segment(x = -Inf, xend = -Inf, y = -1, yend = 1,
                 linewidth = 0.3, lineend = "square") +
    geom_segment(data = df_axis, y = -Inf, yend = -Inf,
                 aes(x = xmin, xend = xmax),
                 linewidth = 0.3, lineend = "square") +
    labs(y = expression(gamma), x = "Item")
  
  return(p)
}

plotScatterPhiGamma <- function (df, delta_t = 18) {
  df_phi <- subset(df, df$parameter == "phi")
  df_gamma <- subset(df, df$parameter == "gamma")
  
  df <- data.frame(phi = df_phi$median, gamma = df_gamma$median,
                   category = df_phi$category)
  
  xBreaks <- pretty(df$phi)
  ctLine <- seq(-0.2, 0.8, 0.01)
  dfLine <- data.frame(x = ctLine,
                       y_ct = ctLine ^ delta_t, y_zero = 0, y_phi = ctLine)
  dfLineLong <- tidyr::pivot_longer(dfLine, cols = starts_with("y"),
                                    names_to = "model", values_to = "y")
  
  dfLineLong$model <- factor(dfLineLong$model,
                             levels = c("y_phi", "y_zero", "y_ct"))
  
  p <- ggplot(df) +
    geom_line(data = dfLineLong, aes(x = x, y = y, linetype = model)) +
    geom_point(aes(x = phi, y = gamma, fill = category, shape = category),
               size = 2) +
    scale_y_continuous(breaks = xBreaks, labels = xBreaks) +
    scale_x_continuous(breaks = xBreaks, labels = xBreaks) +
    viridis::scale_fill_viridis(discrete = TRUE, labels = facetLabels) +
    scale_shape_manual(values = 21:25, labels = facetLabels) +
    scale_linetype_manual(values = c("dashed", "solid", "dotted"),
                          labels = c(bquote(gamma == phi),
                                     bquote(gamma == 0),
                                     bquote(gamma == phi^.(delta_t)))) +
    guides(shape = guide_legend(order = 1),
           fill = guide_legend(order = 1),
           linetype = guide_legend(order = 2)) +
    theme_void() +
    theme(
      text = element_text(family = "sans", size = 12),
      axis.text = element_text(margin = margin(5, 5, 5, 5)),
      axis.text.y = element_text(hjust = 0.95),
      axis.title = element_text(margin = margin(5, 5, 5, 5)),
      axis.ticks = element_line(lineend = "butt", linewidth = 0.3),
      axis.ticks.length = unit(2.5, "pt"),
      strip.text = element_text(margin = margin(5, 5, 5, 5), size = 12),
      panel.spacing = unit(7.5, units = "pt"),
      plot.margin = margin(0, 5, 0, 0),
      legend.position = c(.35, .75),
      legend.box = "horizontal",
      legend.title = element_blank(),
      legend.text.align = 0
    ) +
    geom_segment(x = -Inf, xend = -Inf, y = -0.2, yend = 0.8,
                 linewidth = 0.3, lineend = "square") +
    geom_segment(y = -Inf, yend = -Inf, x = -0.2, xend = 0.8,
                 linewidth = 0.3, lineend = "square") +
    labs(x = expression(phi), y = expression(gamma)) +
    coord_cartesian(xlim = c(-0.2, 0.8), ylim = c(-0.2, 0.8))
  
  return(p)
}

plotPars <- function (df, delta_t = 18) {
  p <- ggplot(df) +
    geom_hline(yintercept = 0, linewidth = 0.3) +
    geom_pointrange(aes(x = parameter, y = median, ymin = lower, ymax = upper,
                        shape = category, fill = category),
                    linewidth = 1, size = 1) +
    scale_y_continuous(breaks = seq(-1, 1, 0.25),
                       labels = c("-1.0", "", 0.5, "", "0.0",
                                  "", 0.5, "", "1.0")) +
    scale_x_discrete(labels = c(bquote(gamma),
                                bquote(phi),
                                bquote(gamma - phi),
                                bquote(gamma - phi^.(delta_t)))) +
    viridis::scale_fill_viridis(discrete = T, labels = facetLabels) +
    scale_shape_manual(values = 21:25, labels = facetLabels) +
    facet_wrap(~ .id, ncol = 5) +
    coord_cartesian(ylim = c(-1, 1)) +
    labs(y = "Estimates", x = "Parameter") +
    theme_void() +
    theme(
      text = element_text(family = "sans", size = 16),
      axis.title.y = element_text(angle = 90),
      axis.text = element_text(margin = margin(5, 5, 5, 5)),
      axis.text.y = element_text(hjust = 0.95),
      axis.text.x = element_text(angle = 15, vjust = 0.5),
      axis.title = element_text(margin = margin(5, 5, 5, 5)),
      axis.ticks = element_line(lineend = "butt",
                                linewidth = 0.3),
      axis.ticks.length = unit(2.5, "pt"),
      strip.text = element_text(margin = margin(5, 5, 5, 5), size = 16),
      panel.spacing = unit(7.5, units = "pt"),
      plot.margin = margin(0, 5, 0, 0),
      legend.position = c(.9, .07),
      legend.title = element_blank(),
      legend.text = element_text(size = 16),
      panel.grid.major.y = element_line(linewidth = 0.3, color = "grey85")
    ) +
    geom_segment(x = -Inf, xend = -Inf, y = -1, yend = 1,
                 linewidth = 0.3, lineend = "square") +
    geom_segment(y = -Inf, yend = -Inf, x = 1, xend = 4,
                 linewidth = 0.3, lineend = "square")
  
  return(p)
}

plotPriorPosterior <- function (df_posteriors, df_bayes_factors, delta_t = 18) {
  df_vline <- tidyr::pivot_longer(df_bayes_factors,
                                  cols = c("phi", "phi_ct", "zero"),
                                  values_to = "null")
  
  # order pauses, stops, continues
  df_vline$name <- factor(df_vline$name, levels = c("phi", "zero", "phi_ct"))
  
  p <- ggplot(df_posteriors) +
    facet_wrap(~ .id, ncol = 5) +
    geom_vline(aes(xintercept = null, colour = name), data = df_vline,
               linewidth = 1) +
    geom_line(aes(x = x, y = density, linetype = line), linewidth = 1) +
    scale_colour_manual(values = viridis::viridis(3)[c(2, 1, 3)],
                        labels = c(bquote(phi), 0,
                                   bquote(phi^.(delta_t)))) +
    # viridis::scale_colour_viridis(discrete = TRUE, direction = -1,
    #                               labels = c(bquote(phi), 0, 
    #                                          bquote(phi^.(delta_t)))) +
    scale_linetype_manual(values = c("solid", "dotted"),
                          labels = c("Posterior", "Prior")) +
    geom_area(aes(x = x, y = area), alpha = 0.2) +
    coord_cartesian(ylim = c(0, 4.5)) +
    geom_text(aes(x = -1, y = 4, label = text_p), data = df_bayes_factors,
              parse = TRUE, hjust = 0, size = 0.36 * 12) +
    geom_text(aes(x = -1, y = 3, label = text_s), data = df_bayes_factors,
              parse = TRUE, hjust = 0, size = 0.36 * 12) +
    geom_text(aes(x = -1, y = 2, label = text_c), data = df_bayes_factors,
              parse = TRUE, hjust = 0, size = 0.36 * 12) +
    theme_void() +
    labs(y = "Density", x = expression(gamma)) +
    theme(
      text = element_text(family = "sans", size = 14),
      axis.title.y = element_text(angle = 90),
      axis.text = element_text(margin = margin(5, 5, 5, 5), size = 12),
      axis.title = element_text(margin = margin(5, 5, 5, 5)),
      axis.ticks = element_line(lineend = "butt",
                                linewidth = 0.3),
      axis.ticks.length = unit(2.5, "pt"),
      strip.text = element_text(margin = margin(5, 5, 5, 5), size = 14),
      panel.spacing = unit(7.5, units = "pt"),
      plot.margin = margin(0, 5, 0, 0),
      legend.position = c(.9, .075),
      legend.box = "horizontal",
      legend.title = element_blank(),
      legend.text = element_text(size = 14),
      legend.text.align = 0
    ) +
    geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 4,
                 linewidth = 0.3, lineend = "square") +
    geom_segment(y = -Inf, yend = -Inf, x = -1,
                 xend = 1,
                 linewidth = 0.3,
                 lineend = "square") +
    guides(colour = guide_legend(order = 2), linetype = guide_legend(order = 1))
  
  return(p)
}

plotPMPs <- function (df) {
  # labels_fill <- c("p(Hp | y)", "p(Hs | y)", "p(Hc | y)", "p(Hd | y)")
  
  df$name <- factor(df$name,
                    levels = c("pause", "stop", "continue", "different"),
                    labels = c("pauses", "stops", "continues", "different"))
  
  df$category <- factor(df$category)
  levels(df$category) <- as.list(facetLabels)
  
  p <- ggplot(df) +
    facet_grid(~ category, scales = "free_x", space = "free",
               labeller = as_labeller(facetLabels)) +
    geom_bar(aes(x = item, y = value, fill = name), stat = "identity") +
    viridis::scale_fill_viridis(discrete = TRUE, name = "Method") +
    theme_void() +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1,
                                 vjust = 1.25, size = 12),
      axis.title.y = element_text(angle = 90, size = 14),
      legend.position = "bottom",
      legend.text = element_text(family = "sans", size = 12),
      legend.box.margin = margin(-10, 0, 0, 0),
      text = element_text(family = "sans", size = 12),
      axis.text = element_text(margin = margin(5, 5, 5, 5)),
      axis.text.y = element_text(hjust = 0.95),
      axis.ticks.y = element_line(lineend = "butt",
                                  linewidth = 0.3),
      axis.ticks.length = unit(2.5, "pt"),
      strip.text = element_text(margin = margin(5, 5, 5, 5), size = 14),
      panel.spacing = unit(7.5, units = "pt"),
      plot.margin = margin(5, 5, 5, 5)
    ) +
    labs(y = "Posterior Model Probabilities")
  
  return(p)
}
