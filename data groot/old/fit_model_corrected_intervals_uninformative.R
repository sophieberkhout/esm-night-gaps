dat <- read.csv("data groot/data/data.csv")

items <- names(dat)[grepl("mood_|pat_|phy_|se_", names(dat))]

n_threads <- length(items)
clus <- parallel::makeCluster(n_threads)
exp <- parallel::clusterExport(clus, "items")

stan_code <- "
data {
  int<lower=1> N;
  int<lower=1, upper=N> D;
  int<lower=1, upper=N> B;
  int<lower=1, upper=N> db1[D - 1];
  int<lower=2, upper=N> dbi[B];
  int<lower=0, upper=N> n_mis;
  int<lower=1, upper=N> ii_obs[N - n_mis];
  int<lower=1, upper=N> ii_mis[n_mis];
  vector[N - n_mis] y_obs;
}
transformed data {
  int<lower=1, upper=N> db1_lag[D - 1];
  int<lower=1, upper=N - 1> dbi_lag[B];
  for (i in 1:(D - 1)) {
    db1_lag[i] = db1[i] - 1;
  }
  for (i in 1:B) {
    dbi_lag[i] = dbi[i] - 1;
  }
}
parameters {
  real mu;
  real phi;
  real<lower=-1, upper=1> gamma; 
  real<lower=0> resvar_i;
  real<lower=0> resvar_1;
  vector[n_mis] y_mis;
}
transformed parameters {
  vector[N] y;
  y[ii_obs] = y_obs;
  y[ii_mis] = y_mis;
}
model {
  // priors
  mu ~ normal(0, 100);
  phi ~ normal(0, 100);
  gamma ~ uniform(-1, 1);
  resvar_i ~ inv_gamma(0.001, 0.001);
  resvar_1 ~ inv_gamma(0.001, 0.001);
  y[1] ~ normal(0, 100);

  // likelihood
  y[dbi] ~ normal(mu + phi * (y[dbi_lag] - mu), sqrt(resvar_i));
  y[db1] ~ normal(mu + gamma * (y[db1_lag] - mu), sqrt(resvar_1));
}
generated quantities {
  real diff;
  real diff_ct;
  
  diff = gamma - phi;
  diff_ct = gamma - phi ^ 18;
}
"

stanData <- function(r, dat, items) {
  out <- tryCatch({
    dat$firstbeep <- FALSE
    for (i in 1:nrow(dat)) {
      dat$firstbeep[i] <- ifelse(min(which(dat$day == dat$day[i])) == i, TRUE, FALSE)
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
      y_obs = y_obs
    )
    dat_stan
  }, error = function(e) return(NULL))
  return(out)
}

fitModel <- function (r, mod, modelout = "data groot/estimation/uniform/", dat, items, seed) {
  out <- tryCatch({
    rstan::sampling(mod, data = dat[[r]], seed = seed,
                    iter = 11000, warmup = 1000)
  }, error = function(e) e)
  
  saveRDS(
    out,
    file = sprintf("%s%s.rds", modelout, items[r])
  )
}

mod <- rstan::stan_model(model_code = stan_code)

dat_stan <-  parallel::parLapplyLB(cl = clus, 1:length(items),
                                   stanData, dat, items)

modelout <- "data groot/estimation/uniform/"
t_fit <- system.time(
  parallel::parLapplyLB(cl = clus, 1:length(items), fitModel,
                        mod, modelout, dat_stan, items, seed = 1)
)

parallel::stopCluster(clus)

readStanResults <- function (r, items) {
  out <- tryCatch({
    out <- readRDS(sprintf("%s%s.rds", "data groot/estimation/uniform/", items[r]))
    out
  }, error = function(e) return(NULL))
  
  return(out)
}

getResults <- function (r, out, pars) {
  out <- tryCatch({
    sfit <- rstan::summary(out[[r]])$summary
    est <- sfit[pars, c("50%", "sd", "2.5%", "97.5%", "Rhat")]
    est <- as.data.frame(est)
    est <- cbind(pars, est)
    names(est) <- c("parameter", "median", "sd", "lower", "upper", "rhat")
    est
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

getDensity <- function(r, fit, step = 0.001) {
  out <- tryCatch({
    x <- seq(-1, 1, step)
    posterior_density <- logspline::dlogspline(x, fit[[r]])
    data.frame(density = posterior_density, x = x)
  }, error = function(e) return(NULL))
  
  return(out)
}

getBF <- function(r, out, fit) {
  out <- tryCatch({
    posterior_d_0 <- logspline::dlogspline(0, fit[[r]])
    BF_0 <- posterior_d_0 / dunif(0, -1, 1)
    
    phi <- rstan::summary(out[[r]])$summary["phi", "mean"]
    
    posterior_d_phi <- logspline::dlogspline(phi, fit[[r]])
    BF_phi <- posterior_d_phi / dunif(phi, -1, 1)
    
    posterior_d_phict <- logspline::dlogspline(phi ^ 18, fit[[r]])
    BF_phict <- posterior_d_phict / dunif(phi ^ 18, -1, 1)
    
    data.frame(phi = phi, phict = phi ^ 18,
               BF_0 = BF_0, BF_phi = BF_phi, BF_phict = BF_phict)
  }, error = function(e) return(NULL))
  
  return(out)
}

prettyNames <- function(idx = 1:29) {
  names <- c("relaxed", "down", "irritated", "satisfied", "lonely", "anxious",
             "enthusiastic", "suspicious", "cheerful", "guilty", "indecisive",
             "strong", "restless", "agitated", "worry", "concentrate well",
             "like myself", "ashamed", "doubt myself", "handle anything", 
             "hungry", "tired", "in pain", "dizzy", "dry mouth", "nauseous",
             "headache", "sleepy", "physically active")
  return(names[idx])
}

out <- lapply(1:length(items), readStanResults, items = items)

pars <- c("mu", "phi", "gamma", "resvar_i", "resvar_1", "diff", "diff_ct")
res <- lapply(1:length(items), getResults, out = out, pars = pars)

logSpline <- lapply(1:length(items), fitLogSpline, out)
posterior_density <- lapply(1:length(items), getDensity, logSpline)
names(posterior_density) <- items
for (i in 1:length(items)) {
  int <- as.numeric(res[[i]]["gamma", c("lower", "upper")])
  areaInterval <- ifelse(posterior_density[[items[i]]]$x > int[1] & posterior_density[[items[i]]]$x < int[2], posterior_density[[items[i]]]$density, 0)
  posterior_density[[items[i]]]$area <- areaInterval
}
names(posterior_density) <- prettyNames()
pd_long <- data.table::rbindlist(posterior_density, idcol = TRUE)

bftest  <- lapply(1:length(items), getBF, out, logSpline)
# names(bftest) <- items
names(bftest) <- prettyNames()
bftest_long <- data.table::rbindlist(bftest, idcol = TRUE)
bftest_long$zero <- 0
bftest_long_vline <- tidyr::pivot_longer(bftest_long, cols = c("phi", "phict", "zero"), values_to = "null")

bftest_long$text0 <- paste("BF[1][0] == ", round(1 / bftest_long$BF_0, 2))
bftest_long$textp <- paste("BF[1][phi] == ", round(1 / bftest_long$BF_phi, 2))
bftest_long$textc <- paste("BF[1][phi^Delta] == ", round(1 / bftest_long$BF_phict, 2))

pd_long$.id <- factor(pd_long$.id, levels = bftest_long$.id[order(bftest_long$BF_phict)])
bftest_long_vline$.id <- factor(bftest_long_vline$.id, levels = bftest_long$.id[order(bftest_long$BF_phict)])
bftest_long$.id <- factor(bftest_long$.id, levels = bftest_long$.id[order(bftest_long$BF_phict)])

library(ggplot2)
ggplot(pd_long) +
  facet_wrap(~ .id) +
  geom_vline(aes(xintercept = null), data = bftest_long_vline, col = rep(c("blue", "green", "red"), 29)) +
  annotate("line", x = seq(-1, 1, 0.001), y = dunif(seq(-1, 1, 0.001), -1, 1), linetype = "dotted") +
  geom_line(aes(x = x, y = density)) +
  geom_area(aes(x = x, y = area), alpha = 0.2) +
  coord_cartesian(ylim = c(0, 4.5)) +
  geom_text(aes(x = -1, y = 4, label = text0), data = bftest_long, parse = TRUE, hjust = 0) +
  geom_text(aes(x = -1, y = 3, label = textc), data = bftest_long, parse = TRUE, hjust = 0) +
  geom_text(aes(x = -1, y = 2, label = textp), data = bftest_long, parse = TRUE, hjust = 0) +
  theme_void() +
  labs(y = "Density", x = expression(gamma)) +
  theme(text = element_text(family = "sans", size = 12),
        axis.title.y = element_text(angle = 90),
        axis.text = element_text(margin = margin(5, 5, 5, 5)),
        axis.title = element_text(margin = margin(5, 5, 5, 5)),
        axis.ticks = element_line(lineend = "butt",
                                  linewidth = 0.3),
        axis.ticks.length = unit(2.5, "pt"),
        strip.text = element_text(margin = margin(5, 5, 5, 5), size = 12),
        panel.spacing = unit(7.5, units = "pt"),
        plot.margin = margin(0, 5, 0, 0),
        legend.position = c(.8, .8)) +
  geom_segment(x = -Inf, xend = -Inf, y = 0, yend = 4,
               linewidth = 0.3, lineend = "square") +
  geom_segment(y = -Inf, yend = -Inf, x = -1,
               xend = 1,
               linewidth = 0.3,
               lineend = "square")
ggsave("bayesfactors_uniform.pdf", height = 10, width = 15)

for(item in items) {
  
  y <- dat[, item]
  ii_mis <- which(is.na(y))
  ii_obs <- which(!is.na(y))
  
  y_obs <- y[ii_obs]
  
  dat_stan <- list(
    N = length(y), D = length(db1) + 1, B = length(dbi),
    db1 = db1, dbi = dbi,
    n_mis = length(ii_mis), ii_obs = ii_obs, ii_mis = ii_mis,
    y_obs = y_obs
  )
  
  out <- rstan::sampling(mod, data = dat_stan,
                         iter = 11000, warmup = 1000,
                         seed = 1111)
  
  saveRDS(
    out,
    file = sprintf("%s%s.rds", "data groot/estimation/uniform/", item)
  )
}

phi <- rstan::summary(out)$summary["phi", "mean"]

posterior_samples_gamma <- extract(out, "gamma")$gamma
posterior_density <- density(posterior_samples_gamma)
post_d_0 <- approx(posterior_density$x, posterior_density$y, 0)
post_d_phi <- approx(posterior_density$x, posterior_density$y, phi)
post_d_phi_ct <- approx(posterior_density$x, posterior_density$y, phi ^ 18)

prior_d_0 <- dnorm(0, 0, sqrt(0.5))
prior_d_phi <- dnorm(phi, 0, sqrt(0.5))
prior_d_phi_ct <- dnorm(phi ^ 18, 0, sqrt(0.5))

bf_0_1 <- post_d_0$y / prior_d_0
bf_phi_1 <- post_d_phi$y / prior_d_phi
bf_phi_ct_1 <- post_d_phi_ct$y / prior_d_phi_ct

bf <- bayesfactor_parameters(posterior_samples_gamma, rnorm(length(posterior_samples_gamma), 0, sqrt(0.5)), null = 0)
plot(bf)

plot(posterior_density, xlim = c(-3, 3))
lines(density(rnorm(1e6, 0, sqrt(0.5))), col = "red")

pars <- c("mu", "phi", "gamma",
          "resvar_i", "resvar_1",
          "delta", "delta_2")

readStanResults <- function (r, item, pars) {
  out <- tryCatch({
    out <- readRDS(sprintf("%s%s.rds", "data groot/estimation/uniform/", item[r]))
    out
    # sfit <- rstan::summary(out)$summary
    # est <- sfit[pars, c("50%", "sd", "2.5%", "97.5%", "Rhat")]
    # est <- as.data.frame(est)
    # est <- cbind(pars, est)
    # names(est) <- c("parameter", "median", "sd", "lower", "upper", "rhat")
    # est
  }, error = function(e) return(NULL))
  
  return(out)
}

computeBF <- function(r, out) {
  out <- tryCatch({
    posterior_samples <- rstan::extract(out[[r]], "gamma")$gamma
    prior_d <- dnorm(0, 0, 0.5)
    posterior_d <- logspline::dlogspline(0, logspline::logspline(posterior_samples))
    BF <- posterior_d / prior_d
    BF
  }, error = function(e) return(NULL))
  
  return(out)
}

library(ggplot2)
items <- items[1:4]
res <- lapply(1:length(items), readStanResults, item = items, pars = pars)
BF <- sapply(1:length(items), computeBF, out = res)
names(res) <- items2[1:length(items)]

long_res <- data.table::rbindlist(res, idcol = TRUE)

ggplot(subset(long_res, parameter %in% c("phi", "gamma", "delta", "delta_2"))) +
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(x = parameter, y = median, ymin = lower, ymax = upper)) +
  facet_wrap(~ .id, scales = "free") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

################################################################################

sig <- logical()
for(i in items2[1:length(items)]) {
  sig <- c(sig, (res[[i]]["delta", "lower"] < 0 & res[[i]]["delta", "upper"] < 0) | (res[[i]]["delta", "lower"] > 0 & res[[i]]["delta", "upper"] > 0))
}

sig_2 <- logical()
for(i in items2[1:length(items)]) {
  sig_2 <- c(sig_2, (res[[i]]["delta_2", "lower"] < 0 & res[[i]]["delta_2", "upper"] < 0) | (res[[i]]["delta_2", "lower"] > 0 & res[[i]]["delta_2", "upper"] > 0))
}

long_res$parameter <- factor(long_res$parameter, levels = pars)
long_res$.id <- factor(long_res$.id, levels = c(items2[both], items2[del2], items2[del], items2[c(-del, -del2, -both)]))

both <- which(sig & sig_2)
del <- which(sig & !sig_2)
del2 <- which(sig_2 & !sig)
for (i in 1:nrow(long_res)) {
  long_res$g[i] <- ifelse(long_res$.id[i] %in% items2[del], "nonzero delta", 
                          ifelse(long_res$.id[i] %in% items2[del2], "nonzero delta_2", 
                                 ifelse(long_res$.id[i] %in% items2[both], "both nonzero", "other")
                          )
  )
}

ggplot(subset(long_res, parameter %in% c("phi", "gamma", "delta", "delta_2"))) +
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(x = parameter, y = median, ymin = lower, ymax = upper, colour = g)) +
  facet_wrap(~ .id, scales = "free") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "bottom") +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(y = "Estimates", x = "Parameter")
ggsave("estimated_corrected_intervals.png", width = 9, height = 8)

################################################################################
# plot beiden

res2 <- lapply(1:length(items), readStanResults, item = items, pars = pars)
names(res2) <- items2

long_res2 <- data.table::rbindlist(res2, idcol = TRUE)

sig <- logical()
for(i in items2) {
  sig <- c(sig, (res2[[i]]["delta", "lower"] < 0 & res2[[i]]["delta", "upper"] < 0) | (res2[[i]]["delta", "lower"] > 0 & res2[[i]]["delta", "upper"] > 0))
}

sig_2 <- logical()
for(i in items2) {
  sig_2 <- c(sig_2, (res2[[i]]["delta_2", "lower"] < 0 & res2[[i]]["delta_2", "upper"] < 0) | (res2[[i]]["delta_2", "lower"] > 0 & res2[[i]]["delta_2", "upper"] > 0))
}

both <- which(sig & sig_2)
del <- which(sig & !sig_2)
del2 <- which(sig_2 & !sig)
for (i in 1:nrow(long_res2)) {
  long_res2$g[i] <- ifelse(long_res2$.id[i] %in% items2[del], "nonzero delta", 
                           ifelse(long_res2$.id[i] %in% items2[del2], "nonzero delta_2", 
                                  ifelse(long_res2$.id[i] %in% items2[both], "both nonzero", "other")
                           )
  )
}

long_res$interval <- "corrected"
long_res2$interval <- "not-corrected"

long <- rbind(long_res, long_res2)
ggplot(subset(long, parameter %in% c("phi", "gamma", "delta", "delta_2"))) +
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(x = parameter, y = median, ymin = lower, ymax = upper,
                      shape = interval, colour = g), position = position_dodge(width = 0.8)) +
  facet_wrap(~ .id, scales = "free") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1), legend.position = "bottom") +
  coord_cartesian(ylim = c(-1, 1)) +
  labs(y = "Estimates", x = "Parameter")

ggsave("estimated_corrected_intervals2.png", width = 10, height = 8)

################################################################################
df_res <- data.frame(matrix(NA, nrow = length(items), ncol = length(pars)))
row.names(df_res) <- items
names(df_res) <- pars
for (i in 1:length(items)) {
  df_res[i, ] <- res[[i]]$median
}

kableExtra::kbl(df_res)

nonZero <- function (r, res, p) {
  (res[[r]][p, "lower"] < 0 & res[[r]][p, "upper"] < 0) | (res[[r]][p, "lower"] > 0 & res[[r]][p, "upper"] > 0)
}


items2 <- gsub(".*_", "", items)
items2[3] <- "irritated"
items2[4] <- "satisfied"
items2[7] <- "enthusiastic"
items2[8] <- "suspicious"
items2[9] <- "cheerful"
items2[11] <- "indecisive"
items2[13] <- "restless"
items2[14] <- "agitated"
items2[16] <- "concentrate well"
items2[17] <- "like myself"
items2[18] <- "ashamed of myself"
items2[19] <- "doubt myself"
items2[20] <- "can handle anything"
items2[23] <- "in pain"
items2[25] <- "dry mouth"
items2[29] <- "physically active"

row.names(df_res) <- items2
nz <- sapply(1:length(items), nonZero, res, pars)
idx <- which(t(nz), arr.ind = TRUE)
library(dplyr)
df_kabel <- apply(df_res, 2, formatC, digits = 2, format = "f")
df_kabel[idx] <- kableExtra::cell_spec(df_kabel[idx], background = "lightgray", format = "latex")
kableExtra::kable(df_kabel, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "") %>%
  kableExtra::kable_classic()


unlist(res)
