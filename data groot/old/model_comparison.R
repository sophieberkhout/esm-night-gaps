stanCode <- function (gamma = c(0, 100)) {
  par <- prior <- delta <- ""
  eq <- 0
  if (!is.null(gamma)) {
    par <- "real gamma;"
    prior <- sprintf("target += normal_lpdf(gamma | %s, %s);",
                     gamma[1], gamma[2])
    eq <- "gamma"
    delta <- "
    real delta;
    real delta_2;
    
    delta = gamma - phi;
    delta_2 = gamma - phi ^ 18;
    "
    
  }
  sprintf("
  data {
    int<lower=1> N;
    int<lower=1, upper=N> D;
    int<lower=1, upper=N> B;
    int<lower=1, upper=N> d1[D];
    int<lower=2, upper=N> db[B];
    int<lower=0, upper=N> n_mis;
    int<lower=1, upper=N> ii_obs[N - n_mis];
    int<lower=1, upper=N> ii_mis[n_mis];
    vector[N - n_mis] y_obs;
    
  }
  transformed data {
    int<lower=1, upper=N> d1_lag[D];
    int<lower=1, upper=N - 1> db_lag[B];
    // int<lower=1, upper=N> n_obs = N - n_mis;
    for (i in 1:D) {
      d1_lag[i] = d1[i] - 1;
    }
    for (i in 1:B) {
      db_lag[i] = db[i] - 1;
    }
  }
  parameters {
    real mu;
    real phi;
    %1$s
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
    target += normal_lpdf(mu | 0, 100);
    target += normal_lpdf(phi | 0, 100);
    %2$s
    target += inv_gamma_lpdf(resvar_i | 0.001, 0.001);
    target += inv_gamma_lpdf(resvar_1 | 0.001, 0.001);
    target += normal_lpdf(y[1] | 0, 100);
  
    // likelihood
    target += normal_lpdf(y[db] | mu + phi * (y[db_lag] - mu), sqrt(resvar_i));
    target += normal_lpdf(y[d1] | mu + %3$s * (y[d1_lag] - mu), sqrt(resvar_1));
  }
  generated quantities {
    vector[N - 1] log_lik;
    
    %4$s

    for (n in d1) log_lik[n - 1] = normal_lpdf(y[n] | mu + %3$s * (y[n - 1] - mu), sqrt(resvar_1));
    for (n in db) log_lik[n - 1] = normal_lpdf(y[n] | mu + phi * (y[n - 1] - mu), sqrt(resvar_i));
  }
  ", par, prior, eq, delta)
}


code_gamma <- stanCode()
code_zero <- stanCode(NULL)

library(rstan)
mod_gamma <- stan_model(model_code = code_gamma, model_name = "gamma")
mod_zero <- stan_model(model_code = code_zero, model_name = "zero")

dat <- read.csv("data groot/data/data.csv")

d1 <- logical(nrow(dat))
for (i in 1:nrow(dat)) {
  d1[i] <- ifelse(min(which(dat$day == dat$day[i])) == i, TRUE, FALSE)
}
db <- which(d1 != 1)
d1 <- which(d1)[-1]

# db <- which(d1 != 1 & !is.na(y) & !is.na(c(NA, y[1:(length(y) - 1)])))
# d1 <- which(d1 & !is.na(y) & !is.na(c(NA, y[1:(length(y) - 1)])))

y <- dat[, "mood_doubt"]
ii_mis <- which(is.na(y))
ii_obs <- which(!is.na(y))
y_obs <- y[ii_obs]

# stan_dat <- list(
#   N = N, D = length(d1), B = length(db), y = dat$y,
#   d1 = d1, db = db
# )

dat_stan <- list(
  N = nrow(dat), D = length(d1), B = length(db),
  d1 = d1, db = db, n_mis = length(ii_mis),
  ii_mis = ii_mis, ii_obs = ii_obs,
  y_obs = y_obs
)

out_gamma <- sampling(mod_gamma, data = dat_stan, seed = 1,
                      iter = 5000, warmup = 1000, cores = 4)
out_zero <- sampling(mod_zero, data = dat_stan, seed = 1,
                     iter = 5000, warmup = 1000, cores = 4)

library(bridgesampling) 
#-898.0731
H_gamma <- bridge_sampler(out_gamma, maxiter = 5000)
H_zero <- bridge_sampler(out_zero, maxiter = 5000)

library(loo)
loo_gamma <- loo(out_gamma)
test <- loo::extract_log_lik(out_gamma)
test_obs <- test[, c(db - 1, d1 - 1)]
test2 <- loo::extract_log_lik(out_zero)
test_obs2 <- test2[, c(db - 1, d1 - 1)]
loo::waic(test)
loo::kfold(out_gamma, K = 10)

loo::waic(test_obs)
loo::waic(test_obs2)
loo_gamma <- loo::loo(test_obs)
loo_zero <- loo::loo(test_obs2)
loo::loo_compare(loo_gamma, loo_zero)
loo::loo_model_weights(list(loo_gamma, loo_zero), method = "pseudobma")
