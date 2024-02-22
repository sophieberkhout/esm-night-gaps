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
    delta_2 = gamma - phi ^ 6;
    "
    
  }
  sprintf("
  data {
    int<lower=1> N;
    int<lower=1, upper=N> D;
    int<lower=1, upper=N> B;
    int<lower=1, upper=N> d1[D];
    int<lower=2, upper=N> db[B];
    vector[N] y;
    
  }
  transformed data {
    int<lower=1, upper=N> d1_lag[D];
    int<lower=1, upper=N - 1> db_lag[B];
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
  }
  model {
    // priors
    target += normal_lpdf(mu | 0, 100);
    target += normal_lpdf(phi | 0, 100);
    %2$s
    target += inv_gamma_lpdf(resvar_i | 0.001, 0.001);
    target += inv_gamma_lpdf(resvar_1 | 0.001, 0.001);

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


dat <- readRDS("simulation/data/phi0.5/dat_days_200_delta_0_r_1.rds")
# get indices for the beeps and first beep
N <- 10 * 200
idx <- 1:N
d1 <- seq(10 + 1, N, 10)
db <- idx[-c(1, d1)]

# put data in stan format
stan_dat <- list(
  N = N, D = length(d1), B = length(db), y = dat$y,
  d1 = d1, db = db
)
stan_dat

out_gamma <- sampling(mod_gamma, data = stan_dat, seed = 1,
                      iter = 5000, warmup = 1000, cores = 4)
out_zero <- sampling(mod_zero, data = stan_dat, seed = 1,
                     iter = 5000, warmup = 1000, cores = 4)

library(bridgesampling) 
H_gamma <- bridge_sampler(out_gamma, maxiter = 5000)
H_zero <- bridge_sampler(out_zero, maxiter = 5000)
bayes_factor(H_gamma, H_zero)
post_prob(H_gamma, H_zero)

library(loo)
loo_gamma <- loo(out_gamma)
loo_zero <- loo(out_zero)
loo_model_weights(list(loo_gamma, loo_zero), method = "pseudobma")
