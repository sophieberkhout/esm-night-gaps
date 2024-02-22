data {
  int<lower=1> N;                           // number of observations
  int<lower=1, upper=N> D;                  // number of days
  vector[N] y;                              // variable
  int<lower=1, upper=D> d[N];               // day
  int<lower=2, upper=N> bi[N - D];          // index for all beeps except first
  int<lower=1, upper=N - 1> bi_lag[N - D];  // index for bi lagged
  int<lower=1, upper=N> b1[D - 1];          // index for first beep
  int<lower=1, upper=N - 1> b1_lag[D - 1];  // index for b1 lagged
}
parameters {
  vector[D] mu;           // day means
  real theta;             // grand mean
  real phi;           // autoregression between beeps
  real gamma;          // autoregression last beep yesterday first beep today
  real beta;            // autoregression between day means
  real<lower=0> resvar_i; // residual variance beeps except first
  real<lower=0> resvar_1; // residual variance first beep
  real<lower=0> var_mu;   // variance of day means
}
model {
  // priors
  mu ~ normal(0, 100);
  theta ~ normal(0, 100);
  phi ~ normal(0, 100);
  gamma ~ normal(0, 100);
  beta ~ normal(0, 100);
  resvar_i ~ inv_gamma(0.001, 0.001);
  resvar_1 ~ inv_gamma(0.001, 0.001);
  var_mu ~ inv_gamma(0.001, 0.001);

  // likelihood
  y[bi] ~ normal(mu[d[bi]] + phi * (y[bi_lag] - mu[d[bi]]), sqrt(resvar_i));
  y[b1] ~ normal(mu[d[b1]] + gamma * (y[b1_lag] - mu[d[b1]]), sqrt(resvar_1));
  mu[2:D] ~ normal(theta + beta * mu[1:(D - 1)], sqrt(var_mu));
}
