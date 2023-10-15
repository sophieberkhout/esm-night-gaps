data {
  int<lower=1> N;                           // number of observations
  int<lower=1, upper=N> D;                  // number of days
  vector[N] y;                              // variable
  int<lower=2, upper=N> bi[N - D];          // index for all beeps except first
  int<lower=1, upper=N - 1> bi_lag[N - D];  // index for bi lagged
  int<lower=1, upper=N> b1[D - 1];          // index for first beep
  int<lower=1, upper=N - 1> b1_lag[D - 1];  // index for b1 lagged
}
parameters {
  real mu;             // grand mean
  real phi;           // autoregression between beeps
  real gamma;          // autoregression last beep yesterday first beep today
  real<lower=0> resvar_i; // residual variance beeps except first
  real<lower=0> resvar_1; // residual variance first beep
}
model {
  // priors
  mu ~ normal(0, 100);
  phi ~ normal(0, 100);
  gamma ~ normal(0, 100);
  resvar_i ~ inv_gamma(0.001, 0.001);
  resvar_1 ~ inv_gamma(0.001, 0.001);

  // likelihood
  y[bi] ~ normal(mu + phi * (y[bi_lag] - mu), sqrt(resvar_i));
  y[b1] ~ normal(mu + gamma * (y[b1_lag] - mu), sqrt(resvar_1));
}
generated quantities {
  real delta;
  real delta_2;
  
  delta = phi - gamma;
  delta_2 = phi ^ 6 - gamma;
}
