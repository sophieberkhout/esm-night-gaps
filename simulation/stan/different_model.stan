data {
  int<lower=1> N;
  int<lower=1, upper=N> D;
  int<lower=1, upper=N> B;
  int<lower=1, upper=N> db1[D - 1];
  int<lower=2, upper=N> dbi[B];
  vector[N] y;
}
transformed data {
  int<lower=1, upper=N - 1> db1_lag[D - 1];
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
  real gamma; 
  real<lower=0> sigma_2;
  real<lower=0> psi_2;
}
model {
  // priors
  mu ~ normal(0, 100);
  phi ~ normal(0, 100);
  gamma ~ normal(0, sqrt(0.5));
  sigma_2 ~ inv_gamma(0.001, 0.001);
  psi_2 ~ inv_gamma(0.001, 0.001);
  y[1] ~ normal(0, 100);

  // likelihood
  y[dbi] ~ normal(mu + phi * (y[dbi_lag] - mu), sqrt(sigma_2));
  y[db1] ~ normal(mu + gamma * (y[db1_lag] - mu), sqrt(psi_2));
}
generated quantities {
  real diff;
  real diff_ct;
  
  diff = gamma - phi;
  diff_ct = gamma - phi ^ 7;
}
