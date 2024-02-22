D <- 10000
B <- 10
burnin <- 50

pars <- data.frame(mu = mu, phi = phi,
                   gamma = phi - delta, resvar_i = resvar_i)
var_i <- pars$resvar_i / (1 - pars$phi ^ 2)
df$resvar_1 <- var_i * (1 - pars$gamma ^ 2)
df$delta <- phi - pars$gamma
df$delta_2 <- phi ^ 6 - df$gamma
return(df)

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

saveRDS(
  out,
  file = sprintf("%sdat_days_%s_delta_%s_r_%s.rds", file, D, pars$delta, r)
)