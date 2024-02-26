dat <- read.csv("data groot/data/data.csv")

days <- unique(as.Date(dat$date))
dat$day <- match(as.Date(dat$date), days)

mod <- rstan::stan_model("stan/twolevel_ar.stan")

items <- names(dat)[grepl("mood_|pat_|phy_|se_", names(dat))]
N <- nrow(dat)
D <-  max(dat$day)

for(i in items) {
  y <- dat[, i]
  ii_mis <- which(is.na(y))
  ii_obs <- which(!is.na(y))
  y_obs <- y[ii_obs]
  
  dat_stan <- list(
    N = N, D = D, day = dat$day,
    n_mis = length(ii_mis),
    ii_obs = ii_obs, ii_mis = ii_mis,
    y_obs = y_obs
  )
  
  out <- rstan::sampling(mod, data = dat_stan,
                         pars = c("beta", "resvar",
                                  "theta", "muvar"),
                         seed = 1212)
  
  saveRDS(
    out,
    file = sprintf("%s%s.rds", "data groot/estimation/twolevel/", i)
  )
}

pars <- c("beta", "resvar",
          "theta", "muvar")

readStanResults <- function (r, item, pars) {
  out <- tryCatch({
    out <- readRDS(sprintf("%s%s.rds", "data groot/estimation/twolevel/", item[r]))
    
    sfit <- rstan::summary(out)$summary
    est <- sfit[pars, c("50%", "sd", "2.5%", "97.5%", "Rhat")]
    est <- as.data.frame(est)
    est <- cbind(pars, est)
    names(est) <- c("parameter", "median", "sd", "lower", "upper", "rhat")
    est
  }, error = function(e) return(NULL))
  
  return(out)
}

res <- lapply(1:length(items), readStanResults, item = items, pars = pars)
names(res) <- prettyNames()

long_res <- data.table::rbindlist(res, idcol = TRUE)

library(ggplot2)

ggplot(subset(long_res, parameter %in% c("beta", "muvar"))) +
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(x = parameter, y = median, ymin = lower, ymax = upper)) +
  facet_wrap(~ .id, scales = "free") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))



long_res$parameter <- factor(long_res$parameter, levels = pars)
long_res$.id <- factor(long_res$.id, levels = c(items2[sig], items2[sig_2], items2[!sig & !sig_2]))

long_res$g <- "other"
long_res$g[sig] <- "nonzero delta"
long_res$g[sig_2] <- "nonzero gamma"

for (i in 1:nrow(long_res)) {
  long_res$g[i] <- ifelse(long_res$.id[i] %in% items2[sig], "nonzero delta", 
                          ifelse(long_res$.id[i] %in% items2[sig_2], "nonzero delta_2", "other")
  )
}

ggplot(subset(long_res, parameter %in% c("phi", "gamma", "delta", "delta_2"))) +
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(x = parameter, y = median, ymin = lower, ymax = upper, colour = g)) +
  facet_wrap(~ .id, scales = "free") +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

sig <- logical()
for(i in items) {
  sig <- c(sig, (res[[i]]["delta", "lower"] < 0 & res[[i]]["delta", "upper"] < 0) | (res[[i]]["delta", "lower"] > 0 & res[[i]]["delta", "upper"] > 0))
}

sig_2 <- logical()
for(i in items) {
  sig_2 <- c(sig_2, (res[[i]]["delta_2", "lower"] < 0 & res[[i]]["delta_2", "upper"] < 0) | (res[[i]]["delta_2", "lower"] > 0 & res[[i]]["delta_2", "upper"] > 0))
}

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
