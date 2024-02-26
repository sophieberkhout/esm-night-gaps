dat <- dat2
# N <- nrow(dat)
# D <- length(unique(dat$date))
# idx <- 1:N
# b1 <- which(dat$beepno == 1)
# bi <- idx[-b1]
# b1 <- b1[-1]
# b1_lag <- b1 - 1
# bi_lag <- bi - 1

items <- names(dat)[grepl("mood_|pat_|phy_|se_", names(dat))]
mod <- rstan::stan_model("stan/ar_beep_night_no_missing.stan")

for(i in items) {
  miss_i <- is.na(dat[, i])
  
  is1 <- dat$beepno == 1 & !miss_i
  is1_lag <- dat$beepno == 10 & !miss_i
  i1_lag <- which(is1[2:length(is1)] & is1_lag[1:(length(is1) - 1)])
  i1 <- i1_lag + 1
  
  isb <- dat$beepno != 1 & !miss_i
  isb_lag <- dat$beepno != 10 & !miss_i
  ib_lag <- which(isb[2:length(isb)] & isb_lag[1:(length(isb) - 1)])
  ib <- ib_lag + 1
  
  idx <- 1:nrow(dat)
  y <- dat[sort(unique(c(i1, i1_lag, ib, ib_lag))), i]
  idx <- idx[sort(unique(c(i1, i1_lag, ib, ib_lag)))]
  d1 <- which(idx %in% i1)
  db <- which(idx %in% ib)
  
  dat_stan <- list(
    N = length(y), D = length(d1), B = length(db),
    d1 = d1, db = db,
    y = y
  )
  
  out <- rstan::sampling(mod, data = dat_stan,
                         pars = c("mu", "phi", "gamma",
                                  "resvar_i", "resvar_1",
                                  "delta", "delta_2"),
                         seed = 1212)
  
  saveRDS(
    out,
    file = sprintf("%s%s.rds", "data groot/estimation/eq_no_mis/", i)
  )
}

pars <- c("mu", "phi", "gamma",
          "resvar_i", "resvar_1",
          "delta", "delta_2")

readStanResults <- function (r, folder, item, pars) {
  out <- tryCatch({
    out <- readRDS(sprintf("%s%s/%s.rds", "data groot/estimation/", folder, item[r]))
    
    sfit <- rstan::summary(out)$summary
    est <- sfit[pars, c("50%", "sd", "2.5%", "97.5%", "Rhat")]
    est <- as.data.frame(est)
    est <- cbind(pars, est)
    names(est) <- c("parameter", "median", "sd", "lower", "upper", "rhat")
    est
  }, error = function(e) return(NULL))
  
  return(out)
}

res <- lapply(1:length(items), readStanResults, folder = "eq_no_mis", item = items, pars = pars)
res2 <- lapply(1:length(items), readStanResults, folder = "eq", item = items, pars = pars)
names(res) <- items2
names(res2) <- items2


long_res <- data.table::rbindlist(res, idcol = TRUE)
long_res2 <- data.table::rbindlist(res2, idcol = TRUE)
long_res$mis <- "yes"
long_res2$mis <- "no"

library(ggplot2)
long_res$parameter <- factor(long_res$parameter, levels = pars)
long_res2$parameter <- factor(long_res2$parameter, levels = pars)

long <- rbind(long_res, long_res2)

ggplot(subset(long, parameter %in% c("phi", "gamma", "delta", "delta_2"))) +
  geom_hline(yintercept = 0) +
  geom_pointrange(aes(x = parameter, y = median, ymin = lower, ymax = upper, colour = mis)) +
  facet_wrap(~ .id) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  coord_cartesian(ylim = c(-1, 1))


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
