dat <- data.i

items <- names(dat)[grepl("mood_|pat_|phy_|se_", names(dat))]
mod <- rstan::stan_model("stan/twolevel_ar_phi.stan")

timeonly <- format(dat$datetime, "%H:%M:%S")
firstblock <- timeonly >= "07:30" & timeonly <= "09:00"
lastblock <- timeonly >= "21:00" & timeonly <= "22:30"

for(item in items) {
  firstbeep <- logical()
  lastbeep <- logical()
  for (d in unique(dat$day)) {
    idx_day <- which(dat$day == d)
    
    idx_firstblock <- idx_day[(firstblock[idx_day])]
    firstna <- is.na(dat[idx_firstblock, item])
    if (sum(!firstna) > 0) {
      whichfirst <- min(idx_firstblock[!firstna])
    } else {
      closestToFirstHalf <- abs(difftime(
        dat$datetime[idx_day],
        as.POSIXct(paste(format(dat$datetime[idx_day], "%Y-%m-%d"), "08:15:00"),
                   tz = "UTC")
      ))
      whichfirst <- idx_day[which.min(closestToFirstHalf)]
    }
    firstbeep <- c(firstbeep, whichfirst)
    
    idx_lastblock <- idx_day[(lastblock[idx_day])]
    lastna <- is.na(dat[idx_lastblock, item])
    if (sum(!lastna) > 0) {
      whichlast <- max(idx_lastblock[!lastna])
    } else {
      closestToLastHalf <- abs(difftime(
        dat$datetime[idx_day],
        as.POSIXct(paste(format(dat$datetime[idx_day], "%Y-%m-%d"), "21:45:00"),
                   tz = "UTC")
      ))
      whichlast <- idx_day[which.min(closestToLastHalf)]
    }
    lastbeep <- c(lastbeep, whichlast)
  }
  
  otherbeeps <- numeric()
  for (j in 1:length(firstbeep)) {
    otherbeeps <- c(otherbeeps, (firstbeep[j] + 1):(lastbeep[j] - 1))
  }
  
  idx <- 1:nrow(dat)
  idx_use <- sort(c(firstbeep, lastbeep, otherbeeps))

  d1 <- which(idx_use %in% firstbeep)
  db <- (1:length(idx_use))[-d1]

  y <- dat[idx_use, item]
  day <- dat[idx_use, "day"]
  
  ii_mis <- which(is.na(y))
  ii_obs <- which(!is.na(y))
  y_obs <- y[ii_obs]
  
  dat_stan <- list(
    N = length(y), D = max(dat$day), B = length(db),
    y_obs = as.numeric(y_obs),
    day = day[db], d1 = d1, db = db, db_lag = db_lag,
    n_mis = length(ii_mis),
    ii_obs = ii_obs, ii_mis = ii_mis
  )
  
  # maybe also get mu, for plotting
  out <- rstan::sampling(mod, data = dat_stan,
                         pars = c("phi", "beta",
                                  "theta", "muvar", "resvar"),
                         seed = 1212)
  
  saveRDS(
    out,
    file = sprintf("%s%s.rds", "data groot/estimation/twolevel_phi/", item)
  )
}

pars <- c("phi", "beta", "resvar",
          "theta", "muvar")

readStanResults <- function (r, item, pars) {
  out <- tryCatch({
    out <- readRDS(sprintf("%s%s.rds", "data groot/estimation/twolevel_phi/", item[r]))
    
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
names(res) <- items2

long_res <- data.table::rbindlist(res, idcol = TRUE)

library(ggplot2)

ggplot(subset(long_res, parameter %in% c("beta", "phi", "muvar"))) +
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
