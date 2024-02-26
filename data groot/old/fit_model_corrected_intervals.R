dat <- data.i

items <- names(dat)[grepl("mood_|pat_|phy_|se_", names(dat))]
mod <- rstan::stan_model("stan/ar_beep_night_corrected_intervals.stan")

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
  # N <- length(idx_use)
  # first <- which(idx_use %in% firstbeep)
  # y <- dat[idx_use, item]
  # miss_i <- which(is.na(y))
  # obs <- y[-miss_i]
  
  b1 <- which(idx_use %in% firstbeep)
  bi <- (1:length(idx_use))[-b1]
  b1_lag <- which(idx_use %in% lastbeep)[-length(lastbeep)]
  bi_lag <- bi - 1
  b1 <- b1[-1]
  
  obs <- dat[, item]
  obs <- obs[idx_use]
  
  miss_i <- which(is.na(obs))
  
  obs[miss_i] <- -99
  
  # b1 <- firstbeep[2:length(firstbeep)]
  # b1_lag <- lastbeep[1:(length(lastbeep) - 1)]
  # 
  # bi <- numeric()
  # for (j in 1:length(firstbeep)) {
  #   bi <- c(bi, (firstbeep[j] + 1):lastbeep[j])
  # }
  # 
  # bi_lag <- bi - 1
  # 
  # obs <- dat[, item]
  # all_idx <- sort(unique(c(b1, b1_lag, bi, bi_lag)))
  # 
  # na_obs <- is.na(obs)
  # obs[na_obs] <- -99
  # 
  # miss_i <- which(na_obs) %in% all_idx
  
  dat_stan <- list(
    N = length(obs), D = length(b1) + 1, B = length(bi),
    obs = as.numeric(obs),
    bi = bi, bi_lag = bi_lag,
    b1 = b1, b1_lag = b1_lag,
    miss_n = length(miss_i),
    miss_i = miss_i
  )
  
  out <- rstan::sampling(mod, data = dat_stan,
                         pars = c("mu", "phi", "gamma",
                                  "resvar_i", "resvar_1",
                                  "delta", "delta_2"),
                         seed = 1212)
  
  saveRDS(
    out,
    file = sprintf("%s%s.rds", "data groot/estimation/cor/", item)
  )
}

pars <- c("mu", "phi", "gamma",
          "resvar_i", "resvar_1",
          "delta", "delta_2")

readStanResults <- function (r, item, pars) {
  out <- tryCatch({
    out <- readRDS(sprintf("%s%s.rds", "data groot/estimation/cor/", item[r]))
    
    sfit <- rstan::summary(out)$summary
    est <- sfit[pars, c("50%", "sd", "2.5%", "97.5%", "Rhat")]
    est <- as.data.frame(est)
    est <- cbind(pars, est)
    names(est) <- c("parameter", "median", "sd", "lower", "upper", "rhat")
    est
  }, error = function(e) return(NULL))
  
  return(out)
}

library(ggplot2)

res <- lapply(1:length(items), readStanResults, item = items, pars = pars)
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
