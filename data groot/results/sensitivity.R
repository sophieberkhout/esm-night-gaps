load("data groot/results/interval hour/objects.Rdata")

df_bf_hour <- df_bayes_factors[, c(".id", "BF_pd", "BF_sd", "BF_cd")]
df_bf_hour <- tidyr::pivot_longer(df_bf_hour, cols = tidyr::starts_with("BF"))
df_bf_hour$set <- "hour"

df_par_hour <- subset(df_pars, parameter %in% c("phi", "gamma"))
df_par_hour$set <- "hour"

df_pmp_hour <- df_pmps
df_pmp_hour$set <- "hour"

.getDFs <- function (r, file, set) {
  load(file[r])
  
  par <- subset(df_pars, parameter %in% c("phi", "gamma"))
  par$set <- set[r]
  
  bf <- df_bayes_factors[, c(".id", "BF_pd", "BF_sd", "BF_cd")]
  bf <- tidyr::pivot_longer(bf, cols = tidyr::starts_with("BF"))
  bf$set <- set[r]
  
  pmp <- df_pmps
  pmp$set <- set[r]
  
  return(list(par = par, bf = bf, pmp = pmp))
}

test <- .getDFs("data groot/results/interval hour/objects.Rdata", "hour")

set <- c("main", "prior narrow", "prior wide", "prior nonzero",
         "interval hour", "interval uncorrected")

fileNames <- sprintf("data groot/results/%s/objects.Rdata", set)

test <- sapply(1:length(fileNames), .getDFs, file = fileNames, set = set)

all_bfs <- data.table::rbindlist(test["bf", ])
all_pars <- data.table::rbindlist(test["par", ])
all_pmps <- data.table::rbindlist(test["pmp", ])

library(ggplot2)

all_pars$set <- factor(all_pars$set, levels = set[c(2, 1, 3:6)])
df <- subset(all_pars, set %in% c("main", "interval hour", "interval uncorrected"))
levels(df$set) <- c("NA", "30", "NA", "NA", "60", "90")
ggplot(df) +
  geom_pointrange(aes(x = set, y = median, ymin = lower, ymax = upper,
                      colour = parameter)) +
  geom_line(aes(x = set, y = median, colour = parameter, group = parameter)) +
  facet_wrap(~ .id, ncol = 5) +
  coord_cartesian(ylim = c(-0.1, 1))

all_bfs$set <- factor(all_bfs$set, levels = set[c(2, 1, 3:6)])
ggplot(all_bfs) +
  geom_point(aes(x = set, y = value, colour = name)) +
  facet_wrap(~ .id, ncol = 5, scales = "free") +
  scale_y_continuous(trans = "log10")

all_pmps$set <- factor(all_pmps$set, levels = set[c(2, 1, 3:6)])
ggplot(all_pmps) +
  geom_point(aes(x = set, y = value, colour = name)) +
  facet_wrap(~ item, ncol = 5)
