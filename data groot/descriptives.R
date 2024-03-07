dat <- read.csv("data groot/data/data.csv")
dat_raw <- read.csv("data groot/data/ESMdata.csv")
dat_post <- dat_raw[dat_raw$phase %in% c(4, 5), ]
dat <- dat_post[, grepl("dayno|mood_|pat_|phy_|se_", names(dat_post))]

colSums(is.na(dat))

varItems <- apply(dat, 2, var, use = "complete.obs")
varItems[varItems < 0.1]

dat <- read.csv("data groot/data/data_uncorrected.csv")
datetime <- as.POSIXct(dat$datetime, tz = "UTC")  # stamp to date
timeonly <- format(datetime, "%H:%M:%S")
firstblock <- timeonly >= "07:30" & timeonly <= "09:00"
lastblock <- timeonly >= "21:00" & timeonly <= "22:30"
hist(timeonly[firstblock])

dat_esm <- dat[, grepl("mood_|pat_|phy_|se_", names(dat))]
range(colSums(is.na(dat_esm)))
range(colSums(is.na(dat_esm)) / nrow(dat_esm))

df <- data.frame()
for(day in unique(dat$date)) {
  nna <- colSums(!is.na(subset(dat, date == day)))
  df <- rbind(df, nna)
}
names(df) <- names(dat)
colMeans(df)
df_esm <- df[, grepl("mood_|pat_|phy_|se_", names(df))]
range(colMeans(df_esm))

# get interval lengths from plot_histogram_intervals


# dat corrected
dat <- read.csv("data groot/data/data.csv")
dat_esm <- dat[, grepl("mood_|pat_|phy_|se_", names(dat))]
sum(is.na(dat_esm)) / (nrow(dat_esm) * ncol(dat_esm))
colSums(is.na(dat_esm)) / nrow(dat_esm)
