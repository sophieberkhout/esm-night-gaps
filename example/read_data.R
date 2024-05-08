source("example/functions_data.R")

dat_raw <- read.csv("example/data/ESMdata.csv")

# keep only last two phases of the study (post medication reduction)
dat_post <- dat_raw[dat_raw$phase %in% c(4, 5), ]

# create datetime variable
dat_post$datetime <- as.POSIXct(paste(dat_post$date, dat_post$resptime_s),
                                tz ="UTC", format = "%d/%m/%y %H:%M:%S")

# keep only variables of interest
dat <- dat_post[, grepl("datetime|mood_|pat_|phy_|se_", names(dat_post))]

# correct intervals
dat_30 <- correctInterval(dat, min_int = 30,
                       as_first = "08:15:00", as_last = "21:45:00")
dat_60 <- correctInterval(dat, min_int = 60,
                       as_first = "08:15:00", as_last = "21:45:00")
dat_90 <- correctInterval(dat, min_int = 90,
                          as_first = "08:15:00", as_last = "21:45:00")

# add missing beep numbers for uncorrected data (leads to 1410 obs)
dat_uncorrected <- dat_post[, grepl("datetime|beepno|mood_|pat_|phy_|se_",
                                    names(dat_post))]
dat_uncorrected$date <- as.Date(dat_uncorrected$datetime)
dates <- unique(as.Date(dat_30$datetime))
df_beeps <- data.frame(beepno = rep(1:10, length(dates)),
                       date = rep(dates, each = 10))
dat_uncorrected <- merge(df_beeps, dat_uncorrected,
                         by = c("date", "beepno"), all = TRUE)
dat_uncorrected$day <- rep(1:length(dates), each = 10)

# save data
write.csv(dat_30, row.names = FALSE, file = "example/data/data.csv")
write.csv(dat_60, row.names = FALSE, file = "example/data/data_60min.csv")
write.csv(dat_90, row.names = FALSE, file = "example/data/data_90min.csv")
write.csv(dat_uncorrected, row.names = FALSE,
          file = "example/data/data_uncorrected.csv")
