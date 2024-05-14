correctInterval <- function(dat, min_int,
                            as_first = "08:15:00", as_last = "21:45:00") {
  # dat should be a data frame with a column called 'datetime'
  # the other columns should represent the observations
  
  min_int <- min_int * 60 # minutes to seconds
  
  n_obs <- nrow(dat) # number of observations
  
  dat$stamp <- as.numeric(dat$datetime) # numeric time stamp
  
  dt <- diff(dat$stamp) # time difference
  
  # number of minimal intervals to be added between observations
  n_min_int <- round(dt / min_int) - 1
  n_min_int[n_min_int < 0 ]   <- 0 # cannot be lower than zero

  # difference in time divided by number of intervals (+ 1)
  dt_min_int <- round(dt / (n_min_int + 1))

  # combine new stamps with existing stamps
  new_stamp <- numeric()
  
  for (t in 1:length(dt)) {
    # add new stamps if needed
    if (n_min_int[t] != 0) {
      new_stamp_t <- c(dat$stamp[t],
                       dat$stamp[t] + 1:n_min_int[t] * dt_min_int[t])
    } else {
      new_stamp_t <- dat$stamp[t]
    }
    new_stamp <- c(new_stamp, new_stamp_t)
  }
  
  # merge dat with new stamps to add NA's
  new_stamp_df <- data.frame(stamp = new_stamp)
  merge_dat <- merge(new_stamp_df, dat, by = "stamp", all = TRUE)

  datetime <- as.POSIXct(merge_dat$stamp, tz = "UTC")  # stamp to date
  difftime <- c(round(diff(merge_dat$stamp) / 60), NA) # difference in minutes
  day      <- match(as.Date(datetime), unique(as.Date(datetime))) # day number
  
  # rearrange columns
  new_dat <- cbind(datetime, day, difftime,
                   merge_dat[, !grepl("datetime|stamp", names(merge_dat))])
  
  # remove night beeps
  final_dat <- .removeNight(new_dat, as_first, as_last)
  
  cat(paste0("NA's added: ", nrow(final_dat) - n_obs, " (",
             round((nrow(final_dat) - n_obs) / nrow(final_dat), 2) * 100,
             "%)\n"))
  
  return(final_dat)
  
}

.removeNight <- function(dat, as_first, as_last) {
  # check which beeps are in time frame of first and last block
  timeonly <- format(dat$datetime, "%H:%M:%S")
  firstblock <- timeonly >= "07:30" & timeonly <= "09:00"
  lastblock <- timeonly >= "21:00" & timeonly <= "22:30"
  
  within_day <- numeric()
  for (d in unique(dat$day)) {
    day <- dat$day == d # get day
    
    # first block of day
    firstblock_d <- day & firstblock
    
    # check what columns are observations so not datetime, day and difftime
    # pick arbitrary first column
    col_obs <- names(dat)[!names(dat) %in% c("datetime", "day", "difftime")][1]
    # check if first block of the day has observations for arbitrary column
    first_obs <- !is.na(dat[firstblock_d, col_obs])
    # if no observations in first block, use closest to as_first as first
    # if observations in first block, use earliest observation as first
    if (sum(first_obs) == 0) {
      closest_first <- .closestToTime(dat$datetime[day], as_first)
      which_first <- which(day)[closest_first]
    } else {
      which_first <- min(which(firstblock_d)[first_obs])
    }

    # last block of day
    lastblock_d <- day & lastblock
    
    # check if last block of the day has observations for arbitrary column
    last_obs <- !is.na(dat[lastblock_d, col_obs])
    # if no observations in last block, use closest to as_last as last
    # if observations in last block, use latest observation as last
    if (sum(last_obs) == 0) {
      closest_last <- .closestToTime(dat$datetime[day], as_last)
      which_last <- which(day)[closest_last]
    } else {
      which_last <- max(which(lastblock_d)[last_obs])
    }
    
    # all beeps within the day
    within_day <- c(within_day, which_first:which_last)
  }
  
  return(dat[within_day, ])
  
}

.closestToTime <- function(datetime, time) {
  # check which in datetime is closest to time
  x <- difftime(
    datetime,
    as.POSIXct(paste(format(datetime, "%Y-%m-%d"), time), tz = "UTC")
  )
  
  idx <- which.min(abs(x))
  
  return(idx)
  
}
