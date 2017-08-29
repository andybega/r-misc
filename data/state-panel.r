attempt_date <- function(date, by) {
  if (!class(date)=="Date") {
    try(date <- as.Date(date), silent=TRUE)
    if (class(date)=="Date") {
      message("Converting to date")
    } else if (by=="year") {
      try(date <- as.Date(paste0(date, "-06-30")), silent=TRUE)
      if (class(date)=="Date") {
        message("Converting to date with yyyy-06-30")
      }
    } else if (by=="month") {
      try(date <- as.Date(paste0(date, "-15")), silent=TRUE)
      if (class(date)=="Date") {
        message("Converting to date with yyyy-mm-15")
      }
    }
  }
  if (!class(date)=="Date") {
    stop(paste("Could not convert to class 'Date'"))
  }
  return(date)
}

state_panel <- function(start_date, end_date, by = "month", useGW=TRUE) {
  # problem potentially for situations where two cases with same ccode, and adjacent
  # dates are inclusive, since cshapes end/start dates are not overlapping
  require(cshapes)
  start_date <- attempt_date(start_date, by)
  end_date   <- attempt_date(end_date,   by)

  cshp_df <- cshp()@data

  if (useGW) {
    cshp_df$c_start <- as.Date(with(cshp_df, paste(GWSYEAR, GWSMONTH, GWSDAY, sep = "-")))
    cshp_df$c_end   <- as.Date(with(cshp_df, paste(GWEYEAR, GWEMONTH, GWEDAY, sep = "-")))
    cshp_df$c_code  <- cshp_df$GWCODE
  } else {
    cshp_df$c_start <- as.Date(with(cshp_df, paste(COWSYEAR, COWSMONTH, COWSDAY, sep = "-")))
    cshp_df$c_end   <- as.Date(with(cshp_df, paste(COWEYEAR, COWEMONTH, COWEDAY, sep = "-")))
    cshp_df$c_code  <- cshp_df$COWCODE
  }
  cshp_df <- cshp_df[!is.na(cshp_df$c_start), ]

  if (end_date > max(cshp_df$c_end)) message(paste0("end_date exceeds cshapes end date, using state of world in ", max(cshp_df$c_end)))
  cshp_df$c_end[cshp_df$c_end==max(cshp_df$c_end)] <- end_date

  # Prune country list
  cshp_df <- cshp_df[with(cshp_df, c_end > start_date & c_start < end_date), ]
  date_range <- seq.Date(start_date, end_date, by = by)
  cshp_df$id <- 1:nrow(cshp_df)
  df <- expand.grid(cshp_df$id, date_range)
  colnames(df) <- c("id", "date")
  cshp_df <- cshp_df[, c("id", "c_code", "c_start", "c_end")]
  df <- left_join(df, cshp_df, by = "id")
  df <- df[with(df, date >= c_start & date <= c_end), c("c_code", "date")]
  colnames(df) <- c(ifelse(useGW, "gwcode", "cowcode"), "date")

  df$id <- paste(df$date, df$gwcode)
  df
}


