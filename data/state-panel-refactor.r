################################################################################
##
##    Create a blank panel data set based on Gleditsch and Ward or COW.
##    Andreas Beger
##    10 Dec. 2013
##
##    G&W: http://privatewww.essex.ac.uk/~ksg/statelist.html
##    COW: http://www.correlatesofwar.org/datasets.html
##
################################################################################

state_panel_old <- function(start.date, end.date, by="month", useGW=TRUE) {
  require(cshapes)
  panel.start <- attempt_date(start.date, by)
  panel.end <- attempt_date(end.date, by)
  
  # Create vector of desired dates
  date <- seq.Date(panel.start, panel.end, by=by)
  
  # Initialize results panel
  panel <- data.frame(NULL)
  
  # Get full data and subset by date; fill in results
  cshp.full <- cshp()@data
  max.cshp.date <- max(as.Date(paste(cshp.full$GWEYEAR, cshp.full$GWEMONTH, cshp.full$GWEDAY, sep = "-")), na.rm=T)
  
  # Progress bar
  pb <- txtProgressBar(1, length(date), initial=1, style=3, width=60)
  
  for (i in seq_along(date)) {
    if (useGW) {
      ctry.start <- as.Date(paste(cshp.full$GWSYEAR, cshp.full$GWSMONTH, cshp.full$GWSDAY, sep = "-"))
      ctry.end   <- as.Date(paste(cshp.full$GWEYEAR, cshp.full$GWEMONTH, cshp.full$GWEDAY, sep = "-"))
      if (date[i] <= max.cshp.date) {
      	date.slice <- cshp.full[ctry.start <= date[i] & ctry.end >= date[i], "GWCODE"]
        date.slice <- date.slice[!is.na(date.slice)]  # fix for NA dates in GW
      } else {
      	date.slice <- cshp.full[ctry.start <= max.cshp.date & ctry.end >= max.cshp.date, "GWCODE"]
      	date.slice <- date.slice[!is.na(date.slice)]  # fix for NA dates in GW
      	warning(paste0("Exceeding cshapes max date, using ", max.cshp.date, " instead of ", date[i]))
      }
    } else if (!useGW) {
      ctry.start <- as.Date(paste(cshp.full$COWSYEAR, cshp.full$COWSMONTH, cshp.full$COWSDAY, sep = "-"))
      ctry.end   <- as.Date(paste(cshp.full$COWEYEAR, cshp.full$COWEMONTH, cshp.full$COWEDAY, sep = "-"))
      if (date[i] <= max.cshp.date) {	
        date.slice <- cshp.full[ctry.start <= date[i] & ctry.end >= date[i], "COWCODE"]
      } else {
      	date.slice <- cshp.full[ctry.start <= max.cshp.date & ctry.end >= max.cshp.date, "COWCODE"]
      	warning(paste0("Exceeding cshapes max date, using ", max.cshp.date, " instead of ", date[i]))
      }
    }
    # Append to results panel
    panel <- rbind(panel, data.frame(ccode=date.slice, date=date[i]))
    
    # Update progress bar
    setTxtProgressBar(pb, i)
  }
  # Create unique ID
  panel$id <- paste(panel$date, panel$ccode)
  panel <- panel[order(panel$ccode, panel$date), ]
  close(pb)
  return(panel)
}

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

# Example
#example1 <- statePanel("2000", "2010", by="year")
#example2 <- statePanel("1955-01", "2012-06", by="month")

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


library(microbenchmark)

start_date    <- as.Date("1990-01-01")
end_date      <- as.Date("2014-12-30")

foo1 <- NULL
foo2 <- state_panel2(start_date, end_date, by="month", useGW=TRUE)
dim(foo1); dim(foo2)
#foo2$id[!foo2$id %in% foo1$id]
#foo1$id[!foo1$id %in% foo2$id]

mbm <- microbenchmark(
  state_panel(start_date, end_date, by="month", useGW=TRUE),
  state_panel2(start_date, end_date, by="month", useGW=TRUE),
  times=100
  )

autoplot(mbm)



