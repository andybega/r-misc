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

statePanel <- function(start.date, end.date, by="month", useGW=TRUE) {
  require(cshapes)
  panel.start <- attemptDate(start.date, by)
  panel.end <- attemptDate(end.date, by)
  
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
        date.slice <- date.slice[!is.na(date.slice$GWCODE), ]  # fix for NA dates in GW
      } else {
      	date.slice <- cshp.full[ctry.start <= max.cshp.date & ctry.end >= max.cshp.date, "GWCODE"]
      	date.slice <- date.slice[!is.na(date.slice$GWCODE), ]  # fix for NA dates in GW
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
  return(panel)
}

attemptDate <- function(date, by) {
  if (!class(date)=="Date") {
  	try(date <- as.Date(date), silent=TRUE)
  	if (class(date)=="Date") {
  	  warning("Converting to 'Date' class")
    } else if (by=="year") {
  	  try(date <- as.Date(paste0(date, "-06-30")), silent=TRUE)
  	  if (class(date)=="Date") {
  	  	warning("Converting to 'Date' class with yyyy-06-30")
  	  }
  	} else if (by=="month") {
  	  try(date <- as.Date(paste0(date, "-15")), silent=TRUE)
  	  if (class(date)=="Date") {
  	  	warning("Converting to 'Date' class with yyyy-mm-15")
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
