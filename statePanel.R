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
  start.date <- attemptDate(start.date, by)
  end.date <- attemptDate(end.date, by)
  # Create vector of desired dates
  date <- seq.Date(start.date, end.date, by=by)
  # Initialize results panel
  panel <- data.frame(NULL)
  for (i in seq_along(date)) {
    if (useGW) {
      date.slice <- cshp(date=date[i], useGW=TRUE)$GWCODE
    } else if (!useGW) {
      date.slice <- cshp(date=date[i], useGW=FALSE)$COWCODE
    }
    # Append to results panel
    panel <- rbind(panel, data.frame(ccode=date.slice, date=date[i]))
  }
  # Create unique ID
  panel$id <- paste(panel$date, panel$ccode)
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