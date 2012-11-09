panelLag <- function(x, id, t, lag=1, data=NULL) {
  ## Returns a version of x lagged within panels given by id, in original order
  # Input validation
  if (lag < 0) { 
    lag <- abs(lag)
    warning(paste('lag is < 0, using', lag, 'instead')) 
  }
  # Construct data if not given
  ## Don't think this part works properly.
  if (is.null(data)==T) data <- data.frame(x=x, id=id, t=t)
  
  k <- lag
  lag.length.flag <- FALSE
  lagger <- function(x, k) { 
    if (k >= length(x)) {
      res <- rep(NA, length(x))
      lag.length.flag <<- TRUE
    }  
    if (k < length(x)) res <- c(rep(NA, k), x[k:(length(x)-k)]) 
    return(res)
  }
  
  data$orig.order <- 1:nrow(data) # to reorder results later
  data <- data[order(data[, id], data[, t]), ]
  result <- unlist(by(data[, x], data[, id], lagger, k=k))
  result <- result[order(data$orig.order)] # reorder results to original order
  
  if (lag.length.flag) warning('Lag order was larger than some or all unit series.')

  return(result)
}

## Test code
# test.data <- data.frame(x1=c(1,2,3,4,5,6,7,8,9,0), id1=c(1,1,1,1,1,2,2,2,2,2), t1=c(1,2,3,4,5,1,2,3,4,5))
# # Result with no warning.
# panelLag('x1', 'id1', 't1', lag=1, data=test.data)
# # Result with warning about lag.
# panelLag('x1', 'id1', 't1', lag=-1, data=test.data)
# # Result with warning about lag length.
# panelLag('x1', 'id1', 't1', lag=-6, data=test.data)