panelLag <- function(x, id, t, lag=1, data=NULL) {
  ## Returns a version of x lagged within panels given by id.
  # Input validation
  if (!class(t)=='Date') stop('t must be of class "Date"')
  # Construct data if not given
  if (is.null(data)==T) data <- data.frame(x=x, id=id, t=t)
  
  k <- lag
  lagger <- function(x, k) { c(rep(NA, k), x[k:(length(x)-k)]) }
  
  data <- data[order(data[, id], data[, t]), ]
  result <- unlist(by(data[, x], data[, id], lagger, k=k))
  return(result)
}

#test.data <- data.frame(x1=c(1,2,3,4,5,6,7,8,9,0), id1=c(1,1,1,1,1,2,2,2,2,2), t1=c(1,2,3,4,5,1,2,3,4,5))
#panelLag('x', 'id', 't', data=test.data)