panelDiff <- function(x, id, t, lag=1, differences=1, data=NULL) {
  ## Returns the first difference of x, suitably lagged
  # Input validation
  arg.char <- c(is.character(substitute(x)), is.character(substitute(id)), 
                is.character(substitute(t)))
  if (is.null(data)) {
    if (all(arg.char)==FALSE) stop('where\'s the data?')
    data <- data.frame(x = x, id = id, t = t)
    }
  
  k <- lag
  d <- differences
  differ <- function(vector, d, k) { c(rep(NA, d), diff(vector, lag=k, differences=d))}
  
  data <- data[order(data[, id], data[, t]), ]
  result <- unlist(by(data[, x], data[, id], differ, d, k))
  names(result) <- NULL
  return(result)
}

# # Test code
vector <- c(0, 0, 1, 1, 5, 1, 1, 5, 3, 3)
country <- c(rep('a', 5), rep('b', 5))
time <- rep(as.Date(c('2012-01-01', '2012-02-01', '2012-03-01', '2012-04-01', '2012-05-01')),2)
data <- data.frame(vector, country, time)
panelDiff('vector', 'country', 'time', data=data, differences=1)