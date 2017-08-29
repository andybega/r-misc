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

#' Lag variables
#' 
#' Lag a single variable in panel data frame.
#' 
#' @param df Data frame containing panel data.
#' @param x Name of variable to lag.
#' @param id Variable that identifies cross-sectional units (subjects,
#'   countries).
#' @param t Variable that identifies time points.
#' @param n Time periods to lag by.
#'   
#' @return A vector of same length and order as \code{x}, lagged by \code{n}
#'   time periods
#'   
#'   
#' @examples
#' ex <- data.frame(
#'   x1=c(1,2,3,4,5,6,7,8,9,0), 
#'   id1=c(1,1,1,1,1,2,2,2,2,2), 
#'   t1=c(1,2,3,4,5,1,2,3,4,5))
#'   
#' lag_panel(ex, x1, id1, t1)
#'   
#' # scramble
#' ex <- ex[sample(nrow(ex)), ]
#' lag_panel(ex, x1, id1, t1)
#' 
#' @export
#' @import dplyr
#' @import lazyeval
lag_panel <- function(df, x, id, t, n=1L) {
  lag_panel_(df, substitute(x), substitute(id), substitute(t), n)
}

lag_panel_ <- function(df, x, id, t, n=1L) {
  df <- as.tbl(df)
  df$org_order <- 1:nrow(df)
  
  # arguments for mutate
  mutate_call <- lazyeval::interp(~ lag(x, n = n), x = as.name(x), n = n)

  df <- df %>%
    group_by_(id) %>%
    arrange_(t) %>%
    mutate_(
      .dots = setNames(list(mutate_call), c("x_lagged"))
    ) %>%
    ungroup() %>%
    arrange(org_order) %>%
    as.data.frame()
  
  return(df$x_lagged)
}


## Test code
test_lag_panel <- function() {
  ex <- data.frame(
    x1=c(1,2,3,4,5,6,7,8,9,0), 
    id1=c(1,1,1,1,1,2,2,2,2,2), 
    t1=c(1,2,3,4,5,1,2,3,4,5))
  
  identical(lag_panel_(ex, "x1", "id1", "t1"), c(NA, 1, 2, 3, 4, NA, 6, 7, 8, 9))
  a <- "x1"
  b <- "id1"
  c <- "t1"
  d <- 1
  identical(lag_panel_(ex, a, b, c, d), c(NA, 1, 2, 3, 4, NA, 6, 7, 8, 9))
  
  identical(lag_panel(ex, x1, id1, t1), c(NA, 1, 2, 3, 4, NA, 6, 7, 8, 9))
}


# # Result with no warning.
# panelLag('x1', 'id1', 't1', lag=1, data=test.data)
# # Result with warning about lag.
# panelLag('x1', 'id1', 't1', lag=-1, data=test.data)
# # Result with warning about lag length.
# panelLag('x1', 'id1', 't1', lag=-6, data=test.data)