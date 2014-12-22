#' Weighted mean
#' 
#' Wrapper for weighted mean that allows partial input
#' 
#' @param x vector to be lagged
#' @param w vector of weights
#' 
weighted.mean_partial <- function(x, w) {
  if (length(w)==length(x)) {
    res <- weighted.mean(x, w)
  } else if (length(w)>length(x)) {
    start <- length(w) - length(x) + 1
    w_short <- w[start:length(w)]
    res <- sum((x * w_short)[w_short != 0])/sum(w_short)
  } 
  return(res)
}