perf_pr  <- function(preds, obs) {
  # Construct relevant cutoff values (assumes obs=1 is minority class)
  cutoffs <- c(0, 1, max(preds), min(preds), preds[obs==1])
  cutoffs <- sort(unique(cutoffs), decreasing=TRUE)
  
  n <- 2*length(cutoffs)
  prec <- vector("numeric", length=n)
  rec  <- vector("numeric", length=n)
  
  for (t in seq_along(cutoffs)) {
    # > t
    bin_preds <- factor(preds > cutoffs[t], levels=c("FALSE", "TRUE"))
    confm <- table(obs, bin_preds)
    prec[2*t-1] <- confm["1", "TRUE"] / ( confm["0", "TRUE"] + confm["1", "TRUE"])   # Precision = TP / (FP + TP)
    rec[2*t-1]  <- confm["1", "TRUE"] / ( confm["1", "FALSE"] + confm["1", "TRUE"])  # Precision = TP / (FN + TP)
    
    # >= t
    bin_preds <- factor(preds >= cutoffs[t], levels=c("FALSE", "TRUE"))
    confm <- table(obs, bin_preds)
    prec[2*t] <- confm["1", "TRUE"] / ( confm["0", "TRUE"] + confm["1", "TRUE"])   # Precision = TP / (FP + TP)
    rec[2*t]  <- confm["1", "TRUE"] / ( confm["1", "FALSE"] + confm["1", "TRUE"])  # Precision = TP / (FN + TP)
  }
  
  pr <- cbind(prec, rec)
  return(pr)
}

# doesn't work faster than prediction, maybe should be implemented lower level.
# Rcpp
# does save space though, not 3 million points

n <- 1000
x <- rnorm(n=n, mean=-1.66)
y <- rbinom(n=n, size=1, prob=plogis(x))
yhat <- plogis(x)

foo <- perf_pr(yhat, y)
