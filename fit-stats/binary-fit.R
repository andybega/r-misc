# Brier score
# AUC-ROC
# AUC-PR
# Max F-score
# rocdf

library(ROCR)
library(caTools)

#' Brier Score
#' 
#' Calculate Brier Score for a binary classification model.
#' 
#' @param obs A vector of observed outcome values.
#' @param pred A vector of predictions.
#'
#' author Benjamin Radford, Andreas Beger
#' 
#' @seealso \code{link{aucMC}}
#' 
#' @examples
#' # Random guessing
#' y <- as.numeric(runif(50)>0.5)
#' p <- as.numeric(runif(50)>0.5)
#' brier(y, p)
#' 
#' @export 
brier <- function(obs, pred) {
  res <- mean((pred-obs)^2) 
  res
}

#' Area under the ROC curve
#'
#' improt ROCR
auc_roc <- function(obs, pred) {
  pred <- prediction(pred, obs)
  auc  <- performance(pred, "auc")@y.values[[1]]
  return(auc)
}

#' Maximum F-score
#'
#' import ROCR
max_f <- function(obs, pred, beta=1) {
  xx.df <- prediction(pred, obs)
  perf  <- performance(xx.df, "f")
  xx    <- max(perf@y.values[[1]], na.rm=T)
  return(xx)
}

#' Area under Precision-recall curve
#'
#' import ROCR
#' import caTools
auc_pr <- function(obs, pred) {
  xx.df <- prediction(pred, obs)
  perf  <- performance(xx.df, "prec", "rec")
  xy    <- data.frame(recall=perf@x.values[[1]], precision=perf@y.values[[1]])
  
  # take out division by 0 for lowest threshold
  xy <- subset(xy, !is.nan(xy$precision))

  # Designate recall = 0 as precision = x...arbitrary
  xy <- rbind(c(0, 0), xy)
  #xy <- xy[!(rowSums(xy)==0), ]
  
  res   <- trapz(xy$recall, xy$precision)
  res
}

# Function to create raw data needed to plot Precision against recall
#
# For a vector of observed and predicted, creates x-y coordinates for a ROC
# or PR curve.
rocdf <- function(pred, obs, data=NULL, type=NULL) {
  # plot_type is "roc" or "pr"
  if (!is.null(data)) {
    pred <- eval(substitute(pred), envir=data)
    obs  <- eval(substitute(obs), envir=data)
  }
  
  rocr_xy <- switch(type, roc=c("tpr", "fpr"), pr=c("prec", "rec"))
  rocr_df <- prediction(pred, obs)
  rocr_pr <- performance(rocr_df, rocr_xy[1], rocr_xy[2])
  xy <- data.frame(rocr_pr@x.values[[1]], rocr_pr@y.values[[1]])

  # If PR, designate first (undefined) point as recall = 0, precision = x
  if (type=="pr") {
    xy[1, 2] <- 0
    #xy <- xy[!(rowSums(xy)==0), ]
  }

  colnames(xy) <- switch(type, roc=c("tpr", "fpr"), pr=c("rec", "prec"))
  return(xy)
}

# To plot, something like:
#xy <- rocdf(pred, obs, type="pr")
#plot(xy[, 1], xy[, 2])

# Fit summary function for caret that works with binary predictions
binClassSummary <- function(data, lev = NULL, model = NULL) {
  
  if (!"event" %in% names(data))
    stop("Are 1's called 'event' in the factor? or, in trainControl, set classProbs = TRUE")

  obs  <- as.numeric(data$obs=="event")
  pred <- data[, "event"]
  
  out <- c(brier = brier(obs, pred),
           auc_roc = tryCatch(auc_roc(obs, pred), error = function(e) NA),
           auc_pr  = tryCatch(auc_pr(obs, pred), error = function(e) NA)
  )
  
  names(out) <- c("Brier", "AUC_ROC", "AUC_PR")
  out
}