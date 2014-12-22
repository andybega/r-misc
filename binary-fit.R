# Brier score
# AUC
# Max F-score

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
auc <- function(obs, pred) {
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
aupr <- function(obs, pred) {
  xx.df <- prediction(pred, obs)
  perf  <- performance(xx.df, "prec", "rec")
  xy    <- data.frame(recall=perf@x.values[[1]], precision=perf@y.values[[1]])

  # take out division by 0 for lowest threshold
  xy <- subset(xy, !is.nan(xy$precision))

  res   <- trapz(xy$recall, xy$precision)
  res
}