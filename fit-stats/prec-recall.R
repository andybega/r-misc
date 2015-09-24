#' Maximum F-score
#'
#' import ROCR
max_f <- function(obs, pred, beta=1) {
  xx.df <- prediction(pred, obs)
  perf  <- performance(xx.df, "f")
  xx    <- max(perf@y.values[[1]], na.rm=T)
  return(xx)
}


# For a vector of observed and predicted, creates x-y coordinates for a ROC
# or PR curve.
make_fit_tradeoff_data <- function(pred, obs, data=NULL, plot_type=NULL) {
  # plot_type is "roc" or "rp"
  if (!is.null(data)) {
    pred <- eval(substitute(pred), envir=data)
    obs  <- eval(substitute(obs), envir=data)
  }
  rocr_xy <- switch(plot_type, roc=c("tpr", "fpr"), rp=c("prec", "rec"))
  rocr_df <- prediction(pred, obs)
  rocr_pr <- performance(rocr_df, rocr_xy[1], rocr_xy[2])
  xy <- data.frame(recall=rocr_pr@x.values[[1]], precision=rocr_pr@y.values[[1]])
  return(xy)
}

colnames(roc_df)[2] <- c("Model")
levels(roc_df$partition) <- c("Ghazni train", "Ghazni test", "Lashkar Gah test")
levels(roc_df$Model) <- c("Constant", "w2", "m14", "m14_logistic", "m49")

p <- ggplot(data=roc_df, aes(x=recall, y=precision, col=Model)) + 
  facet_wrap(~ partition) +
  scale_x_continuous(expand = c(0.01, 0.01)) + 
  scale_y_continuous(expand = c(0.01, 0.01)) + 
  geom_line(show_guide=TRUE, alpha=0.7) +
  geom_abline(slope=1, color="gray", alpha=0.5) +
  labs(x="FPR", y="TPR") + 
  theme_bw()