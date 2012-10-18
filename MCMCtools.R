##########
# Summarize a matrix of posterior density samples with median and HPD interval.
# Returns 3 column data frame with optional row (coefficient) labels.
# params.list = T if params is a string describing related paramters, e.g. x[1], x[2],...
#
##########

summaryPD <- function(data, params, labels=NULL, params.list=FALSE) {
  require(coda)
  
  # Check input is mcmc object
  try(data <- as.mcmc(data), silent=TRUE)
  if (class(data)=="mcmc.list") {
    data <- as.mcmc(do.call(rbind, data))
  }
  if (class(data)!="mcmc") {
    stop("Data must be mcmc or related object")
  }
  
  # Extract and format parameter estimates
  if (params.list==FALSE) {
    data <- data[, params]
  }
  if (params.list==TRUE) {
    data <- data[, grep(params, colnames(data))]
  }
  
  # Calcuate summary stats and format
  CI <- HPDinterval(data, prob=0.8)
  attr(CI, "Probability") <- NULL
  CI <- cbind(apply(data, 2, function(x) {median(x)}), CI)
  CI <- data.frame(CI)
  colnames(CI) <- c("median", "p20", "p80")
  if (!is.null(labels)) {rownames(CI) <- labels}
  
  # Done
  return(CI)
}



##########
# Extract fitted values (i.e. large number of variables).
#
##########

# Create pointrange plot for mcmc coefficients
plotPD <- function(data, params, labels=NULL, main=NULL) {
  require(ggplot2)
  
  # Check input
  if (missing(params)) { stop("No parameter(s) provided") }
  
  # Get table of pd summary
  coef.table <- summaryPD(data, params, labels)
  print(coef.table)
  coef.table$x <- rownames(coef.table)
  
  # Order for plot
  if (is.null(labels)) { 
    xorder <- rev(coef.table$x)
    } else {
      xorder <- rev(labels)
    }
  
  # Plot coefficient pointranges
  p <- ggplot(coef.table, aes(x=x, y=median, ymin=p20, ymax=p80))
  p <- p + geom_pointrange() + 
    geom_hline(yintercept=0, lwd=0.6, colour=hsv(0, 0.6, 0.6), alpha=0.8) +
    xlim(xorder) + coord_flip() + theme_bw() +
    theme(axis.title.x=element_blank(), axis.title.y=element_blank())
  ## add title................., blank background
  
  # Done return plot
  return(p)
}