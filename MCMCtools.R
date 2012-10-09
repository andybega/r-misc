# Extract median and 80% interval from posterior density "data" of "params".
summary.pd <- function(data, params, labels=NULL) {
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
  data <- data[, params]
  
  # Calcuate summary stats and format
  CI <- HPDinterval(data, prob=0.8)
  attr(CI, "Probability") <- NULL
  CI <- cbind(apply(data, 2, function(x) {median(x)}), CI)
  colnames(CI) <- c("median", "p20", "p80")
  if (!is.null(labels)) {rownames(CI) <- labels}
  
  # Done
  return(CI)
}

# Create pointrange plot for mcmc coefficients
plot.pd <- function(data, params, labels=NULL, main=NULL) {
  require(ggplot2)
  
  # Check input
  if (missing(params)) { stop("No parameter(s) provided") }
  
  # Get table of pd summary
  coef.table <- summary.pd(data, params, labels)
  coef.table <- data.frame(x=rownames(coef.table), coef.table)
  
  # Order for plot
  if (is.null(labels)) { 
    xorder <- rev(levels(coef.table$x)) 
    } else {
      xorder <- rev(labels)
    }
  
  # Plot coefficient pointranges
  p <- ggplot(coef.table, aes(x=x, y=median, ymin=p20, ymax=p80))
  p <- p + geom_pointrange() + 
    geom_hline(yintercept=0, lwd=0.6, colour=hsv(0, 0.6, 0.6), alpha=0.8) +
    xlim(xorder) + coord_flip() + theme_bw() +
    opts(axis.title.x=theme_blank(), axis.title.y=theme_blank(), title=main)
  
  # Done return plot
  return(p)
}