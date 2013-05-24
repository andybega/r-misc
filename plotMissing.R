# to add
# grayscale option
# heatmap shading for %missing

plotMissing <- function(data, dim1=NULL, dim2=NULL, na.id=NULL) {
  require(ggplot2)
  require(grid)
  # Function to plot missing values in a data frame, or to plot missing values
  # in a data frame representing panel data (i.e. against 2 dimensions)
  # Leave dim1 and dim2 NULL to plot missing values in data frame.
  if (xor(is.null(dim1), is.null(dim2))) stop("Specify either both or no dims.")
  
  # Set colors for possible data values
  myCol <- c("#DCDCDC", "#32CD32", "#CD4F39")
  names(myCol) <- c("Absent", "Complete", "Missing")
  
  # plot missing in data frame
  if (all(is.null(dim1), is.null(dim2))) {
    require(reshape2)
    df <- is.na(data)
    df <- data.frame(melt(df))
    df$value <- ifelse(df$value==FALSE, "Complete", "Missing")
    df$value <- as.factor(df$value)
    colnames(df) <- c("rows", "columns", "value")

    p <- ggplot(df, aes_string(x="columns", y="rows"))
    p <- p + geom_tile(aes(fill=value)) +
      scale_fill_manual("Value", values=myCol) +
      theme(legend.key.size=unit(2, "cm"), legend.text=element_text(size=30),
            legend.title=element_text(size=30)) +
      scale_y_reverse()
    return(p)
  }
  
  # Function to plot missing data against 2 dimensions, e.g. country/year
  # Need to take into account "NA" versus completely absent rows
  # dim1, dim2 are names of the two variables against which to plot
  # na.id is name of a variable identifying incomplete cases
  if (all(!is.null(dim1), !is.null(dim2))) {
    if (is.null(na.id)) {
      na.id <- "temp_var"
      data$temp_var <- !complete.cases(data)
    }
    
    # Full grid to which we'll add other info
    df <- expand.grid(unique(data[, dim1]), unique(data[, dim2]))
    colnames(df) <- c(eval.parent(dim1), eval.parent(dim2))
    data.merge <- data[, c(dim1, dim2, na.id)]
    
    # Code grid values for absent, missing (NA), and non-missing obs.
    df <- merge(df, data.merge, by=c(dim1, dim2), all.x=TRUE)
    df$pixel <- ifelse(is.na(df[, na.id]), "Absent", df[, na.id])
    df$pixel[df$pixel==T] <- "Missing"
    df$pixel[df$pixel==F] <- "Complete"
    df$pixel <- as.factor(df$pixel)
    df[, dim1] <- as.factor(df[, dim1])
    df[, dim2] <- as.factor(df[, dim2])
    
    p <- ggplot(df, aes_string(x=dim2, y=dim1))
    p <- p + geom_tile(aes(fill=pixel)) +
      scale_fill_manual("Data", values=myCol) +
      theme(legend.key.size=unit(2, "cm"), legend.text=element_text(size=30),
            legend.title=element_text(size=30)) 
    return(p)
  }  
}

# test code, source function first
tdata <- data.frame(ccode=c(1,1,1,2,2,2,2), year=c(rep(seq(2000, 2002), 2), 2003),
                    value=c(1,1,NA,1,NA,1,1))
tdata$missing <- !complete.cases(tdata)
tdata
plotMissing(tdata, "ccode", "year", "missing")
plotMissing(tdata)