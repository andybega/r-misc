##########
# What: Function for chloropleth world map
# Date: October 2012
# Who:  Andreas Beger
#
###########

worldMap <- function(x, id, data, date='2008-01-01', legend.title=NULL) {
  # Input 2-column matrix with unique identifier and data to be mapped for 
  # state slice in "date", output thematic map.
  
  require(cshapes)
  require(maptools)
  require(RColorBrewer) # for color palettes
  require(plyr)
  
  # Validate input
  if (all(c(x, id) %in% colnames(data))==F) { stop('x or id not in data') }
  if (any(duplicated(data[, id]))==T) { stop('id is not unique in data') }
  
  # Load map data
  world <- cshp(date=as.Date(date))
  
  # Merge data into spatial object
  world@data <- data.frame(world@data, data[match(world@data[, 'COWCODE'], data[, id]), ])
  
  ## Assign colors for each country based on nature of plotted variable
  nval <- length(unique(na.omit(world@data[, x])))
  
  # Binary variable
  if (nval==2) {
    colorpal <- c('#FEE0D2', '#EF3B2C')
    colors <- ifelse(is.na(world@data[, x])==T, '#B0B0B0', colorpal[(world@data[, x]+1)])
    
    # Plot map
    plot(world, col='gray30', border='gray30', lwd=1)
    plot(world, col=colors, border=F, add=T)
    
#     # Legend
#     text(70, 90, paste("Instances of ", modelname, ": ", monthname, " ", year, sep = ""))
#     rect(xleft = -170, xright = -165, ybottom = c(-57, -50, -43), ytop = c(-52, -45, -38), border = NA, col = c('#B0B0B0', colorpal))
#     text(-163, c(-57, -50, -43)+0.5, c('No data', paste("No", modelname), modelname), adj = c(0, 0), cex = 0.8)
#     text(-170, -36, "Key:", cex = 0.8, adj = c(0, 0))
#     rect(xleft=-171.5, xright=-130, ytop=-31, ybottom=-59)
  }
  
  # 3 to 9 unique values
  if (nval >= 3 & nval <= 9) {
    colorpal <- rev(brewer.pal(nval, 'Reds'))
    colors <- ifelse(is.na(world@data[, x])==T, '#B0B0B0', colorpal[match(world@data[, x], sort(unique(world@data[, x]), decreasing=T))])
    
    # Plot map
    plot(world, col='gray30', border='gray30', lwd=1)
    plot(world, col=colors, border=F, add=T)
    
    # Legend
    legend.text <- c('No data', rev(unlist(dimnames(table(world@data[, x])))))
    legend(x=-170, y=15, legend=legend.text, fill=c('#B0B0B0', colorpal), 
           bty='n', ...)
  }
  
  # Continuous
  if (nval > 9) {
    require(shape) # for color legend
    
    if (is.null(maxy)) maxy <- max(na.omit(world@data[, x]))
    colorpal <- brewer.pal(9, 'Reds')
    colorpal <- colorRampPalette(colorpal)
    
    breaks <- c(0, round(maxy*1/4), round(maxy/2), round(maxy*3/4), maxy)
    colors <- ifelse(is.na(world@data[, x])==T, '#B0B0B0', colorpal(maxy)[floor(world@data[, x]) + 1] )
    
    # Plot map
    plot(world, col='gray30', border='gray30', lwd=1)
    plot(world, col=colors, border=F, add=T)
    colorlegend(posy=c(0.1, 0.45), posx=c(0.12, 0.14),
                col=colorpal(100), zlim=c(0, maxy), zval=breaks,
                main=legend.title, main.cex=1.2, cex=1.2)
  }
  
  # Legends?
  # What about continuous that spans zero?
  # how to pick breaks in cont. colorlegend
  # transforming continuous data for plotting
}