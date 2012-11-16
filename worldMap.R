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
  require(classInt)
  require(RColorBrewer)
  require(plyr)
  require(shape) # for color legend
  
  # Validate input
  if (all(c(x, id) %in% colnames(data))==F) { stop('x or id not in data') }
  if (any(duplicated(data[, id]))==T) { stop('id is not unique in data') }
  
  # Load map data
  world <- cshp(date=as.Date(date))
  world.outline <- cshp(date=as.Date(date))
  world.outline <- unionSpatialPolygons(world.outline, world.outline@data$COWEYEAR)
  
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
  }
  
  # 3 to 9 unique values
  if (nval >= 3 & nval <= 9) {
    colorpal <- brewer.pal(nval, 'Reds')
    colors <- ifelse(is.na(world@data[, x])==T, '#B0B0B0', colorpal[(world@data[, x]+1)])
    
    # Plot map
    plot(world, col='grey', border='grey', lwd=1)
    plot(world, col=colors, border=F, add=T)
  }
  
  # Continuous
  if (nval > 9) {
    colorpal <- brewer.pal(9, 'Reds')
    colorpal <- colorRampPalette(colorpal)
    maxy <- 100
    breaks <- c(0, 10, 20, 50, 75, 100)
    colors <- ifelse(is.na(world@data[, x])==T, '#B0B0B0', colorpal(maxy)[floor(world@data[, x]) + 1] )
    
    # Plot map
    plot(world, col=colors, border='white', lwd=0.3)
    plot(world.outline, border='gray50', add=T, lwd=0.8)
    colorlegend(posy=c(0.1, 0.5), posx=c(0.1, 0.12),
                col=colorpal(maxy), zlim=c(0, maxy), zval=breaks,
                main=legend.title, main.cex=1, cex=1)
  }
  #   ncol <- length(unique(world@data[, x]))
  #   colorpal <- brewer.pal(ncol, 'Reds')
  #   colors <- colorpal[world@data[, x]]
  
  # Legends?
  # What about continuous that spans zero?
  # how to pick breaks in cont. colorlegend
  # transforming continuous data for plotting
}