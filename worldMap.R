##########
# What: Function for chloropleth world map of PITF related data
# Date: October 2012
# Who:  Andreas Beger
#
###########

worldMap <- function(x, id, data, date='2008-01-01') {
  # Input 2-column matrix with unique identifier and data to be mapped for 
  # state slice in "date", output thematic map.
  
  require(cshapes)
  require(classInt)
  require(RColorBrewer)
  require(plyr)
  
  # Validate input
  if (all(c(x, id) %in% colnames(data))==F) { stop('x or id not in data') }
  if ((dim(data)[2]==2)==F) { stop('data must have 2 columns')}
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
  }
  
  # 3 to 9 unique values
  if (nval >= 3 & nval <= 9) {
    colorpal <- brewer.pal(nval, 'Reds')
    colors <- ifelse(is.na(world@data[, x])==T, '#B0B0B0', colorpal[(world@data[, x]+1)])
  }
  
  # Default (old legacy)
  if (exists('colors')==F) {
    colorpal <- brewer.pal(9, 'Reds')
    breaks <- c(0, 1, 2, 4, 6, 9, 12, 18, 24, 36)
    colors <- colorpal[findInterval(world@data[, x], breaks)]
    #colorpal <- brewer.pal(3, 'Reds')
    #breaks <- c(0, 1, 2, 5)
  }
  # Need to add/finish code for 3to9 categories and continuous data
  # What about continuous that spans zero?
  # Color grey for missing values, remove this when above is done
  colors <- ifelse(is.na(world@data[, x])==T, '#B0B0B0', colors)
  
  # Plot map
  plot(world, col='grey', border='grey', lwd=1)
  plot(world, col=colors, border=F, add=T)
}