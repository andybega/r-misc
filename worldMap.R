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
  
  # Assign colors for each country
  colorpal <- brewer.pal(3, 'Reds')
  breaks <- c(0, 1, 2, 5)
  colors <- colorpal[findInterval(world@data[, x], breaks)]
  colors <- ifelse(is.na(world@data[, x])==T, '#B0B0B0', colors)
  
  # Plot map
  plot(world, col='grey', border='white')
  plot(world, col=colors, border=F, add=T)
}  