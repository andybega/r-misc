
#'
#' @param epsg Optional coordinate system to project inputs before snapping; 
#'   function will not work with lat/long coordinates. If specified, the points
#'   will be projected back to their original CRS
#'
snap_points_to_line <- function(points, lines, maxDist = NA, epsg = NULL) {
  # inputs should be class 'sf' or 'sfc'
  orig_crs <- points %>% st_crs()
  if (!is.null(epsg)) {
    points <- st_transform(points, epsg)
    lines <- st_transform(lines, epsg)
  }
  if (grepl("longlat", st_crs(points)$proj4string)) stop("`points` is not projected")
  if (grepl("longlat", st_crs(lines)$proj4string)) stop("`lines` is not projected")
  
  points <- as(points, "Spatial")
  lines  <- as(lines, "Spatial")
  
  new_points <- maptools::snapPointsToLines(points,  
                                            lines,
                                            maxDist = maxDist) 
  
  if (nrow(new_points)!=nrow(points) && !is.na(maxDist)) {
    stop("Some points were not snapped, try larger maxDist.")
  }
  
  if (class(points)=="SpatialPointsDataFrame") {
    new_points <- new_points %>%  
      st_as_sf(.) %>%
      st_transform(orig_crs$epsg) %>%
      dplyr::select(-nearest_line_id) 
  } else {
    new_points <- new_points %>%  
      st_as_sfc(.) %>%
      st_transform(orig_crs$epsg) 
  }
  
  # for sf: dplyr::select(-nearest_line_id)
  new_points
}


snap_points_to_polygon <- function(points, polygons, maxDist = NA, epsg = NULL,
                                   buffer = NULL) {
  orig_crs <- points %>% st_crs()
  if (!is.null(epsg)) {
    points   <- st_transform(points, epsg)
    polygons <- st_transform(polygons, epsg)
  } 
  
  target <- polygons %>% 
    st_geometry() %>% st_boundary() %>%
    # In case it is a multilinestring
    st_union()
  
  if (!is.null(buffer)) {
    target <- target %>%
      st_buffer(buffer) %>%
      st_boundary() %>%
      st_intersection(., st_geometry(polygons)) %>% 
      st_union()
  }
  
  polygons <- polygons %>% st_cast(., "POLYGON")
  target   <- target %>% st_cast(., "LINESTRING")
  
  is_inside <- st_intersects(points, polygons, sparse = FALSE) %>%
    apply(., 1, any)
  if (inherits(points, "sf")) {
    outside_points <- points[!is_inside, ]
  } else {
    outside_points <- points[!is_inside]
  }
  
  new_points <- snap_points_to_line(outside_points, target, maxDist = maxDist, 
                                    epsg = epsg)
  
  if (inherits(points, "sf")) {
    points[!is_inside, ] <- new_points
  } else {
    points[!is_inside] <- new_points
  }
  
  points <- points %>% st_transform(orig_crs$epsg) 
  points
} 