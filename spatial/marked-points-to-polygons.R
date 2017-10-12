#' Marked points to polygons
#' 
#' Convert points marked with a categorical feature into areas grouped by mark.
#' 
#' @param points A "sf" class object with a point geometry column. 
#' @param mark Name of the feature in \code{points} used to group points by, e.g. "mark".
#' @param envelope A polygon (not line) to use as envelope for tesselating the points.
#' 
marked_points2polygons <- function(points, group_by, envelope) {
  
  stopifnot(inherits(points, "sf"))
  stopifnot(all(st_geometry_type(points) %in% c("POINT", "MULTIPOINT")))
  stopifnot(inherits(envelope, "sfc"))
  
  envelope <- envelope %>% st_union()
  
  # Tesselate 
  point_tiles <- points %>%
    st_geometry() %>%
    st_union() %>%
    st_voronoi(., envelope = envelope) %>%
    st_cast() 
  # At this point the tiles will be too large as they include the expanded 
  # envelope. We can fix this through intersection with envelope, but don't 
  # do this yet. Quicker to do the intersection once the tiles have been
  # grouped by the mark, as there will be potentially far fewer geometries.
  
  #  Add point attributes to point tiles
  point_tiles <- point_tiles %>%
    data.frame(geometry = .) %>%
    st_sf(.) %>%
    st_join(., points)
  
  stopifnot(!any(is.na(point_tiles[[group_by]])))
  
  mark_polygons <- point_tiles %>%
    group_by_at(vars(group_by)) %>%
    summarize(points = n()) %>%
    st_cast() %>%
    # Specify that the atribute values are spatially constant so that 
    # intersection does not generate a warning
    st_set_agr(., "constant") %>%
    st_intersection(., envelope) %>%
    st_cast()
  
  mark_polygons
}
