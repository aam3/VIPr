filter_geography <- function(x, y, filter_method, min_area_of_intersection=NULL, min_distance_from_geography=NULL) {
  
  if (filter_method == "intersection") {
    suppressMessages( {

      x$shape_area <- as.numeric(st_area(x))
      t <- st_intersection(y, x)
      t$intersection_area <- as.numeric(st_area(t))
      t$fraction_intersecting <- t$intersection_area/t$shape_area
      x <- t %>%
        filter(fraction_intersecting > min_area_of_intersection) %>%
        select(-shape_area)
    } )
  } else if (filter_method == "crop") {
    suppressMessages( {
      
      x <- st_crop(x, y)

    } )
  } else if (filter_method == "distance") {
    
    x$distance_to_geom <- as.vector(st_distance(y, x))
    x <- x %>%
      filter(distance_to_geom < min_distance_from_geography)
      # mutate(stpl_fips = city_geo$GEOID, place_name=city_geo$NAME) %>%
      # data.frame(.) %>%
      # dplyr::select(-geometry)
    
  }  
  
  return(x)

}