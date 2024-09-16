#'filter_geography
#'
#'filters a shapefile using the boundary(ies) of another shapefile; utilizes the sf() package 'join' functions.
#'
#'@import sf
#'@import dplyr
#'
#' @param x the shapefile to filter
#' @param y the shapefile to be used for filtering x
#' @param filter_method type of filter to be done ("intersection", "crop", or "distance")
#' @param min_area_of_intersection if filter_method = "intersection", the minimum area of intersection between a geometry from x and y
#' @param min_distance_from_geography if filter_method = "distance", the minimum distance from a geometry in x to y
#'
#' @return a filtered shapefile
#' @export
#'
#'@examples filter_geography(x=tract_map, y=city_map, filter_method="intersection", min_area_of_intersection=0)
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

  }

  return(x)

}
