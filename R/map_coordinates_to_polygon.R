#' map_coordinates_to_polygon
#'
#' joins a list of coordinates to polygons to determine which polygons each coordinate is in
#'
#'@import sf
#'@import dplyr
#'
#' @param coordinate_df data frame with latitude/longitude coordinates
#' @param polygon_map the polygons that the coordinates should be mapped to; sf data frame
#' @param crs_code CRS code of the geographies
#' @param longitude_col name of the longitude column in coordinate_df
#' @param latitude_col name of the latitude column in coordinate_df
#'
#' @return sf dataframe; coordinates with the ID column from polygon map
#' @export
#'
#' @examples
#' map_coordinates_to_polygon(coordinate_df = df %>% filter(!is.na(lat_ggl)), polygon_map = city_tract_geography)
#'
map_coordinates_to_polygon <- function(coordinate_df, polygon_map, crs_code=4326, longitude_col="long_ggl", latitude_col="lat_ggl") {


  coordinate_map <- st_as_sf(coordinate_df,
                             coords = c(longitude_col, latitude_col),
                             crs = crs_code,
                             agr = "constant",
                             remove=FALSE)

  polygon_map <- polygon_map %>% st_transform(crs=crs_code)

  if (class(polygon_map)[1] == "sf") {
    coordinate_map_new <- st_join(polygon_map, coordinate_map, suffix = c("", "_old"))
  } else {
    stop("Polygon map must be sf class.")
  }

  return(coordinate_map_new)

}


