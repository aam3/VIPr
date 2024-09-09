
map_coordinates_to_polygon <- function(coordinate_df, polygon_map, 
                                       crs_code=4326, longitude_col="long_ggl", latitude_col="lat_ggl") {
  #' @title Remove Addresses Outside City
  #' @description Detects or removes addresses that are outside of the 
  #' city of interest.
  #' @note New York was queried on the GVA site by borough, so anything geocoded
  #' outside of the city is incorrect and should be kept in the data for city-level
  #' counts. For all other cities, we want to remove addresses that are outside
  #' of the city of interest so as not to include them in the aggregated counts.
  
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

  # # 1c. Keep first occurring census tract for the duplicates
  # df_tracts <- df_tracts %>%
  #   group_by(incident_id) %>%
  #   mutate(n_row = row_number()) %>%
  #   filter(n_row == 1) %>%
  #   dplyr::select(-n_row) %>%
  #   st_drop_geometry(.)
  # 
  # coordinate_map_new <- coordinate_map %>%
  #   left_join(df_tracts, by=c("city","incident_id"), suffix = c("_old", "")) %>%
  #   fill(place_fips, .direction="downup")
  
  # ### 2a. determine distance of incident from place boundary 
  # coordinate_map_new$distance_to_place <- as.vector(st_distance(place_geo, coordinate_map_new))
  # 
  # ### 2b. Change place_fips label to NA if too far outside
  # ### place boundary
  # coordinate_map_new <- coordinate_map_new %>%
  #   mutate(place_fips = ifelse(distance_to_place <= 500, place_fips, NA))
}


