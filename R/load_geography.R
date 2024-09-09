
#'Return the geography for a city's census
#'place or tracts within a city's census place.
#'
#'@param state_abr character. State abbreviation of city.
#'@param fips_code character. FIPS code for city's census place.
#'@param geography_level character. "place" or "tract" or "block",
#'the geographic level of the data to return.
#'@param census_year integer. year of data from which tracts or places
#' should be loaded. (place data is not available before 2011, but
#' 2010 tract data is used for any year before 2020, so if needing
#' to get tracts within the census place boundary, it is okay
#' to use 2011.)
#'
#'@return sf dataframe
#'@export
load_geography <- function(geography_level, census_year=NULL, state_abr=NULL, counties=NULL, fips_code=NULL, crs_code=4326) {

  if (geography_level == "place") {

    geo_output <- places(state=state_abr, year=census_year) %>%
      st_transform(crs=crs_code)

    if (!is.null(fips_code)) {
      geo_output <- geo_output %>% filter(GEOID == !!fips_code)
    }

  }

  else if (geography_level == "block") {
    suppressMessages(

      geo_output <- blocks(state=state_abr, year=census_year, county=counties) %>%
        st_transform(crs=crs_code)

    )
  }

  else if (geography_level == "block_group") {
    suppressMessages(

      geo_output <- block_groups(state=state_abr, year=census_year, county=counties) %>%
        st_transform(crs=crs_code)

    )
  }

  else if (geography_level == "tract") {
    suppressMessages(

      geo_output <- tracts(state=state_abr, year=census_year, county=counties) %>%
        st_transform(crs=crs_code)

    )
  }

  return(geo_output)

}





