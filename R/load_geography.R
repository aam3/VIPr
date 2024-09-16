#' load_geography
#'
#' Get the geography for census place(s), tract(s), block(s), or block group(s)
#'
#'@import tigris
#'@import sf
#'@import dplyr
#'
#' @param geography_level geography type to return; "place", "tract", "block", or "block group"
#' @param census_year the year of the data to get
#' @param state_abr state abbreviation(s) of the geographies to be returned
#' @param counties county(ies) of the geographies to be returned
#' @param fips_code if geography_level = "place", optionally provide the FIPS code of a census place
#' @param crs_code the CRS code of the geography
#'
#' @return sf data frame
#' @export
#'
#' @examples
#' load_geography(geography_level="place", census_year=2020, state_abr="KY", fips_code="2148000")
#'
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





