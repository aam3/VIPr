#' search_place_fips_code
#'
#' searches for the census place FIPS code given the name and state abbreviation for a city
#'
#'@import tigris
#'@import sf
#'@import dplyr
#'
#' @param city name of city in title case
#' @param state_abr state abbreviation that the city is in
#'
#' @return FIPS code for a city
#' @export
#'
#' @examples
#' search_place_fips_code(city="Chicago", state_abr="IL")
#'
search_place_fips_code <- function(city, state_abr) {

  df <- places(state_abr) %>%
    mutate(city_name = gsub( pattern = '[^a-zA-Z ]',
                             replacement = '',
                             x = NAME))

  if (city != "Honolulu") {
    place_name <- gsub( pattern = '[^a-zA-Z ]', replacement = '', x = city)
  } else {
    place_name <- paste("Urban", city)
  }

  subdf <- df %>%
    dplyr::filter(grepl( !!place_name, gsub( pattern = '[^a-zA-Z ]',
                                      replacement = '',
                                      x = NAMELSAD), ignore.case = T ))

  if (nrow(subdf) > 1) {

    if (city == "Louisville") { # to get Louisville/Jefferson Area
      subdf <- subdf %>%
        group_by(STATEFP) %>%
        mutate(geometry = st_union(geometry))
    }

    if (nrow(subdf %>% dplyr::filter(PCICBSA == "Y")) != 1) {
      subdf <- subdf %>%
        dplyr::filter(city_name == !!place_name)
    } else {
      subdf <- subdf %>% dplyr::filter(PCICBSA == "Y")
    }
  }

  return(subdf$GEOID)

}
