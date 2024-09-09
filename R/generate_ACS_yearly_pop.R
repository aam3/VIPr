# geo-level can only be a geography >= place
generate_ACS_yearly_pop <- function(census_key, state_abr, geo_level, years_to_include=seq(2014,2023), filter_geos=NULL, acs_table="acs5") {
  
  census_api_key(census_key, overwrite = TRUE, install = TRUE)
  
  pop_var <- "S0101_C01_001"
  
  # years_to_include <- 1990:2010
  
  pop_df <- foreach(i = 1:length(state_abr), .combine=rbind) %do% {
    foreach(j = 1:length(years_to_include), .combine=rbind) %do% {
    
      tryCatch(
          {
        get_acs(geography=geo_level, 
                variables=pop_var, 
                state=state_abr[i], 
                survey=acs_table,
                year=years_to_include[j], 
                cache_table=TRUE) %>%
                mutate(year = years_to_include[j])
            },
              error=function(cond) {
                
              },
          warning=function(cond) {
  
        }
      )
    }
  }
  
  if ((min(pop_df$year) < 2010)) {
    warning(paste("ACS api does not provide data before 2010."))
  }
  
  names(pop_df) <- tolower(names(pop_df))
  
  if (!is.null(filter_geos)) {
    pop_df <- pop_df %>% 
      filter(geoid %in% filter_geos)
  }  
  
  pop_df <- pop_df %>%
    rename(place_population_est_year = estimate,
           stpl_fips = geoid) %>%
    dplyr::select(-moe)  
  
  # print(paste0("Latest acs1 pop year available: ", max(pop_df$year)))
  print(paste0("Latest acs5 pop year available: ", max(pop_df$year)))
  
  # add NA for user-specified years
  years_to_include <- as.data.frame(list("year"=years_to_include))
  full_geoid_years <- pop_df %>%
    select(stpl_fips) %>%
    mutate(dummy = 1) %>%
    inner_join(years_to_include %>% mutate(dummy = 1), by=c("dummy")) %>%
    select(-dummy)
  pop_df <- full_geoid_years %>%
    left_join(pop_df, by=c("stpl_fips","year"))
  
  return(pop_df %>% distinct())
  
}