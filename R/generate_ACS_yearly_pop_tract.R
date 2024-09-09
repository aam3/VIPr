generate_ACS_yearly_pop_tract <- function(census_key, state_abr, years_to_include=seq(2014,2023), filter_tracts=NULL) {
  
  census_api_key(census_key, overwrite = TRUE, install = TRUE)
  
  pop_var <- "B01003_001"

  pop_tracts <- foreach(i = 1:length(state_abr), .combine=rbind) %do% {
    foreach(j = 1:length(years_to_include), .combine=rbind) %do% {
      
      tryCatch(
          {
            get_acs(geography="tract", 
                    variables=pop_var, 
                    state=state_abr[i], 
                    year=years_to_include[j],
                    cache_table=TRUE) %>%
                mutate(year = years_to_include[j]) %>%
                rename(population=estimate) %>%
                dplyr::select(-moe)
          },
            error=function(cond) {
              
            },
        warning=function(cond) {
  
        }
      )
    }
  }
  
  print(paste0("Latest acs5 pop year available: ", max(pop_tracts$year)))

  names(pop_tracts) <- tolower(names(pop_tracts))
  
  if (!is.null(filter_tracts)) {
    pop_tracts <- pop_tracts %>% 
      filter(geoid %in% filter_tracts)
  }
  
  # the "NAME" column is causing duplicates because the exact value in column NAME is changing
  # over time
  pop_tracts <- pop_tracts %>%
    dplyr::select(-name) %>%
    distinct()
  
  years_to_include_df <- data.frame(year = years_to_include)
  df_pops <- pop_tracts %>%
    select(geoid, variable) %>%
    distinct() %>%
    mutate(dummy = 1) %>%
    inner_join(years_to_include_df %>% mutate(dummy = 1), by=c("dummy")) %>%
    left_join(pop_tracts, by=c("geoid", "variable", "year")) %>%
    select(-dummy) %>%
    arrange(year)
  
  df_pops <- df_pops %>%
    group_by(geoid)
      
  # get average years
  
  n_yrs_to_avg <- 3
  
  df1 <- df_pops %>%
    arrange(desc(year)) %>%
    filter(!is.na(population)) %>%
    slice(1:n_yrs_to_avg) %>%
    mutate(min_year_w_data = min(year),
           max_year_w_data = max(year)) %>%
    select(geoid, variable, min_year_w_data, max_year_w_data) %>%
    distinct()
  
  df1_w_pops <- df1 %>%
    inner_join(df_pops %>% select(geoid, year, population), by=c("geoid", "min_year_w_data"="year")) %>%
    rename(min_year_population = population) %>%
    inner_join(df_pops %>% select(geoid, year, population), by=c("geoid", "max_year_w_data"="year")) %>%
    rename(max_year_population = population)
  
  df1_w_pops <- df1_w_pops %>%
    filter(max_year_w_data >= 2020) %>%
    mutate(avg_change_per_year = (max_year_population - min_year_population)/(max_year_w_data - min_year_w_data))
  
  df_pops <- df_pops %>%
    left_join(df1_w_pops, by=c("geoid", "variable", "year"="max_year_w_data")) %>%
    arrange(year) %>%
    mutate(max_year_w_data = ifelse(!is.na(max_year_population), year, NA)) %>%
    fill(max_year_population, .direction="down") %>%
    fill(max_year_w_data, .direction="down") %>%
    fill(avg_change_per_year, .direction="down")

  df_pops <- df_pops %>%
    mutate(population_est = ifelse(!is.na(population), population, max_year_population + ((year - max_year_w_data)*avg_change_per_year)))

  df_pops <- df_pops %>%
    arrange(year) %>%
    group_by(geoid) %>%
    fill(population_est, .direction = "up") %>% # replace this with longitudinal database
    select(geoid, year, population_est)
 
  df_pops <- df_pops %>%
    mutate(population_est = ifelse(population_est < 0, 0, population_est))
  
  return(df_pops)
}

