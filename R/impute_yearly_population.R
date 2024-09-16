#'impute_yearly_population
#'
#'imputes population values for years in between census years and for years with population data not yet available.
#'Uses average change in population per year to impute and extrapolate population values.
#'
#'@import dplyr
#'
#' @param pop_df data frame with population values in a 'place_population_est_year' column and a 'year' column
#' @param group_cols column names to group by when imputing values (e.g. an ID column)
#' @param years_to_impute all years to include in the final data
#' @param window window of rows to average in order to calculate an average change
#'
#' @return data frame with population values by year
#' @export
#'
#' @examples
#' impute_yearly_population(pop_df = pop_df, group_cols = c("stpl_fips"), years_to_impute = min(pop_df$year):max(pop_df$year), window=2)
#'
impute_yearly_population <- function(pop_df,
                                     group_cols = c("stpl_fips"),
                                     years_to_impute=min(pop_df$year):max(years_to_impute),
                                     window=2) {

  # the "NAME" column is causing duplicates because the exact value in column NAME is changing
  # over time
  years_to_include <- sort(unique(c(pop_df$year, years_to_impute)))
  years_to_include_df <- data.frame(year = years_to_include)
  pop_df_full_years <- pop_df %>%
    select(all_of(group_cols)) %>%
    distinct() %>%
    mutate(dummy = 1) %>%
    inner_join(years_to_include_df %>% mutate(dummy = 1), by=c("dummy")) %>%
    select(-dummy)

  pop_df_full_years <- pop_df_full_years %>%
    left_join(pop_df, by=c(group_cols, "year")) %>%
    arrange(all_of(group_cols), year)

  pop_df_full_years <- pop_df_full_years %>%
    group_by(stpl_fips) %>%
    mutate(most_recent_pop_estimate = lag(place_population_est_year, window),
           most_recent_pop_year = ifelse(!is.na(most_recent_pop_estimate), lag(year, window), NA),
           next_pop_estimate = lead(place_population_est_year, window),
           next_pop_year = ifelse(!is.na(next_pop_estimate), lead(year, window), NA)) %>%
    fill(most_recent_pop_estimate, most_recent_pop_year, .direction="down") %>%
    fill(next_pop_estimate, next_pop_year, .direction="up")

  #----------------- IMPUTATION

  pop_df_full_years <- pop_df_full_years %>%
    mutate(change_per_year_full_window = round((next_pop_estimate - most_recent_pop_estimate)/(next_pop_year - most_recent_pop_year), 0),
           change_per_year_recent_window = (place_population_est_year - most_recent_pop_estimate) / (year - most_recent_pop_year),
           change_per_year_next_window = (next_pop_estimate - place_population_est_year) / (next_pop_year - year)) %>%
    group_by(all_of(group_cols)) %>%
    mutate(change_per_year_filled = change_per_year_full_window) %>%
    fill(change_per_year_filled, .direction="updown")

  pop_df_full_years <- pop_df_full_years %>%
    mutate(place_population_est_year = ifelse(!is.na(place_population_est_year), place_population_est_year,
                                              ifelse(!is.na(most_recent_pop_estimate), most_recent_pop_estimate + ((year - most_recent_pop_year)*change_per_year_filled),
                                                     ifelse(!is.na(next_pop_estimate), next_pop_estimate - ((next_pop_year - year)*change_per_year_filled), place_population_est_year))),
           place_population_est_year = as.integer(round(place_population_est_year, 2)))

  pop_df_full_years <- pop_df_full_years %>%
    select(all_of(group_cols), year, place_population_est = place_population_est_year)

  return(pop_df_full_years)

}



