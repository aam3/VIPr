#' impute_monthly_population
#'
#'imputes population values for months in between years of population data.
#'Uses average change in population per month to impute and extrapolate population values.
#'
#'@import dplyr
#'
#' @param pops_by_year_df data frame with population values by year and a 'year' column
#' @param id_col column names to group by when imputing values (e.g. an ID column)
#' @param population_col the names of column(s) to impute
#'
#' @return data frame; population by month and year
#' @export
#'
#' @examples
#' impute_monthly_population(pops_by_year_df = yearly_pop_df, id_col = c("stpl_fips"), population_col = "place_population_est")
#'
impute_monthly_population <- function(pops_by_year_df, id_col, population_col) {

  cols_to_impute <- population_col

  exp <- pops_by_year_df %>%
    arrange(year) %>%
    group_by_at(id_col)

  v <- "year"
  next_col <- paste0("next_", v)
  change_col <- paste0("change_", v)
  exp <- exp %>%
    mutate(!!sym(next_col) := lead(!!sym(v)),
           !!sym(change_col) := ifelse(is.na(!!sym(next_col)), 0, (!!sym(next_col) - !!sym(v))))


  for (v in cols_to_impute) {

    next_col <- paste0("next_", v)
    change_col <- paste0("change_", v)

    exp <- exp %>%
      mutate(!!sym(next_col) := lead(!!sym(v)),
             !!sym(next_col) := ifelse(is.na(!!sym(next_col)), !!sym(v), !!sym(next_col)),
             !!sym(change_col) := ifelse(change_year!=0, (!!sym(next_col) - !!sym(v))/change_year, 0))
  }

  all_cols <- append(cols_to_impute, paste0("change_", cols_to_impute))

  # add in all year/months
  years <- exp[,"year"]
  months <- data.frame(month = seq(1, 12))
  cross_list <- years %>%
    mutate(dummy = 1) %>%
    inner_join(months %>% mutate(dummy = 1), by="dummy") %>%
    select(-dummy)

  exp <- cross_list %>%
    left_join(exp, by=c("year"))

  for (v in cols_to_impute) {

    change_col <- paste0("change_", v)

    exp <- exp %>%
      group_by_at(id_col) %>%
      mutate(change_from_month1 = round(ifelse(month==1, 0, (month - 1) * (!!sym(change_col)/12)), 0),
             !!sym(paste0(v, "_month")) := !!sym(v) + change_from_month1)

  }

  pops_by_month_df <- exp %>%
    fill(all_of(cols_to_impute), .direction="up") %>%
    select(all_of(id_col), year, month, all_of(paste0(cols_to_impute, "_month")))

  return(pops_by_month_df)
}
