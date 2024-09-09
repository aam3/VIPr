

impute_population <- function(ct_vars, id_col, cols_to_impute, years_to_include) {

  exp <- ct_vars %>%
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

  # add in all years
  years <- as.data.frame(years_to_include)
  names(years) <- "year"
  cross_list <- ct_vars %>%
    select(all_of(id_col)) %>% distinct() %>%
    mutate(dummy=1) %>%
    inner_join(years %>% mutate(dummy=1), by="dummy") %>%
    select(-dummy)

  exp <- cross_list %>%
    left_join(exp, by=c(id_col, "year"))

  exp <- exp %>%
    group_by_at(id_col) %>%
    fill(all_of(all_cols), .direction="down")

  exp <- exp %>%
    arrange(year) %>%
    group_by_at(c(id_col, cols_to_impute)) %>%
    mutate(m = row_number()-1) %>%
    ungroup()

  exp <- exp %>%
    group_by_at(id_col)

  for (v in cols_to_impute) {

    change_col <- paste0("change_", v)

    exp <- exp %>%
      mutate(!!sym(v) := !!sym(v) + (!!sym(change_col)*m))

  }

  ct_vars <- exp %>%
    fill(all_of(cols_to_impute), .direction="up") %>%
    select(all_of(id_col), year, all_of(cols_to_impute))

  return(ct_vars)
}
