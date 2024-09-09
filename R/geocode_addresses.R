geocode_addresses <- function(API_key, addresses) {
  #' @title Geocode Data Frame of Addresses
  #' @description A wrapper function that geocodes a data frame of 
  #' addresses. 
  #' 
  #' @param API_key character: character string containing Google API key  
  #' @param addresses vector: character vector containing addresses to map to coordinates
  #' @return data frame with output from the Geocoding API as 
  #' columns, including latitude and longitude columns.
  
  if (!is.null(API_key)) {
    register_google(key = API_key, write = TRUE)
  } else {
    stop("No API key provided. Cannot access Google Geocoding API.")
  }


  if (length(addresses) > 0) {
  
    # geocode addresses in parallel in batches of 500 rows
    df_list <- vector(mode = "list")
    n_groups <- 5
    nr <- length(addresses)
    n <- ceiling(nr / n_groups)
    t <- split(addresses, rep(1:n_groups, each=n, length.out=nr))
    for (j in 1:length(t)) {
      print(j)
      out <- .geocode_addresses_subset(t[[j]])
      df_list <- append(df_list, list(out))
    }
    # bind 500-row batches and indicate new columns with suffix "_ggl"
    df_geocoded <- bind_rows(df_list, .id = NULL)
    colnames(df_geocoded) <- paste(colnames(df_geocoded), sep = "_", "ggl")

  } else {
    stop("addresses vector is length zero.")
  }
  
  paste0("# of addresses not geocoded: ", nrow(df_geocoded %>% dplyr::filter(is.na(lat_ggl))))

  return(df_geocoded)
}

.geocode_addresses_subset <- function(addresses_subvector){
  #' @title Geocode Addresses
  #' @description Returns latitude and longitude of addresses in a dataframe.

  row_list <- list()
  for (i in 1:length(addresses_subvector)) {
    # initialize data frame that will hold geocode information
    answer <- .geocode_single_address(addresses_subvector[i])
    
    row_list <- append(row_list, list(answer))
  }
  return(bind_rows(row_list))
}


.geocode_single_address <- function(address) {
  
  answer <- data.frame(lat=NA, long=NA, formatted_address=NA,
                       location_type=NA, address_type=NA)
  
  # answer$address <- address
  
  # perform geocoding on address if it is not NA
  if ((!is.na(address)) & (!address=="")) {

    geo_reply <- tryCatch(
                  {
                    # use the geocode function to query google servers
                    geocode(location=address, output='all', messaging=TRUE, override_limit=FALSE)
                  },
                  error=function(cond) {
                      return(answer)
                  },
                  warning=function(cond) {
                      return(answer)
                  }
                 )
      
    if ("results" %in% names(geo_reply)) {
      # else, extract what we need from the Google server reply into a dataframe:
      answer$lat <- geo_reply$results[[1]]$geometry$location$lat
      answer$long <- geo_reply$results[[1]]$geometry$location$lng
      answer$location_type <- geo_reply$results[[1]]$geometry$location_type
      
      answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
      answer$formatted_address <- geo_reply$results[[1]]$formatted_address
    
      # extract address components, city and state_abr
      add_comps <- unlist(geo_reply$results[[1]]$address_components)
      city_i <- which(add_comps=="locality")
      state_i <- which(add_comps=="administrative_area_level_1")
      if (length(city_i) > 0) {
        answer$city <- add_comps[city_i-1][[1]]
      }
      if (length(state_i) > 0) {
        answer$state_abr <- add_comps[state_i-1][[1]]
      }
    }
  }
  
  return(answer)
  
}

