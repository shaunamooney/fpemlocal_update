
#' list_service_stats
#'
#' @inheritParams fit_fp_c
#' @param service_stats_filepath File path to service stat csv
#' @param model_seq_years Years of model estimation from\code{\link{year_sequence_maker}}.
#'
#' run for most recent emu database
#' check that this is done by observation not by database type - look at diff countries
#' is it just when you use more do they have more impact - check for multiple countries 
#' 
list_service_stats <- function(
    service_stats_filepath,
    model_seq_years,
    division_numeric_code, # we need the unit ID added eventually to use that for filtering
    first_year_observed = NULL# to filter out the data prior to or in year of the most recent survey
)
  
{
  # ******2021 database AW source specific standard error term*****************
  #se_visits = 0.0373
  #se_clients = 0.0354 
  #se_facilities = 0.0568
  #se_users = 0.0835

  
  if (!is.null(service_stats_filepath)) {
    if (is.na(first_year_observed)) {
      stop("No available contraceptive use data for this run. Service statistics cannot be used.")
    }
    if (!is.null(service_stats_filepath)) {
      ss <- readr::read_csv(service_stats_filepath)
    }
    # else {
    #   ss <- fpemservicestat::service_stats
    # }
    
    
    format_check(fpemlocal::service_stats_format, ss)
    ss <- ss %>% dplyr::arrange(year)
    ss <- ss %>%
      dplyr::filter(division_numeric_code == !! division_numeric_code) %>%
      dplyr::filter(year >= first_year_observed) %>%
      dplyr::group_by(ss_type) # %>% # allows delta to calculate properly when multiple emus used
    # tidyr::drop_na() 
    
    if(!("delta_emu" %in% names(ss))){
      ss <- ss %>%
        dplyr::mutate(ss_delta = c(NA,diff(emu))) %>% # have to fix this 
        dplyr::mutate(ss_year_lag = dplyr::lag(year)) 
    }
    
    else{
      ss <- ss %>% 
        dplyr::mutate(ss_delta = delta_emu) %>% # have to fix this 
        dplyr::mutate(ss_year_lag = year) 
    }
    
    #if (!("se" %in% names(ss))) {
     # ss <- ss %>%
    #    dplyr::mutate(ss_se = ifelse(ss_type == "visits", se_visits,
    #                                 ifelse(ss_type == "clients", se_clients,
   #                                         ifelse(ss_type == "facilities", se_facilities,  se_users))))
    #}
    
    else {
      ss <- ss %>%
        dplyr::mutate(ss_se = se)
    }
    if (nrow(ss) == 0) {
      stop("No available service stats after filtering. Service stats were filtered based on division numeric code.")
    }
    
    pop_type <- ss %>% dplyr::pull(pop_type) %>% unique()
    
    if(pop_type == "Y"){
      pop_index <- 1 # inform married women mcpr
    } else{
      pop_index <- 3 # inform all women mcpr
    }
    
    k_index <- ss$ss_delta %>% is.na %>% `!` %>% which
    list_ss_data <- list(K = k_index %>% length,
                         get_t_k = match(x = floor(ss$year),model_seq_years), # k+1 years and k differences
                         ss_delta_k = ss$ss_delta[k_index],
                         ss_se_k = ss$ss_se[k_index],
                         pop_index = pop_index)
  } else {
    list_ss_data <- NULL
  }
  
  return(list_ss_data)
}
