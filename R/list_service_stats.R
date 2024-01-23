#' list_service_stats - no fixed SE estimates used, user inputted only
#'
#' @inheritParams fit_fp_c
#' @param service_stats_filepath File path to service stat csv
#' @param model_seq_years Years of model estimation from\code{\link{year_sequence_maker}}.
#'
#' 
list_service_stats <- function(
    service_stats_filepath,
    model_seq_years,
    division_numeric_code, # we need the unit ID added eventually to use that for filtering
    first_year_observed = NULL# to filter out the data prior to or in year of the most recent survey
)
  
{
  
  if (is.null(service_stats_filepath)) {
    return(NULL)
  }
  
  ss <- readr::read_csv(service_stats_filepath)
  format_check(fpemlocal::service_stats_format, ss)
  
  
  ss <- ss %>%
    dplyr::arrange(year) %>%
    dplyr::filter(division_numeric_code == !! division_numeric_code) %>%
    dplyr::filter(year >= first_year_observed) %>%
    dplyr::group_by(ss_type)
    
    if(!("delta_emu" %in% names(ss))){
      ss <- ss %>%
        dplyr::mutate(ss_delta = c(NA,diff(emu))) %>% 
        dplyr::mutate(ss_year_lag = dplyr::lag(year)) 
    }
    
    else{
      ss <- ss %>% 
        dplyr::mutate(ss_delta = delta_emu) %>% 
        dplyr::mutate(ss_year_lag = year) 
    }
    
    # pull in user inputted se estimates
    ss <- ss %>%
      dplyr::mutate(ss_se = se)
      
    if (nrow(ss) == 0) {
      stop("No available service stats after filtering. Service stats were filtered based on division numeric code.")
    }
    
    pop_type <- ss %>% dplyr::pull(pop_type) %>% unique()
    
    if (length(pop_type) > 1) {
      stop("Multiple population types found. Unable to determine pop_index.")
    }
    
    pop_index <- ifelse(pop_type == "Y", 1, 3) # informing married women or all women mcpr
    
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
