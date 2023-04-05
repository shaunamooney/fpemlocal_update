
#' list_service_stats
#'
#' @inheritParams fit_fp_c
#' @param service_stats_filepath File path to service stat csv
#' @param model_seq_years Years of model estimation from\code{\link{year_sequence_maker}}.
#'
list_service_stats <- function(
  service_stats_filepath,
  model_seq_years,
  division_numeric_code, # we need the unit ID added eventually to use that for filtering
  first_year_observed = NULL # to filter out the data prior to or in year of the most recent survey
)

{
  # ******2021 database AW source specific bias term*****************
  
  se_visits = 0.0371 # SM changed 14022023
  se_clients = 0.0353 
  se_facilities = 0.0564
  se_users = 0.0837

  # setting bias terms to 0 for now SM 07022023 - changing to compare bias results - will change back SM 13022023
  bias_visits = 0.00879
  bias_clients = 0.00657 
  bias_facilities = 0.0131 
  bias_users = 0.0117 
  
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
    
    #browser()
    format_check(service_stats_format, ss)
    ss <- ss %>% dplyr::arrange(year)
    ss <- ss %>%
      dplyr::filter(division_numeric_code == !! division_numeric_code) %>%
      dplyr::filter(year >= first_year_observed) %>%
      tidyr::drop_na() %>%
      dplyr::mutate(ss_delta = c(NA,diff(emu))) %>%
      dplyr::mutate(ss_year_lag = dplyr::lag(year)) %>%
      dplyr::mutate(ss_se = ifelse(ss_type == "visits", se_visits,
                                    ifelse(ss_type == "clients", se_clients,
                                           ifelse(ss_type == "facilities", se_facilities,  se_users)))) %>%
      dplyr::mutate(ss_bias = ifelse(ss_type == "visits", bias_visits,
                                   ifelse(ss_type == "clients", bias_clients,
                                          ifelse(ss_type == "facilities", bias_facilities,  bias_users))))
    if (nrow(ss) == 0) {
      stop("No available service stats after filtering. Service stats were filtered based on division numeric code.")
    }

    k_index <- ss$ss_delta %>% is.na %>% `!` %>% which
    list_ss_data <- list(K = k_index %>% length,
                         get_t_k = match(x = floor(ss$year),model_seq_years), # k+1 years and k differences
                         ss_delta_k = ss$ss_delta[k_index],
                         ss_se_k = ss$ss_se[k_index],
                         ss_bias_k = ss$ss_bias[k_index]
                         )
  } else {
    list_ss_data <- NULL
  }
  return(list_ss_data)
}
