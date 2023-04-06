population_data_import <- function(
  population_data = NULL,
  fit
) 
{
  is_in_union <- fit$core_data$observations$is_in_union %>% unique()# SM 20062022
  ifelse(!purrr::is_empty(is_in_union), is_in_union <- is_in_union, is_in_union <- "ALL")
  division_numeric_code <- fit %>% purrr::chuck("core_data", "units", "division_numeric_code")
  first_year <- fit %>% 
    purrr::chuck("core_data","year_sequence_list", "result_seq_years") %>% 
    min
  last_year <- fit %>% 
    purrr::chuck("core_data","year_sequence_list", "result_seq_years") %>% 
    max
  
  if(!is.null(population_data)) {
    
    if(is_in_union == "ALL") { # SM 22062022
      
      population_data <- population_data %>% 
        filter(division_numeric_code == {{division_numeric_code}}) %>% 
        dplyr::filter(mid_year >= first_year) %>%
        dplyr::filter(mid_year <= last_year) %>% 
        dplyr::group_by(mid_year)  %>% 
        dplyr::summarise(population_count = sum(population_count)) %>% 
        dplyr::mutate(is_in_union = is_in_union, 
               division_numeric_code = division_numeric_code, 
               age_range = "15-49") %>% 
        dplyr::select(is_in_union, 
               division_numeric_code, 
               population_count, 
               age_range, 
               mid_year)
    }
    
    else {
      
      population_data <- population_data %>%
        dplyr::filter(is_in_union == {{is_in_union}}) %>% 
        dplyr::filter(division_numeric_code == {{division_numeric_code}}) %>%
        dplyr::filter(mid_year >= first_year) %>%
        dplyr::filter(mid_year <= last_year)
    }
  } 
  
  else {
    
    if(is_in_union == "ALL") { # SM 22062022
     
       population_data <- fpemlocal::population_counts %>% 
        filter(division_numeric_code == division_numeric_code) %>% 
        dplyr::filter(mid_year >= first_year) %>%
        dplyr::filter(mid_year <= last_year) %>% 
        dplyr::group_by(mid_year)  %>% 
        dplyr::summarise(population_count = sum(population_count)) %>% 
        dplyr::mutate(is_in_union = is_in_union, 
               division_numeric_code = division_numeric_code, 
               age_range = "15-49") %>% 
        dplyr::select(is_in_union, 
               division_numeric_code, 
               population_count, 
               age_range, 
               mid_year)
    }
    
    else {
      
      population_data <- fpemlocal::population_counts %>%
        dplyr::filter(is_in_union == {{is_in_union}}) %>% 
        dplyr::filter(division_numeric_code == {{division_numeric_code}}) %>%
        dplyr::filter(mid_year >= first_year) %>%
        dplyr::filter(mid_year <= last_year)
    }
  }
  return(population_data)
}

