# SM 05102021 population import function for JAGS
# usually done in poster_samples - posterior_samples_all_women function
# this is now bypassed and done in JAGS beforehand

list_union_population <- function(core_data, population_data){
  
  if (is.null(population_data)) {
    population_data <- fpemlocal::population_counts
  }

  division_numeric_code <- core_data$units$division_numeric_code
  first_year <- min(core_data$year_sequence_list$model_seq_years)
  last_year <- max(core_data$year_sequence_list$model_seq_years)
  nyears <- length(core_data$year_sequence_list$model_seq_years)
  in_union_population_counts <- population_data %>%
    dplyr::filter(division_numeric_code == !!division_numeric_code) %>%
    dplyr::filter(is_in_union == "Y") %>%
    dplyr::filter(age_range == "15-49") %>%
    dplyr::filter(mid_year >= first_year) %>%
    dplyr::filter(mid_year <= last_year) %>%
    dplyr::select(population_count) %>%
    unlist() %>%
    as.vector()
  
  if(nyears > length(in_union_population_counts)) in_union_population_counts <- c(rep(in_union_population_counts[1],(nyears-length(in_union_population_counts))),in_union_population_counts)
  
  
  not_in_union_population_counts <- population_data %>%
    dplyr::filter(division_numeric_code == !!division_numeric_code) %>%
    dplyr::filter(is_in_union == "N") %>%
    dplyr::filter(age_range == "15-49") %>%
    dplyr::filter(mid_year >= first_year) %>%
    dplyr::filter(mid_year <= last_year) %>%
    dplyr::select(population_count) %>%
    unlist() %>%
    as.vector()
  
  if(nyears > length(not_in_union_population_counts)) not_in_union_population_counts <- c(rep(not_in_union_population_counts[1],(nyears-length(not_in_union_population_counts))),not_in_union_population_counts)
  
  
  
  return(list(in_union_population_counts = in_union_population_counts, 
              not_in_union_population_counts = not_in_union_population_counts))
}