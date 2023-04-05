#' list_bias_data
#'
#' jags data, numeric form of bias using \code{\link{core_data}}.
#'
#' @param core_data \emph{\sQuote{Data.frame}} The processed data associated with the model run from \code{\link{core_data}}.
#'
list_bias_data <- function(core_data, is_in_union){
  dat <- core_data$observations

  # get bias/mult info
  # is NOT yet set up to work for more than 1 country
  # (same multiplier would be assigned)
  # following old jags model implementation here
  # where 1 refers to no multiplier added
  data_biasmult <- dat %>% dplyr::group_by(is_in_union) %>% 
    dplyr::mutate(geo.ind = ifelse(has_geographical_region_bias == "N",
                                   1,
                                   1+ 1)) %>%
    dplyr::mutate(hw.ind = ifelse(is.element(group_type_relative_to_baseline, c("BS", "HW", "PW")), 2, 1)) %>%
    dplyr::mutate(emal.ind = ifelse(is.element(group_type_relative_to_baseline, c("AL", "EM", "FM")), 2, 1)) %>%
    dplyr::mutate(sa.ind = ifelse(is.element(group_type_relative_to_baseline, c("SA")), 2, 1)) %>%
    dplyr::mutate(posbias.ind = ifelse(has_non_pregnant_and_other_positive_biases == "N", 1,
                                       1+ 1)) %>%
    dplyr::mutate(age.ind = ifelse(age_group_bias != "?", 1, 2)) %>% # again, 1 is baseline
    dplyr::mutate(posage.ind = ifelse(age_group_bias != "+", 1, 2)) %>% # again, 1 is baseline
    dplyr::mutate(negage.ind = ifelse(age_group_bias != "-", 1, 2)) %>% # again, 1 is baseline
    dplyr::mutate(folk.ind = ifelse(has_traditional_method_bias == "Y", 1, 0)) %>%
    dplyr::mutate(source.MICS.ind = ifelse(has_absence_of_probing_questions_bias == "Y", 1, 0)) %>%
    dplyr::mutate(mpos.ind = ifelse(modern_method_bias == "+", 1, 0)) %>%
    dplyr::mutate(mneg.ind = ifelse(modern_method_bias == "-", 1, 0)) %>%
    dplyr::select(is_in_union, geo.ind, hw.ind, emal.ind, sa.ind, posbias.ind, age.ind, posage.ind, negage.ind,
                  folk.ind, source.MICS.ind, mpos.ind, mneg.ind)
  
  
  if(length(data_biasmult$is_in_union %>% unique()) == 1|nrow(data_biasmult) == 0){
    
    maxcat <- apply(data_biasmult, 2, max)
    
    jagsdata_add <- c(
      as.list(data_biasmult),
      list(
        ncat.geo = c(c(maxcat['geo.ind']),c(-Inf)),
        ncat.hw = c(c(maxcat['hw.ind']),c(-Inf)),
        ncat.emal = c(c(maxcat['emal.ind']),c(-Inf)),
        ncat.sa = c(c(maxcat['sa.ind']),c(-Inf)),
        ncat.posbias = c(c(maxcat['posbias.ind']),c(-Inf)),
        ncat.age = c(c(maxcat['age.ind']),c(-Inf)),
        ncat.posage = c(c(maxcat['posage.ind']),c(-Inf)),
        ncat.negage  = c(c(maxcat['negage.ind']),c(-Inf))
      )
    )
  }
  
  else {
    
    maxcat <- data_biasmult %>% 
      dplyr::group_by(is_in_union) %>% 
      dplyr::summarise_if(is.numeric, max) %>% 
      dplyr::arrange(desc(is_in_union))
    
    jagsdata_add <- c(
      as.list(data_biasmult),
      list(
        ncat.geo = maxcat %>% dplyr::pull(geo.ind),
        ncat.hw = maxcat %>% dplyr::pull(hw.ind),
        ncat.emal = maxcat %>% dplyr::pull(emal.ind),
        ncat.sa = maxcat %>% dplyr::pull(sa.ind),
        ncat.posbias = maxcat %>% dplyr::pull(posbias.ind),
        ncat.age = maxcat %>% dplyr::pull(age.ind),
        ncat.posage = maxcat %>% dplyr::pull(posage.ind),
        ncat.negage  = maxcat %>% dplyr::pull(negage.ind)
      )
    )
  }
  

  return(jagsdata_add)
}
