
#' Posterior samples reformat
#'
#' Manipulate posterior samples into dimension chains x samples x years x proportions
#'
#' @param fit the object returned from jags
#' @param core_data consists of manipulated observations and run settings for \code{\link{fit_fp_c}}
#'
#' @return \emph{\sQuote{Numeric array}} An array of samples of dimension chains x samples x years x proportions
posterior_samples_array_format <- function(fit, core_data) {
  props <- c("mod", "trad", "unmet")
  nyears <-  max(core_data$year_sequence_list$model_seq)
  tempdim <- fit$BUGSoutput$sims.list$mod.ct
  total_iter <- dim(tempdim)[1]
  mod <- fit$BUGSoutput$sims.list$mod.ct[,,,] %>% as.vector() # SM 13062021 added u dimension to all
  trad <- fit$BUGSoutput$sims.list$trad.ct[,,,] %>% as.vector()
  unmet <- fit$BUGSoutput$sims.list$unmet.ct[,,,] %>% as.vector()
  union_dim <- ifelse(core_data$is_in_union == "ALL", 3, 1) # SM 12102021 putting this everywhere I have hard coded 3

  
  posterior_samples <- array(
    data = c(mod, trad, unmet),
    dim = c(1, total_iter, union_dim, nyears, length(props)), # SM CHECK 3
    dimnames = list(
      chain = 1,
      iteration = 1:total_iter,
      union_status = 1:union_dim, # SM CHECK 3
      unit_time = 1:nyears,
      props = props
    )
  )
    

# SM Hardcoded 3 below also  
return(posterior_samples[1,1:total_iter, 1:union_dim, core_data$year_sequence_list$result_seq,1:length(props), drop = FALSE])

# SM 23072021 added as.array, unlist, not sure if this just papers over issue
}
