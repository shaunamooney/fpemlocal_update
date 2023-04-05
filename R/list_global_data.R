

#' list_global_data
#'fit_fp_c
#' jags data obtained from the UNPD global model
#'
#' @inheritParams fit_fp_c
#' @param core_data \emph{\sQuote{Data.frame}} The processed data associated with the model run from \code{\link{core_data}}.
#'
list_global_data = function(is_in_union, core_data) {
 
  #is_in_union <- (is_in_union == "Y")
  div <- core_data$units$division_numeric_code
  start_year <- core_data$start_year
  first_year <- core_data$year_sequence_list$model_seq_years %>% min()
  subnational <- core_data$subnational
  
  globaldata <- list() # SM 21062021 create lists for storing
  globaldata_sd <- list()
  link <- list()
  subreg <- list()
  reg <- list() 
  pmax_lower_bound <- list()
  is.dev.c <- list()
  country.c <- list()
  tau.sourcemodonly <- list() # SM 21062021 lists for storing

  globaldata[[1]] <- globalrun_output_m$mcmc.post # SM 21062021 married women indicators
  globaldata_sd[[1]] <- globalrun_output_m$mcmc.post.sd
  link[[1]] <- index_m
  subreg[[1]] <- link[[1]]$index_area_df$subreg.c[link[[1]]$index_area_df$division_numeric_code == div]
  reg[[1]] <- link[[1]]$index_area_df$reg.c[link[[1]]$index_area_df$division_numeric_code == div]
  pmax_lower_bound[[1]] <- .5
  is.dev.c[[1]] <- ifelse(core_data$units$is_developed_region == "N", 0, 1)
  country.c[[1]] <- which(link[[1]]$index_area_df$division_numeric_code == div)
  tau.sourcemodonly[[1]] <- (1/globaldata[[1]]$sigma.sourcemodonly)^2
  
  globaldata[[2]] <- globalrun_output_u$mcmc.post # SM 21062021 unmarried women indicators
  globaldata_sd[[2]] <- globalrun_output_u$mcmc.post.sd
  link[[2]] <- index_u
  subreg[[2]] <- link[[2]]$index_area_df$subreg.c[link[[2]]$index_area_df$division_numeric_code == div]
  reg[[2]] <- link[[2]]$index_area_df$reg.c[link[[2]]$index_area_df$division_numeric_code == div]
  pmax_lower_bound[[2]] <- .1
  is.dev.c[[2]] <- ifelse(core_data$units$is_unmarried_sexual_activity == "Y", 0, 1) # dev = 1 for SA0 group
  country.c[[2]] <- which(link[[2]]$index_area_df$division_numeric_code == div)
  tau.sourcemodonly[[2]] <- NA
  
  unmet.subreg <- list() # SM 22062021 created lists
  sd_unmet.subreg <- list()
  w.subreg <- list()
  sd_w.subreg <- list()
  Rw.subreg <- list()
  sd_Rw.subreg <- list()
  RT.subreg <- list()
  sd_RT.subreg <- list()
  mean_setlevel <- list()
  sd_mean_setlevel <- list()
  var_setlevel <- list()
  lp.world <- list()
  sd_lp.world <- list()
  lr.world <- list()
  sd_lr.world <- list()
  tau_setlevel <- list()
  
  for(u in 1:2){ # SM 22062021 union loop
    
    if (!subnational | is.null(globaldata[[u]][[paste0('setlevel.c[',country.c[[u]], "]")]])) { # SM 21062021 changed index on this added [[]]
      if(is.null(globaldata[[u]][[paste0('setlevel.c[',country.c[[u]], "]")]])) {
        warning("unmarried parameters missing, higher level parameters used")
      }
      unmet.subreg[[u]] <- globaldata[[u]][[paste0('unmet.subreg[',subreg[[u]], "]")]]
      sd_unmet.subreg[[u]] <- globaldata_sd[[u]][[paste0('unmet.subreg[',subreg[[u]], "]")]]
      w.subreg[[u]] <- globaldata[[u]][[paste0('w.subreg[',subreg[[u]], "]")]]
      sd_w.subreg[[u]] <- globaldata_sd[[u]][[paste0('w.subreg[',subreg[[u]], "]")]]
      Rw.subreg[[u]] <- globaldata[[u]][[paste0('Rw.subreg[',subreg[[u]], "]")]]
      sd_Rw.subreg[[u]] <- globaldata_sd[[u]][[paste0('Rw.subreg[',subreg[[u]], "]")]]
      RT.subreg[[u]] <- globaldata[[u]][[paste0('RT.subreg[',subreg[[u]], "]")]] - first_year + 1
      sd_RT.subreg[[u]] <- globaldata_sd[[u]][[paste0('RT.subreg[',subreg[[u]], "]")]]
      mean_setlevel[[u]] <- is.dev.c[[u]]*globaldata[[u]]$Shigher + (1-is.dev.c[[u]])*globaldata[[u]][[paste0('S.subreg[',subreg[[u]], "]")]]
      sd_mean_setlevel[[u]] <- is.dev.c[[u]]*globaldata_sd[[u]]$Shigher + (1-is.dev.c[[u]])*globaldata_sd[[u]][[paste0('S.subreg[',subreg[[u]], "]")]]
      # for unmarried, dev <- 1 is SA0, so we have the hierarchy for SA1
      var_setlevel[[u]] <- is.dev.c[[u]]*globaldata[[u]]$sigma.higherSc^2 + (1-is.dev.c[[u]])*globaldata[[u]]$sigma.Sc^2
      lp.world[[u]] <- globaldata[[u]]$lp.world
      sd_lp.world[[u]] <- globaldata_sd[[u]]$lp.world
      lr.world[[u]] <- globaldata[[u]]$lr.world
      sd_lr.world[[u]] <- globaldata_sd[[u]]$lr.world
    } else {
      unmet.subreg[[u]] <- globaldata[[u]][[paste0('unmet.intercept.c[',country.c[[u]], "]")]]
      sd_unmet.subreg[[u]] <- globaldata_sd[[u]][[paste0('unmet.intercept.c[',country.c[[u]], "]")]]
      RT.subreg[[u]] <- globaldata[[u]][[paste0('RT.c[',country.c[[u]], "]")]] - first_year + 1
      sd_RT.subreg[[u]] <- globaldata[[u]][[paste0('RT.c[',country.c[[u]], "]")]]
      mean_setlevel[[u]] <- globaldata[[u]][[paste0('setlevel.c[',country.c[[u]], "]")]]
      sd_mean_setlevel[[u]] <- globaldata_sd[[u]][[paste0('setlevel.c[',country.c[[u]], "]")]]
      var_setlevel[[u]] <- is.dev.c[[u]]*globaldata[[u]]$sigma.higherSc^2 + (1-is.dev.c[[u]])*globaldata[[u]]$sigma.Sc^2
  
  
      # country parameters that need transformation:
      w.subreg[[u]] <- get_logtr_omegas(globaldata[[u]][[paste0('omega.c[',country.c[[u]], "]")]], mino = 0.01, maxo = 0.5) # w.subreg is on logit omega scale
      sd_w.subreg[[u]] <- 0 #get_logtr_omegas(globaldata_sd[[paste0('omega.c[',country.c, "]")]], mino = 0.01, maxo = 0.5) # w.subreg is on logit omega scale
      Rw.subreg[[u]] <- get_logtr_omegas(globaldata[[u]][[paste0('Romega.c[',country.c[[u]], "]")]], mino = 0.01, maxo = 0.5) # Rw.subreg is on logit omega scale
      sd_Rw.subreg[[u]] <- 0 #get_logtr_omegas(globaldata_sd[[paste0('Romega.c[',country.c, "]")]], mino = 0.01, maxo = 0.5)
      lp.world[[u]] <- get_logtr_omegas(globaldata[[u]][[paste0('pmax.c[',country.c[[u]], "]")]], mino = pmax_lower_bound[[u]], maxo = 1)
      lr.world[[u]] <- get_logtr_omegas(globaldata[[u]][[paste0('Rmax.c[',country.c[[u]], "]")]], mino = 0.5, maxo = 1)
      sd_lp.world[[u]] <- 0
      sd_lr.world[[u]] <- 0
    }
    tau_setlevel[[u]] <- is.dev.c[[u]]*1/globaldata[[u]]$sigma.higherSc^2 + (1-is.dev.c[[u]])*1/globaldata[[u]]$sigma.Sc^2
  } # end union loop
  
    se <- list() # SM 05072021 changed back to lists
    nonsample.se.modern.i <- list()
    nonsample.se.trad.i <- list()
    cor.trad.modern.i <- list()
    se2 <- list()
    nonsample.se.unmet.i <- list()

    for (u in 1:2){ # SM 05072021 trying union loop here
    if (!nrow(core_data$observations) == 0) {
      index_datatype <- core_data$observations$index_datatype
      index_datatype_unmet <- core_data$observations$index_datatype_unmet
      names <- c(
        "nonsample.se.modern.s",
        "nonsample.se.trad.s",
        "cor.trad.modern.s"
      )
      
      se[[u]] <- extract_se(names, globaldata[[u]], index_datatype)
      nonsample.se.modern.i[[u]] <- se[[u]]$nonsample.se.modern.s
      nonsample.se.trad.i[[u]] <- se[[u]]$nonsample.se.trad.s
      cor.trad.modern.i[[u]] <- se[[u]]$cor.trad.modern.s
  
      names <- c("nonsample.se.unmet.s")
      se2[[u]] <- extract_se(names, globaldata[[u]], index_datatype_unmet)
      nonsample.se.unmet.i[[u]] <- se2[[u]]$nonsample.se.unmet.s
      
    } else {
      
      nonsample.se.modern.i[[u]] <- 0
      nonsample.se.trad.i[[u]] <- 0
      cor.trad.modern.i[[u]] <- 0
      nonsample.se.unmet.i[[u]] <- 0
    
    
    }
    
  } # end union
    
    #browser()
    
    if (is_in_union == "ALL") { # SM 31082021 fixing output for plucking
      nonsample.se.modern.i.out <- matrix(unlist(nonsample.se.modern.i), ncol = 2)
      nonsample.se.trad.i.out <-  matrix(unlist(nonsample.se.trad.i), ncol = 2)
      cor.trad.modern.i.out <- matrix(unlist(cor.trad.modern.i), ncol = 2)
      nonsample.se.unmet.i.out <- matrix(unlist(nonsample.se.unmet.i), ncol = 2)
      mu.pos.m.out <- as.matrix(cbind(c(globaldata[[1]]$`mu.pos.m[1]`, globaldata[[1]]$`mu.pos.m[2]`),
                                c(globaldata[[2]]$`mu.pos.m[1]`, globaldata[[2]]$`mu.pos.m[2]`)))
      sigma.geo.m.out <- as.matrix(cbind(c(globaldata[[1]]$`sigma.geo.m[1]`, globaldata[[1]]$`sigma.geo.m[2]`),
                                         c(globaldata[[2]]$`sigma.geo.m[1]`, globaldata[[2]]$`sigma.geo.m[2]`))) 
      t_star.out <- start_year - first_year +1
    }
    
    else {
      nonsample.se.modern.i.out <- matrix((nonsample.se.modern.i), ncol = 2)
      nonsample.se.trad.i.out <-  matrix((nonsample.se.trad.i), ncol = 2)
      cor.trad.modern.i.out <- matrix((cor.trad.modern.i), ncol = 2)
      nonsample.se.unmet.i.out <- matrix((nonsample.se.unmet.i), ncol = 2)
      mu.pos.m.out <- cbind(list(c(globaldata[[1]]$`mu.pos.m[1]`, globaldata[[1]]$`mu.pos.m[2]`)),
                                list(c(globaldata[[2]]$`mu.pos.m[1]`, globaldata[[2]]$`mu.pos.m[2]`)))
      sigma.geo.m.out <- cbind(list(c(globaldata[[1]]$`sigma.geo.m[1]`, globaldata[[1]]$`sigma.geo.m[2]`)),
                                   list(c(globaldata[[2]]$`sigma.geo.m[1]`, globaldata[[2]]$`sigma.geo.m[2]`)))
      t_star.out <- c(start_year - first_year +1, start_year - first_year +1)
    }
    
    
    global_data <- list(
      # pars which change in model depending on sun-national
      tau.sourcemodonly = unlist(tau.sourcemodonly),
      pmax_lower_bound = unlist(pmax_lower_bound),
      nonsample.se.modern.i = nonsample.se.modern.i.out,
      nonsample.se.trad.i = nonsample.se.trad.i.out,
      cor.trad.modern.i = cor.trad.modern.i.out,
      nonsample.se.unmet.i = nonsample.se.unmet.i.out,
      var_setlevel = unlist(var_setlevel),
      # pars which are based on a hierarchical model in the global model
      lp.world = unlist(lp.world),
      sd_lp.world = c(0,0),#sd_lp.world,
      
      lr.world = unlist(lr.world),
      sd_lr.world = c(0,0),#sd_lr.world,
      
      unmet.subreg = unlist(unmet.subreg),
      sd_unmet.subreg = c(0,0),#sd_unmet.subreg,
      
      w.subreg = unlist(w.subreg),
      sd_w.subreg = c(0,0),#sd_w.subreg,
      
      Rw.subreg = unlist(Rw.subreg),
      sd_Rw.subreg = c(0,0),# sd_Rw.subreg,
      
      RT.subreg = unlist(RT.subreg),
      sd_RT.subreg = c(0,0),#sd_RT.subreg,
      
      mean_setlevel = unlist(mean_setlevel),
      sd_mean_setlevel = c(0,0),#sd_mean_setlevel,
      
      ####### end pars based on hier hier
      ###### end pars which change
      
      rho.P = c(globaldata[[1]]$rho.tot, globaldata[[2]]$rho.tot),
      rho.R = c(globaldata[[1]]$rho.rat, globaldata[[2]]$rho.rat),
      rho.Z = c(globaldata[[1]]$rho.unmet, globaldata[[2]]$rho.unmet),
      sigma.P = c(globaldata[[1]]$sigma.tot, globaldata[[2]]$sigma.tot),
      sigma.R = c(globaldata[[1]]$sigma.rat, globaldata[[2]]$sigma.rat),
      sigma.Z = c(globaldata[[1]]$sigma.ar.unmet, globaldata[[2]]$sigma.ar.unmet),
      sigma.lpc = c(globaldata[[1]]$sigma.lpc, globaldata[[2]]$sigma.lpc),
      sigma.lrc = c(globaldata[[1]]$sigma.lrc, globaldata[[2]]$sigma.lrc),
      sigma.wc = c(globaldata[[1]]$sigma.wc, globaldata[[2]]$sigma.wc),
      
      sigma.Rwc = c(globaldata[[1]]$sigma.Rwc, globaldata[[2]]$sigma.Rwc),
      sigma.RTc = c(globaldata[[1]]$sigma.RTc, globaldata[[2]]$sigma.RTc),
      sd_sigma.RTc = c(globaldata_sd[[1]]$sigma.RTc, globaldata_sd[[2]]$sigma.RTc),
      sigma.unmetc = c(globaldata[[1]]$sigma.unmetc, globaldata[[2]]$sigma.unmetc),
      sigma.sourcetot = c(globaldata[[1]]$sigma.sourcetot, globaldata[[2]]$sigma.sourcetot),
      
      a.unmet = c(globaldata[[1]]$a.unmet, globaldata[[2]]$a.unmet),
      b.unmet = c(globaldata[[1]]$b.unmet, globaldata[[2]]$b.unmet),
      c.unmet = c(globaldata[[1]]$c.unmet, globaldata[[2]]$c.unmet),
      
      v.folk = c(globaldata[[1]]$v.folk, globaldata[[2]]$v.folk),
      v.mics = c(globaldata[[1]]$v.abs.probe.q,globaldata[[2]]$v.abs.probe.q),
      v.mpos = c(globaldata[[1]]$v.mpos, globaldata[[2]]$v.mpos),
      v.mneg = c(globaldata[[1]]$v.mneg, globaldata[[2]]$v.mneg),
      
      sigma.pos = c(globaldata[[1]]$sigma.pos,globaldata[[2]]$sigma.pos),
      mu.pos.m = mu.pos.m.out, # SM took out as.matrix and added list()
      sigma.geo.m = sigma.geo.m.out,
      
      
      # hardcoded because not found in globalparameters
      t_star = t_star.out 
      #t_star = global_run_paramestimates$t_star
    )
    
    # SM return full dataset for ALL, 1st element for MW, 2nd element for UW
    if(is_in_union == "ALL"){
      data_list <- global_data
    } 
    
    else if(is_in_union == "Y"){
      data_list <- purrr::map(global_data, 1)
    }
    
    else {
      data_list <- purrr::map(global_data, 2)
    }

    return(data_list)
}


extract_se <- function(names, data.global, numeric_source) {
  se_ls <- list()
  for(j in 1:length(names)){
    temp <- c()
    for(i in 1:length(numeric_source)) {
      temp[i] <- data.global[[which(names(data.global) == paste0(names[j],'[',numeric_source[i], "]") )]]
    }
    se_ls[[j]] <- temp
  }
  names(se_ls) <- names
  return(se_ls)
}


get_logtr_omegas <- function(omega, mino, maxo) {
  log((omega - mino)/(maxo - omega))
}
