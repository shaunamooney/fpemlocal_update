write_jags_model <- function(old_dm = FALSE,
                             include_ss_data = FALSE,
                             nulldata,
                             is_in_union) {
  
  # model taken from https://github.com/FPcounts/ContraceptiveUse_MWRA_Update
  # deleting all global-only stuff
  # remove jumping over years w/o observations
  # kept C because easier
  # but code is coded for C= 1 (ie hardcoded subreg index)
  
  cat("
model{
  Y  ~ dnorm(greghack, 1)
  greghack ~ dnorm(1,1)
  # AR part - modelling epsilon, how much the model deviates from observed
  for (c in 1:C){
    for (u in 1:U){ # union loop SM 05052021
      # AR1 models (ARIMA model) Autoregressive - lagged version (regression at time t-1)
      eps.P.ct[c, u, t_star] ~ dnorm(0, tau.P.st[u]) # for P t_start is probs in jags.parallel start year - first year+1 in global data
      eps.R.ct[c, u, t_star] ~ dnorm(0, tau.R.st[u]) # for R
      eps.Z.ct[c, u, t_star] ~ dnorm(0, tau.Z.st[u]) # for Z - variance tau, 1/variance
      for (t in (t_star+1):nyears){
        eps.P.ct[c, u, t] ~ dnorm(rho.P[u]*eps.P.ct[c, u, t-1], tau.P[u]) # rho is like slope of the model, constrained positive
        eps.R.ct[c, u, t] ~ dnorm(rho.R[u]*eps.R.ct[c, u, t-1], tau.R[u])
        eps.Z.ct[c, u, t] ~ dnorm(rho.Z[u]*eps.Z.ct[c, u, t-1], tau.Z[u])
      }
      for (t in 2:t_star){
        eps.P.ct[c, u, t-1] ~ dnorm(rho.P[u]*eps.P.ct[c, u, t], tau.P[u])
        eps.R.ct[c, u, t-1] ~ dnorm(rho.R[u]*eps.R.ct[c, u, t], tau.R[u])
        eps.Z.ct[c, u, t-1] ~ dnorm(rho.Z[u]*eps.Z.ct[c, u, t], tau.Z[u])
      }
    
        tau.P.st[u] <- tau.P[u]*(1-pow(rho.P[u],2))
        tau.P[u] <- pow(sigma.P[u], -2)
        tau.R.st[u] <- tau.R[u]*(1-pow(rho.R[u],2))
        tau.R[u] <- pow(sigma.R[u], -2)
        tau.Z.st[u] <- tau.Z[u]*(1-pow(rho.Z[u],2))
        tau.Z[u] <- pow(sigma.Z[u], -2)
  
    } # end union loop SM 05052021
  }
  
  # logistic curves and model for Z - we expect this to start slowly, then gets popular quickly, then plateau
  for (c in 1:C){
    for (u in 1:U){ # union loop SM 05052021
      for (t in 1:nyears){
        Rmu.ct[c, u, t] <- Romega.c[c, u]*(t - RT.c[c, u]) # Romega is how fast the growth is at growth part - SM 06042021
        Rstar.ct[c, u, t] <- Rmax.c[c, u]/(1+exp(-Rmu.ct[c, u, t])) # this is the logistic growth curve, Rmax is at what percent do we level off - SM 06042021
        R.ct[c, u, t] <- 1/(1+exp(-( logit(Rstar.ct[c, u, t]) + eps.R.ct[c, u, t]))) # growth at what time point - SM 06042021
        logitZstar.ct[c, u, t] <- (unmet.intercept.c[c, u]
                               + a.unmet[u]
                               + b.unmet[u] * (P.ct[c, u, t] - pmid.for.unmet[u])
                               + c.unmet[u] * pow(P.ct[c, u, t] - pmid.for.unmet[u],2)) # this is the quadratic part - SM 06042021
        Z.ct[c, u, t] <- 1/(1+exp(-(logitZstar.ct[c, u, t] + eps.Z.ct[c, u, t])))
        #neg.explogitZ.ct[c, u, t] = exp(-logitZ.ct[c, u, t])
      }
      for(t in 1:(t_star-1)){
        ls.ct[c, u, (t_star-t)] <- s.ct[c, u, (t_star-t)+1] - eps.P.ct[c, u, t_star-t] #logit
        ils.ct[c, u, (t_star-t)] <- 1/(1+exp(-ls.ct[c, u, (t_star-t)])) #inv.logit
        #Step function; test for x >/= 0
        I[c, u, (t_star-t)] <- step(ils.ct[c, u, (t_star-t)] - pmax.c[c, u])
        ###Get P.ct directly in the backward direction
        #Only need this bit if I=0 i.e., ils.ct<pmax.c
        zeta.ct[c, u, (t_star-t)] <- (1-I[c, u, (t_star-t)])*(logit(min((1-0.00001),ils.ct[c, u, (t_star-t)]/pmax.c[c, u]))-omega.c[c, u])
        P.ct[c, u, (t_star-t)]<-(1-I[c, u, (t_star-t)])*(pmax.c[c, u]*(1/(1+exp(-zeta.ct[c, u, (t_star-t)])))) + I[c, u, (t_star-t)]*ils.ct[c, u, (t_star-t)]
        ###Get logit(P.ct)
        s.ct[c, u, (t_star-t)] <- logit(P.ct[c, u, (t_star-t)])
      } # end back extrapolation
      for(t in (t_star+1):nyears){
        #Step function; test for x >/= 0
        I[c, u, t] <- step(P.ct[c, u, t-1] - pmax.c[c, u])
        #Only need this bit if I=0 i.e., P.ct<pmax.c
        zeta.ct[c, u, t] <- (1-I[c, u, t])*(logit(min((1-0.000001),P.ct[c, u, t-1]/pmax.c[c, u])) + omega.c[c, u])
        s.ct[c, u, t] <- logit(I[c, u, t]*(P.ct[c, u, t-1]) + (1-I[c, u, t])*pmax.c[c, u]*(1/(1+exp(-zeta.ct[c, u, t])))) + eps.P.ct[c, u, t-1]
        P.ct[c, u, t] <- 1/(1 + exp(-s.ct[c, u, t]))
      }
    
      ### add pmax_lower_bound here
    pmax.c[c, u] <- pmax_lower_bound[u] + (1-pmax_lower_bound[u])/(1+exp(-logitpmax.c[c, u])) # need to add union to these as well
    logitpmax.c[c, u] ~ dnorm(lp.world[u], 1/(pow(sigma.lpc[u], 2) + pow(sd_lp.world[u], 2)))
    # lower bound for rmax is 0.5 for married AND unmarried
    Rmax.c[c, u] <- 0.5 + (1-0.5)/(1+exp(-logitRmax.c[c, u]))
    logitRmax.c[c, u] ~ dnorm(lr.world[u], 1/(pow(sigma.lrc[u], 2) + pow(sd_lr.world[u], 2)))
    logitomega.c[c, u] ~ dnorm(w.subreg[u], 1/(pow(sigma.wc[u], 2) + pow(sd_w.subreg[u], 2)))
    omega.c[c, u] <- 0.01 + (0.5-0.01)/(1+exp(-logitomega.c[c, u]))
    Romega.c[c, u] <- 0.01 + (0.5-0.01)/(1+exp(-logitRomega.c[c, u]))
    logitRomega.c[c, u] ~ dnorm(Rw.subreg[u], 1/(pow(sigma.Rwc[u], 2) + pow(sd_Rw.subreg[u], 2)))
    
    s.ct[c, u, t_star] <- setlevel.c[c, u]
    setlevel.c[c, u] ~ dnorm(mean_setlevel[u], 1/(var_setlevel[u] + pow(sd_mean_setlevel[u], 2)))
    P.ct[c, u, t_star] <- 1/(1+exp(-s.ct[c, u, t_star]))
    RT.c[c, u] ~ dnorm(RT.subreg[u], 1/(pow(sigma.RTc[u], 2) + pow(sd_RT.subreg[u], 2)))
    unmet.intercept.c[c, u] ~ dnorm(unmet.subreg[u], 1/(pow(sigma.unmetc[u], 2) + pow(sd_unmet.subreg[u], 2)))
    } # end union loop SM 24052021
  } # end country loop

  # to export
  for (c in 1:C){
    for (u in 1:U){
      for (t in 1:nyears){
        mod.ct[c, u, t] <- P.ct[c, u, t]*R.ct[c, u, t] # working back to get the proportions of interest from ratios - SM 06042021
        trad.ct[c, u, t] <- P.ct[c, u, t]*(1-R.ct[c, u, t])
        unmet.ct[c, u, t] <- (1-P.ct[c, u, t])*Z.ct[c, u, t]
        logit_mod.ct[c, u, t] <- log(mod.ct[c, u, t]/(1-mod.ct[c, u, t])) # putting on logit scale - SM 06042021
        logit_trad.ct[c, u, t] <- log(trad.ct[c, u, t]/(1-trad.ct[c, u, t]))
        logit_unmet.ct[c, u, t] <- log(unmet.ct[c, u, t]/(1-unmet.ct[c, u, t]))
      }
    } # end union loop SM 24052021
  }
  
  ",sep="",append=FALSE, file = "model.txt", fill = TRUE)
  
  if(is_in_union == "ALL"){
    cat("
    for (t in 1:nyears){ # SM 
      mod.ct[1, 3, t] <- (mod.ct[1,1,t]*in_union_population_counts[t] + 
                          mod.ct[1,2,t]*not_in_union_population_counts[t])/
                          (in_union_population_counts[t] + not_in_union_population_counts[t])
      
      trad.ct[1, 3, t] <- (trad.ct[1,1,t]*in_union_population_counts[t] +
                          trad.ct[1,2,t]*not_in_union_population_counts[t])/
                           (in_union_population_counts[t] + not_in_union_population_counts[t])
                           
      unmet.ct[1, 3, t] <- (unmet.ct[1,1,t]*in_union_population_counts[t] +
                            unmet.ct[1,2,t]*not_in_union_population_counts[t])/
                           (in_union_population_counts[t] + not_in_union_population_counts[t])
      
      logit_mod.ct[1, 3, t] <- (logit_mod.ct[1,1,t]*in_union_population_counts[t] +
                                logit_mod.ct[1,2,t]*not_in_union_population_counts[t])/
                                (in_union_population_counts[t] + not_in_union_population_counts[t])
                                
      logit_trad.ct[1, 3, t] <- (logit_trad.ct[1,1,t]*in_union_population_counts[t] +
                                logit_trad.ct[1,2,t]*not_in_union_population_counts[t])/
                                (in_union_population_counts[t] + not_in_union_population_counts[t])
                                
      logit_unmet.ct[1, 3, t] <- (logit_unmet.ct[1,1,t]*in_union_population_counts[t] +
                                logit_unmet.ct[1,2,t]*not_in_union_population_counts[t])/
                                (in_union_population_counts[t] + not_in_union_population_counts[t])
    }
  
  ",sep="",append=TRUE, file = "model.txt", fill = TRUE)
  }
  # DMS
  # update name of observed props to avoid confusion
  if (!old_dm & !nulldata){ # simple
    ## note: does not work yet for partial/all missing
    cat("
    for (i in 1:n_mod){
      modern.i[i] = mod.ct[get_c_i[get_mod_i[i]], get_u_i[get_mod_i[i]], get_t_i[get_mod_i[i]]] #added get_u_i into these 3 loops SM 24052021
      trad.i[i] = trad.ct[get_c_i[get_mod_i[i]], get_u_i[get_mod_i[i]], get_t_i[get_mod_i[i]]]
      modern[get_mod_i[i]] ~ dnorm(modern.i[i], prec)
      trad[get_mod_i[i]] ~ dnorm(trad.i[i], prec)
    }
   for (i in 1:n_unmet){
        unmet[get_unmet_i[i]] ~ dnorm(unmet.ct[get_c_i[get_unmet_i[i]], get_u_i[get_unmet_i[i]], get_t_i[get_unmet_i[i]]], prec)
   }
   for (k in 1:n_ptot){
    ptot.k[k] = mod.ct[get_c_i[get_ptot_i[k]], get_u_i[get_ptot_i[k]], get_t_i[get_ptot_i[k]]] + trad.ct[get_c_i[get_ptot_i[k]], get_u_i[get_ptot_i[k]], get_t_i[get_ptot_i[k]]]
    logit.ptot[k] ~ dnorm(logit(ptot.k[k]), prec)
  }
   
  ",sep="",append=TRUE, file = "model.txt", fill = TRUE)
    
  } else if (!nulldata) {
    # need to check if any updates in bounds min/max used here in Mark's most recent version
    # to decide if still to add periods?
    #  for (i in 1:n_mod){
    #  for (h in 1:getperiod.i[i]) {
    #    trad.ih[i, h] = p.ci[getc.i[i], getest.if[i, h]] * (1 - R.ci[getc.i[i], getis.if[i, h]])
    #      modern.ih[i, h] = p.ci[getc.i[i], getest.if[i, h]] * R.ci[getc.i[i], getis.if[i, h]]
    #      unmet.ih[i, h] = (1 - p.ci[getc.i[i], getest.if[i, h]])*(1 / (1 + neg.explogitZ.ci[getc.i[i], getis.if[i, h]]))
    #}
    #trad.i[i] = 1 / period.i[i] * inprod(trad.ih[i, 1:getperiod.i[i]], partialtime.xi[1:getperiod.i[i], i])
    #modern.i[i] = 1 / period.i[i] * inprod(modern.ih[i, 1:getperiod.i[i]], partialtime.xi[1:getperiod.i[i], i])
    #unmet.i[i] = 1 / period.i[i] * inprod(unmet.ih[i, 1:getperiod.i[i]], partialtime.xi[1:getperiod.i[i], i])
    # 
    cat("
####
# dms
  for (i in 1:n_mod){
# get_mod_i refers to indices with modern+trad use
   ratios.trad.modern.in[get_mod_i[i],1:2] ~ dmnorm(mu.in[get_mod_i[i], ],InvSigma[i,,]) #T.i[i,,])
   InvSigma[i,1:2,1:2] <- inverse(Sigma[i,1:2,1:2])
   Sigma[i,1,2] <- cor.trad.modern.i[get_mod_i[i], get_u_i[i]]*sqrt(Sigma[i,1,1]*Sigma[i,2,2])
   Sigma[i,2,1] <- cor.trad.modern.i[get_mod_i[i], get_u_i[i]]*sqrt(Sigma[i,1,1]*Sigma[i,2,2])
   Sigma[i,1,1] <- pow(se_log_r_traditional_no_use[get_mod_i[i]], 2) + pow(nonsample.se.trad.i[get_mod_i[i], get_u_i[i]],2) # SM 07062021 not sure if I should have get_u_i in the stuff from list_auxiliary_data
   Sigma[i,2,2] <- pow(se_log_r_modern_no_use[get_mod_i[i]], 2) + pow(nonsample.se.modern.i[get_mod_i[i], get_u_i[i]],2)
  }
for (i in 1:n_unmet){
# get_unmet_i refers to indices with unmet
 logitratio.yunmet.i[get_unmet_i[i]] ~ dnorm(
   logitratio.yunmet.hat.i[get_unmet_i[i]], 1/(pow(nonsample.se.unmet.i[get_unmet_i[i], get_u_i[i]],2)+ pow(se_log_r_unmet_no_need[get_unmet_i[i]],2)) )
}

for (k in 1:n_ptot){
    logit.ptot[get_ptot_i[k]] ~ dnorm(logit.ptothat.i[get_ptot_i[k]], tau.sourcetot[get_u_i[k]]) # SM 13082021 added get_u_i
}

for(u in 1:U){ # SM 13072021 added union loop
tau.sourcetot[u] <- pow(sigma.sourcetot[u],-2)
}
# end dms for unmet and trad+modern
# this part refer to union of indices in unmet and modern

  for (i in 1:N){
 modern.i[i] <- mod.ct[get_c_i[i], get_u_i[i], get_t_i[i]] # added get_u_i into modern, trad, unmet SM 10052021
 trad.i[i] <- trad.ct[get_c_i[i], get_u_i[i], get_t_i[i]]
 unmet.i[i] <- unmet.ct[get_c_i[i], get_u_i[i], get_t_i[i]]
 mu.in[i,1] <- log(max(0.0000001, q.ii[1,i])/none.adj.i[i])
 mu.in[i,2] <- log(max(0.0000001, q.ii[2,i])/none.adj.i[i])
 logitratio.yunmet.hat.i[i] <- logit(max(0.0000001,q.ii[3,i])/none.adj.i[i])


 logit.ptothat.i[i] <- logit(max(0.0000001, 1-none.adj.i[i]))

 
    sump.i[i] <- (trad.i[i]*Vtrad.i[i] + modern.i[i]* Vmodern.i[i]
                        + (1- trad.i[i] - modern.i[i]))
      # old order, 1 is trad!!!
    p.perturb.ii[1,i] <-  trad.i[i]*Vtrad.i[i]/sump.i[i]
      p.perturb.ii[2,i] <-  modern.i[i]* Vmodern.i[i]/sump.i[i]
      p.perturb.ii[3,i] <- unmet.i[i]/sump.i[i]
      p.perturb.ii[4,i] <- (1- trad.i[i] - modern.i[i] - unmet.i[i])/sump.i[i]
      ###Biases
      ##Inclusion of folk methods
      folkbias.i[i] <- step(folk.ind[i]-0.5)*v.folk[get_u_i[i]]* p.perturb.ii[3,i]
      ##Absence of probing
      micsbias.i[i] <- step(source.MICS.ind[i]-0.5)* v.mics[get_u_i[i]] * p.perturb.ii[1,i]
      ##Sterilization
      modposbias.i[i] <- step(mpos.ind[i]-0.5)*v.mpos[get_u_i[i]]* p.perturb.ii[4,i]
      modnegbias.i[i] <- step(mneg.ind[i]-0.5)*v.mneg[get_u_i[i]] * p.perturb.ii[2,i]
      ####Perturbed proportions adjusted for biases (1-4)
      q.ii[1,i] <- p.perturb.ii[1,i] - micsbias.i[i] + folkbias.i[i]
      q.ii[2,i] <- p.perturb.ii[2,i] + modposbias.i[i] - modnegbias.i[i]
      q.ii[3,i] <- p.perturb.ii[3,i] + micsbias.i[i] - folkbias.i[i]
      q.ii[4,i] <- p.perturb.ii[4,i] - modposbias.i[i] + modnegbias.i[i]
      none.adj.i[i] <- max(0.0000001, q.ii[3,i] + q.ii[4,i]) #la 2019/3/13
      Vtrad.i[i] <-  (
      V.geo.12i[1,geo.ind[i],get_u_i[i]]
      * V.age.12i[1,age.ind[i],get_u_i[i]]
      * V.hw.12i[1,hw.ind[i],get_u_i[i]]
      * V.emal.12i[1,emal.ind[i],get_u_i[i]]
      * V.sa.12i[1,sa.ind[i],get_u_i[i]]
      * V.posbias.12i[1,posbias.ind[i],get_u_i[i]]
      * V.posage.12i[1, posage.ind[i],get_u_i[i]]
      * V.negage.12i[1, negage.ind[i],get_u_i[i]]
      )
      Vmodern.i[i] <- (
      V.geo.12i[2,geo.ind[i],get_u_i[i]]  ##geographical region
      * V.age.12i[2,age.ind[i],get_u_i[i]] #Age group different from base (bias unknown)
      * V.hw.12i[2,hw.ind[i],get_u_i[i]] ##Husband and wives or both
      * V.emal.12i[2,emal.ind[i],get_u_i[i]] ##Ever married, all women
      * V.sa.12i[2,sa.ind[i],get_u_i[i]] ##All sexually active
      * V.posbias.12i[2,posbias.ind[i],get_u_i[i]] ## Non-pregnant/fertile/married SA women
      * V.posage.12i[2, posage.ind[i],get_u_i[i]] ##Age group with positive bias
      * V.negage.12i[2, negage.ind[i],get_u_i[i]] ##Age group with negative bias
      )
  }
  
for(u in 1:U){
# add dummy column in case ncol(V) = 1 => V becomes vector!
V.geo.12i[1,max(geo.ind)+1,u] <- 0
      V.age.12i[1,max(age.ind)+1,u] <- 0
      V.hw.12i[1,max(hw.ind)+1,u] <- 0
      V.emal.12i[1,max(emal.ind)+1,u] <- 0
      V.sa.12i[1,max(sa.ind)+1,u] <- 0
      V.posbias.12i[1,max(posbias.ind)+1,u] <- 0
      V.posage.12i[1,max(posage.ind)+1,u] <- 0
      V.negage.12i[1,max(negage.ind)+1,u] <- 0
      V.geo.12i[2,max(geo.ind)+1,u] <- 0
      V.age.12i[2,max(age.ind)+1,u] <- 0
      V.hw.12i[2,max(hw.ind)+1,u] <- 0
      V.emal.12i[2,max(emal.ind)+1,u] <- 0
      V.sa.12i[2,max(sa.ind)+1,u] <- 0
      V.posbias.12i[2,max(posbias.ind)+1,u] <- 0
      V.posage.12i[2,max(posage.ind)+1,u] <- 0
      V.negage.12i[2,max(negage.ind)+1,u] <- 0
# Multipliers V in [0,inf): geo, emal, hw, age other, sa (for trad only)
####All of these should get a log normal distribution....
        V.sa.12i[1,1,u] <- 1  ##Sexually active women (trad)
        V.geo.12i[1,1,u] <- 1 ##Geographical region (trad)
        V.geo.12i[2,1,u] <- 1 ##Geographical region (mod)
        V.age.12i[1,1,u] <- 1 ##Age different (trad)
        V.age.12i[2,1,u] <- 1 ##Age different (mod)
        V.hw.12i[1,1,u] <- 1  ##Husband/wives (trad)
        V.hw.12i[2,1,u] <- 1  ##Husband/wives (mod)
        V.emal.12i[1,1,u] <- 1 ##ever married/ all women (trad)
        V.emal.12i[2,1,u] <- 1 ##evr married/ all women (mod)
}
for (u in 1:U){ # SM 13072021 added union loop
for (m in 1:2){
  
        for (i in 2:ncat.geo[u]){
        V.geo.12i[m,i,u] ~ dlnorm(0, tau.geo.m[m,u]) # adding a lot of u's here - replacing get_u_i
        }
        for (i in 2:ncat.age[u]){
        V.age.12i[m,i,u] ~ dlnorm(0, tau.geo.m[m,u])
        }
        
        tau.geo.m[m,u] <- pow(sigma.geo.m[m,u], -2) # CHECK NOT SURE
        }
        
        for (i in 2:ncat.sa[u]){
        V.sa.12i[1,i,u] ~ dlnorm(0, tau.geo.m[1,u])
        }

        # m = 2:
        for (i in 2:ncat.sa[u]){
        W.sa.12i[2,i,u] ~ dlnorm(mu.pos.m[2,u], tau.pos[u])
        V.sa.12i[2,i,u] <- 1+W.sa.12i[2,i,u]
        }
        for (i in 2:ncat.posbias[u]){
        W.posbias.12i[2,i,u] ~ dlnorm(mu.pos.m[2,u], tau.pos[u])
        V.posbias.12i[2,i,u] <- 1+W.posbias.12i[2,i,u]
        }
        for (i in 2:ncat.posage[u]){
        W.posage.12i[2,i,u] ~ dlnorm(mu.pos.m[2,u], tau.pos[u])
        V.posage.12i[2,i,u] <-1+W.posage.12i[2,i,u]
        }
        for (i in 2:ncat.negage[u]){
        W.negage.12i[2,i,u] ~ dlnorm(mu.pos.m[2,u], tau.pos[u])
        V.negage.12i[2,i,u] <- 1/(1+W.negage.12i[2,i,u])
        }
  
        tau.pos[u] <- pow(sigma.pos[u], -2)
        
        # m=1
        # note: could simplify code and throw out these V's
        for (i in 2:ncat.posbias[u]){
        V.posbias.12i[1,i,u] <- 1+exp(mu.pos.m[1,u])
        }
        for (i in 2:ncat.posage[u]){
        V.posage.12i[1,i,u] <- 1+exp(mu.pos.m[1,u])
        }
        for (i in 2:ncat.negage[u]){
        V.negage.12i[1,i,u] <- 1/(1+exp(mu.pos.m[1,u]))
        }
        
        V.sa.12i[2,1,u] <- 1
        V.posbias.12i[1,1,u] <- 1
        V.posbias.12i[2,1,u] <- 1
        V.posage.12i[1,1,u] <- 1
        V.posage.12i[2,1,u] <- 1
        V.negage.12i[1,1,u] <- 1
        V.negage.12i[2,1,u] <- 1
}

        

  ",sep="",append=TRUE, file = "model.txt", fill = TRUE)
  }



if (is_in_union == "Y" & !nulldata ) {
  cat("for(u in 1:U){
        for(k in 1:n.training.modonly) {
            logit.ymodonly.i[geti.training.modonly.k[k]] ~ dnorm(logit(q.ii[2,geti.training.modonly.k[k]]), tau.sourcemodonly[u])
        }
  }
        ",sep="",append=TRUE, file = "model.txt", fill = TRUE)
}
if (is_in_union == "Y") {
  cat("
        for(u in 1:U){
        for (m in 1:2){
        for (i in 2:ncat.emal[u]){
        V.emal.12i[m,i,u] ~ dlnorm(0, tau.geo.m[m,u])
        }
        for (i in 2:ncat.hw[u]){
        V.hw.12i[m,i,u] ~ dlnorm(0, tau.geo.m[m,u])
        }
        }
        }
  
  ",sep="",append=TRUE, file = "model.txt", fill = TRUE)
} else {
  cat("
  ## m = 1
  for(u in 1:U){
  for (i in 2:ncat.emal[u]){
    V.emal.12i[1,i,u] <- 1/(1+exp(mu.pos.m[1,u]))
  }
  
  ## m = 2
  for (i in 2:ncat.emal[u]){
    W.emal.12i[2,i,u] ~ dlnorm(mu.pos.m[2, u], tau.pos[u])
    V.emal.12i[2,i,u] <- 1/(1+W.emal.12i[2,i,u])
  }
  
  ## m = 1
  for (i in 2:ncat.hw[u]){
    V.hw.12i[1,i,u] <- 1+exp(mu.pos.m[1,u])
  }
  
  ## m = 2
  for (i in 2:ncat.hw[u]){
    W.hw.12i[2,i,u] ~ dlnorm(mu.pos.m[2,u], tau.pos[u])
    V.hw.12i[2,i,u] <- 1+W.hw.12i[2,i,u]
  }
  }
  ",sep="",append=TRUE, file = "model.txt", fill = TRUE)
  
}


if (include_ss_data) {
  cat("
      for (k in 1:K) {
      # delta emu           delta p's (no added bias term yet)
        ss_delta_k[k] ~ dnorm(ss_delta_modern_k[k], ss_tau_k[k])
        ss_delta_modern_k[k] <- mod.ct[1, 3, get_t_k[k+1]] - mod.ct[1, 3, get_t_k[k]] 
        ss_tau_k[k] <- pow(ss_se_k[k], -2) # standard dev for each group
}
",sep="",append=TRUE, file = "model.txt", fill = TRUE)
}
cat("} # end model",sep="",append=TRUE, file = "model.txt", fill = TRUE)
} # end write model function