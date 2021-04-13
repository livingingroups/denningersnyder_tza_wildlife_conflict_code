require(rethinking)
require(dagitty)

###add DAG

ls_conf_yes_guard <- 
  dagitty('dag {
  c2070 -> conflict
  bd -> conflict 
  bd <-> road 
  c70 -> conflict 
  hh_size -> guards 
  hh_size -> lsh 
  lsh -> conflict 
  river -> c70 
  river -> conflict 
  sd -> bd 
  sd -> conflict
  bd -> c70
  bd -> c2070
  guards <-> conflict 
  slope -> bd 
  slope -> conflict
  slope -> c2070 
  slope -> river 
  slope -> road 
}')

plot(ls_conf_yes_guard)

#c2070
adjustmentSets( ls_conf_yes_guard , exposure="c2070" , outcome="conflict" , type="minimal") #independent of others should be invariant

ml_c2070_min <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] +  
      (b_C2070 + b_C2070s[species_index])*c2070_std +
      (b_BD + b_BDs[species_index])*build_dens_std +
      (b_SL + b_SLs[species_index])*gse_slope30m_std  ,
    c(a,b_C2070,b_BD,b_SL) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_C2070s,b_BDs,b_SLs)[species_index] ~ dmvnormNC(sigma_s,Rho) ,
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE ,  control=list(adapt_delta=0.99) )

precis(ml_c2070_min, depth=2)

#c70
adjustmentSets( ls_conf_yes_guard , exposure="c70" , outcome="conflict" , type="minimal") #independent of others should be invariant

ml_c70_min <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] +  
      (b_C70 + b_C70s[species_index])*c70_std +
      (b_BD + b_BDs[species_index])*build_dens_std +
      (b_RIV + b_RIVs[species_index])*river_std  ,
    c(a,b_C70,b_BD,b_RIV) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_C70s,b_BDs,b_RIVs)[species_index] ~ dmvnormNC(sigma_s,Rho) ,
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE ,  control=list(adapt_delta=0.99))

precis(ml_c70_min, depth=2)

#bd
adjustmentSets( ls_conf_yes_guard , exposure="bd" , outcome="conflict" , type="minimal") #independent of others should be invariant

ml_bd_min <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] +  
      (b_SD + b_SDs[species_index])*settle_dist_km_std +
      (b_BD + b_BDs[species_index])*build_dens_std +
      (b_SL + b_SLs[species_index])*gse_slope30m_std  ,
    c(a,b_SD,b_BD,b_SL) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_SDs,b_BDs,b_SLs)[species_index] ~ dmvnormNC(sigma_s,Rho) ,
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE ,  control=list(adapt_delta=0.99) )

precis(ml_bd_min, depth=2)

#lsh
adjustmentSets( ls_conf_yes_guard , exposure="lsh" , outcome="conflict" , type="minimal") #independent of others should be invariant

ml_lsh_min <- map2stan(
  alist(
    conflict  ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index]  + 
      (b_LSH + b_LSHs[species_index])*log_livestock_head_std ,
    c(a,b_LSH) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_LSHs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.99))

precis(ml_lsh_min, depth=2)

#river
adjustmentSets( ls_conf_yes_guard , exposure="river" , outcome="conflict" , type="minimal") #independent of others should be invariant

ml_riv_min <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] +  
      (b_SL + b_SLs[species_index])*gse_slope30m_std +
      (b_RIV + b_RIVs[species_index])*river_std  ,
    c(a,b_SL,b_RIV) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_SLs,b_RIVs)[species_index] ~ dmvnormNC(sigma_s,Rho) ,
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE ,  control=list(adapt_delta=0.99))

precis(ml_riv_min , depth=2)

#sd
adjustmentSets( ls_conf_yes_guard , exposure="sd" , outcome="conflict" , type="minimal") #independent of others should be invariant

ml_sd_min <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] +  
      (b_SD + b_SDs[species_index])*settle_dist_km_std ,
    c(a,b_SD) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_SDs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE ,  control=list(adapt_delta=0.99))

#guards
adjustmentSets( ls_conf_yes_guard , exposure="guards" , outcome="conflict" , type="minimal") #independent of others should be invariant
  ##cant do guards
#slope
adjustmentSets( ls_conf_yes_guard , exposure="slope" , outcome="conflict" , type="minimal") #independent of others should be invariant

ml_sl_min <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] + 
      (b_SL + b_SLs[species_index])*gse_slope30m_std ,
    c(a,b_SL) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_SLs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.99))

precis(ml_sl_min, depth=2)

#####landscape all#####
ml_landscape <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] +  
      (b_SD + b_SDs[species_index])*settle_dist_km_std +
      (b_C70 + b_C70s[species_index])*c70_std +
      (b_C2070 + b_C2070s[species_index])*c2070_std +
      (b_RIV + b_RIVs[species_index])*river_std +
      (b_RD + b_RDs[species_index])*road_std +
      (b_BD + b_BDs[species_index])*build_dens_std +
      (b_SL + b_SLs[species_index])*gse_slope30m_std  ,
    c(a,b_SD,b_C70,b_C2070,b_RIV,b_RD,b_BD,b_SL) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_SDs,b_C70s,b_C2070s,b_RIVs,b_RDs,b_BDs,b_SLs)[species_index] ~ dmvnormNC(sigma_s,Rho) ,
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.99) )

precis(ml_landscape , depth=2)

###########counterfactual no double arrow guard DAG

ls_conf_no_guard <- 
  dagitty('dag {
  c2070 -> conflict
  bd -> conflict 
  bd <-> road 
  c70 -> conflict 
  hh_size -> guards 
  hh_size -> lsh 
  lsh -> conflict 
  river -> c70 
  river -> conflict 
  sd -> bd 
  sd -> conflict
  bd -> c70
  bd -> c2070
  guards -> conflict 
  slope -> bd 
  slope -> conflict
  slope -> c2070 
  slope -> river 
  slope -> road 
}')

plot(ls_conf_no_guard)

adjustmentSets( ls_conf_no_guard , exposure="guards" , outcome="conflict" , type="minimal") #independent of others should be invariant

####guards
ml_guard_min <- map2stan(
  alist(
    conflict  ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index]  + 
      (b_LSH + b_LSHs[species_index])*log_livestock_head_std + 
      (b_HH + b_HHs[species_index])*household_size_std + 
      (b_GU + b_GUs[species_index])*guard_ave_day_std, 
    c(a,b_LSH,b_HH,b_GU) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_LSHs,b_HHs,b_GUs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.99))

precis(ml_guard_min , depth=2)

#######house level w/ interaction
ml_lshXguard_min  <- map2stan(
  alist(
    conflict  ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index]  + 
      (b_LSH + b_LSHs[species_index])*log_livestock_head_std + 
      (b_HH + b_HHs[species_index])*household_size_std + 
      (b_GU + b_GUs[species_index] + (b_GUxLSH + b_GUxLSHs[species_index])*log_livestock_head_std)*guard_ave_day_std ,
    c(a,b_LSH,b_HH,b_GU ,b_GUxLSH) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_LSHs,b_HHs,b_GUs,b_GUxLSHs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE ,  control=list(adapt_delta=0.99))

precis(ml_lshXguard_min , depth=2)

###infor criteria
compare(ml_bd_min,ml_c2070_min,ml_c70_min,ml_guard_min,ml_landscape,ml_lsh_min,ml_lshXguard_min,ml_riv_min,ml_sd_min,ml_sl_min)

