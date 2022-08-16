require(rethinking)
require(dagitty)
require(xtable)
require(stringr)

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

##clean up NAs and charachters for new map2stan changes
dl_nochar <- within(dl, rm("village", "species","household_size" )) # so stan is not upset by charachters
dl_nona <- within(dl, rm("village", "species" , "household_size" , "household_size_std")) # so stan is not upset by charachters and NA in HHsize
nrow(dl)==nrow(dl_nochar) #should be true
nrow(dl)==nrow(dl_nona) #shold be true

#c2070
adjustmentSets( ls_conf_yes_guard , exposure="c2070" , outcome="conflict" , type="minimal") #independent of others should be invariant

ml_c2070 <- map2stan(
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
    
  ), data=dl_nona , chains=4 , cores=4 , iter=3000 , log_lik=TRUE ,  control=list(adapt_delta=0.99) , rng_seed=43 )

precis(ml_c2070, depth=2)

#c70
adjustmentSets( ls_conf_yes_guard , exposure="c70" , outcome="conflict" , type="minimal") #independent of others should be invariant

ml_c70 <- map2stan(
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
    
  ), data=dl_nona , chains=4 , cores=4 , iter=3000 , log_lik=TRUE ,  control=list(adapt_delta=0.99) , rng_seed=423)

precis(ml_c70, depth=2)

#bd
adjustmentSets( ls_conf_yes_guard , exposure="bd" , outcome="conflict" , type="minimal") #independent of others should be invariant

ml_bd <- map2stan(
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
    
  ), data=dl_nona , chains=4 , cores=4 , iter=3000 , log_lik=TRUE ,  control=list(adapt_delta=0.99) , rng_seed=112)

precis(ml_bd, depth=2)

#lsh
adjustmentSets( ls_conf_yes_guard , exposure="lsh" , outcome="conflict" , type="minimal") #independent of others should be invariant

ml_lsh <- map2stan(
  alist(
    conflict  ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index]  + 
      (b_LSH + b_LSHs[species_index])*log_livestock_head_std ,
    c(a,b_LSH) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_LSHs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data=dl_nona , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.99) , rng_seed=930)

precis(ml_lsh, depth=2)

#river
adjustmentSets( ls_conf_yes_guard , exposure="river" , outcome="conflict" , type="minimal") #independent of others should be invariant

ml_riv <- map2stan(
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
    
  ), data=dl_nona , chains=4 , cores=4 , iter=3000 , log_lik=TRUE ,  control=list(adapt_delta=0.99) , rng_seed=2917)

precis(ml_riv , depth=2)

#sd
adjustmentSets( ls_conf_yes_guard , exposure="sd" , outcome="conflict" , type="minimal") #independent of others should be invariant

ml_sd <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] +  
      (b_SD + b_SDs[species_index])*settle_dist_km_std ,
    c(a,b_SD) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_SDs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dl_nona , chains=4 , cores=4 , iter=3000 , log_lik=TRUE ,  control=list(adapt_delta=0.99) , rng_seed=384)

#guards
adjustmentSets( ls_conf_yes_guard , exposure="guards" , outcome="conflict" , type="minimal") #independent of others should be invariant
  ##cant do guards
#slope
adjustmentSets( ls_conf_yes_guard , exposure="slope" , outcome="conflict" , type="minimal") #independent of others should be invariant

ml_sl <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] + 
      (b_SL + b_SLs[species_index])*gse_slope30m_std ,
    c(a,b_SL) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_SLs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dl_nona , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.99) , rng_seed=144)

precis(ml_sl, depth=2)

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
    
  ), data=dl_nona , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.99) , rng_seed=9 )

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
ml_guard <- map2stan(
  alist(
    conflict  ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index]  + 
      (b_LSH + b_LSHs[species_index])*log_livestock_head_std + 
      (b_HH + b_HHs[species_index])*household_size_std + 
      (b_GU + b_GUs[species_index])*guard_ave_day_std, 
    household_size_std ~ normal( mu_x , sigma_x ),
    c(a,b_LSH,b_HH,b_GU,mu_x) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_LSHs,b_HHs,b_GUs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s,sigma_x) ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data=dl_nochar , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.99) , rng_seed=434)

precis(ml_guard , depth=2)

#######house level w/ interaction
ml_lshXguard  <- map2stan(
  alist(
    conflict  ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index]  + 
      (b_LSH + b_LSHs[species_index])*log_livestock_head_std + 
      (b_HH + b_HHs[species_index])*household_size_std + 
      (b_GU + b_GUs[species_index] + (b_GUxLSH + b_GUxLSHs[species_index])*log_livestock_head_std)*guard_ave_day_std ,
    household_size_std ~ normal( mu_x , sigma_x ),
    
    c(a,b_LSH,b_HH,b_GU,b_GUxLSH,mu_x) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_LSHs,b_HHs,b_GUs,b_GUxLSHs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s,sigma_x) ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data=dl_nochar , chains=4 , cores=4 , iter=3000 , log_lik=TRUE ,  control=list(adapt_delta=0.99) , rng_seed=83)

precis(ml_lshXguard , depth=2)



###########tables for paper

###info criteria
#WAIC of livestock table
livestock_waic_tab <- compare(ml_bd , ml_c2070 , ml_c70 , ml_riv , 
                              ml_sd , ml_sl , ml_landscape , 
                              ml_guard , ml_lsh , ml_lshXguard)
livestock_waic_tab
print(xtable(livestock_waic_tab[,1:3], type = "latex"), file = "livestock_waic_tab.tex") #print to tex

livestock_waic_tab_lscape <- compare(ml_bd , ml_c2070 , ml_c70 , ml_riv , 
        ml_sd , ml_sl , ml_landscape) #this for map of preds
        
###coef tabs
source(file="02_c_coeff_table_functions.R")

livestock_coeftab <- coeftab(ml_bd , ml_c2070 , ml_c70 , ml_riv , ml_sd , 
         ml_sl , ml_landscape , ml_guard , ml_lsh
         , ml_lshXguard , digits=2)@coefs

param_names <-livestock_coeftab[,0]

dframe <- as.data.frame(livestock_coeftab)
rownames(dframe)
str(dframe)
cc <- as.vector(rownames(dframe))
cc <- gsub("[1]", "_hyena", cc, fixed=TRUE)
cc <- gsub("[2]", "_lion", cc, fixed=TRUE)
rownames(dframe) <- cc

print(xtable(dframe, type = "latex"), file = "livestock_coefs_tab.tex") #print to tex
