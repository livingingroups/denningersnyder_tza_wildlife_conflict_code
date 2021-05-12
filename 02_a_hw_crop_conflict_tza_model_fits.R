require(rethinking)
require(dagitty)
require(xtable)

########add in dag########3
crop_damage_dag <- 
  dagitty('dag {
  c2070 -> crop_damage
  c70 -> crop_damage
  river -> c70
  river -> crop_damage
  months_planted -> crop_damage
  farm_size -> crop_damage
  farm_size -> num_protect
  num_protect -> crop_damage
  crop_damage -> num_protect
  hh_size -> num_protect
  hh_size -> farm_size
  hh_size -> crop_damage
  see_field -> crop_damage
  road <-> bd
  bd -> crop_damage
  bd -> c2070
  bd -> c70
  sd -> bd
  sd -> crop_damage
  cd -> c70
  cd -> c2070
  cd -> crop_damage
  slope -> bd 
  slope -> crop_damage
  slope -> c2070 
  slope -> river 
  slope -> road
  slope -> cd
  }')

########new crop models post dags##########
adjustmentSets( crop_damage_dag , exposure="c2070" , outcome="crop_damage" , type="minimal")

mc_c2070_min <- map2stan(
    alist(
      conflict ~ binomial(1,p),
      logit(p) <- a + av[village_index] + as[species_index] + 
        (b_C2070 + b_C2070s[species_index])*c2070_std +
        (b_BD + b_BDs[species_index])*build_dens_std +
        (b_CR + b_CRs[species_index])*crop_std +
        (b_SL + b_SLs[species_index])*gse_slope30m_std,

      c(a,b_C2070,b_CR,b_BD,b_SL) ~ normal( 0 , 1 ),
      av[village_index] ~ dnorm(0,sigma_v),
      c(as,b_C2070s,b_CRs,b_BDs,b_SLs)[species_index] ~ dmvnormNC(sigma_s,Rho),
      c(sigma_v,sigma_s) ~ dexp(1),
      Rho ~ dlkjcorr(3)
      
    ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.95))

precis(mc_c2070_min , depth=2)

########cover 70#####
adjustmentSets( crop_damage_dag , exposure="c70" , outcome="crop_damage" )

mc_c70_min <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] + 
      (b_C70 + b_C70s[species_index])*c70_std +
      (b_BD + b_BDs[species_index])*build_dens_std +
      (b_CR + b_CRs[species_index])*crop_std +
      (b_RIV + b_RIVs[species_index])*river_std,
    
    c(a,b_C70,b_CR,b_BD,b_RIV) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_C70s,b_CRs,b_BDs,b_RIVs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.95))

precis(mc_c70_min , depth=2)

########crop density#####
mc_cd_min <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] + 
      (b_CR + b_CRs[species_index])*crop_std +
      (b_SL + b_SLs[species_index])*gse_slope30m_std,
    
    c(a,b_CR,b_SL) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_CRs,b_SLs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.95))

precis(mc_cd_min , depth=2)



#########river##
adjustmentSets( crop_damage_dag , exposure="river" , outcome="crop_damage" )

mc_riv_min <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] + 
      (b_SL + b_SLs[species_index])*gse_slope30m_std +
      (b_RIV + b_RIVs[species_index])*river_std,
    
    c(a,b_SL,b_RIV) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_SLs,b_RIVs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.95))

precis(mc_riv_min , depth=2)

######settlement distance#######
adjustmentSets( crop_damage_dag , exposure="sd" , outcome="crop_damage" )

mc_sd_min <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] + 
      (b_SD + b_SDs[species_index])*settle_dist_km_std ,
    a ~ normal( 0 , 1 ),
    b_SD ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v) ,
    c(as,b_SDs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(mc_sd_min , depth=2)

##building density
adjustmentSets( crop_damage_dag , exposure="bd" , outcome="crop_damage" )

mc_bd_min <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] + 
      (b_BD + b_BDs[species_index])*build_dens_std +
      (b_SD + b_SDs[species_index])*settle_dist_km_std +
      (b_SL + b_SLs[species_index])*gse_slope30m_std,
    
    c(a,b_SD,b_BD,b_SL) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_SDs,b_BDs,b_SLs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.95))

precis(mc_bd_min , depth=2)

#####months planted
adjustmentSets( crop_damage_dag , exposure="months_planted" , outcome="crop_damage" )

mc_mp_min <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] + 
      (b_MP + b_MPs[species_index])*months_planted_std,
    c(a,b_MP) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_MPs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(mc_mp_min, depth=2)

###see field
adjustmentSets( crop_damage_dag , exposure="see_field" , outcome="crop_damage" )

mc_see_min <- map2stan(
  alist(
    conflict  ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index]  + 
      (b_SEE + b_SEEs[species_index])*see_field ,
    a ~ normal( 0 , 1 ),
    b_SEE ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_SEEs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(mc_see_min, depth=2)

###household size imputed
adjustmentSets( crop_damage_dag , exposure="hh_size" , outcome="crop_damage" )

mc_hh_min <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] 
    + (b_HH + b_HHs[species_index])*household_size_std,
    household_size_std ~ dnorm( mu_x, sigma_x ),
    c(a,b_HH) ~ dnorm( 0 , 1 ),
    mu_x ~ dnorm( 0 , 3 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_HHs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s,sigma_x) ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(mc_hh_min, depth=2)

###farm size
adjustmentSets( crop_damage_dag , exposure="farm_size" , outcome="crop_damage" )

mc_fs_min <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] 
    + (b_FS + b_FSs[species_index])*farm_size_std,
    a ~ normal( 0 , 1 ),
    b_FS ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_FSs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(mc_fs_min, depth=2)

####slope
adjustmentSets( crop_damage_dag , exposure="slope" , outcome="crop_damage" )
mc_slope_min <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] + 
      (b_SL + b_SLs[species_index])*gse_slope30m_std,

    c(a,b_SL) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_SLs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.95))


precis(mc_slope_min , depth=2)

####all landscape level variables
mc_landscape <- map2stan(
    alist(
      conflict ~ binomial(1,p),
      logit(p) <- a + av[village_index] + as[species_index] 
      + (b_SD + b_SDs[species_index])*settle_dist_km_std 
      + (b_C70 + b_C70s[species_index])*c70_std 
      + (b_C2070 + b_C2070s[species_index])*c2070_std 
      + (b_RIV + b_RIVs[species_index])*river_std  
      + (b_RD + b_RDs[species_index])*road_std 
      + (b_BD + b_BDs[species_index])*build_dens_std 
      + (b_SL + b_SLs[species_index])*gse_slope30m_std 
      + (b_CR + b_CRs[species_index])*crop_std,
      c(a,b_SD,b_C70,b_C2070,b_RIV,b_RD,b_BD,b_CR,b_SL) ~ normal( 0 , 1 ),
      av[village_index] ~ dnorm(0,sigma_v),
      c(as,b_SDs,b_C70s,b_C2070s,b_RIVs,b_RDs,b_BDs,b_CRs,b_SLs)[species_index] ~ dmvnormNC(sigma_s,Rho),
      c(sigma_v,sigma_s) ~ dexp(1),
      Rho ~ dlkjcorr(3)
      
    ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE ,control=list(adapt_delta=0.99))
  

precis(mc_landscape , depth=2)

###########tables for paper

#WAIC of crop table
crop_waic_tab <- compare(mc_bd_min,mc_c2070_min,mc_c70_min,mc_cd_min,mc_fs_min,mc_hh_min,mc_mp_min,mc_riv_min,mc_sd_min,mc_see_min,mc_slope_min,mc_landscape)
crop_waic_tab
print(xtable(crop_waic_tab[,1:3], type = "latex"), file = "crop_waic_tab.tex") # save to tex


###coef tabs
source(file="02_c_coeff_table_functions.R")

crop_coeftab <- coeftab(mc_bd_min,mc_c2070_min,mc_c70_min,mc_cd_min,
                             mc_fs_min,mc_hh_min,mc_mp_min,mc_riv_min,mc_sd_min
                             ,mc_see_min,mc_slope_min,mc_landscape, digits=2)@coefs

param_names <-crop_coeftab[,0]

dframe <- as.data.frame(crop_coeftab)
rownames(dframe)
str(dframe)
cc <- as.vector(rownames(dframe))
cc <- gsub("[1]", "_baboon", cc, fixed=TRUE)
cc <- gsub("[2]", "_elephant", cc, fixed=TRUE)
cc <- gsub("[3]", "_vervet", cc, fixed=TRUE)

rownames(dframe) <- cc

print(xtable(dframe, type = "latex"), file = "crop_coefs_tab.tex") #print to tex
