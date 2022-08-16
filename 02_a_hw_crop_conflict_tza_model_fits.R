require(rethinking)
require(dagitty)
require(xtable)
require(stringr)
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

##drop charachters fro ease of model

dc_nochar <- within(dc, rm("village", "species","household_size" )) # so stan is not upset by charachters
dc_nona <- within(dc, rm("village", "species" , "household_size" , "household_size_std")) # so stan is not upset by charachters and NA in HHsize
nrow(dc)==nrow(dc_nochar) #should be true
nrow(dc)==nrow(dc_nona) #shold be true

########new crop models post dags##########
adjustmentSets( crop_damage_dag , exposure="c2070" , outcome="crop_damage" , type="minimal")

mc_c2070 <- map2stan(
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
      
    ), data=dc_nona , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.98) , rng_seed=434)

precis(mc_c2070 , depth=2)

########cover 70#####
adjustmentSets( crop_damage_dag , exposure="c70" , outcome="crop_damage" )

mc_c70 <- map2stan(
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
    
  ), data=dc_nona , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.98), rng_seed=44)

precis(mc_c70 , depth=2)

########crop density#####
mc_cd <- map2stan(
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
    
  ), data=dc_nona , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.98) , rng_seed = 483)

precis(mc_cd , depth=2)



#########river##
adjustmentSets( crop_damage_dag , exposure="river" , outcome="crop_damage" )

mc_riv <- map2stan(
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
    
  ), data=dc_nona , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.98) ,  rng_seed = 76)

precis(mc_riv , depth=2)

######settlement distance#######
adjustmentSets( crop_damage_dag , exposure="sd" , outcome="crop_damage" )

mc_sd <- map2stan(
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
    
  ), data=dc_nona , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.98),  rng_seed = 349)

precis(mc_sd , depth=2)

##building density
adjustmentSets( crop_damage_dag , exposure="bd" , outcome="crop_damage" )

mc_bd <- map2stan(
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
    
  ), data=dc_nona , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.98) , rng_seed = 729)

precis(mc_bd , depth=2)

#####months planted
adjustmentSets( crop_damage_dag , exposure="months_planted" , outcome="crop_damage" )

mc_mp <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] + 
      (b_MP + b_MPs[species_index])*months_planted_std,
    c(a,b_MP) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_MPs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dc_nona , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.98) , rng_seed = 2)

precis(mc_mp, depth=2)

###see field
adjustmentSets( crop_damage_dag , exposure="see_field" , outcome="crop_damage" )

mc_see <- map2stan(
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
  ), data=dc_nona , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.98) , rng_seed = 894)

precis(mc_see, depth=2)

###household size imputed
adjustmentSets( crop_damage_dag , exposure="hh_size" , outcome="crop_damage" )

mc_hh <- map2stan(
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
  ), data=dc_nochar , chains=4 , cores=4 , iter=3000 , log_lik=TRUE, control=list(adapt_delta=0.98) , rng_seed = 683)

precis(mc_hh, depth=2)

###farm size
adjustmentSets( crop_damage_dag , exposure="farm_size" , outcome="crop_damage" )

mc_fs <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] 
    + (b_FS + b_FSs[species_index])*farm_size_std + (b_HH + b_HHs[species_index])*household_size_std,
    a ~ normal( 0 , 1 ),
    b_FS ~ normal( 0 , 1 ),
    household_size_std ~ dnorm( mu_x, sigma_x ),
    c(a,b_HH,b_FS) ~ dnorm( 0 , 1 ),
    mu_x ~ dnorm( 0 , 3),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_FSs,b_HHs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s,sigma_x) ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data=dc_nochar , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.98) , rng_seed = 4894) 

precis(mc_fs, depth=2)

####slope
adjustmentSets( crop_damage_dag , exposure="slope" , outcome="crop_damage" )
mc_slope <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] + 
      (b_SL + b_SLs[species_index])*gse_slope30m_std,

    c(a,b_SL) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_SLs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dc_nona , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.98) , rng_seed = 84)


precis(mc_slope , depth=2)

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
      
    ), data=dc_nona , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.99) , rng_seed = 234)
  

precis(mc_landscape , depth=2)

##likely confounded model with mitigation strategy
##num prot glmm needs HHSize and farm size // Hh drived visual things farm size affects effectiveness of things like fences

mc_np <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] 
    + (b_NP + b_NPs[species_index])*num_crop_prot_strats_std
    + (b_FS + b_FSs[species_index])*farm_size_std
    + (b_HH + b_HHs[species_index])*household_size_std,
    household_size_std ~ dnorm( mu_x, sigma_x ),
    c(a,b_HH,b_FS,b_NP) ~ normal( 0 , 1 ),
    mu_x ~ dnorm( 0 , 3 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_FSs,b_HHs,b_NPs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s,sigma_x) ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data=dc_nochar, chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.98) , rng_seed = 2063)

precis(mc_np , depth=2)

mc_cpwf <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] 
    + (b_WF + b_WFs[species_index])*crop_prot_w_fence 
    + (b_FS + b_FSs[species_index])*farm_size_std,
    c(a,b_FS,b_WF) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_FSs,b_WFs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data=dc_nona , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.98) , rng_seed = 639)

precis(mc_cpwf , depth=2)

mc_cpsf <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] 
    + (b_SF + b_SFs[species_index])*crop_prot_sisal
    + (b_FS + b_FSs[species_index])*farm_size_std,
    c(a,b_FS,b_SF) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_FSs,b_SFs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data=dc_nona , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.98) , rng_seed = 639)

precis(mc_cpsf , depth=2)

##musoc
mc_cpmusic <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] 
    + (b_MUSIC + b_MUSICs[species_index])*crop_prot_music
    + (b_FS + b_FSs[species_index])*farm_size_std,
    c(a,b_FS,b_MUSIC) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_FSs,b_MUSICs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data=dc_nona , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.98) , rng_seed = 39)

precis(mc_cpmusic , depth=2)

##chase

mc_cpchase <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] 
    + (b_CHASE + b_CHASEs[species_index])*crop_prot_chase
    + (b_FS + b_FSs[species_index])*farm_size_std,
    c(a,b_FS,b_CHASE) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_FSs,b_CHASEs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data=dc_nona , chains=4 , cores=4 , iter=3000 , log_lik=TRUE , control=list(adapt_delta=0.98) , rng_seed = 639)

precis(mc_cpchase , depth=2)

###guard
mc_cpguard <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] 
    + (b_GUARD + b_GUARDs[species_index])*crop_prot_guard
    + (b_FS + b_FSs[species_index])*farm_size_std,
    c(a,b_FS,b_GUARD) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_FSs,b_GUARDs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data=dc_nona , chains=4 , cores=4 , iter=3000 , log_lik=TRUE, control=list(adapt_delta=0.98) , rng_seed = 639)

precis(mc_cpguard , depth=2)
###########tables for paper

#WAIC of crop table
crop_waic_tab <- compare(mc_bd,mc_c2070,mc_c70,mc_cd,mc_fs,mc_hh,mc_mp,mc_riv,mc_sd,mc_see,mc_slope,mc_np,mc_landscape)
crop_waic_tab
print(xtable(crop_waic_tab[,1:3], type = "latex"), file = "crop_waic_tab.tex") # save to tex##chase

dpred <- list(
  village_index=rep(1,6),
  crop_prot_chase=c(0,1,0,1,0,1),
  species_index=c(1,1,2,2,3,3),
  farm_size_std=rep(0,6)
)

av_z <- matrix(0,1000,length(unique(dc$village_index))) #need to add zeros in VE to plot main effect
link2 <- link(mc_cpchase, data=dpred , replace=list(village_index=av_z) )
pdf(file = "plots/chase_crop_conflict_bab.pdf",   width = 6, height = 6) 
dens(link2[,1] , lty=2 , col="blue" , ylim=c(0,15) , xlim=c(0,0.5) , main="probability baboon crop conflict")
abline(v=mean(link2[,1]) , col="blue" , lty=2)
dens(link2[,2] , add=TRUE , col="darkblue")
abline(v=median(link2[,2]) , col="darkblue" , lty=1)
legend('topright' , c("no chase" , "yes chase") , col=c("blue" , "darkblue") , lty=c(2,1))
dev.off()

pdf(file = "plots/chase_crop_conflict_ele.pdf",   width = 6, height = 6) 
dens(link2[,3] , lty=2 , col="grey" , ylim=c(0,15) , xlim=c(0.2,1) , main="probability elephant crop conflict")
abline(v=mean(link2[,3]) , col="grey" , lty=2)
dens(link2[,4] , add=TRUE , col="black" )
abline(v=median(link2[,4]) , col="black" , lty=1)
legend('topleft' , c("no chase" , "yes chase") , col=c("grey" , "black") , lty=c(2,1))
dev.off()

pdf(file = "plots/chase_crop_conflict_verv.pdf",   width = 6, height = 6) 
dens(link2[,5] , lty=2 , col="green" , ylim=c(0,15) , xlim=c(0,0.6) , main="probability vervet crop conflict")
abline(v=mean(link2[,5]) , col="green" , lty=2)
dens(link2[,6] , add=TRUE , col="darkgreen" )
abline(v=median(link2[,6]) , col="darkgreen" , lty=1)
legend('topleft' , c("no chase" , "yes chase") , col=c("green" , "darkgreen") , lty=c(2,1))
dev.off()

########add in dag########3
crop_damage_dag_confound <- 
  dagitty('dag {
  c2070 -> crop_damage
  c70 -> crop_damage
  river -> c70
  river -> crop_damage
  months_planted -> crop_damage
  farm_size -> crop_damage
  farm_size -> num_protect
  num_protect -> crop_damage
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

##drop charachters fro ease of model





###coef tabs
source(file="02_c_coeff_table_functions.R")

crop_coeftab <- coeftab(mc_bd,mc_c2070,mc_c70,mc_cd,
                             mc_fs,mc_hh,mc_mp,mc_riv,mc_sd
                             ,mc_see,mc_slope,mc_np,mc_landscape, digits=2)@coefs

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

## confound
adjustmentSets( crop_damage_dag_confound , exposure="num_protect" , outcome="crop_damage" , type="minimal")

crop_coeftab_confound <- coeftab(mc_np,mc_cpsf,mc_cpsf, digits=2)@coefs
param_names <-crop_coeftab_confound[,0]

dframe <- as.data.frame(crop_coeftab_confound)
rownames(dframe)
str(dframe)
cc <- as.vector(rownames(dframe))
cc <- gsub("[1]", "_baboon", cc, fixed=TRUE)
cc <- gsub("[2]", "_elephant", cc, fixed=TRUE)
cc <- gsub("[3]", "_vervet", cc, fixed=TRUE)

rownames(dframe) <- cc

print(xtable(dframe, type = "latex"), file = "crop_coefs_confounds_tab.tex") #print to tex

