require(rethinking)
require(dagitty)

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
        (b_SL + b_SLs[species_index])*slope_std,

      c(a,b_C2070,b_CR,b_BD,b_SL) ~ normal( 0 , 1 ),
      av[village_index] ~ dnorm(0,sigma_v),
      c(as,b_C2070s,b_CRs,b_BDs,b_SLs)[species_index] ~ dmvnormNC(sigma_s,Rho),
      c(sigma_v,sigma_s) ~ dexp(1),
      Rho ~ dlkjcorr(3)
      
    ), data=dc , chains=4 , cores=4 , iter=1000 , log_lik=TRUE , control=list(adapt_delta=0.95))

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
    
  ), data=dc , chains=4 , cores=4 , iter=1000 , log_lik=TRUE , control=list(adapt_delta=0.95))

precis(mc_c70_min , depth=2)

########crop density#####
mc_cd_min <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] + 
      (b_CR + b_CRs[species_index])*crop_std +
      (b_SL + b_SLs[species_index])*slope_std,
    
    c(a,b_CR,b_SL) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_CRs,b_SLs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=1000 , log_lik=TRUE , control=list(adapt_delta=0.95))

precis(mc_cd_min , depth=2)



#########river##
adjustmentSets( crop_damage_dag , exposure="river" , outcome="crop_damage" )

mc_riv_min <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] + 
      (b_SL + b_SLs[species_index])*slope_std +
      (b_RIV + b_RIVs[species_index])*river_std,
    
    c(a,b_SL,b_RIV) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_SLs,b_RIVs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=1000 , log_lik=TRUE , control=list(adapt_delta=0.95))

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
    
  ), data=dc , chains=4 , cores=4 , iter=1000 , log_lik=TRUE)

precis(mc_sd_min , depth=2)

##building density
adjustmentSets( crop_damage_dag , exposure="bd" , outcome="crop_damage" )

mc_bd_min <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] + 
      (b_BD + b_BDs[species_index])*build_dens_std +
      (b_SD + b_SDs[species_index])*settle_dist_km_std +
      (b_SL + b_SLs[species_index])*slope_std,
    
    c(a,b_SD,b_BD,b_SL) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_SDs,b_BDs,b_SLs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=1000 , log_lik=TRUE , control=list(adapt_delta=0.95))

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
    
  ), data=dc , chains=4 , cores=4 , iter=1000 , log_lik=TRUE)

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
  ), data=dc , chains=4 , cores=4 , iter=1000 , log_lik=TRUE)

precis(mc_see_min, depth=2)

###household size
adjustmentSets( crop_damage_dag , exposure="hh_size" , outcome="crop_damage" )

mc_hh_min <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] 
    + (b_FS + b_FSs[species_index])*farm_size_std
    + (b_HH + b_HHs[species_index])*household_size_std,
    c(a,b_HH,b_FS) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_HHs,b_FSs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data=dc , chains=4 , cores=4 , iter=1000 , log_lik=TRUE)

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
  ), data=dc , chains=4 , cores=4 , iter=1000 , log_lik=TRUE)

precis(mc_fs_min, depth=2)

####slope
mc_slope_min <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] + 
      (b_SL + b_SLs[species_index])*slope_std,

    c(a,b_SL) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_SLs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=1000 , log_lik=TRUE , control=list(adapt_delta=0.95))

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
      + (b_SL + b_SLs[species_index])*slope_std 
      + (b_CR + b_CRs[species_index])*crop_std,
      c(a,b_SD,b_C70,b_C2070,b_RIV,b_RD,b_BD,b_CR,b_SL) ~ normal( 0 , 1 ),
      av[village_index] ~ dnorm(0,sigma_v),
      c(as,b_SDs,b_C70s,b_C2070s,b_RIVs,b_RDs,b_BDs,b_CRs,b_SLs)[species_index] ~ dmvnormNC(sigma_s,Rho),
      c(sigma_v,sigma_s) ~ dexp(1),
      Rho ~ dlkjcorr(3)
      
    ), data=dc , chains=4 , cores=4 , iter=1000 , log_lik=TRUE ,control=list(adapt_delta=0.99))
  

precis(mc_landscape , depth=2)

compare(mc_bd_min,mc_c2070_min,mc_c70_min,mc_cd_min,mc_fs_min,mc_hh_min,mc_landscape,mc_mp_min,mc_riv_min,mc_sd_min,mc_see_min,mc_slope_min)
#########################
# mc0 <- ulam(
#   alist(
#     conflict ~ binomial(1,p),
#     logit(p) <- av[village_index] + as[species_index] ,
#     a ~ normal( 0 , 1 ),
#     av[village_index] ~ dnorm(a,sigma_v),
#     as[species_index] ~ dnorm(a,sigma_s),
#     c(sigma_v,sigma_s) ~ dexp(1)
#     
#   ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik = TRUE)

mc0 <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] ,
    a ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    as[species_index] ~ dnorm(0,sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1)
    
  ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)



post <- extract.samples(mc0)
str(post)
dens( logistic(post$a + post$as[,1]) , col="red" , xlim=c(0,1) , ylim=c(0,230)) 
dens( logistic(post$a + post$as[,2] ), col="orange" , add=TRUE) 

precis(mc0, depth=2)

precislist <- list(
  PrBaboon = logistic(post$a + post$as[,1] ),
  PrElephant =logistic(post$a + post$as[,2]),
  PrVervet =logistic(post$a + post$as[,3])
)

plot(precis(precislist , ci=.89) )
ylabels=c("probability baboon crop conflict" , "probability elephant crop conflict")

dpred <- list(
  village_index=seq(1:max(dc$village_index)),
  species_index=rep(1,30)
)

link2 <- link(mc0, data=dpred  )

str(post)
##crop
mc1 <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] 
    + b_CR + b_CRs[species_index]*crop_std ,
    a ~ normal( 0 , 1 ),
    b_CR ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_CRs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)


precis(mc1, depth=3)
precis(mc1nc, depth=3)


plot_seq <- seq(from=min(dc$crop_std) , to=max(dc$crop_std) , length=30)
av_z <- matrix(0,1000,length(unique(dc$village_index))) #need to add zeros in VE to plot main effect

precis(post$sigma_v)
ylabels=c("probability baboon crop conflict" , "probability elephant crop conflict")
colpal=c("blue" , "grey")
for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    crop_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(mc1nc, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dc$baboon_c ~ dc$crop_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="crop density standardized")}
  if(i==2){plot(dc$elephant_c ~ dc$crop_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="crop density standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}


##settlement distance
# mc2 <- ulam(
#   alist(
#     conflict ~ binomial(1,p),
#     logit(p) <- av[village_index] + as[species_index] +  b_SDs[species_index]*settle_dist_km_std ,
#     a ~ normal( 0 , 1 ),
#     b_SD ~ normal( 0 , 0.5 ),
#     av[village_index] ~ dnorm(a,sigma_v),
#     c(as,b_SDs)[species_index] ~ multi_normal( c(a,b_SD) , Rho , sigma_s),
#     c(sigma_v,sigma_s) ~ dexp(1),
#     Rho ~ lkj_corr(3)
# 
#   ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)

mc2 <- map2stan(
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
    
  ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)

precis(mc2, depth=2)

plot_seq <- seq(from=min(dc$settle_dist_km_std) , to=max(dc$settle_dist_km_std) , length=30)
av_z <- matrix(0,1000,length(unique(dc$village_index))) #need to add zeros in VE to plot main effect


plot(dc$baboon_c ~ dc$settle_dist_km_std , col=col.alpha("slateblue", 0.1) , pch=19 , ylab="baboon bothers" , xlab="standardize km from settlement area")

ylabels=c("probability baboon crop conflict" , "probability elephant crop conflict")
colpal=c("blue" , "grey")
for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(mc2, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dc$baboon_c ~ dc$settle_dist_km_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="standardize km from settlement")}
  if(i==2){plot(dc$elephant_c ~ dc$settle_dist_km_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="standardize km from settlement")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

#####household size
# mc3 <- ulam(
#   alist(
#     conflict ~ binomial(1,p),
#     logit(p) <- av[village_index] + as[species_index] + b_HHs[species_index]*household_size_std,
#     a ~ normal( 0 , 1 ),
#     b_HH ~ normal( 0 , 0.5 ),
#     av[village_index] ~ dnorm(a,sigma_v),
#     c(as,b_HHs)[species_index] ~ multi_normal( c(a,b_HH) , Rho , sigma_s),
#     c(sigma_v,sigma_s) ~ dexp(1),
#     Rho ~ lkj_corr(3)
#     
#   ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)

mc3 <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] 
    + (b_HH + b_HHs[species_index])*household_size_std,
    a ~ normal( 0 , 1 ),
    b_HH ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_HHs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
  ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)
precis(mc3, depth=2)


plot_seq <- seq(from=min(dc$household_size_std) , to=max(dc$household_size_std) , length=30)


for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    household_size_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(mc3, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dc$baboon_c ~ dc$household_size_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size standardized")}
  if(i==2){plot(dc$elephant_c ~ dc$household_size_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

##########farmsize#####
mc4 <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] 
    + (b_FS + b_FSs[species_index])*farm_size_std ,
    a ~ normal( 0 , 1 ),
    b_FS ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_FSs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)

precis(mc4, depth=2)


plot_seq <- seq(from=min(dc$farm_size_std) , to=max(dc$farm_size_std) , length=30)


for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    farm_size_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(mc4, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dc$baboon_c ~ dc$farm_size_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="farm size standardized")}
  if(i==2){plot(dc$elephant_c ~ dc$farm_size_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="farm size standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

####see field
mc5 <- map2stan(
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
  ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)


dpred <- list(
  village_index=rep(1,2),
  see_field=c(0,1),
  species_index=rep(1,2)
)

link2 <- link(mc5, data=dpred , replace=list(village_index=av_z) )

str(link2)
dens(link2[,1] , lty=2 , col="blue" , ylim=c(0,15) , xlim=c(0,0.5) , main="baboon conplict probz")
abline(v=mean(link2[,1]) , col="blue" , lty=2)
dens(link2[,2] , add=TRUE , col="darkblue")
abline(v=mean(link2[,2]) , col="darkblue" , lty=1)

dpred <- list(
  village_index=rep(1,2),
  see_field=c(0,1),
  species_index=rep(2,2)
)

link2 <- link(mc6, data=dpred , replace=list(village_index=av_z) )

str(link2)
dens(link2[,1] , lty=2 , col="grey" , ylim=c(0,15) , xlim=c(0.2,1) , main="elephant conflict probz")
abline(v=mean(link2[,1]) , col="grey" , lty=2)
dens(link2[,2] , add=TRUE , col="black" )
abline(v=mean(link2[,2]) , col="black" , lty=1)

###cover 70
mc6 <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] 
    + (b_C70 + b_C70s[species_index])*c70_std ,
    c(a,b_C70) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_C70s)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)

precis(mc6, depth=2)


plot_seq <- seq(from=min(dc$c70_std) , to=max(dc$c70_std) , length=30)

for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    c70_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(mc6, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dc$baboon_c ~ dc$c70_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="Tree Cover 70 percent standardized")}
  if(i==2){plot(dc$elephant_c ~ dc$c70_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="Tree Cover 70 percent standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

#####cover 2070
mc7 <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] 
    + (b_C2070 + b_C2070s[species_index])*c2070_std, 
    c(a,b_C2070) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_C2070s)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)

precis(mc7 , depth=2)

plot_seq <- seq(from=min(dc$c2070_std) , to=max(dc$c2070_std) , length=30)


for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,30),
    c2070_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(mc7, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dc$baboon_c ~ dc$c2070_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="Tree Cover 20-70 percent standardized")}
  if(i==2){plot(dc$elephant_c ~ dc$c2070_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="Tree Cover 20-70 percent standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}


####river
mc8 <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] 
    + (b_RIV + b_RIVs[species_index])*river_std ,
    c(a,b_RIV) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_RIVs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)

precis(mc8, depth=2)


plot_seq <- seq(from=min(dc$river_std) , to=max(dc$river_std) , length=30)


for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    river_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(mc8, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dc$baboon_c ~ dc$river_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="river standardized")}
  if(i==2){plot(dc$elephant_c ~ dc$river_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="river standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

####building dens
mc9 <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] + 
    (b_BD + b_BDs[species_index])*build_dens_std,
    c(a,b_BD) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_BDs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)

precis(mc9, depth=2)

plot_seq <- seq(from=min(dc$build_dens_std) , to=max(dc$build_dens_std) , length=30)


for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    build_dens_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(mc9, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dc$baboon_c ~ dc$build_dens_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="building density standardized")}
  if(i==2){plot(dc$elephant_c ~ dc$build_dens_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="building density standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}


###road
mc10 <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] + 
    (b_RD + b_RDs[species_index])*road_std ,
    c(a,b_RD) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_RDs)[species_index] ~ dmvnormNC(sigma_s,Rho) ,
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)

precis(mc10, depth=2)

plot_seq <- seq(from=min(dc$road_std) , to=max(dc$road_std) , length=30)


for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    road_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(mc10, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dc$baboon_c ~ dc$road_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="road density standardized")}
  if(i==2){plot(dc$elephant_c ~ dc$road_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="road density standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

###months planted
mc11 <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] + 
    (b_MP + b_MPs[species_index])*months_planted_std,
    c(a,b_MP) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_MPs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)

  ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)

precis(mc11, depth=2)

plot_seq <- seq(from=min(dc$months_planted_std) , to=max(dc$months_planted_std) , length=30)


for (i in 1:2){

  dpred <- list(
    village_index=rep(1,30),
    months_planted_std=plot_seq,
    species_index=rep(i,30)
  )

  link2 <- link(mc11, data=dpred , replace=list(village_index=av_z) )

  if(i==1){plot(dc$baboon_c ~ dc$months_planted_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="months planted standardized")}
  if(i==2){plot(dc$elephant_c ~ dc$months_planted_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="months planted standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

##########all protection strategies
#"crop_prot_music" , "crop_prot_w_fence" , "crop_prot_sisal" , "crop_prot_shout" , "crop_prot_fire" , "crop_prot_chase" , "crop_prot_guard"
# 
# mc12 <- ulam(
#   alist(
#     conflict ~ binomial(1,p),
#     logit(p) <- av[village_index] + as[species_index] + b_CPMs[species_index]*crop_prot_music + b_CPWFs[species_index]*crop_prot_w_fence
#                + b_CPSIs[species_index]*crop_prot_sisal + b_CPSHs[species_index]*crop_prot_shout + b_CPFs[species_index]*crop_prot_fire
#                + b_CPCs[species_index]*crop_prot_chase + b_CPGs[species_index]*crop_prot_guard ,
#     a ~ normal( 0 , 1 ),
#     c(b_CPM,b_CPWF,bCPSI,b_CPSH,b_CPF,b_CPC,b_CPG) ~ normal( 0 , 0.5 ),
#     av[village_index] ~ dnorm(a,sigma_v),
#     c(as,b_CPMs,b_CPWFs,b_CPSIs,b_CPSHs,b_CPFs,b_CPCs,b_CPGs)[species_index] ~ multi_normal( c(a,b_CPM,b_CPWF,bCPSI,b_CPSH,b_CPF,b_CPC,b_CPG) , Rho , sigma_s),
#     c(sigma_v,sigma_s) ~ dexp(1),
#     Rho ~ dlkjcorr(3)
#     
#   ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)
# 
# precis(mc12)
# precis(mc12 , depth=3)
# 
# #traceplot(mc12)
# 
# dpred <- list(
#   village_index=rep(1,8),
#   crop_prot_music=c(1,mean(dc$crop_prot_music),mean(dc$crop_prot_music),mean(dc$crop_prot_music),mean(dc$crop_prot_music),mean(dc$crop_prot_music),mean(dc$crop_prot_music),mean(dc$crop_prot_music)),
#   crop_prot_chase=c(mean(d$crop_prot_chase),1,mean(d$crop_prot_chase),mean(d$crop_prot_chase),mean(d$crop_prot_chase),mean(d$crop_prot_chase),mean(d$crop_prot_chase),mean(d$crop_prot_chase)),
#   crop_prot_w_fence=c(mean(d$crop_prot_w_fence),mean(d$crop_prot_w_fence),1,mean(d$crop_prot_w_fence),mean(d$crop_prot_w_fence),mean(d$crop_prot_w_fence),mean(d$crop_prot_w_fence),mean(d$crop_prot_w_fence)),
#   crop_prot_sisal=c(mean(d$crop_prot_sisal),mean(d$crop_prot_sisal),mean(d$crop_prot_sisal),1,mean(d$crop_prot_sisal),mean(d$crop_prot_sisal),mean(d$crop_prot_sisal),mean(d$crop_prot_sisal)),
#   crop_prot_shout=c(mean(d$crop_prot_shout),mean(d$crop_prot_shout),mean(d$crop_prot_shout),mean(d$crop_prot_shout),1,mean(d$crop_prot_shout),mean(d$crop_prot_shout),mean(d$crop_prot_shout)),
#   crop_prot_fire=c(mean(d$crop_prot_fire),mean(d$crop_prot_fire),mean(d$crop_prot_fire),mean(d$crop_prot_fire),mean(d$crop_prot_fire),1,mean(d$crop_prot_fire),mean(d$crop_prot_fire)),
#   crop_prot_guard=c(mean(d$crop_prot_guard),mean(d$crop_prot_guard),mean(d$crop_prot_guard),mean(d$crop_prot_guard),mean(d$crop_prot_guard),mean(d$crop_prot_guard),1,mean(d$crop_prot_guard)),
#   species_index=rep(1,8)
# )
# 
# 
# # dpred <- list(
# #   village_index=rep(1,8),
# #   crop_prot_music=c(1,0,0,0,0,0,0,0),
# #   crop_prot_chase=c(0,1,0,0,0,0,0,0),
# #   crop_prot_w_fence=c(0,0,1,0,0,0,0,0),
# #   crop_prot_sisal=c(0,0,0,1,0,0,0,0),
# #   crop_prot_shout=c(0,0,0,0,1,0,0,0),
# #   crop_prot_fire=c(0,0,0,0,0,1,0,0),
# #   crop_prot_guard=c(0,0,0,0,0,0,1,0),
# #   species_index=rep(1,8)
# # )
# 
# 
# link2 <- link(mc12, data=dpred , replace=list(village_index=av_z) )
# str(link2)
# precis(link2)
# 
# precislist <- list(
#   crop_prot_music=link2[,1],
#   crop_prot_chase=link2[,2],
#   crop_prot_w_fence=link2[,3],
#   crop_prot_sisal=link2[,4],
#   crop_prot_shout=link2[,5],
#   crop_prot_fire=link2[,6],
#   crop_prot_guard=link2[,7]
# )
#   
# 
# plot(precis(precislist , ci=.89) , xlab="Probabity Baboon Crop Conflict" )
# 
# dpred <- list(
#   village_index=rep(1,8),
#   crop_prot_music=c(1,mean(dc$crop_prot_music),mean(dc$crop_prot_music),mean(dc$crop_prot_music),mean(dc$crop_prot_music),mean(dc$crop_prot_music),mean(dc$crop_prot_music),mean(dc$crop_prot_music)),
#   crop_prot_chase=c(mean(d$crop_prot_chase),1,mean(d$crop_prot_chase),mean(d$crop_prot_chase),mean(d$crop_prot_chase),mean(d$crop_prot_chase),mean(d$crop_prot_chase),mean(d$crop_prot_chase)),
#   crop_prot_w_fence=c(mean(d$crop_prot_w_fence),mean(d$crop_prot_w_fence),1,mean(d$crop_prot_w_fence),mean(d$crop_prot_w_fence),mean(d$crop_prot_w_fence),mean(d$crop_prot_w_fence),mean(d$crop_prot_w_fence)),
#   crop_prot_sisal=c(mean(d$crop_prot_sisal),mean(d$crop_prot_sisal),mean(d$crop_prot_sisal),1,mean(d$crop_prot_sisal),mean(d$crop_prot_sisal),mean(d$crop_prot_sisal),mean(d$crop_prot_sisal)),
#   crop_prot_shout=c(mean(d$crop_prot_shout),mean(d$crop_prot_shout),mean(d$crop_prot_shout),mean(d$crop_prot_shout),1,mean(d$crop_prot_shout),mean(d$crop_prot_shout),mean(d$crop_prot_shout)),
#   crop_prot_fire=c(mean(d$crop_prot_fire),mean(d$crop_prot_fire),mean(d$crop_prot_fire),mean(d$crop_prot_fire),mean(d$crop_prot_fire),1,mean(d$crop_prot_fire),mean(d$crop_prot_fire)),
#   crop_prot_guard=c(mean(d$crop_prot_guard),mean(d$crop_prot_guard),mean(d$crop_prot_guard),mean(d$crop_prot_guard),mean(d$crop_prot_guard),mean(d$crop_prot_guard),1,mean(d$crop_prot_guard)),
#   species_index=rep(2,8)
# )
# 
# ##kate says wire_fencing, sisal, music
# link2 <- link(mc12, data=dpred , replace=list(village_index=av_z) )
# str(link2)
# 
# precislist <- list(
#   crop_prot_music=link2[,1],
#   crop_prot_chase=link2[,2],
#   crop_prot_w_fence=link2[,3],
#   crop_prot_sisal=link2[,4],
#   crop_prot_shout=link2[,5],
#   crop_prot_fire=link2[,6],
#   crop_prot_guard=link2[,7],
#   crop_prot_none=link2[,8]
#   
# )
# 
# 
# plot(precis(precislist , ci=.89) , xlab="Probabity Elephant Crop Conflict" )
# 
# #wire fence
# mc12.1 <- ulam(
#   alist(
#     conflict ~ binomial(1,p),
#     logit(p) <- av[village_index] + as[species_index] + b_CPWFs[species_index]*crop_prot_w_fence,
#     a ~ normal( 0 , 1 ),
#     b_CPWF ~ normal( 0 , 0.5 ),
#     av[village_index] ~ dnorm(a,sigma_v),
#     c(as,b_CPWFs)[species_index] ~ multi_normal( c(a,b_CPWF) , Rho , sigma_s),
#     c(sigma_v,sigma_s) ~ dexp(1),
#     Rho ~ lkj_corr(3)
#     
#   ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)
# 
# 
# dpred <- list(
#   village_index=rep(1,4),
#   crop_prot_w_fence=c(0,1,0,1),
#   species_index=c(1,1,2,2)
# )
# 
# link2 <- link(mc12.1, data=dpred , replace=list(village_index=av_z) )
# 
# precislist <- list(
#   baboon_crop_prot_no_wfence=link2[,1],
#   baboon_crop_prot_wfence=link2[,2],
#   elephant_crop_prot_no_wfence=link2[,3],
#   elephant_crop_prot_wfence=link2[,4]
# )
# 
# plot(precis(precislist , ci=.89) , xlab="Probabity Crop Conflict" )
# 
# #sisal
# mc12.2 <- ulam(
#   alist(
#     conflict ~ binomial(1,p),
#     logit(p) <- av[village_index] + as[species_index] + b_CPSIs[species_index]*crop_prot_sisal, 
#     a ~ normal( 0 , 1 ),
#     bCPSI ~ normal( 0 , 0.5 ),
#     av[village_index] ~ dnorm(a,sigma_v),
#     c(as,b_CPSIs)[species_index] ~ multi_normal( c(a,bCPSI) , Rho , sigma_s),
#     c(sigma_v,sigma_s) ~ dexp(1),
#     Rho ~ lkj_corr(3)
#     
#   ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)
# 
# dpred <- list(
#   village_index=rep(1,4),
#   crop_prot_sisal=c(0,1,0,1),
#   species_index=c(1,1,2,2)
# )
# 
# link2 <- link(mc12.2, data=dpred , replace=list(village_index=av_z) )
# 
# precislist <- list(
#   baboon_crop_prot_no_sisal=link2[,1],
#   baboon_crop_prot_sisal=link2[,2],
#   elephant_crop_prot_no_sisal=link2[,3],
#   elephant_crop_prot_sisal=link2[,4]
# )
# 
# plot(precis(precislist , ci=.89) , xlab="Probabity Crop Conflict" )
# ##fire
# mc12.3 <- ulam(
#   alist(
#     conflict ~ binomial(1,p),
#     logit(p) <- av[village_index] + as[species_index] + b_CPFs[species_index]*crop_prot_fire,
#     a ~ normal( 0 , 1 ),
#     b_CPF ~ normal( 0 , 0.5 ),
#     av[village_index] ~ dnorm(a,sigma_v),
#     c(as,b_CPFs)[species_index] ~ multi_normal( c(a,b_CPF) , Rho , sigma_s),
#     c(sigma_v,sigma_s) ~ dexp(1),
#     Rho ~ lkj_corr(3)
#     
#   ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)
# 
# dpred <- list(
#   village_index=rep(1,4),
#   crop_prot_fire=c(0,1,0,1),
#   species_index=c(1,1,2,2)
# )
# 
# link2 <- link(mc12.3, data=dpred , replace=list(village_index=av_z) )
# 
# precislist <- list(
#   baboon_crop_prot_no_fire=link2[,1],
#   baboon_crop_prot_fire=link2[,2],
#   elephant_crop_prot_no_fire=link2[,3],
#   elephant_crop_prot_fire=link2[,4]
# )
# 
# plot(precis(precislist , ci=.89) , xlab="Probabity Crop Conflict" )
# 
# #guard
# mc12.4 <- ulam(
#   alist(
#     conflict ~ binomial(1,p),
#     logit(p) <- av[village_index] + as[species_index] +  b_CPGs[species_index]*crop_prot_guard ,
#     a ~ normal( 0 , 1 ),
#     b_CPG ~ normal( 0 , 0.5 ),
#     av[village_index] ~ dnorm(a,sigma_v),
#     c(as,b_CPGs)[species_index] ~ multi_normal( c(a,b_CPG) , Rho , sigma_s),
#     c(sigma_v,sigma_s) ~ dexp(1),
#     Rho ~ lkj_corr(3)
#     
#   ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)
# 
# dpred <- list(
#   village_index=rep(1,4),
#   crop_prot_guard=c(0,1,0,1),
#   species_index=c(1,1,2,2)
# )
# 
# link2 <- link(mc12.4, data=dpred , replace=list(village_index=av_z) )
# 
# precislist <- list(
#   baboon_crop_prot_no_guard=link2[,1],
#   baboon_crop_prot_guard=link2[,2],
#   elephant_crop_prot_no_guard=link2[,3],
#   elephant_crop_prot_guard=link2[,4]
# )
# 
# plot(precis(precislist , ci=.89) , xlab="Probabity Crop Conflict" )
# 
# mc12.6 <- ulam(
#   alist(
#     conflict ~ binomial(1,p),
#     logit(p) <- av[village_index] + as[species_index] + b_NCPSs[species_index]*num_crop_prot_strats_std,
#     a ~ normal( 0 , 1 ),
#     b_NCPS ~ normal( 0 , 0.5 ),
#     av[village_index] ~ dnorm(a,sigma_v),
#     c(as,b_NCPSs)[species_index] ~ multi_normal( c(a,b_NCPS) , Rho , sigma_s),
#     c(sigma_v,sigma_s) ~ dexp(1),
#     Rho ~ lkj_corr(3)
#     
#   ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)
# 
# precis(mc12.6, depth=2)
# 
# plot_seq <- seq(from=min(dc$num_crop_prot_strats_std) , to=max(dc$num_crop_prot_strats_std) , length=30)
# 
# 
# for (i in 1:2){
#   
#   dpred <- list(
#     village_index=rep(1,30),
#     num_crop_prot_strats_std=plot_seq,
#     species_index=rep(i,30)
#   )
#   
#   link2 <- link(mc12.6, data=dpred , replace=list(village_index=av_z) )
#   
#   if(i==1){plot(dc$baboon_c ~ dc$num_crop_prot_strats_std, col=col.alpha(colpal[1], 0.01) , pch=19 , ylab=ylabels[i] , xlab="num crop protection strategies stdized")}
#   if(i==2){plot(dc$elephant_c ~ dc$num_crop_prot_strats_std , col=col.alpha(colpal[2], 0.01) , pch=19 , ylab=ylabels[i] , xlab="num crop protection strategies stdized")}
#   pred_mean <- apply(link2 , 2 , mean)
#   lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
#   for (j in sample( c(1:1000) , 100) ){
#     lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
#   }
# }
# 
# #guarding and fire inteaction
# mc12.7 <- ulam(
#   alist(
#     conflict ~ binomial(1,p),
#     logit(p) <- av[village_index] + as[species_index] + b_CPGs[species_index]*crop_prot_guard + (b_CPFs[species_index] + bCPGxbCPFs[species_index]*crop_prot_guard )*crop_prot_fire,
#     a ~ normal( 0 , 1 ),
#     c(b_CPG,b_CPF,bCPGxbCPF) ~ normal( 0 , 0.5 ),
#     av[village_index] ~ dnorm(a,sigma_v),
#     c(as,b_CPGs,b_CPFs,bCPGxbCPFs)[species_index] ~ multi_normal( c(a,b_CPG,b_CPF,bCPGxbCPF) , Rho , sigma_s),
#     c(sigma_v,sigma_s) ~ dexp(1),
#     Rho ~ lkj_corr(3)
#     
#   ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)
# 
# precis(mc12.7, depth=2)
# 
# dpred <- list(
#   village_index=rep(1,8),
#   crop_prot_guard=c(0,1,0,1,0,1,0,1),
#   crop_prot_fire= c(0,0,1,1,0,0,1,1),
#   species_index=c(1,1,1,1,2,2,2,2)
# )
# 
# link2 <- link(mc12.7, data=dpred , replace=list(village_index=av_z) )
# 
# str(link2)
# precislist <- list(
#   baboon_crop_prot_no_fire_or_guard=link2[,1],
#   baboon_crop_prot_guard=link2[,2],
#   baboon_crop_prot_fire=link2[,3],
#   baboon_crop_prot_guard_and_fire=link2[,4],
#   elephant_crop_prot_no_fire_or_guard=link2[,5],
#   elephant_crop_prot_guard=link2[,6],
#   elephant_crop_prot_fire=link2[,7],
#   elephant_crop_prot_guard_and_fire=link2[,8]
# )
# 
# plot(precis(precislist , ci=.89) , xlab="Probabity Crop Conflict" )
# 
# 
# ########## add in settle dist to fire pen interaction model
# mc12.8 <- ulam(
#   alist(
#     conflict ~ binomial(1,p),
#     logit(p) <- av[village_index] + as[species_index] + b_CPGs[species_index]*crop_prot_guard + (b_CPFs[species_index] + bCPGxbCPFs[species_index]*crop_prot_guard )*crop_prot_fire + b_SDs[species_index]*settle_dist_km_std,
#     a ~ normal( 0 , 1 ),
#     c(b_CPG,b_CPF,bCPGxbCPF,b_SD) ~ normal( 0 , 0.5 ),
#     av[village_index] ~ dnorm(a,sigma_v),
#     c(as,b_CPGs,b_CPFs,bCPGxbCPFs,b_SDs)[species_index] ~ multi_normal( c(a,b_CPG,b_CPF,bCPGxbCPF,b_SD) , Rho , sigma_s),
#     c(sigma_v,sigma_s) ~ dexp(1),
#     Rho ~ lkj_corr(3)
#     
#   ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)
# 
# dpred <- list(
#   village_index=rep(1,8),
#   settle_dist_km_std=rep(0,8),
#   crop_prot_guard=c(0,1,0,1,0,1,0,1),
#   crop_prot_fire= c(0,0,1,1,0,0,1,1),
#   species_index=c(1,1,1,1,2,2,2,2)
# )
# 
# link2 <- link(mc12.8, data=dpred , replace=list(village_index=av_z) )
# 
# str(link2)
# precislist <- list(
#   baboon_crop_prot_no_fire_or_guard=link2[,1],
#   baboon_crop_prot_guard=link2[,2],
#   baboon_crop_prot_fire=link2[,3],
#   baboon_crop_prot_guard_and_fire=link2[,4],
#   elephant_crop_prot_no_fire_or_guard=link2[,5],
#   elephant_crop_prot_guard=link2[,6],
#   elephant_crop_prot_fire=link2[,7],
#   elephant_crop_prot_guard_and_fire=link2[,8]
# )
# 
# plot(precis(precislist , ci=.89) , xlab="Probabity Crop Conflict" )
# 
# mc12.9 <- ulam(
#   alist(
#     conflict ~ binomial(1,p),
#     logit(p) <- av[village_index] + as[species_index] + b_CPGs[species_index]*crop_prot_guard + (b_CPFs[species_index] + bCPGxbCPFs[species_index]*crop_prot_guard*settle_dist_km_std )*crop_prot_fire + b_SDs[species_index]*settle_dist_km_std,
#     a ~ normal( 0 , 1 ),
#     c(b_CPG,b_CPF,bCPGxbCPF,b_SD) ~ normal( 0 , 0.5 ),
#     av[village_index] ~ dnorm(a,sigma_v),
#     c(as,b_CPGs,b_CPFs,bCPGxbCPFs,b_SDs)[species_index] ~ multi_normal( c(a,b_CPG,b_CPF,bCPGxbCPF,b_SD) , Rho , sigma_s),
#     c(sigma_v,sigma_s) ~ dexp(1),
#     Rho ~ lkj_corr(3)
#   ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)
# 
# 
# dpred <- list(
#   village_index=rep(1,8),
#   settle_dist_km_std=rep(2,8),
#   crop_prot_guard=c(0,1,0,1,0,1,0,1),
#   crop_prot_fire= c(0,0,1,1,0,0,1,1),
#   species_index=c(1,1,1,1,2,2,2,2)
# )
# 
# link2 <- link(mc12.9, data=dpred , replace=list(village_index=av_z) )
# 
# str(link2)
# precislist <- list(
#   baboon_crop_prot_no_fire_or_guard=link2[,1],
#   baboon_crop_prot_guard=link2[,2],
#   baboon_crop_prot_fire=link2[,3],
#   baboon_crop_prot_guard_and_fire=link2[,4],
#   elephant_crop_prot_no_fire_or_guard=link2[,5],
#   elephant_crop_prot_guard=link2[,6],
#   elephant_crop_prot_fire=link2[,7],
#   elephant_crop_prot_guard_and_fire=link2[,8]
# )
# 
# plot(precis(precislist , ci=.89) , xlab="Probabity Crop Conflict" )

##interactionmodels
mc13 <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] + 
      (b_HH + b_HHs[species_index])*household_size_std + 
      (b_FS + b_FSs[species_index] +b_HHxFS + b_HHxFSs[species_index]*household_size_std )*farm_size_std,
    c(a,b_HH,b_FS,b_HHxFS) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_HHs,b_FSs,b_HHxFSs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE , control=list(adapt_delta=0.99) )

precis(mc13, depth=3)


#sort(unique(dc$household_size_std))
#sort(unique(dc$household_size))
plot_seq <- seq(from=min(dc$farm_size_std) , to=max(dc$farm_size_std) , length=30)
colpal1=brewer.pal(6,"Blues")
colpal2=brewer.pal(6,"Greens")

for (i in 1:2){
    for(j in -1:4){
    dpred <- list(
      village_index=rep(1,30),
      farm_size_std=plot_seq,
      species_index=rep(i,30),
      household_size_std=rep(j , 30) 
      #3.91968493  25
    )
    
    link2 <- link(mc12, data=dpred , replace=list(village_index=av_z) )
    
    if(i==1 & j==-1){plot(dc$baboon_c ~ dc$farm_size_std, col=col.alpha(colpal1[j+2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="farm size standardized")}

    if(i==2 & j==-1){plot(dc$elephant_c ~ dc$farm_size_std , col=col.alpha(colpal2[j+2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="farm size standardized")
 }

      pred_mean <- apply(link2 , 2 , mean)
      lines(pred_mean ~ plot_seq , lw=2, col=colpal1[j+2] , lty=1)
    # for (j in sample( c(1:1000) , 100) ){
    #   lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
    # }
    }
}

########household variables
mc14 <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] 
    + (b_HH + b_HHs[species_index])*household_size_std  
    + (b_FS + b_FSs[species_index])*farm_size_std 
    + (b_MP + b_MPs[species_index])*months_planted_std 
    + (b_SEE + b_SEEs[species_index])*see_field,
    c(a,b_HH,b_FS,b_MP,b_SEE) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_HHs,b_FSs,b_MPs,b_SEEs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)


precis(mc14 , depth=2)
###hh size

plot_seq <- seq(from=min(dc$household_size_std) , to=max(dc$household_size_std) , length=30)


for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    household_size_std=plot_seq,
    farm_size_std=rep(0,30),
    months_planted_std=rep(0,30),
    see_field=rep(mean(dc$see_field),30),
    species_index=rep(i,30)
  )
  
  link2 <- link(mc14, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dc$baboon_c ~ dc$household_size_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size standardized")}
  if(i==2){plot(dc$elephant_c ~ dc$household_size_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

##farm size
plot_seq <- seq(from=min(dc$farm_size_std) , to=max(dc$farm_size_std) , length=30)


for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    farm_size_std=plot_seq,
    household_size_std=rep(0,30),
    farm_size_std=rep(0,30),
    months_planted_std=rep(0,30),
    see_field=rep(mean(dc$see_field),30),
    species_index=rep(i,30)
  )
  
  link2 <- link(mc14, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dc$baboon_c ~ dc$farm_size_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="farm size standardized")}
  if(i==2){plot(dc$elephant_c ~ dc$farm_size_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="farm size standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

##month planted

plot_seq <- seq(from=min(dc$months_planted_std) , to=max(dc$months_planted_std) , length=30)


for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    months_planted_std=plot_seq,
    farm_size_std=rep(0,30),
    household_size_std=rep(0,30),
    farm_size_std=rep(0,30),
    months_planted_std=rep(0,30),
    see_field=rep(mean(dc$see_field),30),
    species_index=rep(i,30)
  )
  
  link2 <- link(mc14, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dc$baboon_c ~ dc$months_planted_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="months planted standardized")}
  if(i==2){plot(dc$elephant_c ~ dc$months_planted_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="months planted standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

##SEE
dpred <- list(
  village_index=rep(1,4),
  see_field=c(0,1,0,1),
  months_planted_std=rep(0,4),
  farm_size_std=rep(0,4),
  household_size_std=rep(0,4),
  farm_size_std=rep(0,4),
  months_planted_std=rep(0,4),
  species_index=c(1,1,2,2)
)

link2 <- link(mc14, data=dpred , replace=list(village_index=av_z) )

dens(link2[,1] , lty=2 , col="blue" , ylim=c(0,15) , xlim=c(0,0.5) , main="probability baboon crop conflict")
abline(v=mean(link2[,1]) , col="blue" , lty=2)
dens(link2[,2] , add=TRUE , col="darkblue")
abline(v=mean(link2[,2]) , col="darkblue" , lty=1)
legend('topright' , c("can't see field" , "see field") , col=c("blue" , "darkblue") , lty=c(2,1))


dens(link2[,3] , lty=2 , col="grey" , ylim=c(0,15) , xlim=c(0.2,1) , main="elephant conflict probz")
abline(v=mean(link2[,3]) , col="grey" , lty=2)
dens(link2[,4] , add=TRUE , col="black" )
abline(v=mean(link2[,4]) , col="black" , lty=1)
legend('topleft' , c("can't see field" , "see field") , col=c("grey" , "black") , lty=c(2,1))


#############landscape variables####################################
mc15 <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] 
    + (b_SD + b_SDs[species_index])*settle_dist_km_std 
    + (b_C70 + b_C70s[species_index])*c70_std 
    + (b_C2070 + b_C2070s[species_index])*c2070_std 
    + (b_RIV + b_RIVs[species_index])*river_std  
    + (b_RD + b_RDs[species_index])*road_std 
    + (b_BD + b_BDs[species_index])*build_dens_std 
    + (b_CR + b_CRs[species_index])*crop_std,
    c(a,b_SD,b_C70,b_C2070,b_RIV,b_RD,b_BD,b_CR) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_SDs,b_C70s,b_C2070s,b_RIVs,b_RDs,b_BDs,b_CRs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE ,control=list(adapt_delta=0.99))


precis(mc15)

#plot distance to settlement
plot_seq <- seq(from=min(dc$settle_dist_km_std) , to=max(dc$settle_dist_km_std) , length=30)
av_z <- matrix(0,1000,length(unique(dc$village_index))) #need to add zeros in VE to plot main effect


ylabels=c("probability baboon crop conflict" , "probability elephant crop conflict","probability vervet crop conflict")
colpal=c("blue" , "grey", "green")
for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=plot_seq,
    c70_std = rep(0,30),
    c2070_std = rep(0,30),
    river_std= rep(0,30),
    road_std= rep(0,30),
    build_dens_std= rep(0,30),
    crop_std= rep(0,30),
    species_index=rep(i,30)
  )
  
  link2 <- link(mc15, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dc$baboon_c ~ dc$settle_dist_km_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="standardize km from settlement")}
  if(i==2){plot(dc$elephant_c ~ dc$settle_dist_km_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="standardize km from settlement")}
  if(i==3){plot(dc$vervet_c ~ dc$settle_dist_km_std , col=col.alpha(colpal[3], 0.1) , pch=19 , ylab=ylabels[i] , xlab="standardize km from settlement")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

##cov70
plot_seq <- seq(from=min(dc$c70_std) , to=max(dc$c70_std) , length=30)

for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=rep(0,30),
    c70_std = plot_seq,
    c2070_std = rep(0,30),
    river_std= rep(0,30),
    road_std= rep(0,30),
    build_dens_std= rep(0,30),
    crop_std= rep(0,30),
    species_index=rep(i,30)
  )
  
  link2 <- link(mc15, data=dpred , replace=list(village_index=av_z) )
  if(i==1){plot(dc$baboon_c ~ dc$c70_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="Tree Cover 70 percent standardized")}
  if(i==2){plot(dc$elephant_c ~ dc$c70_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="Tree Cover 70 percent standardized")}
  if(i==3){plot(dc$vervet_c ~ dc$c70_std , col=col.alpha(colpal[3], 0.1) , pch=19 , ylab=ylabels[i] ,  xlab="Tree Cover 70 percent standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}


##cov2070
plot_seq <- seq(from=min(dc$c2070_std) , to=max(dc$c2070_std) , length=30)

for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=rep(0,30),
    c70_std = rep(0,30),
    c2070_std = plot_seq ,
    river_std= rep(0,30),
    road_std= rep(0,30),
    build_dens_std= rep(0,30),
    crop_std= rep(0,30),
    species_index=rep(i,30)
  )
  
  link2 <- link(mc15, data=dpred , replace=list(village_index=av_z) )
  if(i==1){plot(dc$baboon_c ~ dc$c2070_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="Tree Cover 20/70 percent standardized")}
  if(i==2){plot(dc$elephant_c ~ dc$c2070_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="Tree Cover 20/70 percent standardized")}
  if(i==3){plot(dc$vervet_c ~ dc$c2070_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="Tree Cover 20/70 percent standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

###rivers

plot_seq <- seq(from=min(dc$river_std) , to=max(dc$river_std) , length=30)


for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=rep(0,30),
    c70_std = rep(0,30),
    c2070_std = rep(0,30) ,
    river_std= plot_seq,
    road_std= rep(0,30),
    build_dens_std= rep(0,30),
    crop_std= rep(0,30),
    species_index=rep(i,30)
  )
  
  link2 <- link(mc15, data=dpred , replace=list(village_index=av_z) )
  if(i==1){plot(dc$baboon_c ~ dc$river_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="river density standardized")}
  if(i==2){plot(dc$elephant_c ~ dc$river_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="river density standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}


##roads
plot_seq <- seq(from=min(dc$road_std) , to=max(dc$road_std) , length=30)


for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=rep(0,30),
    c70_std = rep(0,30),
    c2070_std = rep(0,30) ,
    river_std= rep(0,30) ,
    road_std= plot_seq,
    build_dens_std= rep(0,30),
    crop_std= rep(0,30),
    species_index=rep(i,30)
  )
  
  link2 <- link(mc15, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dc$baboon_c ~ dc$road_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="road density standardized")}
  if(i==2){plot(dc$elephant_c ~ dc$road_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="road density standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}


##building density

plot_seq <- seq(from=min(dc$build_dens_std) , to=max(dc$build_dens_std) , length=30)


for (i in 1:2){
  
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=rep(0,30),
    c70_std = rep(0,30),
    c2070_std = rep(0,30) ,
    river_std= rep(0,30) ,
    road_std= rep(0,30),
    build_dens_std= plot_seq ,
    crop_std= rep(0,30),
    species_index=rep(i,30)
  )
  
  link2 <- link(mc15, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dc$baboon_c ~ dc$build_dens_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="building density standardized")}
  if(i==2){plot(dc$elephant_c ~ dc$build_dens_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="building density standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

##crop

plot_seq <- seq(from=min(dc$crop_std) , to=max(dc$crop_std) , length=30)

for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=plot_seq,
    c70_std = rep(0,30),
    c2070_std = rep(0,30),
    river_std= rep(0,30),
    road_std= rep(0,30),
    build_dens_std= rep(0,30),
    crop_std= rep(0,30),
    species_index=rep(i,30)
  )
  
  link2 <- link(mc15, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dc$baboon_c ~ dc$crop_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="crop density standardized")}
  if(i==2){plot(dc$elephant_c ~ dc$crop_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="crop density standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}


############################global########################

mc17 <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] + 
      (b_SD + b_SDs[species_index])*settle_dist_km_std +
      (b_C70 + b_C70s[species_index])*c70_std +
      (b_C2070 + b_C2070s[species_index])*c2070_std +
      (b_RIV + b_RIVs[species_index])*river_std +
      (b_RD + b_RDs[species_index])*road_std +
      (b_BD + b_BDs[species_index])*build_dens_std +
      (b_CR + b_CRs[species_index])*crop_std +
      (b_HH + b_HHs[species_index])*household_size_std + 
      (b_FS + b_FSs[species_index])*farm_size_std +
      (b_SEE + b_SEEs[species_index])*see_field,
    
    c(a,b_SD,b_C70,b_C2070,b_CR,b_BD,b_HH,b_FS,b_SEE,b_RD,b_RIV) ~ normal( 0 , 1 ),
    c(b_HH,b_FS,b_SEE) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_SDs,b_C70s,b_C2070s,b_HHs,b_FSs,b_SEEs,b_CRs,b_BDs,b_RDs,b_RIVs)[species_index] ~ dmvnormNC(sigma_s,Rho),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE , control=list(adapt_delta=0.99))


#plot distance to settlement
plot_seq <- seq(from=min(dc$settle_dist_km_std) , to=max(dc$settle_dist_km_std) , length=30)
av_z <- matrix(0,1000,length(unique(dc$village_index))) #need to add zeros in VE to plot main effect


ylabels=c("probability baboon crop conflict" , "probability elephant crop conflict", "probability vervet conflict")
colpal=c("slateblue" , "darkgrey", "seagreen")

for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=plot_seq,
    c70_std = rep(0,30),
    c2070_std = rep(0,30),
    river_std= rep(0,30),
    road_std= rep(0,30),
    build_dens_std= rep(0,30),
    crop_std= rep(0,30),
    household_size_std=rep(0,30),
    farm_size_std=rep(0,30),
    months_planted_std=rep(0,30),
    see_field=rep(mean(dc$see_field),30),
    species_index=rep(i,30)
  )
  
  link2 <- ensemble(mc17,mc15, data=dpred , replace=list(village_index=av_z) )
    par( mar=c(4,4,1,1)+.1 )
  
  if(i==1){
    pdf(file = "plots/settle_dist_crop_global_conflict_bab2.pdf",   width = 6, height = 6)
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$settle_dist_km_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="distance to settlement edge (km)" , xaxt='n',  cex.lab=1.3)}
    
  if(i==2){
    pdf(file = "plots/settle_dist_crop_global_conflict_ele2.pdf",   width = 6, height = 6)
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$settle_dist_km_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="distance to settlement edge (km)" , xaxt='n' ,  cex.lab=1.3) }
    
    if(i==3){
      pdf(file = "plots/settle_dist_crop_global_conflict_verv2.pdf",   width = 6, height = 6)
      par( mar=c(4,4,1,1)+.1 )
      plot(dc$vervet_c ~ dc$settle_dist_km_std , col=col.alpha(colpal[3], 0.1) , pch=19 , ylab=ylabels[i] , xlab="distance to settlement edge (km)" , xaxt='n' ,  cex.lab=1.3) }
    
  pred_mean <- apply(link2$link , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2$link[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at= ( c(0:12) - mean(dc$settle_dist_km))/sd(dc$settle_dist_km) , labels=c(0:12))
  
  dev.off()
}

##cov70
plot_seq <- seq(from=min(dc$c70_std) , to=max(dc$c70_std) , length=30)

for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=rep(0,30),
    c70_std = plot_seq,
    c2070_std = rep(0,30),
    river_std= rep(0,30),
    road_std= rep(0,30),
    build_dens_std= rep(0,30),
    crop_std= rep(0,30),
    household_size_std=rep(0,30),
    farm_size_std=rep(0,30),
    months_planted_std=rep(0,30),
    see_field=rep(mean(dc$see_field),30),
    species_index=rep(i,30)
  )
  
  link2 <- ensemble(mc17,mc15, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "plots/c70_crop_global_conflict_bab.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$c70_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="forest/thicket density" , xaxt='n' , cex.lab=1.3)}
  if(i==2){
    pdf(file = "plots/c70_crop_global_conflict_ele.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$c70_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="forest/thicket density" ,  xaxt='n' , cex.lab=1.3)}
  if(i==3){
    pdf(file = "plots/c70_crop_global_conflict_ver.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$vervet_c ~ dc$c70_std , col=col.alpha(colpal[3], 0.1) , pch=19 , ylab=ylabels[i] , xlab="forest/thicket density" ,  xaxt='n' , cex.lab=1.3)}
  pred_mean <- apply(link2$link , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2$link[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at= ( seq(from=0 , to=0.25 , by=0.05) - mean(dc$c70))/sd(dc$c70) , labels= seq(from=0 , to=0.25 , by=0.05) )
  dev.off()
}

##cov2070
plot_seq <- seq(from=min(dc$c2070_std) , to=max(dc$c2070_std) , length=30)

for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=rep(0,30),
    c70_std = rep(0,30),
    c2070_std = plot_seq ,
    river_std= rep(0,30),
    road_std= rep(0,30),
    build_dens_std= rep(0,30),
    crop_std= rep(0,30),
    household_size_std=rep(0,30),
    farm_size_std=rep(0,30),
    months_planted_std=rep(0,30),
    see_field=rep(mean(dc$see_field),30),
    species_index=rep(i,30)
  )
  
  link2 <- ensemble(mc17,mc15, data=dpred , replace=list(village_index=av_z) )
  if(i==1){
    pdf(file = "plots/c2070_crop_global_conflict_bab.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$c2070_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="woodland/open thicket/shrubland density" , xaxt='n' , cex.lab=1.3)}
  if(i==2){
    pdf(file = "plots/c2070_crop_global_conflict_ele.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$c2070_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="woodland/open thicket/shrubland density" , xaxt='n' , cex.lab=1.3)}
  if(i==3){
    pdf(file = "plots/c2070_crop_global_conflict_ver.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$vervet_c ~ dc$c2070_std , col=col.alpha(colpal[3], 0.1) , pch=19 , ylab=ylabels[i] , xlab="woodland/open thicket/shrubland density" , xaxt='n' , cex.lab=1.3)}
  pred_mean <- apply(link2$link , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2$link[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
axis( 1 , at= ( seq(from=0 , to=0.7 , by=0.1) - mean(dc$c2070))/sd(dc$c2070) , labels= seq(from=0 , to=0.70 , by=0.1) )
   dev.off()
}

###rivers

plot_seq <- seq(from=min(dc$river_std) , to=max(dc$river_std) , length=30)

for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=rep(0,30),
    c70_std = rep(0,30),
    c2070_std = rep(0,30) ,
    river_std= plot_seq,
    road_std= rep(0,30),
    build_dens_std= rep(0,30),
    crop_std= rep(0,30),
    household_size_std=rep(0,30),
    farm_size_std=rep(0,30),
    months_planted_std=rep(0,30),
    see_field=rep(mean(dc$see_field),30),
    species_index=rep(i,30)
  )
  
  link2 <- ensemble(mc17,mc15, data=dpred , replace=list(village_index=av_z) )
  if(i==1){
    pdf(file = "plots/river_crop_global_conflict_bab.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$river_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="river density" ,  xaxt='n' , cex.lab=1.3)}
  if(i==2){
    pdf(file = "plots/river_crop_global_conflict_ele.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$river_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="river density" ,  xaxt='n' , cex.lab=1.3)}
  if(i==3){
    pdf(file = "plots/river_crop_global_conflict_ver.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$vervet_c ~ dc$river_std , col=col.alpha(colpal[3], 0.1) , pch=19 , ylab=ylabels[i] , xlab="river density" ,  xaxt='n' , cex.lab=1.3)}
  pred_mean <- apply(link2$link , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2$link[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at= ( seq(from=0 , to=0.06 , by=0.01) - mean(dc$river))/sd(dc$river) , labels= seq(from=0 , to=0.06 , by=0.01) )
  dev.off()
}
range(dc$river)

##roads
plot_seq <- seq(from=min(dc$road_std) , to=max(dc$road_std) , length=30)

for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=rep(0,30),
    c70_std = rep(0,30),
    c2070_std = rep(0,30) ,
    river_std= rep(0,30) ,
    road_std= plot_seq,
    build_dens_std= rep(0,30),
    crop_std= rep(0,30),
    household_size_std=rep(0,30),
    farm_size_std=rep(0,30),
    months_planted_std=rep(0,30),
    see_field=rep(mean(dc$see_field),30),
    species_index=rep(i,30)
  )
  
  link2 <- ensemble(mc17,mc15, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "plots/road_crop_global_conflict_bab.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$road_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="road density"  ,xaxt='n' , cex.lab=1.3)}
  if(i==2){
    pdf(file = "plots/road_crop_global_conflict_ele.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$road_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="road density" ,  xaxt='n' , cex.lab=1.3)}
  if(i==3){
    pdf(file = "plots/road_crop_global_conflict_ver.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$vervet_c ~ dc$road_std , col=col.alpha(colpal[3], 0.1) , pch=19 , ylab=ylabels[i] , xlab="road density" ,  xaxt='n' , cex.lab=1.3)}
  pred_mean <- apply(link2$link , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2$link[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at= ( seq(from=0 , to=0.04 , by=0.005) - mean(dc$road))/sd(dc$road) , labels= seq(from=0 , to=0.04 , by=0.005) )
  dev.off()
}
range(dc$road)

##building density

plot_seq <- seq(from=min(dc$build_dens_std) , to=max(dc$build_dens_std) , length=30)


for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=rep(0,30),
    c70_std = rep(0,30) ,
    c2070_std = rep(0,30) ,
    river_std= rep(0,30) ,
    road_std= rep(0,30),
    build_dens_std= plot_seq ,
    crop_std= rep(0,30),
    household_size_std=rep(0,30),
    farm_size_std=rep(0,30),
    months_planted_std=rep(0,30),
    see_field=rep(mean(dc$see_field),30) ,
    species_index=rep(i,30)
  )
  
  link2 <- ensemble(mc17,mc15, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "plots/build_dens_crop_global_conflict_bab.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$build_dens_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="building density" ,  xaxt='n' , cex.lab=1.3)}
  if(i==2){
    pdf(file = "plots/build_dens_crop_global_conflict_ele.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$build_dens_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="building density" ,  xaxt='n' , cex.lab=1.3)}
  if(i==3){
    pdf(file = "plots/build_dens_crop_global_conflict_ver.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$vervet_c ~ dc$build_dens_std , col=col.alpha(colpal[3], 0.1) , pch=19 , ylab=ylabels[i] , xlab="building density" ,  xaxt='n' , cex.lab=1.3)}
  pred_mean <- apply(link2$link , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2$link[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
   axis( 1 , at= ( seq(from=0 , to=250 , by=25) - mean(dc$build_dens))/sd(dc$build_dens) , labels= seq(from=0 , to=250 , by=25) )
  dev.off()
}
##crop

plot_seq <- seq(from=min(dc$crop_std) , to=max(dc$crop_std) , length=30)

for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=rep(0,30),
    c70_std = rep(0,30),
    c2070_std = rep(0,30),
    river_std= rep(0,30),
    road_std= rep(0,30),
    build_dens_std= rep(0,30),
    crop_std= plot_seq,
    household_size_std=rep(0,30),
    farm_size_std=rep(0,30),
    months_planted_std=rep(0,30),
    see_field=rep(mean(dc$see_field),30),
    species_index=rep(i,30)
  )
  
  link2 <- ensemble(mc17,mc15, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "plots/crop_dens_crop_global_conflict_bab.pdf",   width = 6, height = 6)
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$crop_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="crop density" ,  xaxt='n' , cex.lab=1.3)}
 
   if(i==2){
    pdf(file = "plots/crop_dens_crop_global_conflict_ele.pdf",   width = 6, height = 6)
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$crop_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="crop density" ,  xaxt='n' , cex.lab=1.3)}
  
  if(i==3){
    pdf(file = "plots/crop_dens_crop_global_conflict_ver.pdf",   width = 6, height = 6)
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$vervet_c ~ dc$crop_std , col=col.alpha(colpal[3], 0.1) , pch=19 , ylab=ylabels[i] , xlab="crop density" ,  xaxt='n' , cex.lab=1.3)}
  
  pred_mean <- apply(link2$link , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2$link[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at= ( seq(from=0 , to=1 , by=0.1) - mean(dc$crop))/sd(dc$crop) , labels= seq(from=0 , to=1 , by=0.1) )
  dev.off()
}

range(dc$crop)

###hh size
plot_seq <- seq(from=min(dc$household_size_std) , to=max(dc$household_size_std) , length=30)

for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=rep(0,30),
    c70_std = rep(0,30),
    c2070_std = rep(0,30),
    river_std= rep(0,30),
    road_std= rep(0,30),
    build_dens_std= rep(0,30),
    crop_std= rep(0,30),
    household_size_std=plot_seq,
    farm_size_std=rep(0,30),
    months_planted_std=rep(0,30),
    see_field=rep(mean(dc$see_field),30),
    species_index=rep(i,30)
  )
  
  link2 <- ensemble(mc17,mc15, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "plots/hhsize_crop_global_conflict_bab.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$household_size_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size",  xaxt='n' , cex.lab=1.3)}
 
   if(i==2){
    pdf(file = "plots/hhsize_crop_global_conflict_ele.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$household_size_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size",  xaxt='n' , cex.lab=1.3)}
 
   if(i==3){
    pdf(file = "plots/hhsize_crop_global_conflict_ver.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$vervet_c ~ dc$household_size_std , col=col.alpha(colpal[3], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size",  xaxt='n' , cex.lab=1.3)}
  
  pred_mean <- apply(link2$link , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2$link[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at= ( seq(from=0 , to=30 , by=5) - mean(dc$household_size))/sd(dc$household_size) , labels= seq(from=0 , to=30 , by=5) )
  dev.off()
}

##farm size
plot_seq <- seq(from=min(dc$farm_size_std) , to=max(dc$farm_size_std) , length=30)

for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=rep(0,30),
    c70_std = rep(0,30),
    c2070_std = rep(0,30),
    river_std= rep(0,30),
    road_std= rep(0,30),
    build_dens_std= rep(0,30),
    crop_std= rep(0,30),
    farm_size_std=plot_seq,
    household_size_std=rep(0,30),
    farm_size_std=rep(0,30),
    months_planted_std=rep(0,30),
    see_field=rep(mean(dc$see_field),30),
    species_index=rep(i,30)
  )
  
  link2 <- ensemble(mc17,mc15, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "plots/farmsize_crop_global_conflict_bab.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$farm_size_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="farm size" ,  xaxt='n' , cex.lab=1.3)}
  
  if(i==2){
    pdf(file = "plots/farmsize_crop_global_conflict_ele.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$farm_size_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="farm size",  xaxt='n' , cex.lab=1.3)}
 
   if(i==3){
    pdf(file = "plots/farmsize_crop_global_conflict_ver.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$vervet_c ~ dc$farm_size_std , col=col.alpha(colpal[3], 0.1) , pch=19 , ylab=ylabels[i] , xlab="farm size",  xaxt='n' , cex.lab=1.3)}
  
  pred_mean <- apply(link2$link , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2$link[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at= ( seq(from=0 , to=30 , by=5) - mean(dc$farm_size))/sd(dc$farm_size) , labels= seq(from=0 , to=30 , by=5) )
  dev.off()
}

##month planted

plot_seq <- seq(from=min(dc$months_planted_std) , to=max(dc$months_planted_std) , length=30)

for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=rep(0,30),
    c70_std = rep(0,30),
    c2070_std = rep(0,30),
    river_std= rep(0,30),
    road_std= rep(0,30),
    build_dens_std= rep(0,30),
    crop_std= rep(0,30),
    months_planted_std=plot_seq,
    farm_size_std=rep(0,30),
    household_size_std=rep(0,30),
    farm_size_std=rep(0,30),
    months_planted_std=rep(0,30),
    see_field=rep(mean(dc$see_field),30),
    species_index=rep(i,30)
  )
  
  link2 <- ensemble(mc17,mc15, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "plots/monthsplanted_crop_global_conflict_bab.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$months_planted_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="months planted",  xaxt='n' , cex.lab=1.3 )}
  
  if(i==2){
    pdf(file = "plots/monthsplanted_crop_global_conflict_ele.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$months_planted_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="months planted" ,  xaxt='n' , cex.lab=1.3)}
  
  if(i==3){
    pdf(file = "plots/monthsplanted_crop_global_conflict_ver.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$vervet_c ~ dc$months_planted_std , col=col.alpha(colpal[3], 0.1) , pch=19 , ylab=ylabels[i] , xlab="months planted" ,  xaxt='n' , cex.lab=1.3)}  
  
  pred_mean <- apply(link2$link , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2$link[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at= ( seq(from=1 , to=12 , by=1) - mean(dc$months_planted))/sd(dc$months_planted) , labels= seq(from=1 , to=12 , by=1) )
  dev.off()
}

##SEE
dpred <- list(
  village_index=rep(1,6),
  settle_dist_km_std=rep(0,6),
  c70_std = rep(0,6),
  c2070_std = rep(0,6),
  river_std= rep(0,6),
  road_std= rep(0,6),
  build_dens_std= rep(0,6),
  crop_std= rep(0,6),
  see_field=c(0,1,0,1,0,1),
  months_planted_std=rep(0,6),
  farm_size_std=rep(0,6),
  household_size_std=rep(0,6),
  farm_size_std=rep(0,6),
  months_planted_std=rep(0,6),
  species_index=c(1,1,2,2,3,3)
)

link2 <- ensemble(mc17,mc15, data=dpred , replace=list(village_index=av_z) )

pdf(file = "plots/seefield_crop_global_conflict_bab.pdf",   width = 6, height = 6) 
par( mar=c(4,4,1,1)+.1 )
dens(link2$link[,1] , lty=2 , col="blue" , ylim=c(0,15) , xlim=c(0,0.5) , main="probability baboon crop conflict")
abline(v=mean(link2$link[,1]) , col="blue" , lty=2 ,lw=2)
dens(link2$link[,2] , add=TRUE , col="darkblue")
abline(v=mean(link2$link[,2]) , col="darkblue" , lty=1 , lw=2)
legend('topright' , c("field not visible" , "field visible") , col=c("blue" , "darkblue") , lty=c(2,1) , bty='n', lw=2)
dev.off()

pdf(file = "plots/seefield_crop_global_conflict_ele.pdf",   width = 6, height = 6) 
par( mar=c(4,4,1,1)+.1 )
dens(link2$link[,3] , lty=2 , col="darkgrey" , ylim=c(0,15) , xlim=c(0.2,1) , main="probability elephant crop conflict")
abline(v=mean(link2$link[,3]) , col="darkgrey" , lty=2, lw=2)
dens(link2$link[,4] , add=TRUE , col="black" )
abline(v=mean(link2$link[,4]) , col="black" , lty=1, lw=2)
legend('topleft' , c("field not visible" , "field visible") , col=c("darkgrey" , "black") , lty=c(2,1) , bty='n' , lw=2)
dev.off()


pdf(file = "plots/seefield_crop_global_conflict_ver.pdf",   width = 6, height = 6) 
par( mar=c(4,4,1,1)+.1 )
dens(link2$link[,5] , lty=2 , col="seagreen" , ylim=c(0,15) , xlim=c(0.2,1) , main="probability vervet crop conflict")
abline(v=mean(link2$link[,5]) , col="seagreen" , lty=2, lw=2)
dens(link2$link[,6] , add=TRUE , col="black" )
abline(v=mean(link2$link[,6]) , col="black" , lty=1, lw=2)
legend('topleft' , c("field not visible" , "field visible") , col=c("seagreen" , "black") , lty=c(2,1) , bty='n' , lw=2)
dev.off()



# ####global restrict iusing WAIC
# mc17.1 <- ulam(
#   alist(
#     conflict ~ binomial(1,p),
#     logit(p) <- av[village_index] + 
#       as[species_index] + 
#       b_SDs[species_index]*settle_dist_km_std + 
#       b_C70s[species_index]*c70_std + 
#       b_C2070s[species_index]*c2070_std + 
#       b_HHs[species_index]*household_size_std + 
#       b_FSs[species_index]*farm_size_std + 
#       b_SEEs[species_index]*see_field + 
#       b_CRs[species_index]*crop_std,
#     
#     a ~ normal( 0 , 1 ),
#     c(b_SD,b_C70,b_C2070,b_CR) ~ normal( 0 , 0.5 ),
#     c(b_HH,b_FS,b_SEE) ~ normal( 0 , 0.5 ),
#     av[village_index] ~ dnorm(a,sigma_v),
#     c(as,b_SDs,b_C70s,b_C2070s,b_HHs,b_FSs,b_SEEs,b_CRs)[species_index] ~ multi_normal( c(a,b_SD,b_C70,b_C2070,b_HH,b_FS,b_SEE,b_CR) , Rho , sigma_s),
#     c(sigma_v,sigma_s) ~ dexp(1),
#     Rho ~ dlkjcorr(3)
#     
#   ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE)


WAICcropmods <- compare(mc0,mc1,mc2,mc3,mc4,mc5,mc6,mc7,mc8,mc9,mc10,mc11,mc13,mc14,mc15,mc17)
write.csv(WAICcropmods , file="WAICcropmodels.csv")
#compare(mc12.1 , mc12.2, mc12.3, mc12.4, mc12.6 , mc12.7)

# 
# WAICpredmods <-compare(ml1,ml2,ml3,ml4,ml5,ml6,ml7,ml8,ml9,ml10,ml11,ml13,ml14,ml14.1,ml15,ml16.1)
# write.csv(WAICpredmods , file="WAICpredmodels.csv")

##########################################
#############raster preds################
#########################################
###############baboons
ras_bab<-  read.csv("~/Dropbox/tza_wildlife_conflict/baboonRasterstacktopoints_survext.csv")
ras_bab<-  read.csv("baboonRasterstacktopoints_survext.csv")

ras_bab$settle_dist_km <- ras_bab$settle_dist/1000
ras_bab$crop_std <- (ras_bab$crop-mean(dc$crop) )/sd(dc$crop) 
ras_bab$c70_std <- (ras_bab$c70 -mean(dc$c70 ) )/ sd(dc$c70 ) 
ras_bab$c2070_std <- (ras_bab$c2070 -mean(dc$c2070 ) )/ sd(dc$c2070 ) 
ras_bab$river_std <- (ras_bab$river -mean(dc$river ) )/ sd(dc$river) 
ras_bab$road_std <- (ras_bab$road -mean(dc$road ) )/ sd(dc$road) 
ras_bab$build_dens_std <- (ras_bab$build_dens-mean(dc$build_dens ) )/ sd(dc$build_dens) 
ras_bab$settle_dist_km_std <- (ras_bab$settle_dist_km-mean(dc$settle_dist_km ) )/ sd(dc$settle_dist_km) 
ras_bab$species_index <- 1

p <- extract.samples(mc17)


dpred <- list(
  village_index=rep(1,nrow(ras_bab)),
  settle_dist_km_std=ras_bab$settle_dist_km_std,
  c70_std = ras_bab$c70_std,
  c2070_std = ras_bab$c2070_std,
  river_std= ras_bab$river_std,
  road_std= ras_bab$road_std,
  build_dens_std=ras_bab$build_dens_std,
  crop_std= ras_bab$crop_std,
  household_size_std=rep(0,nrow(ras_bab)),
  farm_size_std=rep(0,nrow(ras_bab)),
  months_planted_std=rep(0,nrow(ras_bab)),
  see_field=rep(mean(dc$see_field),nrow(ras_bab)),
  species_index=ras_bab$species_index
)

##note this is with other variables at mean, excluded b/c standardized so mean in 0, conputationally kwiker
ras_bab$pred_bab_crop_conflict <- logistic( mean(p$a + p$as[,1]) + 
                                              mean(p$b_SD + p$b_SDs[,1])*dpred$settle_dist_km_std  + 
                                              mean(p$b_C70 + p$b_C70s[,1])*dpred$c70_std +
                                              mean(p$b_C2070 + p$b_C2070s[,1])*dpred$c2070_std +
                                              mean(p$b_BD + p$b_BDs[,1])*dpred$build_dens_std +
                                              mean(p$b_RD + p$b_RDs[,1])*dpred$crop_std  + 
                                              mean(p$b_RIV + p$b_RIVs[,1])*dpred$crop_std  + 
                                              mean(p$b_CR + p$b_CRs[,1])*dpred$crop_std  + 
                                              mean(p$b_SEE + p$b_SEEs[,1])*dpred$see_field 
)

dens(ras_bab$pred_bab_crop_conflict)
ras_bab_sub <- cbind( ras_bab[1:3] , ras_bab$pred_bab_crop_conflict)
write.csv(ras_bab_sub , file="ras_baboon_crop_predsv.csv")


###elephants###################

ras_ele<-  read.csv("~/Dropbox/tza_wildlife_conflict/elephantRasterstacktopoints_survext.csv")
ras_ele<-  read.csv("elephantRasterstacktopoints_survext.csv")

ras_ele$settle_dist_km <- ras_ele$settle_dist/1000
ras_ele$crop_std <- (ras_ele$crop-mean(dc$crop) )/sd(dc$crop) 
ras_ele$c70_std <- (ras_ele$c70 -mean(dc$c70 ) )/ sd(dc$c70 ) 
ras_ele$c2070_std <- (ras_ele$c2070 -mean(dc$c2070 ) )/ sd(dc$c2070 ) 
ras_ele$river_std <- (ras_ele$river -mean(dc$river ) )/ sd(dc$river) 
ras_ele$road_std <- (ras_ele$road -mean(dc$road ) )/ sd(dc$road) 
ras_ele$build_dens_std <- (ras_ele$build_dens-mean(dc$build_dens ) )/ sd(dc$build_dens) 
ras_ele$settle_dist_km_std <- (ras_ele$settle_dist_km-mean(dc$settle_dist_km ) )/ sd(dc$settle_dist_km) 
ras_ele$species_index <- 2

p <- extract.samples(mc17)

dpred <- list(
    village_index=rep(1,nrow(ras_ele)),
    settle_dist_km_std=ras_ele$settle_dist_km_std,
    c70_std = ras_ele$c70_std,
    c2070_std = ras_ele$c2070_std,
    river_std= ras_ele$river_std,
    road_std= ras_ele$road_std,
    build_dens_std=ras_ele$build_dens_std,
    crop_std= ras_ele$crop_std,
    household_size_std=rep(0,nrow(ras_ele)),
    farm_size_std=rep(0,nrow(ras_ele)),
    months_planted_std=rep(0,nrow(ras_ele)),
    see_field=rep(mean(dc$see_field),nrow(ras_ele)),
    species_index=ras_ele$species_index
  )
  
##note this is with other variables at mean, excluded b/c standardized so mean in 0, conputationally kwiker
ras_ele$pred_ele_crop_conflict <-0
ras_ele$pred_ele_crop_conflict <- logistic( mean(p$a + p$as[,2]) + 
            mean(p$b_SD + p$b_SDs[,2])*dpred$settle_dist_km_std  + 
            mean(p$b_C70 + p$b_C70s[,2])*dpred$c70_std +
            mean(p$b_C2070 + p$b_C2070s[,2])*dpred$c2070_std +
            mean(p$b_BD + p$b_BDs[,2])*dpred$build_dens_std +
            mean(p$b_CR + p$b_CRs[,2])*dpred$crop_std  + 
            mean(p$b_SEE + p$b_SEEs[,2])*dpred$see_field +
            mean(p$b_RD + p$b_RDs[,2])*dpred$crop_std  + 
            mean(p$b_RIV + p$b_RIVs[,2])*dpred$crop_std 

)
dens(ras_ele$pred_ele_crop_conflict)
ras_ele_sub <- cbind( ras_ele[1:3] , ras_ele$pred_ele_crop_conflict)
write.csv(ras_ele_sub , file="ras_elephant_crop_predsv.csv")


#####Vervets baby#####

ras_ver<-  read.csv("~/Dropbox/tza_wildlife_conflict/vervetRasterstacktopoints_survext.csv")
ras_ver<-  read.csv("vervetRasterstacktopoints_survext.csv")

ras_ver$settle_dist_km <- ras_ver$settle_dist/1000
ras_ver$crop_std <- (ras_ver$crop-mean(dc$crop) )/sd(dc$crop) 
ras_ver$c70_std <- (ras_ver$c70 -mean(dc$c70 ) )/ sd(dc$c70 ) 
ras_ver$c2070_std <- (ras_ver$c2070 -mean(dc$c2070 ) )/ sd(dc$c2070 ) 
ras_ver$river_std <- (ras_ver$river -mean(dc$river ) )/ sd(dc$river) 
ras_ver$road_std <- (ras_ver$road -mean(dc$road ) )/ sd(dc$road) 
ras_ver$build_dens_std <- (ras_ver$build_dens-mean(dc$build_dens ) )/ sd(dc$build_dens) 
ras_ver$settle_dist_km_std <- (ras_ver$settle_dist_km-mean(dc$settle_dist_km ) )/ sd(dc$settle_dist_km) 
ras_ver$species_index <- 3

p <- extract.samples(mc17)

dpred <- list(
  village_index=rep(1,nrow(ras_ver)),
  settle_dist_km_std=ras_ver$settle_dist_km_std,
  c70_std = ras_ver$c70_std,
  c2070_std = ras_ver$c2070_std,
  river_std= ras_ver$river_std,
  road_std= ras_ver$road_std,
  build_dens_std=ras_ver$build_dens_std,
  crop_std= ras_ver$crop_std,
  household_size_std=rep(0,nrow(ras_ver)),
  farm_size_std=rep(0,nrow(ras_ver)),
  months_planted_std=rep(0,nrow(ras_ver)),
  see_field=rep(mean(dc$see_field),nrow(ras_ver)),
  species_index=ras_ver$species_index
)

##note this is with other variables at mean, excluded b/c standardized so mean in 0, conputationally kwiker
ras_ver$pred_ver_crop_conflict <-0
ras_ver$pred_ver_crop_conflict <- logistic( mean(p$a + p$as[,3]) + 
                                              mean(p$b_SD + p$b_SDs[,3])*dpred$settle_dist_km_std  + 
                                              mean(p$b_C70 + p$b_C70s[,3])*dpred$c70_std +
                                              mean(p$b_C2070 + p$b_C2070s[,3])*dpred$c2070_std +
                                              mean(p$b_BD + p$b_BDs[,3])*dpred$build_dens_std +
                                              mean(p$b_CR + p$b_CRs[,3])*dpred$crop_std  + 
                                              mean(p$b_SEE + p$b_SEEs[,3])*dpred$see_field +
                                              mean(p$b_RD + p$b_RDs[,3])*dpred$crop_std  + 
                                              mean(p$b_RIV + p$b_RIVs[,3])*dpred$crop_std 
                                            
)
dens(ras_ver$pred_ver_crop_conflict)
ras_ver_sub <- cbind( ras_ver[1:3] , ras_ver$pred_ver_crop_conflict)
write.csv(ras_ver_sub , file="ras_vervet_crop_preds.csv")


###export relevant map2stan objects
save(dc,#WAICcropmods, 
     mc17,mc15, file="relevant_crop_plot_modelsnoWAIC.rdata")
save(dl,WAICstockmod,ml16.1,ml14,file="relevant_livestock_plot_models.rdata")


##############other graphs of imporatnce#

p <- extract.samples(mc17)

p_crop_global <- list(
  b_BD_baboon = p$b_BD + p$b_BDs[,1],
  b_CR_baboon = p$b_CR + p$b_CRs[,1],
  b_C70_baboon = p$b_C70 + p$b_C70s[,1],
  b_C2070_baboon = p$b_C2070 + p$b_C2070s[,1],
  b_RD_baboon = p$b_RD + p$b_RDs[,1],
  b_RIV_baboon = p$b_RIV + p$b_RIVs[,1],
  b_SD_baboon = p$b_SD + p$b_SDs[,1],
  b_FS_baboon = p$b_FS + p$b_FSs[,1],
  b_HH_baboon = p$b_HH + p$b_HHs[,1],
  b_SEE_baboon = p$b_SEE + p$b_SEEs[,1],
  b_BD_elephant = p$b_BD + p$b_BDs[,2],
  b_CR_elephant = p$b_CR + p$b_CRs[,2],
  b_C70_elephant = p$b_C70 + p$b_C70s[,2],
  b_C2070_elephant = p$b_C2070 + p$b_C2070s[,2],
  b_RD_elephant = p$b_RD + p$b_RDs[,2],
  b_RIV_elephant = p$b_RIV + p$b_RIVs[,2],
  b_SD_elephant = p$b_SD + p$b_SDs[,2],
  b_FS_elephant = p$b_FS + p$b_FSs[,2],
  b_HH_elephant = p$b_HH + p$b_HHs[,2],
  b_SEE_elephant = p$b_SEE + p$b_SEEs[,2]
  )
 
pdf(file = "crop_conflict_species_parameter_dotplots.pdf",   width = 7, height = 7) 
plot(precis(p_crop_global))
dev.off()

write.csv( precis(mc17 , depth=2) , file="crop_global_model_medium_paramsv.csv" )
write.csv( precis(mc17 , depth=3) , file="crop_global_model_long_paramsv.csv" )

