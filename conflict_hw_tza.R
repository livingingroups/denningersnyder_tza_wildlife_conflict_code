library('rethinking')
library('rstan')
require(rethinking)
require(lubridate)
require(RColorBrewer)
library(janitor)

# library(lme4)
# vv <- glmer(conflict ~ (1|ethnicity) , data=hw , family="binomial")
# vv <- glmer(conflict ~ born + (1|village) + (1+born|species), data=hw , family="binomial")
# vv <- glmer(conflict ~ mon + (1|village) + (1+born|species), data=hw , family="binomial")
# ranef(vv)
# summary(vv)

# hw <- read.csv("~/Downloads/SS_Final_NoDups.csv") #old version with errorz
#hw <- read.csv("~/Downloads/Spatial_Household_Survey_Clean_1Jun20sp.csv") 
hw <-  read.csv("~/Dropbox/tza_wildlife_conflict/HWc_surveyClean_extract_envslopbuild.csv")

hw[,42] <- ifelse(hw[,42] < 0 , 0 , hw[,42])
hw[,43] <- ifelse(hw[,43] < 0 , 0 , hw[,43])
hw[,44] <- ifelse(hw[,44] < 0 , 0 , hw[,44])
hw[,45] <- ifelse(hw[,45] < 0 , 0 , hw[,45])
hw[,46] <- ifelse(hw[,46] < 0 , 0 , hw[,46])
hw[,47] <- ifelse(hw[,47] < 0 , 0 , hw[,47])
hw[,48] <- ifelse(hw[,48] < 0 , 0 , hw[,48])
hw[,49] <- ifelse(hw[,48] < 0 , 0 , hw[,49])

hw$guard_ave_day <- ifelse(hw$guard_ave_day < 0 , 0 , hw$guard_ave_day)
hw$livestock_head <- hw[,42] + hw[,43] +hw[,44] + hw[,45] 
hw$livestock_head_all <- hw[,42] + hw[,43] +hw[,44] + hw[,45] + hw[,46] +  hw[,47] +  hw[,48] 

hw[hw == "-2147483648"] <- "NA"

hw$farm <- ifelse(hw$farm=="Yes" , 1 , 0)
hw$livestock <- ifelse(hw$livestock=="Yes" , 1 , 0)
hw$see_field <- ifelse(hw$field_sight=="Yes" , 1 , 0)
hw$months_planted <- as.integer(hw$months_planted)

##protection strategies
sort(unique(hw$med_lar_lv_prot))
hw$med_lar_lv_prot <- as.character(hw$med_lar_lv_prot)
sort(unique(hw$med_lar_lv_prot))

hw$lv_prot_day_guard <- ifelse( grepl( "Day_guard", hw$med_lar_lv_prot) , 1 , 0 )
hw$lv_prot_day_dogs <- ifelse( grepl( "Day_dogs", hw$med_lar_lv_prot) , 1 , 0 )
hw$lv_prot_night_dogs <- ifelse( grepl( "Night_dogs",hw$med_lar_lv_prot) , 1 , 0 )
hw$lv_prot_night_contain <- ifelse( grepl( "Night_contain", hw$med_lar_lv_prot) , 1 , 0 )
#other is excluded

sort(unique(hw$crop_prot))
hw$crop_prot <- as.character(hw$crop_prot)
sort(unique(hw$crop_prot))
hw$crop_prot_guard <- ifelse( grepl( "Guarding", hw$crop_prot) , 1 , 0 )
hw$crop_prot_chase <- ifelse( grepl( "Chasing", hw$crop_prot) , 1 , 0 )
hw$crop_prot_fire <- ifelse( grepl( "Fire", hw$crop_prot ) , 1 , 0 )
hw$crop_prot_shout <- ifelse( grepl( "Shouting", hw$crop_prot ) , 1 , 0 )
hw$crop_prot_sisal <- ifelse( grepl( "Sisal", hw$crop_prot ) , 1 , 0 )
hw$crop_prot_w_fence <- ifelse( grepl( "Wire_fence", hw$crop_prot ) , 1 , 0 )
hw$crop_prot_music <- ifelse( grepl( "Music", hw$crop_prot ) , 1 , 0 )
hw$crop_prot_none<- ifelse( grepl( "None", hw$crop_prot) , 1 , 0 )
hw$num_crop_prot_strats <- hw$crop_prot_guard + hw$crop_prot_chase + hw$crop_prot_fire + hw$crop_prot_shout + hw$crop_prot_sisal + hw$crop_prot_w_fence + hw$crop_prot_music 
hw$household_size <- as.integer(hw$household_size)


d <- clean_names(hw)

d$med_lar_lv_prot
d$months_planted

##need household ID
myvars <- c("conflict" , "village", "species" , "elephant_c", "baboon_c" , "hyena_l" , "lion_l" , "farm_walk" , "farm_size" , "household_size" , "fid" , "settle_dist" , "see_field" , "c70" , "c2070" ,"river" , "road" , "crop" , "gse_slope30m" ,"build_dens" , "cattle" , "sheep" , "goat" , "donkey" , "farm" , "livestock" , "months_planted" , "lv_prot_day_guard" , "lv_prot_day_dogs" , "lv_prot_night_dogs" , "lv_prot_night_contain" , "crop_prot_music" , "crop_prot_w_fence" , "crop_prot_sisal" , "crop_prot_shout" , "crop_prot_fire" , "crop_prot_chase" , "crop_prot_guard" , "guard_ave_day" , "num_crop_prot_strats" , "livestock_head")
#fthese are all the variables we are interested in 
d <- d[myvars]


sort(unique(hw$Village)) #check this
###create index variables for each village

d[d == "-2147483648"] <- "NA"

##############################CROP DAMAGE##############################

#fthese are all the variables we are interested in 

myvars2 <- c("conflict" , "village", "elephant_c", "baboon_c" , "farm_size" , "household_size" , "fid" , "settle_dist" , "see_field" , "c70" , "c2070" ,"river" , "road" , "crop" ,"build_dens" , "farm"  , "months_planted"  , "crop_prot_music" , "crop_prot_w_fence" , "crop_prot_sisal" , "crop_prot_shout" , "crop_prot_fire" , "crop_prot_chase" , "crop_prot_guard" , "species" , "num_crop_prot_strats" )

dc <- d[myvars2]

dc <- dc[dc$species!="lion",]
dc <- dc[dc$farm==1,] #only look at conflicts with households that have farms


nrow(dc)
str(dc)
dc <- dc[complete.cases(dc), ] ##we will impute later but we lose 34 observations
dc <- droplevels(dc)
dc$crop_std <- (dc$crop-mean(dc$crop) )/sd(dc$crop)
dc$settle_dist_km <- dc$settle_dist/1000
dc$settle_dist_km_std <- (dc$settle_dist_km-mean(dc$settle_dist_km) )/sd(dc$settle_dist_km)
dc$farm_size_std <- (dc$farm_size -mean(dc$farm_size ) )/sd(dc$farm_size)
dc$c70_std <- (dc$c70-mean(dc$c70) )/sd(dc$c70)
dc$c2070_std <- (dc$c2070-mean(dc$c2070) )/sd(dc$c2070)
dc$village <- as.character(dc$village)
dc$village[dc$fid==179] <- "Nyamatoke_jklol"
dc$village_index <- as.integer(as.factor(dc$village))
dc$river_std <- (dc$river-mean(dc$river) )/sd(dc$river) #
dc$crop_std <- (dc$crop-mean(dc$crop) )/sd(dc$crop) 
dc$build_dens_std <- (dc$build_dens-mean(dc$build_dens) )/sd(dc$build_dens) ##nonlinear
dc$household_size_std <- (dc$household_size-mean(dc$household_size) )/sd(dc$household_size)
dc$road_std <- (dc$road-mean(dc$road) )/sd(dc$road) 
dc$months_planted_std <- (dc$months_planted-mean(dc$months_planted) )/sd(dc$months_planted) 
dc$num_crop_prot_strats_std <- (dc$num_crop_prot_strats-mean(dc$num_crop_prot_strats) )/sd(dc$num_crop_prot_strats) 
nrow(dc)

dc$species_index <- as.integer(as.factor(dc$species))
dc$village_index <- as.integer(as.factor(dc$village))

###models


mc0 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] ,
    a ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(a,sigma_v),
    as[species_index] ~ dnorm(a,sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik = TRUE)

post <- extract.samples(mc1)
str(post)
dens( logistic(post$a + post$as[,1]) , col="red" , xlim=c(0,1) , ylim=c(0,230)) 
dens( logistic(post$a + post$as[,2] ), col="orange" , add=TRUE) 

precis(mc0, depth=2)

precislist <- list(
  PrBaboon = logistic(post$a + post$as[,1] ),
  PrElephant =logistic(post$a + post$as[,2])
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
mc1 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] +  b_CRs[species_index]*crop_std ,
    a ~ normal( 0 , 1 ),
    b_CR ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_CRs)[species_index] ~ multi_normal( c(a,b_CR) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(mc1, depth=2)


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
  
  link2 <- link(mc1, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dc$baboon_c ~ dc$crop_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="crop density standardized")}
  if(i==2){plot(dc$elephant_c ~ dc$crop_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="crop density standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}
dens(logistic(post$av[,1] + post$a))
precis(mc0 , depth=3)

##settlement distance
mc2 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] +  b_SDs[species_index]*settle_dist_km_std ,
    a ~ normal( 0 , 1 ),
    b_SD ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_SDs)[species_index] ~ multi_normal( c(a,b_SD) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

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
mc3 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_HHs[species_index]*household_size_std,
    a ~ normal( 0 , 1 ),
    b_HH ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_HHs)[species_index] ~ multi_normal( c(a,b_HH) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

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
mc4 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_FSs[species_index]*farm_size_std ,
    a ~ normal( 0 , 1 ),
    b_FS ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_FSs)[species_index] ~ multi_normal( c(a,b_FS) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

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
mc5 <- ulam(
  alist(
    conflict  ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index]  + b_SEEs[species_index]*see_field ,
    a ~ normal( 0 , 1 ),
    b_SEE ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_SEEs)[species_index] ~ multi_normal( c(a,b_SEE) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
  ), data=dc , chains=4 , cores=4 , iter=5000 , log_lik=TRUE)


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
mc6 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_C70s[species_index]*c70_std ,
    a ~ normal( 0 , 1 ),
    b_C70 ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_C70s)[species_index] ~ multi_normal( c(a,b_C70) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=5000 , log_lik=TRUE)

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
mc7 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_C2070s[species_index]*c2070_std ,
    a ~ normal( 0 , 1 ),
    b_C2070 ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_C2070s)[species_index] ~ multi_normal( c(a,b_C2070) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=5000 , log_lik=TRUE)

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
mc8 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_RIVs[species_index]*river_std,
    a ~ normal( 0 , 1 ),
    b_RIV ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_RIVs)[species_index] ~ multi_normal( c(a,b_RIV) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

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
mc9 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_BDs[species_index]*build_dens_std,
    a ~ normal( 0 , 1 ),
    b_BD ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_BDs)[species_index] ~ multi_normal( c(a,b_BD) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

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
mc10 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_RDs[species_index]*road_std,
    a ~ normal( 0 , 1 ),
    b_RD ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_RDs)[species_index] ~ multi_normal( c(a,b_RD) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

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
mc11 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_MPs[species_index]*months_planted_std,
    a ~ normal( 0 , 1 ),
    b_MP ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_MPs)[species_index] ~ multi_normal( c(a,b_MP) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)

  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

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

mc12 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_CPMs[species_index]*crop_prot_music + b_CPWFs[species_index]*crop_prot_w_fence
               + b_CPSIs[species_index]*crop_prot_sisal + b_CPSHs[species_index]*crop_prot_shout + b_CPFs[species_index]*crop_prot_fire
               + b_CPCs[species_index]*crop_prot_chase + b_CPGs[species_index]*crop_prot_guard ,
    a ~ normal( 0 , 1 ),
    c(b_CPM,b_CPWF,bCPSI,b_CPSH,b_CPF,b_CPC,b_CPG) ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_CPMs,b_CPWFs,b_CPSIs,b_CPSHs,b_CPFs,b_CPCs,b_CPGs)[species_index] ~ multi_normal( c(a,b_CPM,b_CPWF,bCPSI,b_CPSH,b_CPF,b_CPC,b_CPG) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(mc12)
precis(mc12 , depth=3)

#traceplot(mc12)

dpred <- list(
  village_index=rep(1,8),
  crop_prot_music=c(1,mean(dc$crop_prot_music),mean(dc$crop_prot_music),mean(dc$crop_prot_music),mean(dc$crop_prot_music),mean(dc$crop_prot_music),mean(dc$crop_prot_music),mean(dc$crop_prot_music)),
  crop_prot_chase=c(mean(d$crop_prot_chase),1,mean(d$crop_prot_chase),mean(d$crop_prot_chase),mean(d$crop_prot_chase),mean(d$crop_prot_chase),mean(d$crop_prot_chase),mean(d$crop_prot_chase)),
  crop_prot_w_fence=c(mean(d$crop_prot_w_fence),mean(d$crop_prot_w_fence),1,mean(d$crop_prot_w_fence),mean(d$crop_prot_w_fence),mean(d$crop_prot_w_fence),mean(d$crop_prot_w_fence),mean(d$crop_prot_w_fence)),
  crop_prot_sisal=c(mean(d$crop_prot_sisal),mean(d$crop_prot_sisal),mean(d$crop_prot_sisal),1,mean(d$crop_prot_sisal),mean(d$crop_prot_sisal),mean(d$crop_prot_sisal),mean(d$crop_prot_sisal)),
  crop_prot_shout=c(mean(d$crop_prot_shout),mean(d$crop_prot_shout),mean(d$crop_prot_shout),mean(d$crop_prot_shout),1,mean(d$crop_prot_shout),mean(d$crop_prot_shout),mean(d$crop_prot_shout)),
  crop_prot_fire=c(mean(d$crop_prot_fire),mean(d$crop_prot_fire),mean(d$crop_prot_fire),mean(d$crop_prot_fire),mean(d$crop_prot_fire),1,mean(d$crop_prot_fire),mean(d$crop_prot_fire)),
  crop_prot_guard=c(mean(d$crop_prot_guard),mean(d$crop_prot_guard),mean(d$crop_prot_guard),mean(d$crop_prot_guard),mean(d$crop_prot_guard),mean(d$crop_prot_guard),1,mean(d$crop_prot_guard)),
  species_index=rep(1,8)
)


# dpred <- list(
#   village_index=rep(1,8),
#   crop_prot_music=c(1,0,0,0,0,0,0,0),
#   crop_prot_chase=c(0,1,0,0,0,0,0,0),
#   crop_prot_w_fence=c(0,0,1,0,0,0,0,0),
#   crop_prot_sisal=c(0,0,0,1,0,0,0,0),
#   crop_prot_shout=c(0,0,0,0,1,0,0,0),
#   crop_prot_fire=c(0,0,0,0,0,1,0,0),
#   crop_prot_guard=c(0,0,0,0,0,0,1,0),
#   species_index=rep(1,8)
# )


link2 <- link(mc12, data=dpred , replace=list(village_index=av_z) )
str(link2)
precis(link2)

precislist <- list(
  crop_prot_music=link2[,1],
  crop_prot_chase=link2[,2],
  crop_prot_w_fence=link2[,3],
  crop_prot_sisal=link2[,4],
  crop_prot_shout=link2[,5],
  crop_prot_fire=link2[,6],
  crop_prot_guard=link2[,7]
)
  

plot(precis(precislist , ci=.89) , xlab="Probabity Baboon Crop Conflict" )

dpred <- list(
  village_index=rep(1,8),
  crop_prot_music=c(1,mean(dc$crop_prot_music),mean(dc$crop_prot_music),mean(dc$crop_prot_music),mean(dc$crop_prot_music),mean(dc$crop_prot_music),mean(dc$crop_prot_music),mean(dc$crop_prot_music)),
  crop_prot_chase=c(mean(d$crop_prot_chase),1,mean(d$crop_prot_chase),mean(d$crop_prot_chase),mean(d$crop_prot_chase),mean(d$crop_prot_chase),mean(d$crop_prot_chase),mean(d$crop_prot_chase)),
  crop_prot_w_fence=c(mean(d$crop_prot_w_fence),mean(d$crop_prot_w_fence),1,mean(d$crop_prot_w_fence),mean(d$crop_prot_w_fence),mean(d$crop_prot_w_fence),mean(d$crop_prot_w_fence),mean(d$crop_prot_w_fence)),
  crop_prot_sisal=c(mean(d$crop_prot_sisal),mean(d$crop_prot_sisal),mean(d$crop_prot_sisal),1,mean(d$crop_prot_sisal),mean(d$crop_prot_sisal),mean(d$crop_prot_sisal),mean(d$crop_prot_sisal)),
  crop_prot_shout=c(mean(d$crop_prot_shout),mean(d$crop_prot_shout),mean(d$crop_prot_shout),mean(d$crop_prot_shout),1,mean(d$crop_prot_shout),mean(d$crop_prot_shout),mean(d$crop_prot_shout)),
  crop_prot_fire=c(mean(d$crop_prot_fire),mean(d$crop_prot_fire),mean(d$crop_prot_fire),mean(d$crop_prot_fire),mean(d$crop_prot_fire),1,mean(d$crop_prot_fire),mean(d$crop_prot_fire)),
  crop_prot_guard=c(mean(d$crop_prot_guard),mean(d$crop_prot_guard),mean(d$crop_prot_guard),mean(d$crop_prot_guard),mean(d$crop_prot_guard),mean(d$crop_prot_guard),1,mean(d$crop_prot_guard)),
  species_index=rep(2,8)
)

##kate says wire_fencing, sisal, music
link2 <- link(mc12, data=dpred , replace=list(village_index=av_z) )
str(link2)

precislist <- list(
  crop_prot_music=link2[,1],
  crop_prot_chase=link2[,2],
  crop_prot_w_fence=link2[,3],
  crop_prot_sisal=link2[,4],
  crop_prot_shout=link2[,5],
  crop_prot_fire=link2[,6],
  crop_prot_guard=link2[,7],
  crop_prot_none=link2[,8]
  
)


plot(precis(precislist , ci=.89) , xlab="Probabity Elephant Crop Conflict" )

#wire fence
mc12.1 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_CPWFs[species_index]*crop_prot_w_fence,
    a ~ normal( 0 , 1 ),
    b_CPWF ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_CPWFs)[species_index] ~ multi_normal( c(a,b_CPWF) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)


dpred <- list(
  village_index=rep(1,4),
  crop_prot_w_fence=c(0,1,0,1),
  species_index=c(1,1,2,2)
)

link2 <- link(mc12.1, data=dpred , replace=list(village_index=av_z) )

precislist <- list(
  baboon_crop_prot_no_wfence=link2[,1],
  baboon_crop_prot_wfence=link2[,2],
  elephant_crop_prot_no_wfence=link2[,3],
  elephant_crop_prot_wfence=link2[,4]
)

plot(precis(precislist , ci=.89) , xlab="Probabity Crop Conflict" )

#sisal
mc12.2 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_CPSIs[species_index]*crop_prot_sisal, 
    a ~ normal( 0 , 1 ),
    bCPSI ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_CPSIs)[species_index] ~ multi_normal( c(a,bCPSI) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

dpred <- list(
  village_index=rep(1,4),
  crop_prot_sisal=c(0,1,0,1),
  species_index=c(1,1,2,2)
)

link2 <- link(mc12.2, data=dpred , replace=list(village_index=av_z) )

precislist <- list(
  baboon_crop_prot_no_sisal=link2[,1],
  baboon_crop_prot_sisal=link2[,2],
  elephant_crop_prot_no_sisal=link2[,3],
  elephant_crop_prot_sisal=link2[,4]
)

plot(precis(precislist , ci=.89) , xlab="Probabity Crop Conflict" )
##fire
mc12.3 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_CPFs[species_index]*crop_prot_fire,
    a ~ normal( 0 , 1 ),
    b_CPF ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_CPFs)[species_index] ~ multi_normal( c(a,b_CPF) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

dpred <- list(
  village_index=rep(1,4),
  crop_prot_fire=c(0,1,0,1),
  species_index=c(1,1,2,2)
)

link2 <- link(mc12.3, data=dpred , replace=list(village_index=av_z) )

precislist <- list(
  baboon_crop_prot_no_fire=link2[,1],
  baboon_crop_prot_fire=link2[,2],
  elephant_crop_prot_no_fire=link2[,3],
  elephant_crop_prot_fire=link2[,4]
)

plot(precis(precislist , ci=.89) , xlab="Probabity Crop Conflict" )

#guard
mc12.4 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] +  b_CPGs[species_index]*crop_prot_guard ,
    a ~ normal( 0 , 1 ),
    b_CPG ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_CPGs)[species_index] ~ multi_normal( c(a,b_CPG) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

dpred <- list(
  village_index=rep(1,4),
  crop_prot_guard=c(0,1,0,1),
  species_index=c(1,1,2,2)
)

link2 <- link(mc12.4, data=dpred , replace=list(village_index=av_z) )

precislist <- list(
  baboon_crop_prot_no_guard=link2[,1],
  baboon_crop_prot_guard=link2[,2],
  elephant_crop_prot_no_guard=link2[,3],
  elephant_crop_prot_guard=link2[,4]
)

plot(precis(precislist , ci=.89) , xlab="Probabity Crop Conflict" )

mc12.6 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_NCPSs[species_index]*num_crop_prot_strats_std,
    a ~ normal( 0 , 1 ),
    b_NCPS ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_NCPSs)[species_index] ~ multi_normal( c(a,b_NCPS) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(mc12.6, depth=2)

plot_seq <- seq(from=min(dc$num_crop_prot_strats_std) , to=max(dc$num_crop_prot_strats_std) , length=30)


for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    num_crop_prot_strats_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(mc12.6, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dc$baboon_c ~ dc$num_crop_prot_strats_std, col=col.alpha(colpal[1], 0.01) , pch=19 , ylab=ylabels[i] , xlab="num crop protection strategies stdized")}
  if(i==2){plot(dc$elephant_c ~ dc$num_crop_prot_strats_std , col=col.alpha(colpal[2], 0.01) , pch=19 , ylab=ylabels[i] , xlab="num crop protection strategies stdized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

#guarding and fire inteaction
mc12.7 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_CPGs[species_index]*crop_prot_guard + (b_CPFs[species_index] + bCPGxbCPFs[species_index]*crop_prot_guard )*crop_prot_fire,
    a ~ normal( 0 , 1 ),
    c(b_CPG,b_CPF,bCPGxbCPF) ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_CPGs,b_CPFs,bCPGxbCPFs)[species_index] ~ multi_normal( c(a,b_CPG,b_CPF,bCPGxbCPF) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(mc12.7, depth=2)

dpred <- list(
  village_index=rep(1,8),
  crop_prot_guard=c(0,1,0,1,0,1,0,1),
  crop_prot_fire= c(0,0,1,1,0,0,1,1),
  species_index=c(1,1,1,1,2,2,2,2)
)

link2 <- link(mc12.7, data=dpred , replace=list(village_index=av_z) )

str(link2)
precislist <- list(
  baboon_crop_prot_no_fire_or_guard=link2[,1],
  baboon_crop_prot_guard=link2[,2],
  baboon_crop_prot_fire=link2[,3],
  baboon_crop_prot_guard_and_fire=link2[,4],
  elephant_crop_prot_no_fire_or_guard=link2[,5],
  elephant_crop_prot_guard=link2[,6],
  elephant_crop_prot_fire=link2[,7],
  elephant_crop_prot_guard_and_fire=link2[,8]
)

plot(precis(precislist , ci=.89) , xlab="Probabity Crop Conflict" )


########## add in settle dist to fire pen interaction model
mc12.8 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_CPGs[species_index]*crop_prot_guard + (b_CPFs[species_index] + bCPGxbCPFs[species_index]*crop_prot_guard )*crop_prot_fire + b_SDs[species_index]*settle_dist_km_std,
    a ~ normal( 0 , 1 ),
    c(b_CPG,b_CPF,bCPGxbCPF,b_SD) ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_CPGs,b_CPFs,bCPGxbCPFs,b_SDs)[species_index] ~ multi_normal( c(a,b_CPG,b_CPF,bCPGxbCPF,b_SD) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

dpred <- list(
  village_index=rep(1,8),
  settle_dist_km_std=rep(0,8),
  crop_prot_guard=c(0,1,0,1,0,1,0,1),
  crop_prot_fire= c(0,0,1,1,0,0,1,1),
  species_index=c(1,1,1,1,2,2,2,2)
)

link2 <- link(mc12.8, data=dpred , replace=list(village_index=av_z) )

str(link2)
precislist <- list(
  baboon_crop_prot_no_fire_or_guard=link2[,1],
  baboon_crop_prot_guard=link2[,2],
  baboon_crop_prot_fire=link2[,3],
  baboon_crop_prot_guard_and_fire=link2[,4],
  elephant_crop_prot_no_fire_or_guard=link2[,5],
  elephant_crop_prot_guard=link2[,6],
  elephant_crop_prot_fire=link2[,7],
  elephant_crop_prot_guard_and_fire=link2[,8]
)

plot(precis(precislist , ci=.89) , xlab="Probabity Crop Conflict" )

mc12.9 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_CPGs[species_index]*crop_prot_guard + (b_CPFs[species_index] + bCPGxbCPFs[species_index]*crop_prot_guard*settle_dist_km_std )*crop_prot_fire + b_SDs[species_index]*settle_dist_km_std,
    a ~ normal( 0 , 1 ),
    c(b_CPG,b_CPF,bCPGxbCPF,b_SD) ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_CPGs,b_CPFs,bCPGxbCPFs,b_SDs)[species_index] ~ multi_normal( c(a,b_CPG,b_CPF,bCPGxbCPF,b_SD) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)


dpred <- list(
  village_index=rep(1,8),
  settle_dist_km_std=rep(2,8),
  crop_prot_guard=c(0,1,0,1,0,1,0,1),
  crop_prot_fire= c(0,0,1,1,0,0,1,1),
  species_index=c(1,1,1,1,2,2,2,2)
)

link2 <- link(mc12.9, data=dpred , replace=list(village_index=av_z) )

str(link2)
precislist <- list(
  baboon_crop_prot_no_fire_or_guard=link2[,1],
  baboon_crop_prot_guard=link2[,2],
  baboon_crop_prot_fire=link2[,3],
  baboon_crop_prot_guard_and_fire=link2[,4],
  elephant_crop_prot_no_fire_or_guard=link2[,5],
  elephant_crop_prot_guard=link2[,6],
  elephant_crop_prot_fire=link2[,7],
  elephant_crop_prot_guard_and_fire=link2[,8]
)

plot(precis(precislist , ci=.89) , xlab="Probabity Crop Conflict" )

##interactionmodels
mc13 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_HHs[species_index]*household_size_std + (b_FSs[species_index] + b_HHxFSs[species_index]*household_size_std )*farm_size_std,
    a ~ normal( 0 , 1 ),
    c(b_HH,b_FS,b_HHxFS) ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_HHs,b_FSs,b_HHxFSs)[species_index] ~ multi_normal( c(a,b_HH,b_FS,b_HHxFS) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE )

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

mc14 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_HHs[species_index]*household_size_std  + b_FSs[species_index]*farm_size_std + b_MPs[species_index]*months_planted_std + b_SEEs[species_index]*see_field,
    a ~ normal( 0 , 1 ),
    c(b_HH,b_FS,b_MP,b_SEE) ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_HHs,b_FSs,b_MPs,b_SEEs)[species_index] ~ multi_normal( c(a,b_HH,b_FS,b_MP,b_SEE) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=5000 , log_lik=TRUE)

precis(mc14)
###hh size

###pick up here
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



compare(mc0,mc1,mc2,mc3,mc4,mc5,mc6,mc7,mc8,mc9,mc10,mc11,mc12,mc13,mc14,mc15,mc12.3,mc12.4,mc12.9)

#############landscape variables####################################
mc15 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] +  b_SDs[species_index]*settle_dist_km_std + b_C70s[species_index]*c70_std + b_C2070s[species_index]*c2070_std + b_RIVs[species_index]*river_std  + b_RDs[species_index]*road_std + b_BDs[species_index]*build_dens_std +  b_CRs[species_index]*crop_std,
    a ~ normal( 0 , 1 ),
    c(b_SD,b_C70,b_C2070,b_RIV,b_RD,b_BD,b_CR) ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_SDs,b_C70s,b_C2070s,b_RIVs,b_RDs,b_BDs,b_CRs)[species_index] ~ multi_normal( c(a,b_SD,b_C70,b_C2070,b_RIV,b_RD,b_BD,b_CR) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(mc15)

#plot distance to settlement
plot_seq <- seq(from=min(dc$settle_dist_km_std) , to=max(dc$settle_dist_km_std) , length=30)
av_z <- matrix(0,1000,length(unique(dc$village_index))) #need to add zeros in VE to plot main effect


ylabels=c("probability baboon crop conflict" , "probability elephant crop conflict")
colpal=c("blue" , "grey")
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
  
  if(i==1){plot(dc$baboon_c ~ dc$settle_dist_km_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="standardize km from settlement")}
  if(i==2){plot(dc$elephant_c ~ dc$settle_dist_km_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="standardize km from settlement")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

##cov70
plot_seq <- seq(from=min(dc$c70_std) , to=max(dc$c70_std) , length=30)

for (i in 1:2){
  
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
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}


##cov2070
plot_seq <- seq(from=min(dc$c2070_std) , to=max(dc$c2070_std) , length=30)

for (i in 1:2){
  
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

#########what is this

mc16 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] +  b_SDs[species_index]*settle_dist_km_std + b_C70s[species_index]*c70_std + b_C2070s[species_index]*c2070_std + b_RIVs[species_index]*river_std  + b_RDs[species_index]*road_std,
    a ~ normal( 0 , 1 ),
    c(b_SD,b_C70,b_C2070,b_RIV,b_RD) ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_SDs,b_C70s,b_C2070s,b_RIVs,b_RDs)[species_index] ~ multi_normal( c(a,b_SD,b_C70,b_C2070,b_RIV,b_RD) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)


############################global########################
mc17 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + 
      as[species_index] + 
      b_SDs[species_index]*settle_dist_km_std + 
      b_C70s[species_index]*c70_std + 
      b_C2070s[species_index]*c2070_std + 
      b_BDs[species_index]*build_dens_std + 
      b_HHs[species_index]*household_size_std + 
      b_FSs[species_index]*farm_size_std + 
      b_SEEs[species_index]*see_field + 
      b_CRs[species_index]*crop_std,
    
    a ~ normal( 0 , 1 ),
    c(b_SD,b_C70,b_C2070,b_CR,b_BD) ~ normal( 0 , 0.5 ),
    c(b_HH,b_FS,b_SEE) ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_SDs,b_C70s,b_C2070s,b_HHs,b_FSs,b_SEEs,b_CRs,b_BDs)[species_index] ~ multi_normal( c(a,b_SD,b_C70,b_C2070,b_HH,b_FS,b_SEE,b_CR,b_BD) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=4000 , log_lik=TRUE , control=list(adapt_delta=0.999))



#plot distance to settlement
plot_seq <- seq(from=min(dc$settle_dist_km_std) , to=max(dc$settle_dist_km_std) , length=30)
av_z <- matrix(0,1000,length(unique(dc$village_index))) #need to add zeros in VE to plot main effect


ylabels=c("probability baboon crop conflict" , "probability elephant crop conflict")
colpal=c("slateblue" , "darkgrey")
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
    household_size_std=rep(0,30),
    farm_size_std=rep(0,30),
    months_planted_std=rep(0,30),
    see_field=rep(mean(dc$see_field),30),
    species_index=rep(i,30)
  )
  
  link2 <- link(mc17, data=dpred , replace=list(village_index=av_z) )
    par( mar=c(4,4,1,1)+.1 )
  
  if(i==1){
    pdf(file = "settle_dist_crop_global_conflict_bab.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$settle_dist_km_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="standardized km from settlement")}
  if(i==2){
    pdf(file = "settle_dist_crop_global_conflict_ele.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$settle_dist_km_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="standardized km from settlement")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  dev.off()
}
 dev.off()

##cov70
plot_seq <- seq(from=min(dc$c70_std) , to=max(dc$c70_std) , length=30)

for (i in 1:2){
  
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
  
  link2 <- link(mc17, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "c70_crop_global_conflict_bab.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$c70_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="Tree Cover 70 percent standardized")}
  if(i==2){
    pdf(file = "c70_crop_global_conflict_ele.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$c70_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="Tree Cover 70 percent standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  dev.off()
}
dev.off()

##cov2070
plot_seq <- seq(from=min(dc$c2070_std) , to=max(dc$c2070_std) , length=30)

for (i in 1:2){
  
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
  
  link2 <- link(mc17, data=dpred , replace=list(village_index=av_z) )
  if(i==1){
    pdf(file = "c2070_crop_global_conflict_bab.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$c2070_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="Tree Cover 20/70 percent standardized")}
  if(i==2){
    pdf(file = "c2070_crop_global_conflict_ele.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$c2070_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="Tree Cover 20/70 percent standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  dev.off()
}
dev.off()

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
    household_size_std=rep(0,30),
    farm_size_std=rep(0,30),
    months_planted_std=rep(0,30),
    see_field=rep(mean(dc$see_field),30),
    species_index=rep(i,30)
  )
  
  link2 <- link(mc17, data=dpred , replace=list(village_index=av_z) )
  if(i==1){
    pdf(file = "river_crop_global_conflict_bab.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$river_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="river density standardized")}
  if(i==2){
    pdf(file = "river_crop_global_conflict_ele.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$river_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="river density standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  dev.off()
}
dev.off()

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
    household_size_std=rep(0,30),
    farm_size_std=rep(0,30),
    months_planted_std=rep(0,30),
    see_field=rep(mean(dc$see_field),30),
    species_index=rep(i,30)
  )
  
  link2 <- link(mc17, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "road_crop_global_conflict_bab.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$road_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="road density standardized")}
  if(i==2){
    pdf(file = "road_crop_global_conflict_ele.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$road_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="road density standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  dev.off()
}
dev.off()

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
    household_size_std=rep(0,30),
    farm_size_std=rep(0,30),
    months_planted_std=rep(0,30),
    see_field=rep(mean(dc$see_field),30),
    species_index=rep(i,30)
  )
  
  link2 <- link(mc17, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "build_dens_crop_global_conflict_bab.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$build_dens_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="building density standardized")}
  if(i==2){
    pdf(file = "build_dens_crop_global_conflict_ele.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$build_dens_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="building density standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  dev.off()
}
dev.off()
##crop

plot_seq <- seq(from=min(dc$crop_std) , to=max(dc$crop_std) , length=30)

for (i in 1:2){
  
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
  
  link2 <- link(mc17, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "crop_dens_crop_global_conflict_bab.pdf",   width = 5, height = 5)
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$crop_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="crop density standardized")}
  if(i==2){
    pdf(file = "crop_dens_crop_global_conflict_ele.pdf",   width = 5, height = 5)
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$crop_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="crop density standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  dev.off()
}
dev.off()

###hh size
plot_seq <- seq(from=min(dc$household_size_std) , to=max(dc$household_size_std) , length=30)

for (i in 1:2){
  
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
  
  link2 <- link(mc17, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "hhsize_crop_global_conflict_bab.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$household_size_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size standardized")}
  if(i==2){
    pdf(file = "hhsize_crop_global_conflict_ele.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$household_size_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  dev.off()
}
dev.off()
##farm size
plot_seq <- seq(from=min(dc$farm_size_std) , to=max(dc$farm_size_std) , length=30)

for (i in 1:2){
  
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
  
  link2 <- link(mc17, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "farmsize_crop_global_conflict_bab.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$farm_size_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="farm size standardized")}
  if(i==2){
    pdf(file = "farmsize_crop_global_conflict_ele.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$farm_size_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="farm size standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  dev.off()
}
dev.off()

##month planted

plot_seq <- seq(from=min(dc$months_planted_std) , to=max(dc$months_planted_std) , length=30)

for (i in 1:2){
  
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
  
  link2 <- link(mc17, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "monthsplanted_crop_global_conflict_bab.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$months_planted_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="months planted standardized")}
  if(i==2){
    pdf(file = "monthsplanted_crop_global_conflict_ele.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$months_planted_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="months planted standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  dev.off()
}
dev.off()

##SEE
dpred <- list(
  village_index=rep(1,4),
  settle_dist_km_std=rep(0,30),
  c70_std = rep(0,30),
  c2070_std = rep(0,30),
  river_std= rep(0,30),
  road_std= rep(0,30),
  build_dens_std= rep(0,30),
  crop_std= rep(0,30),
  see_field=c(0,1,0,1),
  months_planted_std=rep(0,4),
  farm_size_std=rep(0,4),
  household_size_std=rep(0,4),
  farm_size_std=rep(0,4),
  months_planted_std=rep(0,4),
  species_index=c(1,1,2,2)
)

link2 <- link(mc17, data=dpred , replace=list(village_index=av_z) )
pdf(file = "seefield_crop_global_conflict_bab.pdf",   width = 5, height = 5) 
par( mar=c(4,4,1,1)+.1 )
dens(link2[,1] , lty=2 , col="blue" , ylim=c(0,15) , xlim=c(0,0.5) , main="probability baboon crop conflict")
abline(v=mean(link2[,1]) , col="blue" , lty=2)
dens(link2[,2] , add=TRUE , col="darkblue")
abline(v=mean(link2[,2]) , col="darkblue" , lty=1)
legend('topright' , c("can't see field" , "see field") , col=c("blue" , "darkblue") , lty=c(2,1) , , bty='n')
dev.off()

pdf(file = "seefield_crop_global_conflict_ele.pdf",   width = 5, height = 5) 
par( mar=c(4,4,1,1)+.1 )
dens(link2[,3] , lty=2 , col="darkgrey" , ylim=c(0,15) , xlim=c(0.2,1) , main="probability elephant crop conflict")
abline(v=mean(link2[,3]) , col="darkgrey" , lty=2)
dens(link2[,4] , add=TRUE , col="black" )
abline(v=mean(link2[,4]) , col="black" , lty=1)
legend('topleft' , c("can't see field" , "see field") , col=c("grey" , "black") , lty=c(2,1) , bty='n')
dev.off()


####global restrict iusing WAIC
mc17.1 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + 
      as[species_index] + 
      b_SDs[species_index]*settle_dist_km_std + 
      b_C70s[species_index]*c70_std + 
      b_C2070s[species_index]*c2070_std + 
      b_HHs[species_index]*household_size_std + 
      b_FSs[species_index]*farm_size_std + 
      b_SEEs[species_index]*see_field + 
      b_CRs[species_index]*crop_std,
    
    a ~ normal( 0 , 1 ),
    c(b_SD,b_C70,b_C2070,b_CR) ~ normal( 0 , 0.5 ),
    c(b_HH,b_FS,b_SEE) ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_SDs,b_C70s,b_C2070s,b_HHs,b_FSs,b_SEEs,b_CRs)[species_index] ~ multi_normal( c(a,b_SD,b_C70,b_C2070,b_HH,b_FS,b_SEE,b_CR) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)


compare(mc0,mc1,mc2,mc3,mc4,mc5,mc6,mc7,mc8,mc9,mc10,mc11,mc13,mc14,mc15,mc16,mc17)
compare(mc12.1 , mc12.2, mc12.3, mc12.4, mc12.6 , mc12.7)


compare(mc10,mc9,mc8,mc7,mc6,mc5,mc4,mc3,mc2)


##########################################################################
###########################LIVESTOCK DAMAGE############################
##########################################################################

myvars3 <- c( "village" , "hyena_l" , "lion_l", "fid" , "settle_dist" , "c70" , "c2070" ,"river" , "road" , "species" , "crop" , "gse_slope30m" , "build_dens" , "household_size" , "guard_ave_day" , "livestock_head" , "lv_prot_day_guard" , "lv_prot_day_dogs" , "lv_prot_night_dogs" , "lv_prot_night_contain" , "livestock") #food

# HH size // livestock
# guard ave day # people ## skip logic don't guard is NA
# number of livestock owened
# cattle sheep goat donkey SUM of them

dl <- d[myvars3]
dl <- dl[dl$livestock==1,]
dl <- dl[dl$species=="lion",]
dl <- dl[complete.cases(dl), ]

dL <- dl
dL$conflict <- dL$lion_l
dL$species <- "lion"
dH <- dl
dH$conflict <- dH$hyena_l
dH$species <- "hyena"
dl <-  rbind(dL,dH)

dl$settle_dist_km <- dl$settle_dist/1000
dl$settle_dist_km_std <- (dl$settle_dist_km-mean(dl$settle_dist_km) )/sd(dl$settle_dist_km)
dl$c70_std <- (dl$c70-mean(dl$c70) )/sd(dl$c70)
dl$c2070_std <- (dl$c2070-mean(dl$c2070) )/sd(dl$c2070)
dl$village <- as.character(dl$village)
dl$village[dl$fid==179] <- "Nyamatoke_jklol"
dl$village_index <- as.integer(as.factor(dl$village))
dl$river_std <- (dl$river-mean(dl$river) )/sd(dl$river)
dl$road_std <- (dl$road-mean(dl$road) )/sd(dl$road)
dl$crop_std <- (dl$crop-mean(dl$crop) )/sd(dl$crop)
dl$gse_slope30m_std <- (dl$gse_slope30m-mean(dl$gse_slope30m) )/sd(dl$gse_slope30m)
dl$build_dens_std <- (dl$build_dens-mean(dl$build_dens) )/sd(dl$build_dens)
dl$guard_ave_day_std <- (dl$guard_ave_day-mean(dl$guard_ave_day) )/sd(dl$guard_ave_day) 
dl$household_size_std <- (dl$household_size-mean(dl$household_size) )/sd(dl$household_size)
dl$log_livestock_head <- log(dl$livestock_head + 1)
dl$log_livestock_head_std <- (dl$log_livestock_head-mean(dl$log_livestock_head) )/sd(dl$log_livestock_head)
dl$livestock_head_std <- (dl$livestock_head-mean(dl$livestock_head) )/sd(dl$livestock_head)



dl$species_index <- as.integer(as.factor(dl$species))
dl$village_index <- as.integer(as.factor(dl$village))

ml1 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] ,
    a ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(a,sigma_v),
    as[species_index] ~ dnorm(a,sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1)
    
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

post <- extract.samples(ml1)
str(post)
dens( logistic(post$a + post$as[,1]) , col="red" , xlim=c(0,1) , ylim=c(0,230)) 
dens( logistic(post$a + post$as[,2] ), col="orange" , add=TRUE) 

precis(ml1, depth=2)

precislist <- list(
  PrHyena = logistic(post$a + post$as[,1] ),
  PrLion =logistic(post$a + post$as[,2])
)

plot(precis(precislist , ci=.89) )

##settlement distance
ml2 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] +  b_dSAs[species_index]*settle_dist_km_std ,
    a ~ normal( 0 , 1 ),
    b_dSA ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_dSAs)[species_index] ~ multi_normal( c(a,b_dSA) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(ml2, depth=2)

plot_seq <- seq(from=min(dl$settle_dist_km_std) , to=max(dl$settle_dist_km_std) , length=30)
av_z <- matrix(0,1000,length(unique(dl$village_index))) #need to add zeros in VE to plot main effect


plot(dl$hyena_l ~ dl$settle_dist_km_std , col=col.alpha("slateblue", 0.1) , pch=19 , ylab="baboon bothers" , xlab="standardize km from settlement area")

ylabels=c("probability hyena livestock conflict" , "probability lion livestock conflict")
colpal=c("slateblue" , "orange")
for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(ml2, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dl$hyena_l ~ dl$settle_dist_km_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="standardize km from settlement")}
  if(i==2){plot(dl$lion_l ~ dl$settle_dist_km_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="standardize km from settlement")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

#####household size
ml3 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_HHs[species_index]*household_size_std,
    a ~ normal( 0 , 1 ),
    b_HH ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_HHs)[species_index] ~ multi_normal( c(a,b_HH) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(ml3, depth=2)


plot_seq <- seq(from=min(dl$household_size_std) , to=max(dl$household_size_std) , length=30)


for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    household_size_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(ml3, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dl$hyena_l ~ dl$household_size_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size standardized")}
  if(i==2){plot(dl$lion_l ~ dl$household_size_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

##########slope#####
ml4 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_SLs[species_index]*gse_slope30m_std ,
    a ~ normal( 0 , 1 ),
    b_SL ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_SLs)[species_index] ~ multi_normal( c(a,b_SL) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)

  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(ml4, depth=2)


plot_seq <- seq(from=min(dl$gse_slope30m_std) , to=max(dl$gse_slope30m_std) , length=30)


for (i in 1:2){

  dpred <- list(
    village_index=rep(1,30),
    gse_slope30m_std=plot_seq,
    species_index=rep(i,30)
  )

  link2 <- link(ml4, data=dpred , replace=list(village_index=av_z) )

  if(i==1){plot(dl$hyena_l ~ dl$gse_slope30m_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="slope 30m size standardized")}
  if(i==2){plot(dl$lion_l ~ dl$gse_slope30m_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="slope 30m size standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

####livestock head
ml5 <- ulam(
  alist(
    conflict  ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index]  + b_LSHs[species_index]*log_livestock_head_std ,
    a ~ normal( 0 , 1 ),
    b_LSH ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_LSHs)[species_index] ~ multi_normal( c(a,b_LSH) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

plot_seq <- seq(from=min(dl$livestock_head_std) , to=max(dl$livestock_head_std) , length=30)

precis(ml5 , depth=2)
for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    livestock_head_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(ml5, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dl$hyena_l ~ dl$livestock_head_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="livestock head standardized")}
  if(i==2){plot(dl$lion_l ~ dl$livestock_head_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="livestock head standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

###cover 70
ml6 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_C70s[species_index]*c70_std ,
    a ~ normal( 0 , 1 ),
    b_C70 ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_C70s)[species_index] ~ multi_normal( c(a,b_C70) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dl , chains=4 , cores=4 , iter=5000 , log_lik=TRUE)

precis(ml6, depth=2)


plot_seq <- seq(from=min(dl$c70_std) , to=max(dl$c70_std) , length=30)


for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    c70_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(ml6, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dl$hyena_l ~ dl$c70_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="Tree Cover 70 percent standardized")}
  if(i==2){plot(dl$lion_l ~ dl$c70_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="Tree Cover 70 percent standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

#####cover 2070
ml7 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_C2070s[species_index]*c2070_std ,
    a ~ normal( 0 , 1 ),
    b_C2070 ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_C2070s)[species_index] ~ multi_normal( c(a,b_C2070) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dl , chains=4 , cores=4 , iter=5000 , log_lik=TRUE)

precis(ml7 , depth=2)

plot_seq <- seq(from=min(dl$c2070_std) , to=max(dl$c2070_std) , length=30)


for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,30),
    c2070_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(ml7, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dl$hyena_l ~ dl$c2070_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="Tree Cover 20-70 percent standardized")}
  if(i==2){plot(dl$lion_l ~ dl$c2070_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="Tree Cover 20-70 percent standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}


####river
ml8 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_RIVs[species_index]*river_std,
    a ~ normal( 0 , 1 ),
    b_RIV ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_RIVs)[species_index] ~ multi_normal( c(a,b_RIV) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(ml8, depth=2)


plot_seq <- seq(from=min(dl$river_std) , to=max(dl$river_std) , length=30)


for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    river_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(ml8, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dl$hyena_l ~ dl$river_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="river standardized")}
  if(i==2){plot(dl$lion_l ~ dl$river_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="river standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

####building dens
ml9 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_BDs[species_index]*build_dens_std,
    a ~ normal( 0 , 1 ),
    b_BD ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_BDs)[species_index] ~ multi_normal( c(a,b_BD) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(ml9, depth=2)

plot_seq <- seq(from=min(dl$build_dens_std) , to=max(dl$build_dens_std) , length=30)


for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    build_dens_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(ml9, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dl$hyena_l ~ dl$build_dens_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="building density standardized")}
  if(i==2){plot(dl$lion_l ~ dl$build_dens_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="building density standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}


###road
ml10 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_RDs[species_index]*road_std,
    a ~ normal( 0 , 1 ),
    b_RD ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_RDs)[species_index] ~ multi_normal( c(a,b_RD) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(ml10, depth=2)

plot_seq <- seq(from=min(dl$road_std) , to=max(dl$road_std) , length=30)


for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    road_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(ml10, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dl$hyena_l ~ dl$road_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="road density standardized")}
  if(i==2){plot(dl$lion_l ~ dl$road_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="road density standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

###guards
ml11 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_GUs[species_index]*guard_ave_day_std,
    a ~ normal( 0 , 1 ),
    b_GU ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_GUs)[species_index] ~ multi_normal( c(a,b_GU) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(ml11, depth=2)

plot_seq <- seq(from=min(dl$guard_ave_day_std) , to=max(dl$guard_ave_day_std) , length=30)


for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    guard_ave_day_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(ml11, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dl$hyena_l ~ dl$guard_ave_day_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="avg folx guarding standardized")}
  if(i==2){plot(dl$lion_l ~ dl$guard_ave_day_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="avg folx guardin standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

#######crop prto stuff

#contain and protect interact
ml12 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_LPDGs[species_index]*lv_prot_day_guard + (b_LPNCs[species_index] + b_LPDGsxLPNCs[species_index]*lv_prot_day_guard)*lv_prot_night_contain ,
    a ~ normal( 0 , 1 ),
    c(b_LPDG,b_LPNC,b_LPDGxLPNC) ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_LPDGs,b_LPNCs,b_LPDGsxLPNCs)[species_index] ~ multi_normal( c(a,b_LPDG,b_LPNC,b_LPDGxLPNC) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(ml12, depth=2)


dpred <- list(
  village_index=rep(1,8),
  lv_prot_day_guard=     c(0,1,0,1,0,1,0,1),
  lv_prot_night_contain= c(0,0,1,1,0,0,1,1),
  species_index=c(1,1,1,1,2,2,2,2)
)

link2 <- link(ml12, data=dpred , replace=list(village_index=av_z) )

str(link2)

precislist <- list(
  hyena_crop_prot_no_contain_or_guard=link2[,1],
  hyena_crop_prot_dayguard=link2[,2],
  hyena_crop_prot_contain=link2[,3],
  hyena_crop_prot_guard_and_contain=link2[,4],
  lion_crop_prot_no_contain_or_guard=link2[,5],
  lion_crop_prot_guard=link2[,6],
  lion_crop_prot_contain=link2[,7],
  lion_crop_prot_guard_and_contain=link2[,8]
)

plot(precis(precislist , ci=.89) , xlab="Probabity Crop Conflict" )


#######interaction livestock head
ml13 <- ulam(
  alist(
    conflict  ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index]  + b_LSHs[species_index]*livestock_head_std + (b_GUs[species_index] + b_GUxLSHs[species_index]*livestock_head_std)*guard_ave_day_std ,
    a ~ normal( 0 , 1 ),
    c(b_LSH,b_GU ,b_GUxLSH) ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_LSHs,b_GUs,b_GUxLSHs)[species_index] ~ multi_normal( c(a,b_LSH,b_GU,b_GUxLSH) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

plot_seq <- seq(from=min(dl$livestock_head_std) , to=max(dl$livestock_head_std) , length=30)
colpal1=brewer.pal(5,"Blues")
colpal2=brewer.pal(5,"Greens")

sort(unique(dl$guard_ave_day_std))
sort(unique(dl$guard_ave_day))

for (i in 1:2){
  for(j in 1:5){
    dpred <- list(
      village_index=rep(1,30),
      livestock_head_std=plot_seq,
      species_index=rep(i,30),
      guard_ave_day_std=rep( sort(unique(dl$guard_ave_day_std))[j] , 30) 
    )
    
    link2 <- link(ml13, data=dpred , replace=list(village_index=av_z) )
    
    if(i==1 & j==1){plot(dl$hyena_l ~ dl$livestock_head_std, col=col.alpha(colpal1[j], 0.1) , pch=19 , ylab=ylabels[i] , xlab="livestock head standardized")}
    
    if(i==2 & j==1){plot(dl$lion_l ~ dl$livestock_head_std , col=col.alpha(colpal2[j], 0.1) , pch=19 , ylab=ylabels[i] , xlab="livestock head standardized")
    }
    
    pred_mean <- apply(link2 , 2 , mean)
    lines(pred_mean ~ plot_seq , lw=2, col=colpal1[j] , lty=1)
  }
}

###multipanel
for (i in 1:2){
  for(j in 1:5){
    dpred <- list(
      village_index=rep(1,30),
      livestock_head_std=plot_seq,
      species_index=rep(i,30),
      #household_size_std=rep(j , 30) 
      guard_ave_day_std=rep( sort(unique(dl$guard_ave_day_std))[j] , 30) 
    )
    
    link2 <- link(ml13, data=dpred , replace=list(village_index=av_z) )
    
    if(i==1){plot(dl$hyena_l ~ dl$livestock_head_std, col=col.alpha(colpal1[j], 0.1) , pch=19 , ylab=ylabels[i] , xlab="livestock head standardized")
      pred_mean <- apply(link2 , 2 , mean)
      lines(pred_mean ~ plot_seq , lw=2, col=colpal1[j] , lty=1)
        for (k in sample( c(1:1000) , 100) ){
          lines( link2[k,] ~ plot_seq , lw=3, col=col.alpha(colpal1[j], alpha=0.1) , lty=1)
        }
      }
    
    if(i==2){plot(dl$lion_l ~ dl$livestock_head_std , col=col.alpha(colpal2[j], 0.1) , pch=19 , ylab=ylabels[i] , xlab="livestock head standardized")
      pred_mean <- apply(link2 , 2 , mean)
      lines(pred_mean ~ plot_seq , lw=2, col=colpal2[j] , lty=1)
        for (k in sample( c(1:1000) , 100) ){
          lines( link2[k,] ~ plot_seq , lw=3, col=col.alpha(colpal2[j], alpha=0.1) , lty=1)
        }
    }
    
  }
}

#######house level
ml14 <- ulam(
  alist(
    conflict  ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index]  + b_LSHs[species_index]*livestock_head_std + b_HHs[species_index]*household_size_std + b_GUs[species_index]*guard_ave_day_std ,
    a ~ normal( 0 , 1 ),
    c(b_LSH,b_HH,b_GU) ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_LSHs,b_HHs,b_GUs)[species_index] ~ multi_normal( c(a,b_LSH,b_HH,b_GU) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)



#######house level w/ interaction
ml14.1 <- ulam(
  alist(
    conflict  ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index]  + b_LSHs[species_index]*livestock_head_std + b_HHs[species_index]*household_size_std + (b_GUs[species_index] + b_GUxLSHs[species_index]*livestock_head_std)*guard_ave_day_std ,
    a ~ normal( 0 , 1 ),
    c(b_LSH,b_HH,b_GU ,b_GUxLSH) ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_LSHs,b_HHs,b_GUs,b_GUxLSHs)[species_index] ~ multi_normal( c(a,b_LSH,b_HH,b_GU,b_GUxLSH) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(ml14.1 , depth=2)

###interaction between num guards and livestock size
plot_seq <- seq(from=min(dl$livestock_head_std) , to=max(dl$livestock_head_std) , length=30)

for (i in 1:2){
  for(j in 1:5){
    
    dpred <- list(
      village_index=rep(1,30),
      livestock_head_std=plot_seq,
      species_index=rep(i,30),
      household_size_std=rep(0,30), 
      guard_ave_day_std=rep( sort(unique(dl$guard_ave_day_std))[j] , 30) 
    )
    
    link2 <- link(ml14.1, data=dpred , replace=list(village_index=av_z) )
    
    if(i==1){plot(dl$hyena_l ~ dl$livestock_head_std, col=col.alpha(colpal1[j], 0.1) , pch=19 , ylab=ylabels[i] , xlab="livestock head standardized")
      pred_mean <- apply(link2 , 2 , mean)
      lines(pred_mean ~ plot_seq , lw=2, col=colpal1[j] , lty=1)
      for (k in sample( c(1:1000) , 100) ){
        lines( link2[k,] ~ plot_seq , lw=3, col=col.alpha(colpal1[j], alpha=0.1) , lty=1)
      }
    }
    
    if(i==2){plot(dl$lion_l ~ dl$livestock_head_std , col=col.alpha(colpal2[j], 0.1) , pch=19 , ylab=ylabels[i] , xlab="livestock head standardized")
      pred_mean <- apply(link2 , 2 , mean)
      lines(pred_mean ~ plot_seq , lw=2, col=colpal2[j] , lty=1)
      for (k in sample( c(1:1000) , 100) ){
        lines( link2[k,] ~ plot_seq , lw=3, col=col.alpha(colpal2[j], alpha=0.1) , lty=1)
      }
    }
    
  }
}

#num guards
plot_seq <- seq(from=min(dl$guard_ave_day_std) , to=max(dl$guard_ave_day_std) , length=30)


for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    livestock_head_std=rep(0,30),
    species_index=rep(i,30),
    household_size_std=rep(0,30), 
    guard_ave_day_std=plot_seq 
  )
  
  link2 <- link(ml14.1, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dl$hyena_l ~ dl$guard_ave_day_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="avg folx guarding standardized")}
  if(i==2){plot(dl$lion_l ~ dl$guard_ave_day_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="avg folx guardin standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}


plot_seq <- seq(from=min(dl$household_size_std) , to=max(dl$household_size_std) , length=30)


for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    livestock_head_std=rep(0,30),
    species_index=rep(i,30),
    household_size_std=plot_seq, 
    guard_ave_day_std=rep(0,30) 
  )
  
  link2 <- link(ml3, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dl$hyena_l ~ dl$household_size_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size standardized")}
  if(i==2){plot(dl$lion_l ~ dl$household_size_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}




###all household and interventions household model

ml14.2 <- ulam(
  alist(
    conflict  ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index]  + b_LSHs[species_index]*livestock_head_std + b_HHs[species_index]*household_size_std + (b_GUs[species_index] + b_GUxLSHs[species_index]*livestock_head_std)*guard_ave_day_std + (b_LPNCs[species_index] + b_GUxLPNCs[species_index]*guard_ave_day)*lv_prot_night_contain ,
    a ~ normal( 0 , 1 ),
    c(b_LSH,b_HH,b_GU ,b_GUxLSH,b_LPNC,b_GUxLPNC) ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_LSHs,b_HHs,b_GUs,b_GUxLSHs,b_LPNCs,b_GUxLPNCs)[species_index] ~ multi_normal( c(a,b_LSH,b_HH,b_GU,b_GUxLSH,b_LPNC,b_GUxLPNC) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(ml14.2 , depth=2)


#########landscape
ml15 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] +  b_SDs[species_index]*settle_dist_km_std + b_C70s[species_index]*c70_std + b_C2070s[species_index]*c2070_std + b_RIVs[species_index]*river_std  + b_RDs[species_index]*road_std + b_BDs[species_index]*build_dens_std + b_SLs[species_index]*gse_slope30m_std  ,
    a ~ normal( 0 , 1 ),
    c(b_SD,b_C70,b_C2070,b_RIV,b_RD,b_BD,b_SL) ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_SDs,b_C70s,b_C2070s,b_RIVs,b_RDs,b_BDs,b_SLs)[species_index] ~ multi_normal( c(a,b_SD,b_C70,b_C2070,b_RIV,b_RD,b_BD,b_SL) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(ml5 , depth=2)

#########SDistance

plot_seq <- seq(from=min(dl$settle_dist_km_std) , to=max(dl$settle_dist_km_std) , length=30)
av_z <- matrix(0,1000,length(unique(dl$village_index))) #need to add zeros in VE to plot main effect


plot(dl$hyena_l ~ dl$settle_dist_km_std , col=col.alpha("slateblue", 0.1) , pch=19 , ylab="baboon bothers" , xlab="standardize km from settlement area")

ylabels=c("probability hyena livestock conflict" , "probability lion livestock conflict")
colpal=c("slateblue" , "orange")
for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=plot_seq,
    c70_std=rep(0,30),
    c2070_std=rep(0,30),
    river_std=rep(0,30),
    road_std=rep(0,30),
    build_dens_std=rep(0,30),
    gse_slope30m_std=rep(0,30),
    species_index=rep(i,30)
  )
  
  link2 <- link(ml15, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dl$hyena_l ~ dl$settle_dist_km_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="standardize km from settlement")}
  if(i==2){plot(dl$lion_l ~ dl$settle_dist_km_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="standardize km from settlement")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

######cover 70
plot_seq <- seq(from=min(dl$c70_std) , to=max(dl$c70_std) , length=30)
av_z <- matrix(0,1000,length(unique(dl$village_index))) #need to add zeros in VE to plot main effect


plot(dl$hyena_l ~ dl$c70_std , col=col.alpha("slateblue", 0.1) , pch=19 , ylab="baboon bothers" , xlab="standardize km from settlement area")

ylabels=c("probability hyena livestock conflict" , "probability lion livestock conflict")
colpal=c("slateblue" , "orange")
for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=rep(0,30),
    c70_std=plot_seq,
    c2070_std=rep(0,30),
    river_std=rep(0,30),
    road_std=rep(0,30),
    build_dens_std=rep(0,30),
    gse_slope30m=rep(0,30),
    species_index=rep(i,30)
  )
  
  link2 <- link(ml2, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dl$hyena_l ~ dl$c70_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="standardize km from settlement")}
  if(i==2){plot(dl$lion_l ~ dl$c70_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="standardize km from settlement")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

######cover 2070


plot_seq <- seq(from=min(dl$c2070_std) , to=max(dl$c2070_std) , length=30)
av_z <- matrix(0,1000,length(unique(dl$village_index))) #need to add zeros in VE to plot main effect

ylabels=c("probability hyena livestock conflict" , "probability lion livestock conflict")
colpal=c("slateblue" , "orange")
for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=rep(0,30),
    c2070_std=plot_seq,
    c70_std=rep(0,30),
    river_std=rep(0,30),
    road_std=rep(0,30),
    build_dens_std=rep(0,30),
    gse_slope30m=rep(0,30),
    species_index=rep(i,30)
  )
  
  link2 <- link(ml2, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dl$hyena_l ~ dl$c2070_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="cover 2070 density standardized ")}
  if(i==2){plot(dl$lion_l ~ dl$c2070_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="cover 2070 density standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}


compare(ml1,ml2,ml3,ml4,ml5,ml6,ml7,ml8,ml9,ml10,ml11,ml12,ml13,ml14,ml14.1,ml14.2,ml15,ml16.1,m16.2)


#######global mofo won't converge
ml16 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + LS + HH,
    LS <- b_SDs[species_index]*settle_dist_km_std + b_C70s[species_index]*c70_std + b_C2070s[species_index]*c2070_std + b_RIVs[species_index]*river_std  + b_RDs[species_index]*road_std + b_BDs[species_index]*build_dens_std + b_SLs[species_index]*gse_slope30m_std,
    HH <-  b_LSHs[species_index]*livestock_head_std + b_HHs[species_index]*household_size_std + (b_GUs[species_index] + b_GUxLSHs[species_index]*livestock_head_std)*guard_ave_day_std + (b_LPNCs[species_index] + b_GUxLPNCs[species_index]*guard_ave_day)*lv_prot_night_contain ,
    a ~ normal( 0 , 1 ),
    c(b_SD,b_C70,b_C2070,b_RIV,b_RD,b_BD,b_SL) ~ normal( 0 , 0.5 ),
    c(b_LSH,b_HH,b_GU ,b_GUxLSH,b_LPNC,b_GUxLPNC) ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_SDs,b_C70s,b_C2070s,b_RIVs,b_RDs,b_BDs,b_SLs,b_LSHs,b_HHs,b_GUs,b_GUxLSHs,b_LPNCs,b_GUxLPNCs)[species_index] ~ multi_normal( c(a,b_SD,b_C70,b_C2070,b_RIV,b_RD,b_BD,b_SL,b_LSH,b_HH,b_GU,b_GUxLSH,b_LPNC,b_GUxLPNC) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

##########global mofo livestock
ml16.1 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] +  
      b_SDs[species_index]*settle_dist_km_std + 
      b_C70s[species_index]*c70_std + 
      b_RDs[species_index]*road_std + 
      b_BDs[species_index]*build_dens_std + 
      b_SLs[species_index]*gse_slope30m_std +  
      b_LSHs[species_index]*livestock_head_std + 
      b_HHs[species_index]*household_size_std + 
      (b_GUs[species_index] + b_GUxLSHs[species_index]*livestock_head_std)*guard_ave_day_std,
    a ~ normal( 0 , 1 ),
    c(b_SD,b_C70,b_RD,b_BD,b_SL) ~ normal( 0 , 0.5 ),
    c(b_LSH,b_HH,b_GU ,b_GUxLSH) ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_SDs,b_C70s,b_RDs,b_BDs,b_SLs,b_LSHs,b_HHs,b_GUs,b_GUxLSHs)[species_index] ~ multi_normal( c(a,b_SD,b_C70,b_RD,b_BD,b_SL,b_LSH,b_HH) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dl , chains=4 , cores=4 , iter=4000 , log_lik=TRUE ,  control=list(adapt_delta=0.999))

#########SDistance

plot_seq <- seq(from=min(dl$settle_dist_km_std) , to=max(dl$settle_dist_km_std) , length=30)
av_z <- matrix(0,1000,length(unique(dl$village_index))) #need to add zeros in VE to plot main effect

ylabels=c("probability hyena livestock conflict" , "probability lion livestock conflict")
colpal=c("red" , "orange")
for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=plot_seq,
    c70_std=rep(0,30),
    road_std=rep(0,30),
    build_dens_std=rep(0,30),
    gse_slope30m_std=rep(0,30),
    livestock_head_std=rep(0,30),
    species_index=rep(i,30),
    household_size_std=rep(0,30), 
    guard_ave_day_std=rep(0,30), 
    species_index=rep(i,30)
  )
  
  link2 <- link(ml16.1, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "settle_dist_livestock_global_conflict_hyena.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$hyena_l ~ dl$settle_dist_km_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="standardized km from settlement")}
  if(i==2){
    pdf(file = "settle_dist_livestock_global_conflict_lion.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$lion_l ~ dl$settle_dist_km_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="standardized km from settlement")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  dev.off()
}

######cover 70
plot_seq <- seq(from=min(dl$c70_std) , to=max(dl$c70_std) , length=30)
av_z <- matrix(0,1000,length(unique(dl$village_index))) #need to add zeros in VE to plot main effect

ylabels=c("probability hyena livestock conflict" , "probability lion livestock conflict")
for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=rep(0,30),
    c70_std=plot_seq,
    c2070_std=rep(0,30),
    river_std=rep(0,30),
    road_std=rep(0,30),
    build_dens_std=rep(0,30),
    gse_slope30m_std=rep(0,30),
    livestock_head_std=rep(0,30),
    species_index=rep(i,30),
    household_size_std=rep(0,30), 
    guard_ave_day_std=rep(0,30), 
    species_index=rep(i,30)
  )
  
  link2 <- link(ml16.1, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "c70_livestock_global_conflict_hyena.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$hyena_l ~ dl$c70_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="forest/thicket density (standardized)")}
  if(i==2){
    pdf(file = "c70_livestock_global_conflict_lion.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$lion_l ~ dl$c70_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="forest/thicket density (standardized)")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  dev.off()
}


########road

plot_seq <- seq(from=min(dl$road_std) , to=max(dl$road_std) , length=30)

for (i in 1:2){
  
  dpred <- list(

    village_index=rep(1,30),
    settle_dist_km_std=rep(0,30),
    c70_std=rep(0,30),
    c2070_std=rep(0,30),
    river_std=rep(0,30),
    road_std=plot_seq,
    build_dens_std=rep(0,30),
    gse_slope30m_std=rep(0,30),
    livestock_head_std=rep(0,30),
    species_index=rep(i,30),
    household_size_std=rep(0,30), 
    guard_ave_day_std=rep(0,30), 
    species_index=rep(i,30)
  )
  
  link2 <- link(ml16.1, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "road_livestock_global_conflict_hyena.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$hyena_l ~ dl$road_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="road density standardized")}
  if(i==2){
    pdf(file = "road_livestock_global_conflict_lion.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$lion_l ~ dl$road_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="road density standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  dev.off()
}

#buiding density

plot_seq <- seq(from=min(dl$build_dens_std) , to=max(dl$build_dens_std) , length=30)

for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    build_dens_std=plot_seq,
    settle_dist_km_std=rep(0,30),
    c70_std=rep(0,30),
    c2070_std=rep(0,30),
    river_std=rep(0,30),
    road_std=rep(0,30),
    gse_slope30m_std=rep(0,30),
    livestock_head_std=rep(0,30),
    species_index=rep(i,30),
    household_size_std=rep(0,30), 
    guard_ave_day_std=rep(0,30), 
    species_index=rep(i,30)
  )
  
  link2 <- link(ml16.1, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "build_dens_livestock_global_conflict_hyena.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$hyena_l ~ dl$build_dens_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="building density standardized")}
  if(i==2){
    pdf(file = "build_dens_livestock_global_conflict_lion.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$lion_l ~ dl$build_dens_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="building density standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  dev.off()
}

##slope
plot_seq <- seq(from=min(dl$gse_slope30m_std) , to=max(dl$gse_slope30m_std) , length=30)

for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    gse_slope30m_std=plot_seq,
    build_dens_std=rep(0,30),
    settle_dist_km_std=rep(0,30),
    c70_std=rep(0,30),
    c2070_std=rep(0,30),
    river_std=rep(0,30),
    road_std=rep(0,30),
    livestock_head_std=rep(0,30),
    species_index=rep(i,30),
    household_size_std=rep(0,30), 
    guard_ave_day_std=rep(0,30), 
    species_index=rep(i,30)
  )
  
  link2 <- link(ml16.1, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "slope30m_livestock_global_conflict_hyena.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$hyena_l ~ dl$gse_slope30m_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="slope 30m standardized")}
  if(i==2){
    pdf(file = "slope30m_livestock_global_conflict_lion.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$lion_l ~ dl$gse_slope30m_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="slope 30m standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  dev.off()
}

###livestock head
plot_seq <- seq(from=min(dl$livestock_head_std) , to=max(dl$livestock_head_std) , length=30)

for (i in 1:2){
  dpred <- list(
    village_index=rep(1,30),
    livestock_head_std=plot_seq,
    gse_slope30m_std=rep(0,30),
    build_dens_std=rep(0,30),
    settle_dist_km_std=rep(0,30),
    c70_std=rep(0,30),
    c2070_std=rep(0,30),
    river_std=rep(0,30),
    road_std=rep(0,30),
    species_index=rep(i,30),
    household_size_std=rep(0,30), 
    guard_ave_day_std=rep(0,30), 
    species_index=rep(i,30)
  )
  
  link2 <- link(ml16.1, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "livestock head_livestock_global_conflict_hyena.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$hyena_l ~ dl$livestock_head_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="livestock head standardized")}
  if(i==2){
    pdf(file = "livestock head_livestock_global_conflict_lion.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$lion_l ~ dl$livestock_head_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="livestock head standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  dev.off()
}

########hhsize######3

plot_seq <- seq(from=min(dl$household_size_std) , to=max(dl$household_size_std) , length=30)

for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    household_size_std=plot_seq,
    livestock_head_std=rep(0,30),
    gse_slope30m_std=rep(0,30),
    build_dens_std=rep(0,30),
    settle_dist_km_std=rep(0,30),
    c70_std=rep(0,30),
    c2070_std=rep(0,30),
    river_std=rep(0,30),
    road_std=rep(0,30),
    species_index=rep(i,30),
    guard_ave_day_std=rep(0,30), 
    species_index=rep(i,30)
  )
  
  link2 <- link(ml16.1, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "household_size_livestock_global_conflict_hyena.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$hyena_l ~ dl$household_size_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size standardized")}
  if(i==2){
    pdf(file = "household_size_livestock_global_conflict_lion.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$lion_l ~ dl$household_size_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  dev.off()
}

##num guards
plot_seq <- seq(from=min(dl$guard_ave_day_std) , to=max(dl$guard_ave_day_std) , length=30)

for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    guard_ave_day_std=plot_seq,
    household_size_std=rep(0,30),
    livestock_head_std=rep(0,30),
    gse_slope30m_std=rep(0,30),
    build_dens_std=rep(0,30),
    settle_dist_km_std=rep(0,30),
    c70_std=rep(0,30),
    c2070_std=rep(0,30),
    river_std=rep(0,30),
    road_std=rep(0,30),
    species_index=rep(i,30)
  )
  
  link2 <- link(ml16.1, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "num_guards_livestock_global_conflict_hyena.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$hyena_l ~ dl$guard_ave_day_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="average number of guards per day standardized")}
  if(i==2){
    pdf(file = "num_guards_livestock_global_conflict_lion.pdf",   width = 5, height = 5) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$lion_l ~ dl$guard_ave_day_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="average number of guards per day standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  dev.off()
}

###interaction between num guards and livestock size
plot_seq <- seq(from=min(dl$livestock_head_std) , to=max(dl$livestock_head_std) , length=30)

for (i in 1:2){
  for(j in 1:5){
    
    dpred <- list(
      village_index=rep(1,30),
      livestock_head_std=plot_seq,
      species_index=rep(i,30),
      household_size_std=rep(0,30), 
      guard_ave_day_std=rep( sort(unique(dl$guard_ave_day_std))[j] , 30),
      gse_slope30m_std=rep(0,30),
      build_dens_std=rep(0,30),
      settle_dist_km_std=rep(0,30),
      c70_std=rep(0,30),
      c2070_std=rep(0,30),
      road_std=rep(0,30),
      species_index=rep(i,30),
      guard_ave_day_std=rep(0,30)
    )
    
    link2 <- link(ml16.1, data=dpred , replace=list(village_index=av_z) )
    
    if(i==1){plot(dl$hyena_l ~ dl$livestock_head_std, col=col.alpha(colpal1[j], 0.1) , pch=19 , ylab=ylabels[i] , xlab="livestock head standardized")
      title(main=paste("num. guards = ", j-1) )
      pred_mean <- apply(link2 , 2 , mean)
      lines(pred_mean ~ plot_seq , lw=2, col=colpal1[j] , lty=1)
      for (k in sample( c(1:1000) , 100) ){
        lines( link2[k,] ~ plot_seq , lw=3, col=col.alpha(colpal1[j], alpha=0.1) , lty=1)
      }
    }
    
    if(i==2){
      plot(dl$lion_l ~ dl$livestock_head_std , col=col.alpha(colpal2[j], 0.1) , pch=19 , ylab=ylabels[i] , xlab="livestock head standardized")
      title(main=paste("num. guards = ", j-1) )
      pred_mean <- apply(link2 , 2 , mean)
      lines(pred_mean ~ plot_seq , lw=2, col=colpal2[j] , lty=1)
      for (k in sample( c(1:1000) , 100) ){
        lines( link2[k,] ~ plot_seq , lw=3, col=col.alpha(colpal2[j], alpha=0.1) , lty=1)
      }
    }
    
  }
}

plot(precis(ml16.1 , depth=1))
plot(precis(ml16.1 , depth=2 ,pars=c('b_SLs' , 'b_BDs' , 'b_RDs' , 'b_C70s' ,'b_SDs' , 'b_GUxLSH ' ,'b_GU ' , 'b_HH ' , 'b_LSH')))

##########################################
#############raster preds################
#########################################
###############baboons
ras_bab<-  read.csv("~/Dropbox/tza_wildlife_conflict/baboonRasterstacktopoints_survext.csv")

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
                                              mean(p$b_CRs + p$b_CRs[,1])*dpred$crop_std  + 
                                              mean(p$b_SEEs + p$b_SEEs[,1])*dpred$see_field 
)

dens(ras_bab$pred_bab_crop_conflict)
ras_bab_sub <- cbind( ras_bab[1:3] , ras_bab$pred_bab_crop_conflict)
write.csv(ras_bab_sub , file="ras_baboon_crop_preds.csv")


###elephants###################

ras_ele<-  read.csv("~/Dropbox/tza_wildlife_conflict/elephantRasterstacktopoints_survext.csv")
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
            mean(p$b_CRs + p$b_CRs[,2])*dpred$crop_std  + 
            mean(p$b_SEEs + p$b_SEEs[,2])*dpred$see_field 
)
dens(ras_ele$pred_ele_crop_conflict)
ras_ele_sub <- cbind( ras_ele[1:3] , ras_ele$pred_ele_crop_conflict)
write.csv(ras_ele_sub , file="ras_elephant_crop_preds.csv")

######to catch a predator############
ras_leo<-  read.csv("~/Dropbox/tza_wildlife_conflict/lionRasterstacktopoints_survext.csv")
ras_leo$settle_dist_km <- ras_leo$settle_dist/1000
ras_leo$c70_std <- (ras_leo$c70 -mean(dl$c70 ) )/ sd(dl$c70 ) 
ras_leo$road_std <- (ras_leo$road -mean(dl$road ) )/ sd(dl$road) 
ras_leo$build_dens_std <- (ras_leo$build_dens-mean(dl$build_dens ) )/ sd(dl$build_dens) 
ras_leo$gse_slope30m_std <- (ras_leo$gse_slope30m-mean(dl$gse_slope30m) )/sd(dl$gse_slope30m) 
ras_leo$settle_dist_km_std <- (ras_leo$settle_dist_km-mean(dl$settle_dist_km ) )/ sd(dl$settle_dist_km) 
ras_leo$species_index <- 2

p <- extract.samples(ml16.1)

dpred <- list(
  village_index=rep(1,nrow(ras_leo)),
  settle_dist_km_std=ras_leo$settle_dist_km_std,
  c70_std = ras_leo$c70_std,
  road_std= ras_leo$road_std,
  build_dens_std=ras_leo$build_dens_std,
  gse_slope30m_std= ras_leo$gse_slope30m_st,
  household_size_std=rep(0,nrow(ras_leo)),
  livestock_head_std=rep(0,nrow(ras_leo)),
  guard_ave_day_std=rep(0,nrow(ras_leo)),
  species_index=ras_leo$species_index
)

ras_leo$pred_leo_crop_conflict <-0
ras_leo$pred_leo_crop_conflict <- logistic( mean(p$a + p$as[,2]) + 
                                              mean(p$b_SD + p$b_SDs[,2])*dpred$settle_dist_km_std  + 
                                              mean(p$b_C70 + p$b_C70s[,2])*dpred$c70_std +
                                              mean(p$b_RD + p$b_RDs[,2])*dpred$road_std +
                                              mean(p$b_BD + p$b_BDs[,2])*dpred$build_dens_std +
                                              mean(p$b_SLs + p$b_SLs[,2])*dpred$gse_slope30m_std   
)

dens(ras_leo$pred_leo_crop_conflict)

ras_leo_sub <- cbind( ras_leo[1:3] , ras_leo$pred_leo_crop_conflict)
write.csv(ras_leo_sub , file="ras_lion_deadstock_preds.csv")
