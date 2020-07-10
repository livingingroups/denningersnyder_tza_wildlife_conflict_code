library('rethinking')
library('rstan')
require(rethinking)
require(lubridate)
require(RColorBrewer)
library(janitor)


# hw <- read.csv("~/Downloads/SS_Final_NoDups.csv") #old version with errorz
#hw <- read.csv("~/Downloads/Spatial_Household_Survey_Clean_1Jun20sp.csv") 
hw <-  read.csv("~/Dropbox/tza_wildlife_conflict/HWc_surveyClean_extract_envslopbuild.csv")

d <- hw[hw$farm=="Yes",] #only look at conflicts with households that have farms
d[d == "-2147483648"] <- "NA"

d$see_field <- ifelse(d$field_sight=="Yes" , 1 , 0)
d <- clean_names(d)

myvars <- c("conflict" , "village", "elephant_c", "baboon_c" , "hyena_l" , "lion_l" , "farm_walk" , "farm_size" , "household_size" , "fid" , "settle_dist" , "see_field" , "c70" , "c2070" ,"river" , "road" , "crop" , "gse_slope30m" ,"build_dens" , "cattle" , "sheep" , "goat" , "donkey" ) #fthese are all the variables we are interested in 
d <- d[myvars]

hw[,42] <- ifelse(hw[,42] < 0 , 0 , hw[,42])
hw[,43] <- ifelse(hw[,43] < 0 , 0 , hw[,43])
hw[,44] <- ifelse(hw[,44] < 0 , 0 , hw[,44])
hw[,45] <- ifelse(hw[,45] < 0 , 0 , hw[,45])

hw$livestock_head <- hw[,42] + hw[,43] +hw[,44] + hw[,45] 

sort(unique(hw$Village)) #check this
###create index variables for each village

d <- hw[hw$farm=="Yes",] #only look at conflicts with households that have farms
d[d == "-2147483648"] <- "NA"

d$see_field <- ifelse(d$field_sight=="Yes" , 1 , 0)
d <- clean_names(d)
d$household_size <- as.integer(d$household_size)
d$guard_ave_day <- ifelse(d$guard_ave_day =="NA" , 0 , d$guard_ave_day)
d$guard_ave_day <- as.integer(d$guard_ave_day)
##############################CROP DAMAGE##############################

myvars <- c("village", "elephant_c", "baboon_c" , "buffalo_c" , "hippo_c" , "other_c" , "farm_size" , "household_size" , "fid" , "settle_dist" , "see_field" , "c70" , "c2070" ,"river" , "crop" , "species" , "build_dens" , "road" , "conflict" , "guard_ave_day" , "livestock_head") #food

dc <- d[myvars]
dc <- dc[dc$species!="lion",]

nrow(dc)
str(dc)
dc <- dc[complete.cases(dc), ] ##we will impute later but we lose 34 observations
dc <- droplevels(dc)
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
dc$guard_ave_day_std <- (dc$guard_ave_day-mean(dc$guard_ave_day) )/sd(dc$guard_ave_day) 


nrow(dc)

dc$species_index <- as.integer(as.factor(dc$species))
dc$village_index <- as.integer(as.factor(dc$village))

###models


mc1 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] ,
    a ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(a,sigma_v),
    as[species_index] ~ dnorm(a,sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1)
    
  ), data=dc , chains=4 , cores=4 , iter=3000)

post <- extract.samples(mc1)
str(post)
dens( logistic(post$a + post$as[,1]) , col="red" , xlim=c(0,1) , ylim=c(0,230)) 
dens( logistic(post$a + post$as[,2] ), col="orange" , add=TRUE) 

precis(mc1, depth=2)

precislist <- list(
  PrBaboon = logistic(post$a + post$as[,1] ),
  PrElephant =logistic(post$a + post$as[,2])
)

plot(precis(precislist , ci=.89) )

##settlement distance
mc2 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] +  b_dSAs[species_index]*settle_dist_km_std ,
    a ~ normal( 0 , 1 ),
    b_dSA ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_dSAs)[species_index] ~ multi_normal( c(a,b_dSA) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(mc2, depth=2)

plot_seq <- seq(from=min(dc$settle_dist_km_std) , to=max(dc$settle_dist_km_std) , length=30)
av_z <- matrix(0,1000,length(unique(dc$village_index))) #need to add zeros in VE to plot main effect


plot(dc$baboon_c ~ dc$settle_dist_km_std , col=col.alpha("slateblue", 0.1) , pch=19 , ylab="baboon bothers" , xlab="standardize km from settlement area")

ylabels=c("baboon bothers" , "elephant trubbelz")
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
    
  ), data=dc , chains=4 , cores=4 , iter=5000 , log_lik=TRUE)

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
    logit(p) <- av[village_index] + as[species_index] + b_RivS[species_index]*river_std,
    a ~ normal( 0 , 1 ),
    b_Riv ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_RivS)[species_index] ~ multi_normal( c(a,b_Riv) , Rho , sigma_s),
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
    logit(p) <- av[village_index] + as[species_index] + b_BuildS[species_index]*build_dens_std,
    a ~ normal( 0 , 1 ),
    b_Build ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_BuildS)[species_index] ~ multi_normal( c(a,b_Build) , Rho , sigma_s),
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
    logit(p) <- av[village_index] + as[species_index] + b_RoadS[species_index]*road_std,
    a ~ normal( 0 , 1 ),
    b_Road ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_RoadS)[species_index] ~ multi_normal( c(a,b_Road) , Rho , sigma_s),
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

###guards
mc11 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_GuardS[species_index]*guard_ave_day_std,
    a ~ normal( 0 , 1 ),
    b_Guard ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_GuardS)[species_index] ~ multi_normal( c(a,b_Guard) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(mc11, depth=2)

plot_seq <- seq(from=min(dc$guard_ave_day_std) , to=max(dc$guard_ave_day_std) , length=30)


for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    guard_ave_day_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(mc11, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(dc$baboon_c ~ dc$guard_ave_day_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="avg folx guarding standardized")}
  if(i==2){plot(dc$elephant_c ~ dc$guard_ave_day_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="avg folx guardin standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

  ##########environ variables
d sett area + c70 + c2070 + river + road + build


############household variabels

HHsize + f size + see farm + guarding

# 
#   mc9 <- ulam(
#   alist(
#     conflict ~ binomial(1,p),
#     logit(p) <- av[village_index] + as[species_index] + b_BuildS[species_index]*build_dens_std,
#     a ~ normal( 0 , 1 ),
#     b_Build ~ normal( 0 , 0.5 ),
#     av[village_index] ~ dnorm(a,sigma_v),
#     c(as,b_BuildS)[species_index] ~ multi_normal( c(a,b_Build) , Rho , sigma_s),
#     c(sigma_v,sigma_s) ~ dexp(1),
#     Rho ~ lkj_corr(3)
#     
#   ), data=dc , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)
# 
# precis(mc9, depth=2)
# 
# plot_seq <- seq(from=min(dc$build_dens_std) , to=max(dc$build_dens_std) , length=30)
# 
# 
# for (i in 1:2){
#   
#   dpred <- list(
#     village_index=rep(1,30),
#     build_dens_std=plot_seq,
#     species_index=rep(i,30)
#   )
#   
#   link2 <- link(mc9, data=dpred , replace=list(village_index=av_z) )
#   
#   if(i==1){plot(dc$baboon_c ~ dc$build_dens_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="building density standardized")}
#   if(i==2){plot(d$elephant_c ~ d$build_dens_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="building density standardized")}
#   pred_mean <- apply(link2 , 2 , mean)
#   lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
#   for (j in sample( c(1:1000) , 100) ){
#     lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
#   }
# }
# 
# 
# mc9 <- ulam(
#   alist(
#     conflict ~ binomial(1,p),
#     logit(p) <- av[village_index] + as[species_index] + b_HHs[species_index]*household_size_std + b_FSs[species_index]*farm_size_std,
#     a ~ normal( 0 , 1 ),
#     c(b_HH,b_FS) ~ normal( 0 , 0.5 ),
#     av[village_index] ~ dnorm(a,sigma_v),
#     c(as,b_HHs,b_FSs)[species_index] ~ multi_normal( c(a,b_HH,b_FS) , Rho , sigma_s),
#     c(sigma_v,sigma_s) ~ dexp(1),
#     Rho ~ lkj_corr(3)
#     
#   ), data=dcrop , chains=4 , cores=4 , iter=5000 , log_lik=TRUE )
# 
# precis(mc9 , depth=2)
# 
# plot_seq <- seq(from=min(dcrop$c2070_std) , to=max(dcrop$c2070_std) , length=30)
# 
# 
# 
# 
# mc10 <- ulam(
#   alist(
#     conflict ~ binomial(1,p),
#     logit(p) <- av[village_index] + as[species_index] + b_HHs[species_index]*household_size_std + (b_FSs[species_index] + b_HHxFSs[species_index]*household_size_std )*farm_size_std,
#     a ~ normal( 0 , 1 ),
#     c(b_HH,b_FS,b_HHxFS) ~ normal( 0 , 0.5 ),
#     av[village_index] ~ dnorm(a,sigma_v),
#     c(as,b_HHs,b_FSs,b_HHxFSs)[species_index] ~ multi_normal( c(a,b_HH,b_FS,b_HHxFS) , Rho , sigma_s),
#     c(sigma_v,sigma_s) ~ dexp(1),
#     Rho ~ lkj_corr(3)
#     
#   ), data=dcrop , chains=4 , cores=4 , iter=5000 , log_lik=TRUE )
# 
# 
# 
# mc11 <- ulam(
#   alist(
#     conflict ~ binomial(1,p),
#     logit(p) <- av[village_index] + as[species_index] + b_HHs[species_index]*household_size_std + b_FSs[species_index]*farm_size_std +  b_dSAs[species_index]*settle_dist_km_std,
#     a ~ normal( 0 , 1 ),
#     c(b_HH,b_FS,b_dSA) ~ normal( 0 , 0.5 ),
#     av[village_index] ~ dnorm(a,sigma_v),
#     c(as,b_HHs,b_FSs,b_dSAs)[species_index] ~ multi_normal( c(a,b_HH,b_FS,b_dSA) , Rho , sigma_s),
#     c(sigma_v,sigma_s) ~ dexp(1),
#     Rho ~ lkj_corr(3)
#     
#   ), data=dcrop , chains=4 , cores=4 , iter=5000 , log_lik=TRUE )
# 
# precis(mc11 , depth=2)
# compare(mc10,mc9,mc8,mc7,mc6,mc5,mc4,mc3)
compare(mc10,mc9,mc8,mc7,mc6,mc5,mc4,mc3,mc2)






###########################LIVESTOCK DAMAGE############################
myvars <- c( "village","elephant_l" , "hyena_l" , "lion_l" , "leopard_l" , "fid" , "settle_dist" , "c70" , "c2070" ,"river" , "road" , "species" , "crop" , "gse_slope30m" , "build_dens" , "household_size" , "guard_ave_day" , "livestock_head") #food

# HH size // livestock
# guard ave day # people ## skip logic don't guard is NA
# number of livestock owened
# cattle sheep goat donkey SUM of them

dl <- d[myvars]
dl <- dl[dl$species=="lion",]
dl <- dl[complete.cases(dl), ]

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
dl$livestock_head_std <- (dl$livestock_head-mean(dl$livestock_head) )/sd(dl$livestock_head)

dL <- dl
dL$conflict <- dL$lion_l
dL$species <- "lion"
dH <- dl
dH$conflict <- dH$hyena_l
dH$species <- "hyena"
dl <-  rbind(dL,dH)

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

ylabels=c("hyena hedonism" , "lion lunches")
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
    logit(p) <- av[village_index] + as[species_index] + b_SlopeS[species_index]*gse_slope30m_std ,
    a ~ normal( 0 , 1 ),
    b_Slope ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_SlopeS)[species_index] ~ multi_normal( c(a,b_Slope) , Rho , sigma_s),
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
    logit(p) <- av[village_index] + as[species_index]  + b_LivestockS[species_index]*livestock_head_std ,
    a ~ normal( 0 , 1 ),
    b_Livestock ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_LivestockS)[species_index] ~ multi_normal( c(a,b_Livestock) , Rho , sigma_s),
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
    logit(p) <- av[village_index] + as[species_index] + b_RivS[species_index]*river_std,
    a ~ normal( 0 , 1 ),
    b_Riv ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_RivS)[species_index] ~ multi_normal( c(a,b_Riv) , Rho , sigma_s),
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
    logit(p) <- av[village_index] + as[species_index] + b_BuildS[species_index]*build_dens_std,
    a ~ normal( 0 , 1 ),
    b_Build ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_BuildS)[species_index] ~ multi_normal( c(a,b_Build) , Rho , sigma_s),
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
    logit(p) <- av[village_index] + as[species_index] + b_RoadS[species_index]*road_std,
    a ~ normal( 0 , 1 ),
    b_Road ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_RoadS)[species_index] ~ multi_normal( c(a,b_Road) , Rho , sigma_s),
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
    logit(p) <- av[village_index] + as[species_index] + b_GuardS[species_index]*guard_ave_day_std,
    a ~ normal( 0 , 1 ),
    b_Guard ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_GuardS)[species_index] ~ multi_normal( c(a,b_Guard) , Rho , sigma_s),
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














#######################old shite##

###intercept only models of elephant
m0 <- ulam(
  alist(
    elephant_c ~ binomial(1,p),
    logit(p) <- a,
    a ~ normal( 0 , 1 )
  ), data=hw , chains=2 , cores=2)

##add varying effects
m1 <- ulam(
  alist(
    elephant_c ~ binomial(1,p),
    logit(p) <- av[village_index],
    a ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(a,sigma),
    sigma ~ dexp(1)
    
  ), data=list(elephant_c=hw$elephant_c , village_index=hw$village_index ) , chains=2 , cores=2)
precis(m1, depth=2)


###add a slope 
m2 <- ulam(
  alist(
    elephant_c  ~ binomial(1,p),
    logit(p) <- av[village_index] + b_dSA*settle_dist_km_std,
    a ~ normal( 0 , 1 ),
    b_dSA ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(a,sigma),
    sigma ~ dexp(1)
    
  ), data=d , chains=2 , cores=2)
precis(m2, depth=2)

#graph preds of main effect
plot_seq <- seq(from=min(d$settle_dist_km_std) , to=max(d$settle_dist_km_std) , length=30)
av_z <- matrix(0,1000,length(unique(d$village_index))) #need to add zeros in VE to plot main effect
dpred <- list(
  village_index=rep(1,30),
  settle_dist_km_std=plot_seq 
)

link2 <- link(m2, data=dpred , replace=list(village_index=av_z) )

plot(d$elephant_c ~ d$settle_dist_km_std , col=col.alpha("slateblue", 0.1) , pch=19 , ylab="elephant trubbelz" , xlab="standardize km from reserve")

pred_mean <- apply(link2 , 2 , mean)
pred_PI <- apply(link2  , 2 , PI , prob=0.91)

lines(pred_mean ~ plot_seq , lw=2, col="slateblue" , lty=1)

for (j in sample( c(1:1000) , 100) ){
  lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha("slateblue", alpha=0.1) , lty=1)
}


###m3 household size
unique(hw$Household_size)
table(hw$Household_size)

###add a slope 
m3 <- ulam(
  alist(
    elephant_c  ~ binomial(1,p),
    logit(p) <- av[village_index] + b_HH*household_size_std,
    a ~ normal( 0 , 1 ),
    b_HH ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(a,sigma),
    sigma ~ dexp(1)
    
  ), data=d , chains=2 , cores=2)
precis(m3, depth=2)

plot_seq <- seq(from=min(d$household_size_std) , to=max(d$household_size_std) , length=30)
av_z <- matrix(0,1000,length(unique(d$village_index))) #need to add zeros in VE to plot main effect
dpred <- list(
  village_index=rep(1,30),
  household_size_std=plot_seq 
)

link2 <- link(m3, data=dpred , replace=list(village_index=av_z) )

plot(d$elephant_c ~ d$household_size_std , col=col.alpha("black", 0.1) , pch=1 , ylab="elephant trubbelz" , xlab="standardized household size")

pred_mean <- apply(link2 , 2 , mean)
pred_PI <- apply(link2  , 2 , PI , prob=0.91)

lines(pred_mean ~ plot_seq , lw=2, col="red" , lty=1)

for (j in sample( c(1:1000) , 100) ){
  lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha("red", alpha=0.1) , lty=1)
}


#farmwalk with everything as the lower limit
m4 <- ulam(
  alist(
    elephant_c  ~ binomial(1,p),
    logit(p) <- av[village_index] + b_FW*farm_walk_low_std,
    a ~ normal( 0 , 1 ),
    b_FW ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(a,sigma),
    sigma ~ dexp(1)
  ), data=d , chains=2 , cores=2)
precis(m4, depth=2)

m5 <- ulam(
  alist(
    elephant_c  ~ binomial(1,p),
    logit(p) <- av[village_index] + b_FW*farm_walk_low_std,
    a ~ normal( 0 , 1 ),
    b_FW ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(a,sigma),
    sigma ~ dexp(1)
  ), data=d , chains=2 , cores=2)
precis(m5, depth=2)

plot_seq <- seq(from=min(d$household_size_std) , to=max(d$household_size_std) , length=30)
av_z <- matrix(0,1000,length(unique(d$village_index))) #need to add zeros in VE to plot main effect
dpred <- list(
  village_index=rep(1,30),
  household_size_std=plot_seq 
)

link2 <- link(m3, data=dpred , replace=list(village_index=av_z) )

plot(d$elephant_c ~ d$household_size_std , col=col.alpha("black", 0.1) , pch=1 , ylab="elephant trubbelz" , xlab="standardized household size")

pred_mean <- apply(link2 , 2 , mean)
pred_PI <- apply(link2  , 2 , PI , prob=0.91)

lines(pred_mean ~ plot_seq , lw=2, col="red" , lty=1)

for (j in sample( c(1:1000) , 100) ){
  lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha("red", alpha=0.1) , lty=1)
}

###############

d$farm_walk_index <- as.integer(as.factor(d$farm_walk_low_std))

m6 <- ulam(
  alist(
    elephant_c  ~ binomial(1,p),
    logit(p) <- av[village_index] + b_FW[farm_walk_index],
    a ~ normal( 0 , 1 ),
    b_FW[farm_walk_index] ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma),
    sigma ~ dexp(1)
  ), data=d , chains=2 , cores=2)

plot(precis(m6, depth=2))
post <- extract.samples(m6)
str(post)
dens( logistic(post$a + post$b_FW[,1]) , col="red" , xlim=c(0.5,1) , ylim=c(0,10)) 
dens( logistic(post$a + post$b_FW[,2] ), col="orange" , add=TRUE) 
dens( logistic(post$a + post$b_FW[,3]) , col="green" , add=TRUE) 
dens( logistic(post$a + post$b_FW[,4]) , col="blue" , add=TRUE) 
dens( logistic(post$a + post$b_FW[,5] ), col="violet" , add=TRUE) 

#or a dot plot can be done of probs
precislist <- list(
  PrEC_0_to_15min = logistic(post$a + post$b_FW[,1] ),
  PrEC_15_to_30min = logistic(post$a + post$b_FW[,2] ),
  PrEC_30_to_45min =logistic(post$a + post$b_FW[,3] ),
  PrEC_45_to_60min =logistic(post$a + post$b_FW[,4] ),
  PrEC_greater_than_60min =logistic(post$a + post$b_FW[,5] )
)

plot(precis(precislist , depth=2) )

m7 <- ulam(
  alist(
    elephant_c  ~ binomial(1,p),
    logit(p) <- av[village_index] + b_FS*farm_size_std,
    a ~ normal( 0 , 1 ),
    b_FS ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(a,sigma),
    sigma ~ dexp(1)
  ), data=d , chains=2 , cores=2)
precis(m7, depth=2)

plot_seq <- seq(from=min(d$farm_size_std) , to=max(d$farm_size_std) , length=30)
av_z <- matrix(0,1000,length(unique(d$village_index))) #need to add zeros in VE to plot main effect
dpred <- list(
  village_index=rep(1,30),
  farm_size_std=plot_seq 
)

link2 <- link(m7, data=dpred , replace=list(village_index=av_z) )

plot(d$elephant_c ~ d$farm_size_std , col=col.alpha("black", 0.1) , pch=1 , ylab="elephant trubbelz" , xlab="standardized farm size")

pred_mean <- apply(link2 , 2 , mean)
pred_PI <- apply(link2  , 2 , PI , prob=0.91)

lines(pred_mean ~ plot_seq , lw=2, col="orange" , lty=1)

for (j in sample( c(1:1000) , 100) ){
  lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha("orange", alpha=0.1) , lty=1)
}

m8 <- ulam(
  alist(
    elephant_c  ~ binomial(1,p),
    logit(p) <- av[village_index] + b_FW[farm_walk_index] + b_HH*household_size_std + b_dSA*settle_dist_km_std,
    a ~ normal( 0 , 1 ),
    b_FW[farm_walk_index] ~ normal( 0 , 0.5 ),
    c(b_HH,b_dSA) ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma),
    sigma ~ dexp(1)
  ), data=d , chains=2 , cores=2 , iter=3000)

precis(m7, depth=2)


##Lions specifically point pattern
##Elephants sdms? pseudo absence

#household size + distance to farm (Farmwlak)are important. 
unique(hw$Farm_walk)
table(hw$Farm_walk)

m9 <- ulam(
  alist(
    elephant_c  ~ binomial(1,p),
    logit(p) <- av[village_index] + b_FW[farm_walk_index] + b_HH*household_size_std ,
    a ~ normal( 0 , 1 ),
    b_FW[farm_walk_index] ~ normal( 0 , 0.5 ),
    b_HH ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma),
    sigma ~ dexp(1)
  ), data=d , chains=2 , cores=2 , iter=3000)

precis(m9 , depth=2)
#Environmental Models

#Social Models

#Both 



#########add all the leaf lovers to a single long dataframe####
dE <- d
dE$conflict <- dE$elephant_c
dE$species <- "elephant"
dB <- d
dB$conflict <- dB$baboon_c
dB$species <- "baboon"
dF <- d
dF$conflict <- dF$buffalo_c
dF$species <- "buffalo"
dH <- d
dH$conflict <- dH$hippo_c
dH$species <- "hippo"
dO <- d
dO$conflict <- dO$other_c
dO$species <- "other"

dcrop <- rbind(dE,dB) #make it long, baby relax

dcrop$species_index <- as.integer(as.factor(dcrop$species))
dcrop$farm_walk_index <- as.integer(as.factor(dcrop$farm_walk_low_std))
# dcrop$c70_std <- (dcrop$cov70per-mean(dcrop$cov70per) )/sd(dcrop$cov70per)
# dcrop$c2070_std <- (dcrop$cov2070per-mean(dcrop$cov2070per) )/sd(dcrop$cov2070per)

dc$species_index <- as.integer(as.factor(dc$species))


mc1 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] ,
    a ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(a,sigma_v),
    as[species_index] ~ dnorm(a,sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1)
    
  ), data=dcrop , chains=4 , cores=4 , iter=3000)

post <- extract.samples(mc1)
str(post)
dens( logistic(post$a + post$as[,1]) , col="red" , xlim=c(0,1) , ylim=c(0,230)) 
dens( logistic(post$a + post$as[,2] ), col="orange" , add=TRUE) 
# dens( logistic(post$a + post$as[,3]) , col="green" , add=TRUE) 
# dens( logistic(post$a + post$as[,4]) , col="blue" , add=TRUE) 
# dens( logistic(post$a + post$as[,5] ), col="violet" , add=TRUE) 

precis(mc1, depth=2)

precislist <- list(
  PrBaboon = logistic(post$a + post$as[,1] ),
  PrBuffalo = logistic(post$a + post$as[,2] ),
  PrElephant =logistic(post$a + post$as[,3]),
  PrHippo =logistic(post$a + post$as[,4] ),
  PrOther =logistic(post$a + post$as[,5] )
)

plot(precis(precislist , ci=.94) )

mc2 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] +  b_dSAs[species_index]*settle_dist_km_std ,
    a ~ normal( 0 , 1 ),
    b_dSA ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_dSAs)[species_index] ~ multi_normal( c(a,b_dSA) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dcrop , chains=4 , cores=4 , iter=5000 , log_lik=TRUE)

precis(mc2, depth=2)

plot_seq <- seq(from=min(dcrop$settle_dist_km_std) , to=max(dcrop$settle_dist_km_std) , length=30)
av_z <- matrix(0,1000,length(unique(dcrop$village_index))) #need to add zeros in VE to plot main effect


plot(d$baboon_c ~ d$settle_dist_km_std , col=col.alpha("slateblue", 0.1) , pch=19 , ylab="baboon bothers" , xlab="standardize km from settlement area")

ylabels=c("baboon bothers" , "elephant trubbelz")
colpal=c("blue" , "grey")
for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(mc2, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(d$baboon_c ~ d$settle_dist_km_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="standardize km from settlement")}
  if(i==2){plot(d$elephant_c ~ d$settle_dist_km_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="standardize km from settlement")}
  # plot(d$elephant_c ~ d$settle_dist_km_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="standardize km from reserve")
  pred_mean <- apply(link2 , 2 , mean)
  #pred_PI <- apply(link2  , 2 , PI , prob=0.91)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

precis(mc2 , depth=3)

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
    
  ), data=dcrop , chains=4 , cores=4 , iter=5000 , log_lik=TRUE)

precis(mc3, depth=2)


plot_seq <- seq(from=min(dcrop$household_size_std) , to=max(dcrop$household_size_std) , length=30)


for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    household_size_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(mc3, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(d$baboon_c ~ d$household_size_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size standardized")}
  if(i==2){plot(d$elephant_c ~ d$household_size_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

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
    
  ), data=dcrop , chains=4 , cores=4 , iter=5000 , log_lik=TRUE)

precis(mc4, depth=2)


plot_seq <- seq(from=min(dcrop$farm_size_std) , to=max(dcrop$farm_size_std) , length=30)


for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    farm_size_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(mc4, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(d$baboon_c ~ d$farm_size_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="farm size standardized")}
  if(i==2){plot(d$elephant_c ~ d$farm_size_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="farm size standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}


mc5 <- ulam(
  alist(
    conflict  ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index]  + b_FWs[species_index]*farm_walk_low_std ,
    a ~ normal( 0 , 1 ),
    b_FW ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_FWs)[species_index] ~ multi_normal( c(a,b_FW) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
  ), data=dcrop , chains=4 , cores=4 , iter=5000 , log_lik=TRUE)

precis(mc5, depth=2)

###need to look at index version, soe coding issues

mc6 <- ulam(
  alist(
    conflict  ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index]  + b_SEEs[species_index]*see_field ,
    a ~ normal( 0 , 1 ),
    b_SEE ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_SEEs)[species_index] ~ multi_normal( c(a,b_SEE) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
  ), data=dcrop , chains=4 , cores=4 , iter=5000 , log_lik=TRUE)


  dpred <- list(
    village_index=rep(1,2),
    see_field=c(0,1),
    species_index=rep(1,2)
  )
  
  link2 <- link(mc6, data=dpred , replace=list(village_index=av_z) )
  
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
  dens(link2[,1] , lty=2 , col="grey" , ylim=c(0,15) , xlim=c(0.2,1) , main="elephant conplict probz")
  abline(v=mean(link2[,1]) , col="grey" , lty=2)
  dens(link2[,2] , add=TRUE , col="black" )
  abline(v=mean(link2[,2]) , col="black" , lty=1)

  mc7 <- ulam(
    alist(
      conflict ~ binomial(1,p),
      logit(p) <- av[village_index] + as[species_index] + b_C70s[species_index]*c70_std ,
      a ~ normal( 0 , 1 ),
      b_C70 ~ normal( 0 , 0.5 ),
      av[village_index] ~ dnorm(a,sigma_v),
      c(as,b_C70s)[species_index] ~ multi_normal( c(a,b_C70) , Rho , sigma_s),
      c(sigma_v,sigma_s) ~ dexp(1),
      Rho ~ lkj_corr(3)
      
    ), data=dcrop , chains=4 , cores=4 , iter=5000 , log_lik=TRUE)
  
precis(mc7, depth=2)


plot_seq <- seq(from=min(dcrop$c70_std) , to=max(dcrop$c70_std) , length=30)


for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    c70_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(mc7, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(d$baboon_c ~ d$c70_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="Tree Cover 70 percent standardized")}
  if(i==2){plot(d$elephant_c ~ d$c70_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="Tree Cover 70 percent standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}


mc8 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_C2070s[species_index]*c2070_std ,
    a ~ normal( 0 , 1 ),
    b_C2070 ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_C2070s)[species_index] ~ multi_normal( c(a,b_C2070) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dcrop , chains=4 , cores=4 , iter=5000 , log_lik=TRUE)

precis(mc8 , depth=2)

plot_seq <- seq(from=min(dcrop$c2070_std) , to=max(dcrop$c2070_std) , length=30)


for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,30),
    c2070_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(mc8, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(d$baboon_c ~ d$c2070_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="Tree Cover 20-70 percent standardized")}
  if(i==3){plot(d$elephant_c ~ d$c2070_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="Tree Cover 20-70 percent standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

mc9 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_HHs[species_index]*household_size_std + b_FSs[species_index]*farm_size_std,
    a ~ normal( 0 , 1 ),
    c(b_HH,b_FS) ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_HHs,b_FSs)[species_index] ~ multi_normal( c(a,b_HH,b_FS) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dcrop , chains=4 , cores=4 , iter=5000 , log_lik=TRUE )

precis(mc9 , depth=2)

plot_seq <- seq(from=min(dcrop$c2070_std) , to=max(dcrop$c2070_std) , length=30)

mc10 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_HHs[species_index]*household_size_std + (b_FSs[species_index] + b_HHxFSs[species_index]*household_size_std )*farm_size_std,
    a ~ normal( 0 , 1 ),
    c(b_HH,b_FS,b_HHxFS) ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_HHs,b_FSs,b_HHxFSs)[species_index] ~ multi_normal( c(a,b_HH,b_FS,b_HHxFS) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dcrop , chains=4 , cores=4 , iter=5000 , log_lik=TRUE )



mc11 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_HHs[species_index]*household_size_std + b_FSs[species_index]*farm_size_std +  b_dSAs[species_index]*settle_dist_km_std,
    a ~ normal( 0 , 1 ),
    c(b_HH,b_FS,b_dSA) ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_HHs,b_FSs,b_dSAs)[species_index] ~ multi_normal( c(a,b_HH,b_FS,b_dSA) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dcrop , chains=4 , cores=4 , iter=5000 , log_lik=TRUE )

precis(mc11 , depth=2)
compare(mc10,mc9,mc8,mc7,mc6,mc5,mc4,mc3)


####river
mc12 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_RivS[species_index]*river_std,
    a ~ normal( 0 , 1 ),
    b_Riv ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_RivS)[species_index] ~ multi_normal( c(a,b_Riv) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=dcrop , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(mc12, depth=2)


plot_seq <- seq(from=min(dcrop$river_std) , to=max(dcrop$river_std) , length=30)


for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    river_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(mc12, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(d$baboon_c ~ d$river_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="river standardized")}
  if(i==2){plot(d$elephant_c ~ d$river_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="river standardized")}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}

######crop

##############
###babboonz only
################

mb2 <- ulam(
  alist(
    baboon_c  ~ binomial(1,p),
    logit(p) <- av[village_index] + b_dSA*settle_dist_km_std,
    a ~ normal( 0 , 1 ),
    b_dSA ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(a,sigma),
    sigma ~ dexp(1)
    
  ), data=d , chains=2 , cores=2)
precis(mb2, depth=2)

#graph preds of main effect
plot_seq <- seq(from=min(d$settle_dist_km_std) , to=max(d$settle_dist_km_std) , length=30)
av_z <- matrix(0,1000,length(unique(d$village_index))) #need to add zeros in VE to plot main effect
dpred <- list(
  village_index=rep(1,30),
  settle_dist_km_std=plot_seq 
)

link2 <- link(mb2, data=dpred , replace=list(village_index=av_z) )

plot(d$baboon_c ~ d$settle_dist_km_std , col=col.alpha("slateblue", 0.1) , pch=19 , ylab="baboobn badness" , xlab="standardize km from reserve")

pred_mean <- apply(link2 , 2 , mean)
pred_PI <- apply(link2  , 2 , PI , prob=0.91)

lines(pred_mean ~ plot_seq , lw=2, col="slateblue" , lty=1)

for (j in sample( c(1:1000) , 100) ){
  lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha("slateblue", alpha=0.1) , lty=1)
}


mb3 <- ulam(
  alist(
    baboon_c  ~ binomial(1,p),
    logit(p) <- av[village_index] + b_HH*household_size_std,
    a ~ normal( 0 , 1 ),
    b_HH ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(a,sigma),
    sigma ~ dexp(1)
    
  ), data=d , chains=2 , cores=2)
precis(mb3, depth=2)

plot_seq <- seq(from=min(d$household_size_std) , to=max(d$household_size_std) , length=30)
av_z <- matrix(0,1000,length(unique(d$village_index))) #need to add zeros in VE to plot main effect
dpred <- list(
  village_index=rep(1,30),
  household_size_std=plot_seq 
)

link2 <- link(mb3, data=dpred , replace=list(village_index=av_z) )

plot(d$baboon_c ~ d$household_size_std , col=col.alpha("black", 0.1) , pch=1 , ylab="baboon trubbelz" , xlab="standardized household size")

pred_mean <- apply(link2 , 2 , mean)
pred_PI <- apply(link2  , 2 , PI , prob=0.91)

lines(pred_mean ~ plot_seq , lw=2, col="red" , lty=1)

for (j in sample( c(1:1000) , 100) ){
  lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha("red", alpha=0.1) , lty=1)
}


#farmwalk with everything as the lower limit
mb4 <- ulam(
  alist(
    baboon_c  ~ binomial(1,p),
    logit(p) <- av[village_index] + b_FW*farm_walk_low_std,
    a ~ normal( 0 , 1 ),
    b_FW ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(a,sigma),
    sigma ~ dexp(1)
  ), data=d , chains=2 , cores=2)
precis(mb4, depth=2)

mb5 <- ulam(
  alist(
    baboon_c  ~ binomial(1,p),
    logit(p) <- av[village_index] + b_FW*farm_walk_low_std,
    a ~ normal( 0 , 1 ),
    b_FW ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(a,sigma),
    sigma ~ dexp(1)
  ), data=d , chains=2 , cores=2)
precis(mb5, depth=2)


d$farm_walk_index <- as.integer(as.factor(d$farm_walk_low_std))

mb6 <- ulam(
  alist(
    baboon_c  ~ binomial(1,p),
    logit(p) <- av[village_index] + b_FW[farm_walk_index],
    a ~ normal( 0 , 1 ),
    b_FW[farm_walk_index] ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma),
    sigma ~ dexp(1)
  ), data=d , chains=2 , cores=2)
plot(precis(m6, depth=2))
post <- extract.samples(mb6)
str(post)
dens( logistic(post$a + post$b_FW[,1]) , col="red" , xlim=c(0.5,1) , ylim=c(0,10)) 
dens( logistic(post$a + post$b_FW[,2] ), col="orange" , add=TRUE) 
dens( logistic(post$a + post$b_FW[,3]) , col="green" , add=TRUE) 
dens( logistic(post$a + post$b_FW[,4]) , col="blue" , add=TRUE) 
dens( logistic(post$a + post$b_FW[,5] ), col="violet" , add=TRUE) 

#or a dot plot can be done of probs
precislist <- list(
  PrEC_0_to_15min = logistic(post$a + post$b_FW[,1] ),
  PrEC_15_to_30min = logistic(post$a + post$b_FW[,2] ),
  PrEC_30_to_45min =logistic(post$a + post$b_FW[,3] ),
  PrEC_45_to_60min =logistic(post$a + post$b_FW[,4] ),
  PrEC_greater_than_60min =logistic(post$a + post$b_FW[,5] )
)

plot(precis(precislist , depth=2) )
m7 <- ulam(
  alist(
    baboon_c  ~ binomial(1,p),
    logit(p) <- av[village_index] + b_FS*farm_size_std,
    a ~ normal( 0 , 1 ),
    b_FS ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(a,sigma),
    sigma ~ dexp(1)
  ), data=d , chains=2 , cores=2)
precis(m7, depth=2)

plot_seq <- seq(from=min(d$farm_size_std) , to=max(d$farm_size_std) , length=30)
av_z <- matrix(0,1000,length(unique(d$village_index))) #need to add zeros in VE to plot main effect
dpred <- list(
  village_index=rep(1,30),
  farm_size_std=plot_seq 
)

link2 <- link(m7, data=dpred , replace=list(village_index=av_z) )

plot(d$baboon_c ~ d$farm_size_std , col=col.alpha("black", 0.1) , pch=1 , ylab="baboon trubbelz" , xlab="standardized farm size")

pred_mean <- apply(link2 , 2 , mean)
pred_PI <- apply(link2  , 2 , PI , prob=0.91)

lines(pred_mean ~ plot_seq , lw=2, col="orange" , lty=1)

for (j in sample( c(1:1000) , 100) ){
  lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha("orange", alpha=0.1) , lty=1)
}

#no landscan layer
#human disturbance variables are likely multicollinear

##elephant and baboons crop damage
#1) HHsize and farm size
#2) building density + walk and/or seeing the field
#3) rivers (1 layer/major minor)-elephants distance vs. density (PA might interact)
#4) river + cover70 might be correlated...
#5) PAdist + rivers
#6) guarding behavior and household size and farm size (interaction guards/area unit)
#7) NDVImax 

plot(d$household_size_std,d$farm_size_std , col=col.alpha("orange", alpha=0.1) , pch=19)


##################################
###gnashy-cow-eaters##############
##################################
dL <- d
dL$conflict <- dL$lion_l
dL$species <- "lion"
dH <- d
dH$conflict <- dH$hyena_l
dH$species <- "hyena"
dP <- d
dP$conflict <- dP$leopard_l
dP$species <- "leopard"
dEl <- d
dEl$conflict <- dEl$elephant_l
dEl$species <- "elephant"
d_stock <- rbind(dEl,dL,dP,dH)
d_stock$species_index <- as.integer(as.factor(d_stock$species))
d_stock$farm_walk_index <- as.integer(as.factor(d_stock$farm_walk_low_std))


ml1 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] ,
    a ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(a,sigma_v),
    as[species_index] ~ dnorm(a,sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1)
    
  ), data=d_stock , chains=4 , cores=4 , iter=3000)

post <- extract.samples(ml1)
str(post)
dens( logistic(post$a + post$as[,1]) , col="red" , xlim=c(0,0.1) , ylim=c(0,230)) 
dens( logistic(post$a + post$as[,2] ), col="orange" , add=TRUE) 
dens( logistic(post$a + post$as[,3]) , col="green" , add=TRUE) 
dens( logistic(post$a + post$as[,4]) , col="blue" , add=TRUE) 

precis(ml1, depth=2)

precislist <- list(
  PrElephant =logistic(post$a + post$as[,1]),
  PrHyena =logistic(post$a + post$as[,2] ),
  PrLeopard = logistic(post$a + post$as[,3] ),
  PrLion =logistic(post$a + post$as[,4] )
)

plot(precis(precislist , ci=.94) )
#######settlement areas

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
    
  ), data=d_stock , chains=4 , cores=4 , iter=5000 , log_lik=TRUE)

precis(ml2, depth=2)

plot_seq <- seq(from=min(d_stock$settle_dist_km_std) , to=max(d_stock$settle_dist_km_std) , length=30)
av_z <- matrix(0,1000,length(unique(d_stock$village_index))) #need to add zeros in VE to plot main effect


plot(d$elephant_l ~ d$settle_dist_km_std , col=col.alpha("slateblue", 0.1) , pch=19 , ylab="elephant goat stompin " , xlab="standardize km from settlement area")

ylabels=c("elephant goat stompin" , "hyena hustlez" , "leopard chompz" , "lion snax")
colpal=c("blue" , "grey" , "violet" , "gold")
for (i in 1:4){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(ml2, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){plot(d$elephant_l ~ d$settle_dist_km_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="standardize km from settlement")}
  if(i==2){plot(d$hyena_l ~ d$settle_dist_km_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="standardize km from settlement")}
  if(i==3){plot(d$leopard_l ~ d$settle_dist_km_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="standardize km from settlement")}
  if(i==3){plot(d$lion_l ~ d$settle_dist_km_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="standardize km from settlement")}
  pred_mean <- apply(link2 , 2 , mean)
  #pred_PI <- apply(link2  , 2 , PI , prob=0.91)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}


########road density
ml3 <- ulam(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- av[village_index] + as[species_index] + b_roadS[species_index]*road_std ,
    a ~ normal( 0 , 1 ),
    b_road ~ normal( 0 , 0.5 ),
    av[village_index] ~ dnorm(a,sigma_v),
    c(as,b_roadS)[species_index] ~ multi_normal( c(a,b_road) , Rho , sigma_s),
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ lkj_corr(3)
    
  ), data=d_stock , chains=4 , cores=4 , iter=3000 , log_lik=TRUE)

precis(mc7, depth=2)
