library('rethinking')
linrary('rstan')
# hw <- read.csv("~/Downloads/SS_Final_NoDups.csv") #old version with errorz
#hw <- read.csv("~/Downloads/Spatial_Household_Survey_Clean_1Jun20sp.csv") 
hw <-  read.csv("~/Dropbox/tza conflict/HWc_surveyClean_extract_envslopbuild.csv")

#new verzion
str(hw)


hw$elephant_c <- ifelse(grepl("Elephant", hw$crop_species), 1, 0)
hw$BaboonC <- ifelse(grepl("Baboon", hw$crop_species), 1, 0)
hw$BuffaloC <- ifelse(grepl("Buffalo", hw$crop_species), 1, 0)
hw$HippoC <- ifelse(grepl("Hippo", hw$crop_species), 1, 0)
hw$OtherC <- ifelse(grepl("Other", hw$crop_species), 1, 0)

hw$ElephantL <- ifelse(grepl("Elephant", hw$species_med_lv), 1, 0)
hw$HyenaL <- ifelse(grepl("Hyena", hw$species_med_lv), 1, 0)
hw$LionL <- ifelse(grepl("Lion", hw$species_med_lv), 1, 0)
hw$LeopardL <- ifelse(grepl("Leopard", hw$species_med_lv), 1, 0)

table(hw$Species_medLV) #livestock that is not chuckens or ducks

table(hw$Crop_species) #species that eff up

#Possibly combine Baboon, monkey

#Primate crop damage

#elephant crop damage

#Lion livestock

#Hyena livestock

#correlegram (spelling)
#scale defined by species
#Elephant mean daily displacement (5km)
#K to run mdd (packer 3-5km/day)
#Hyena

#okay so! The environmental variables too!

require(rethinking)
require(lubridate)
require(RColorBrewer)
library(janitor)

sort(unique(hw$Village)) #check this
###create index variables for each village

d <- hw[hw$farm=="Yes",] #only look at conflicts with households that have farms
d[d == "-2147483648"] <- "NA"

d$see_field <- ifelse(d$field_sight=="Yes" , 1 , 0)
d <- clean_names(d)


myvars <- c("village", "elephant_c", "baboon_c" , "buffalo_c" , "hippo_c" , "other_c" , "elephant_l" , "hyena_l" , "lion_l" , "leopard_l" , "farm_walk" , "farm_size" , "household_size" , "fid" , "settle_dist" , "see_field" , "c70" , "c2070" ,"river" , "road" , "crop" ) #food uncertaincy + education
d <- d[myvars]
nrow(d)
str(d)
d$household_size <- as.integer(d$household_size)
d <- d[complete.cases(d), ] ##we will impute later but we lose 34 observations
nrow(d)
d$settle_dist_km <- d$settle_dist/1000
d$settle_dist_km_std <- (d$settle_dist_km-mean(d$settle_dist_km) )/sd(d$settle_dist_km) #standardize so intercept is meaningful and it fits
d$farm_size_std <- (d$farm_size-mean(d$farm_size) )/sd(d$farm_size)
d$household_size_std <- (d$household_size-mean(d$household_size) )/sd(d$household_size)
d <- droplevels(d)
d$farm_walk_low <- 44
d$farm_walk_low <-ifelse(d$farm_walk=='0_15' , 0 , d$farm_walk_low )
d$farm_walk_low <-ifelse(d$farm_walk=='15_30' , 15 , d$farm_walk_low )
d$farm_walk_low <-ifelse(d$farm_walk=='30_45' , 30 , d$farm_walk_low )
d$farm_walk_low <-ifelse(d$farm_walk=='45_60' , 45 , d$farm_walk_low )
d$farm_walk_low <-ifelse(d$farm_walk=='60_plus' , 60 , d$farm_walk_low )
d$farm_walk_low_std <- (d$farm_walk_low -mean(d$farm_walk_low ) )/sd(d$farm_walk_low)
d$farm_size_std <- (d$farm_size -mean(d$farm_size ) )/sd(d$farm_size)
d$c70_std <- (d$c70-mean(d$c70) )/sd(d$c70)
d$c2070_std <- (d$c2070-mean(d$c2070) )/sd(d$c2070)
d$village <- as.character(d$village)
d$village[d$fid==179] <- "Nyamatoke_jklol"
d$village_index <- as.integer(as.factor(d$village))
d$river_std <- (d$river-mean(d$river) )/sd(d$river)
d$road_std <- (d$road-mean(d$road) )/sd(d$road)
d$crop_std <- (d$crop-mean(d$crop) )/sd(d$crop)

nrow(d)
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
