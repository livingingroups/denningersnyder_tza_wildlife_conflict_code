library('rstan')
require(rethinking)
require(lubridate)
require(RColorBrewer)
require(janitor)

this <- system('hostname', TRUE)
if (this == "DESKTOP-J9EEJ0L") {
  dp <- "C:/Users/Kate/Dropbox/R_data/Dis_Prop/LUC/hwc_risk/data"
} else {
  dp <- "Brendan or kristen's working directory" ##
}

setwd(dp) ### set directory

#hw <- read.csv("C:/Users/Kate/Dropbox/tza_wildlife_conflict/HWc_surveyClean_extract_envslopbuildALLSPECIESVervet41.csv")
hw <- read.csv("HWc_surveyClean_extract_envslopbuildALLSPECIESVervet41.csv")

###eleganterish way to replace NA with zero so
names(hw[42:49]) #check names. assume are livestock types in order

for (species in names(hw[42:49]) ){
  hw[which( is.na(hw[,species]) ) , species] <- 0 #replaces NA with zeros
  hw[,species] <- ifelse(hw[,species] < 0 , 0 , hw[,species]) #makes negative values zero
}

hw$guard_ave_day <- ifelse(hw$guard_ave_day < 0 , 0 , hw$guard_ave_day)
hw$livestock_head <- hw[,"cattle"] + hw[,"sheep"] + hw[,"goat"] + hw[,"donkey"]
hw$livestock_head_all <- hw[,"cattle"] +hw[,"sheep"] + hw[,"goat"] + hw[,"donkey"] +  hw[,"dog"] +  hw[,"chicken"] +  hw[,"duck"] +hw[,"other_livestock"] #i don't think this is correct anymore

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


d <- janitor::clean_names(hw)

d$med_lar_lv_prot
d$months_planted

##need household ID
myvars <- c("conflict" , "village", "species" , "elephant_c", "baboon_c" , "vervet_c", "hyena_l" , "lion_l" , "farm_walk" , "farm_size" , "household_size" , "fid" , "settle_dist" , "see_field" , "c70" , "c2070" ,"river" , "road" , "crop" , "gse_slope30m" ,"build_dens" , "cattle" , "sheep" , "goat" , "donkey" , "farm" , "livestock" , "months_planted" , "lv_prot_day_guard" , "lv_prot_day_dogs" , "lv_prot_night_dogs" , "lv_prot_night_contain" , "crop_prot_music" , "crop_prot_w_fence" , "crop_prot_sisal" , "crop_prot_shout" , "crop_prot_fire" , "crop_prot_chase" , "crop_prot_guard" , "guard_ave_day" , "num_crop_prot_strats" , "livestock_head")
#fthese are all the variables we are interested in 
d <- d[myvars]


sort(unique(hw$village)) #check this
###create index variables for each village

d[d == "-2147483648"] <- "NA"

##############################CROP DAMAGE##############################

#fthese are all the variables we are interested in 

myvars2 <- c("conflict" , "village", "elephant_c", "baboon_c" ,"vervet_c",  "farm_size" , "household_size" , "fid" , "settle_dist" , "see_field" , "c70" , "c2070" ,"river" , "road" , "crop" ,"build_dens" , "farm"  , "months_planted"  , "crop_prot_music" , "crop_prot_w_fence" , "crop_prot_sisal" , "crop_prot_shout" , "crop_prot_fire" , "crop_prot_chase" , "crop_prot_guard" , "species" , "num_crop_prot_strats" )

dc <- d[myvars2]

dc <- dc[dc$species!="lion",]
dc <- dc[dc$species!="hyena",]

dc <- dc[dc$farm==1,] #only look at conflicts with households that have farms


nrow(dc) #now 1239
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

###########################LIVESTOCK DAMAGE############################
##########################################################################

myvars3 <- c( "village" , "hyena_l" , "lion_l", "fid" , "settle_dist" , "c70" , "c2070" ,"river" , "road" , "species" , "crop" , "gse_slope30m" , "build_dens" , "household_size" , "guard_ave_day" , "livestock_head" , "lv_prot_day_guard" , "lv_prot_day_dogs" , "lv_prot_night_dogs" , "lv_prot_night_contain" , "livestock") #food

# HH size // livestock
# guard ave day # people ## skip logic don't guard is NA
# number of livestock owened
# cattle sheep goat donkey SUM of them

dl <- d[myvars3]
dl <- dl[dl$livestock==1,] #only look at folks with livestock
dl <- dl[dl$species=="lion",]
dl$guard_ave_day[is.na(dl$guard_ave_day)] <- 0 #replaced NA with zeros, we hAVE INFO THAT THERE WAS GUARDS IN dl$guard_ave_day
##we can also look at tj
dl <- dl[complete.cases(dl), ] #good to manually check most like

dL <- dl
dL$conflict <- dL$lion_l
dL$species <- "lion"
dH <- dl
dH$conflict <- dH$hyena_l
dH$species <- "hyena"


dl <-  rbind(dL,dH)

dl <- dl[dl$livestock_head>0,] ##remove people with livestock, but no mediuum or large
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
