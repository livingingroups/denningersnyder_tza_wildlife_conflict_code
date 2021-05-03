#take predictions from posterior mean and make them a raster 

#https://www.rdocumentation.org/packages/raster/versions/3.1-5/topics/rasterFromXYZ
library(raster)
library(janitor)


ros <- shapefile("data/GSME/Roads/GSME_roads/GSME_roads.shp")
crs2 <- crs(ros)

lcst5k <- stack("data/LCC_rivrastbuildslopesettleStack5k.tif")

#xyzz <- read.csv("elephantRasterstacktopoints_survext.csv")


###Survey manipulation for plotting####
surv <- read.csv("data/Spatial_Household_Survey_Clean_1Jun20.csv", stringsAsFactors = FALSE)

hw <- read.csv("C:/Users/Kate/Dropbox/tza_wildlife_conflict/HWc_surveyClean_extract_envslopbuildALLSPECIESVervet41.csv")


surv$ElephantC <- ifelse(grepl("Elephant", surv$Crop_species), 1, 0) 
surv$BaboonC <- ifelse(grepl("Baboon", surv$Crop_species), 1, 0) 
surv$BuffaloC <- ifelse(grepl("Buffalo", surv$Crop_species), 1, 0) 
surv$HippoC <- ifelse(grepl("Hippo", surv$Crop_species), 1, 0) 
surv$OtherC <- ifelse(grepl("Other", surv$Crop_species), 1, 0) 
surv$VervetC <- ifelse(grepl("Tumbili", surv$Other_crop_species), 1, 0) 

surv$ElephantL <- ifelse(grepl("Elephant", surv$Species_medLV), 1, 0) 
surv$HyenaL <- ifelse(grepl("Hyena", surv$Species_medLV), 1, 0) 
surv$LionL <- ifelse(grepl("Lion", surv$Species_medLV), 1, 0) 
surv$LeopardL <- ifelse(grepl("Leopard", surv$Species_medLV), 1, 0) 
surv <- clean_names(surv)
coordinates(surv) <- ~household_location_longitude+household_location_latitude
crs(surv) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
surv <- spTransform(surv, crs(ros))

########
lc <- lcst5k[[1]]
lc2 <- crop(lc, extent(surv))



#Read in grumeti shapefile
grum <- shapefile("data/GSME/Boundaries/GSE/GSE_Makundusi_updated.shp")
gonly <- shapefile("C:/Users/Kate/Dropbox/R_data/Dis_Prop/LUC/grumeti-remote-sensing/GRBndwLL/GRBndwLL/GRBndwLL.shp")

gonly <- spTransform(gonly, crs(grum))




#######Elephants 


#ep <- read.csv("C:/Users/Kate/Dropbox/tza_wildlife_conflict/ras_elephant_crop_preds.csv") #previous 7/21 
#ep <- read.csv("C:/Users/Kate/Dropbox/tza_wildlife_conflict/raster5august2020preds/ras_elephant_crop_preds.csv")

ep <- read.csv("C:/Users/Kate/Dropbox/tza_wildlife_conflict/ras_elephant_crop_predsv.csv")

head(ep)
ep2 <- ep[,c("x","y", "ras_ele.pred_ele_crop_conflict")]


ras <- raster(extent(lc2), crs=projection(lc2), resolution=res(lc2))

cells <- cellFromXY(ras, ep2[,1:2])
ras[cells] <- ep2[,3]




ras2 <- ras*1000000

plot(ras)
plot(grum, add=T)
plot(surv[surv$elephant_c == 0 & surv$farm == "Yes",], add=T, pch = 19, col="red")
plot(surv[surv$elephant_c == 1,], add=T, pch = 19, col="black")

writeRaster(ras, "C:/Users/Kate/Dropbox/tza_wildlife_conflict/elephant_prediction_raster_02_25_2021.tif")
ras <- raster("C:/Users/Kate/Dropbox/tza_wildlife_conflict/elephant_prediction_raster_08_10_2020.tif")


###################Baboon################
#bap <- read.csv("C:/Users/Kate/Dropbox/tza_wildlife_conflict/ras_baboon_crop_preds.csv") 
bap <- read.csv("C:/Users/Kate/Dropbox/tza_wildlife_conflict/ras_baboon_crop_predsv.csv")
head(bap)


ba2 <- bap[,c("x","y", "ras_bab.pred_bab_crop_conflict")]



rasba <- raster(extent(lc2), crs=projection(lc2), resolution=res(lc2))

cells <- cellFromXY(rasba, ba2[,1:2])
rasba[cells] <- ba2[,3]

plot(rasba)
plot(grum, add=T)
plot(surv[surv$baboon_c == 0 & surv$farm == "Yes",], add=T, pch = 1, col="black")
plot(surv[surv$baboon_c == 1,], add=T, pch = 19, col="black")

writeRaster(rasba, "C:/Users/Kate/Dropbox/tza_wildlife_conflict/baboon_prediction_raster_02_25_2021.tif")

#bap <- read.csv("C:/Users/Kate/Dropbox/tza_wildlife_conflict/ras_baboon_crop_preds.csv") 
ver <- read.csv("C:/Users/Kate/Dropbox/tza_wildlife_conflict/ras_vervet_crop_preds.csv")
head(ver)


ver2 <- ver[,c("x","y", "ras_ver.pred_ver_crop_conflict")]



verba <- raster(extent(lc2), crs=projection(lc2), resolution=res(lc2))

cells <- cellFromXY(verba, ver2[,1:2])
verba[cells] <- ver2[,3]

plot(verba)
plot(grum, add=T)
plot(surv[surv$vervet_c == 0 & surv$farm == "Yes",], add=T, pch = 1, col="black")
plot(surv[surv$vervet_c == 1,], add=T, pch = 19, col="black")

writeRaster(verba, "C:/Users/Kate/Dropbox/tza_wildlife_conflict/vervet_prediction_raster_02_25_2021.tif")





#Rasteroze grumeti 
grumrast <- rasterize(grum, rasba)

grumrastsas <- grum[grum$AREANAME != "Robanda Village Land",]

grumrastsas <- rasterize(grumrastsas, rasba)

babmasked<- mask(rasba, grumrastsas, inverse =T)
plot(babmasked)
plot(grum, add=T)
plot(surv[surv$baboon_c == 0 & surv$farm == "Yes",], add=T, pch = 1, col="black")
plot(surv[surv$baboon_c == 1,], add=T, pch = 19, col="black")





elemasked <- mask(ras, grumrastsas, inverse=T)

plot(elemasked)
plot(grum, add=T)
plot(surv[surv$elephant_c == 0 & surv$farm == "Yes",], add=T, pch = 1, col="black")
plot(surv[surv$elephant_c == 1,], add=T, pch = 19, col="black")


###Vervet
vermasked <- mask(verba, grumrastsas, inverse=T)

plot(vermasked)
plot(grum, add=T)
plot(surv[surv$vervet_c == 0 & surv$farm == "Yes",], add=T, pch = 1, col="black")
plot(surv[surv$vervet_c == 1,], add=T, pch = 19, col="black")


################Lions###############

#lip <- read.csv("C:/Users/Kate/Dropbox/tza_wildlife_conflict/ras_lion_deadstock_preds.csv")
lip <- read.csv("C:/Users/Kate/Dropbox/tza_wildlife_conflict/raster5august2020preds/ras_leo_deadstock_preds_5_08_2020.csv")
head(lip)

lip2 <- lip[,c("x","y", "ras_leo.pred_leo_crop_conflict")]



rasleo <- raster(extent(lc2), crs=projection(lc2), resolution=res(lc2))

cells <- cellFromXY(rasleo, lip2[,1:2])
rasleo[cells] <- lip2[,3]

plot(rasleo)


writeRaster(rasleo, "C:/Users/Kate/Dropbox/tza_wildlife_conflict/lion_prediction_raster.tif")
rasleo <- raster("C:/Users/Kate/Dropbox/tza_wildlife_conflict/global crop conflict/lion_prediction_raster.tif")


#####Hyenas ####

#hya <- read.csv("C:/Users/Kate/Dropbox/tza_wildlife_conflict/ras_hyena_deadstock_preds.csv")
hya <- read.csv("C:/Users/Kate/Dropbox/tza_wildlife_conflict/raster5august2020preds/ras_hyena_deadstock_preds_5_08_2020.csv")
head(hya)

hya2 <- hya[,c("x","y", "ras_hyena.pred_hyena_crop_conflict")]



rashya <- raster(extent(lc2), crs=projection(lc2), resolution=res(lc2))

cells <- cellFromXY(rashya, hya2[,1:2])
rashya[cells] <- hya2[,3]


hyamask <- mask(rashya, grumrastsas, inverse=T)


###Check plot Lion
leomasked <- mask(rasleo, grumrastsas, inverse=T)

plot(leomasked)
plot(grum, add=T)
plot(surv[surv$lion_l == 0 & surv$livestock == "Yes",], add=T, pch = 1, col="black")
plot(surv[surv$lion_l == 1,], add=T, pch = 19, col="black")

# To put robanda back in there 

babmasked2 <- mask(rasba, grumrastsas, inverse=T)
elemasked2 <- mask(ras, grumrastsas, inverse=T)


gonly12 <- buffer(gonly, width=12000)


ext12 <- extent(gonly12)





leo12 <- mask(leomasked,gonly12)












n = 50
col.pal = colorRampPalette(RColorBrewer::brewer.pal(8, "YlOrRd"))(n)


#pdf(file ="C:/Users/Kate/Dropbox/tza_wildlife_conflict/Predictions_map_output/lion_prediction.pdf")
png(file ="C:/Users/Kate/Dropbox/tza_wildlife_conflict/Predictions_map_output/lion_prediction.png")
plot(gonly12, border=NA, main="Probability of Lion Livestock Damage")
plot(leomasked, col= col.pal, add=T)
plot(grum, add=T)
plot(surv[surv$lion_l == 0 & surv$livestock == "Yes",], add=T, pch = 1, cex = .5, col="black")
plot(surv[surv$lion_l == 1,], add=T, pch = 19,cex = .5,  col="black")
dev.off()


png(file ="C:/Users/Kate/Dropbox/tza_wildlife_conflict/Predictions_map_output/hyena_prediction2.png")
plot(gonly12, #border=NA,
     main="Probability of Hyena Livestock Damage")
plot(hyamask, col= col.pal, add=T)
plot(grum, add=T)
plot(surv[surv$hyena_l == 0 & surv$livestock == "Yes",], add=T, pch = 1, cex = .5, col="black")
plot(surv[surv$hyena_l == 1,], add=T, pch = 19,cex = .5,  col="black")
dev.off()


pdf(file ="C:/Users/Kate/Dropbox/tza_wildlife_conflict/Predictions_map_output/elephant_prediction02252021.pdf")
plot(gonly12, border=NA, main="Probability of Elephant Crop Damage")
plot(elemasked, col=col.pal, add=T)
plot(grum, add=T)
plot(surv[surv$elephant_c == 0 & surv$farm == "Yes",], add=T, pch = 1,cex = .5, col="black")
plot(surv[surv$elephant_c == 1,], add=T, pch = 19,cex = .5, col="black")
dev.off()


pdf(file ="C:/Users/Kate/Dropbox/tza_wildlife_conflict/Predictions_map_output/elephant_prediction_logscale02252021.pdf")
plot(gonly12, border=NA, main="Probability of Elephant Crop Damage")
plot(log(elemasked), col=col.pal, add=T)
plot(grum, add=T)
plot(surv[surv$elephant_c == 0 & surv$farm == "Yes",], add=T, pch = 1,cex = .5, col="black")
plot(surv[surv$elephant_c == 1,], add=T, pch = 19,cex = .5, col="black")
dev.off()




pdf(file ="C:/Users/Kate/Dropbox/tza_wildlife_conflict/Predictions_map_output/baboon_prediction02252021.pdf")
plot(gonly12, border=NA, main="Probability of Baboon Crop Damage")
raster::plot(babmasked, col=col.pal,ext = extent(gonly12), add=T)
plot(grum, add=T)
plot(surv[surv$baboon_c == 0 & surv$farm == "Yes",], add=T, pch = 1, cex = .5, col="black")
plot(surv[surv$baboon_c == 1,], add=T, pch = 19, cex = .5, col="black")
dev.off()

xy=click()

xy = c(x =609030.1, y = 9758551 )
xy 
#x       y
#609030.1 9758551
###############PNGS######
 
 #https://www.rdocumentation.org/packages/raster/versions/3.3-7/topics/scalebar
png(file ="C:/Users/Kate/Dropbox/tza_wildlife_conflict/Predictions_map_output/hyena_predictionmap02252021.png",  width = 973, height = 600, pointsize = 18)
#par(mai=rep(2,4))
plot(gonly12, border=NA)#, main="Probability of Hyena Livestock Damage")
raster::plot(mask(hyamask, gonly12), col=col.pal,ext = extent(gonly12), add=T)
plot(grum, add=T)
plot(surv[surv$hyena_l == 0 & surv$livestock == "Yes",], add=T, pch = 1, cex = .8, col="black")
plot(surv[surv$hyena_l == 1,], add=T, pch = 19, cex = .8, col="black")
scalebar(20000, xy=xy, type='bar', divs=4, label= c("0", "10km", "20km"))

dev.off()


png(file ="C:/Users/Kate/Dropbox/tza_wildlife_conflict/Predictions_map_output/baboon_predictionmap02252021.png",  width = 973, height = 600, pointsize = 18)
#par(mai=rep(2,4))
plot(gonly12, border=NA)#, main="Probability of Hyena Livestock Damage")
raster::plot(mask(babmasked, gonly12), col=col.pal,ext = extent(gonly12), add=T)
plot(grum, add=T)

plot(surv[surv$baboon_c == 0 & surv$farm == "Yes",], add=T, pch = 1, cex = .8, col="black")
plot(surv[surv$baboon_c == 1,], add=T, pch = 19, cex = .8, col="black")
scalebar(20000, xy=xy, type='bar', divs=4, label= c("0", "10km", "20km"))

dev.off()



png(file ="C:/Users/Kate/Dropbox/tza_wildlife_conflict/Predictions_map_output/elephant_predictionmap02252021.png",  width = 973, height = 600, pointsize = 18)
#par(mai=rep(2,4))
plot(gonly12, border=NA)#, main="Probability of Hyena Livestock Damage")
raster::plot(mask(elemasked, gonly12), col=col.pal,ext = extent(gonly12), add=T)
plot(grum, add=T)

plot(surv[surv$elephant_c == 0 & surv$farm == "Yes",], add=T, pch = 1, cex = .8, col="black")
plot(surv[surv$elephant_c == 1,], add=T, pch = 19, cex = .8, col="black")
scalebar(20000, xy=xy, type='bar', divs=4, label= c("0", "10km", "20km"))

dev.off()


png(file ="C:/Users/Kate/Dropbox/tza_wildlife_conflict/Predictions_map_output/vervet_predictionmap02252021.png",  width = 973, height = 600, pointsize = 18)
#par(mai=rep(2,4))
plot(gonly12, border=NA)#, main="Probability of Hyena Livestock Damage")
raster::plot(mask(vermasked, gonly12), col=col.pal,ext = extent(gonly12), add=T)
plot(grum, add=T)

plot(surv[surv$vervet_c == 0 & surv$farm == "Yes",], add=T, pch = 1, cex = .8, col="black")
plot(surv[surv$vervet_c == 1,], add=T, pch = 19, cex = .8, col="black")
scalebar(20000, xy=xy, type='bar', divs=4, label= c("0", "10km", "20km"))

dev.off()



png(file ="C:/Users/Kate/Dropbox/tza_wildlife_conflict/Predictions_map_output/lion_predictionmap02252021.png",  width = 973, height = 600, pointsize = 18)
#par(mai=rep(2,4))
plot(gonly12, border=NA)#, main="Probability of Hyena Livestock Damage")
raster::plot(mask(leomasked, gonly12), col=col.pal,ext = extent(gonly12), add=T)
plot(grum, add=T)

plot(surv[surv$lion_l == 0 & surv$livestock == "Yes",], add=T, pch = 1, cex = .8, col="black")
plot(surv[surv$lion_l == 1,], add=T, pch = 19, cex = .8, col="black")
scalebar(20000, xy=xy, type='bar', divs=4, label= c("0", "10km", "20km"))

dev.off()
