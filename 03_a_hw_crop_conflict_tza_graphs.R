require(rethinking)
require(RColorBrewer)
###########dotplots of parameters

pbd <- extract.samples(mc_bd_min)
pcr <- extract.samples(mc_cd_min)
pc70 <- extract.samples(mc_c70_min)
pc2070 <- extract.samples(mc_c2070_min)
priv <- extract.samples(mc_riv_min)
psd <- extract.samples(mc_sd_min)
psl <- extract.samples(mc_slope_min)
pfs <- extract.samples(mc_fs_min)
phh <- extract.samples(mc_hh_min)
psee <- extract.samples(mc_see_min)
pmp <- extract.samples(mc_mp_min)

p_crop_params <- list(
  b_BD_baboon = pbd$b_BD + pbd$b_BDs[,1],
  b_CR_baboon = pcr$b_CR + pcr$b_CRs[,1],
  b_C70_baboon = pc70$b_C70 + pc70$b_C70s[,1],
  b_C2070_baboon = pc2070$b_C2070 + pc2070$b_C2070s[,1],
  b_RIV_baboon = priv$b_RIV + priv$b_RIVs[,1],
  b_SD_baboon = psd$b_SD + psd$b_SDs[,1],
  b_SL_baboon = psl$b_SL + psl$b_SLs[,1],
  b_FS_baboon = pfs$b_FS + pfs$b_FSs[,1],
  b_HH_baboon = phh$b_HH + phh$b_HHs[,1],
  b_SEE_baboon = psee$b_SEE + psee$b_SEEs[,1],
  b_MP_baboon = pmp$b_MP + pmp$b_MPs[,1],
  b_BD_elephant = pbd$b_BD + pbd$b_BDs[,2],
  b_CR_elephant = pcr$b_CR + pcr$b_CRs[,2],
  b_C70_elephant = pc70$b_C70 + pc70$b_C70s[,2],
  b_C2070_elephant = pc2070$b_C2070 + pc2070$b_C2070s[,2],
  b_RIV_elephant = priv$b_RIV + priv$b_RIVs[,2],
  b_SD_elephant = psd$b_SD + psd$b_SDs[,2],
  b_SL_elephant = psl$b_SL + psl$b_SLs[,2],
  b_FS_elephant = pfs$b_FS + pfs$b_FSs[,2],
  b_HH_elephant = phh$b_HH + phh$b_HHs[,2],
  b_SEE_elephant = psee$b_SEE + psee$b_SEEs[,2],
  b_MP_elephant = pmp$b_MP + pmp$b_MPs[,2],
  b_BD_vervet = pbd$b_BD + pbd$b_BDs[,3],
  b_CR_vervet = pcr$b_CR + pcr$b_CRs[,3],
  b_C70_vervet = pc70$b_C70 + pc70$b_C70s[,3],
  b_C2070_vervet = pc2070$b_C2070 + pc2070$b_C2070s[,3],
  b_RIV_vervet = priv$b_RIV + priv$b_RIVs[,3],
  b_SD_vervet = psd$b_SD + psd$b_SDs[,3],
  b_SL_vervet = psl$b_SL + psl$b_SLs[,3],
  b_FS_vervet = pfs$b_FS + pfs$b_FSs[,3],
  b_HH_vervet = phh$b_HH + phh$b_HHs[,3],
  b_SEE_vervet = psee$b_SEE + psee$b_SEEs[,3],
  b_MP_vervet = pmp$b_MP + pmp$b_MPs[,3]
)
plot(precis(p_crop_params))

pdf(file = "plots/crop_conflict_species_parameter_dotplots.pdf",   width = 7, height = 7) 
plot(precis(p_crop_params))
points( precis(p_crop_params)[[1]] , length(precis(p_crop_params)[[1]]):1  , col=rep(brewer.pal(11,"Spectral"), 3) , pch=19 , cex=1)
dev.off()

####lets plot per species effects from minimal models
av_z <- matrix(0,1000,length(unique(dc$village_index))) #need to add zeros in VE to plot main effect
ylabels=c("probability baboon crop conflict" , "probability elephant crop conflict","probability vervet crop conflict")
colpal=c("blue" , "grey", "darkgreen")

#c70
precis(mc_c70_min)

plot_seq <- seq(from=min(dc$c70_std) , to=max(dc$c70_std) , length=30)

for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,30),
    c70_std = plot_seq,
    river_std= rep(0,30),
    build_dens_std= rep(0,30),
    crop_std= rep(0,30),
    species_index=rep(i,30)
  )
  
  link2 <- link(mc_c70_min, data=dpred , replace=list(village_index=av_z) )
  if(i==1){
    pdf(file = "plots/c70_crop_min_conflict_bab.pdf",   width = 6, height = 6)
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$c70_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="proportion densely wooded cover" , xaxt='n' , cex.lab=1.3)
    }
  if(i==2){
    pdf(file = "plots/c70_crop_min_conflict_ele.pdf",   width = 6, height = 6)
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$c70_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="proportion densely wooded cover" ,  xaxt='n' , cex.lab=1.3)
    }
  if(i==3){
   pdf(file = "plots/c70_crop_min_conflict_ver.pdf",   width = 6, height = 6)
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$vervet_c ~ dc$c70_std , col=col.alpha(colpal[3], 0.1) , pch=19 , ylab=ylabels[i] , xlab="proportion densely wooded cover" ,  xaxt='n' , cex.lab=1.3)
    }
  
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  
  axis( 1 , at= ( seq(from=0 , to=0.45 , by=0.05) - mean(dc$c70))/sd(dc$c70) , labels= seq(from=0 , to=0.45 , by=0.05) )
 dev.off()
}


#c2070
precis(mc_c2070_min , depth=2)
plot_seq <- seq(from=min(dc$c2070_std) , to=max(dc$c2070_std) , length=30)

for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,30),
    crop_std=rep(0,30),
    c2070_std = plot_seq ,
    build_dens_std= rep(0,30),
    gse_slope30m_std= rep(0,30),
    species_index=rep(i,30)
  )
  
  link2 <- link(mc_c2070_min, data=dpred , replace=list(village_index=av_z) )
  if(i==1){
    pdf(file = "plots/c2070_crop_min_conflict_bab.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$c2070_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="proportion moderately wooded cover" , xaxt='n' , cex.lab=1.3)
    }
  if(i==2){
    pdf(file = "plots/c2070_crop_min_conflict_ele.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$c2070_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="proportion moderately wooded cover" , xaxt='n' , cex.lab=1.3)
    }
  if(i==3){
    pdf(file = "plots/c2070_crop_min_conflict_ver.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$vervet_c ~ dc$c2070_std , col=col.alpha(colpal[3], 0.1) , pch=19 , ylab=ylabels[i] , xlab="proportion moderately wooded cover" , xaxt='n' , cex.lab=1.3)
    }
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at= ( seq(from=0 , to=1 , by=0.1) - mean(dc$c2070))/sd(dc$c2070) , labels= seq(from=0 , to=1 , by=0.1) )
  dev.off()
}

###crop density
precis(mc_cd_min)
plot_seq <- seq(from=min(dc$crop_std) , to=max(dc$crop_std) , length=30)

for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=rep(0,30),
    gse_slope30m_std = rep(0,30),
    crop_std= plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(mc_cd_min, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "plots/crop_dens_crop_min_conflict_bab.pdf",   width = 6, height = 6)
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$crop_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="crop density" ,  xaxt='n' , cex.lab=1.3)
    }
  
  if(i==2){
    pdf(file = "plots/crop_dens_crop_min_conflict_ele.pdf",   width = 6, height = 6)
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$crop_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="crop density" ,  xaxt='n' , cex.lab=1.3)
    }
  
  if(i==3){
    pdf(file = "plots/crop_dens_crop_min_conflict_ver.pdf",width = 6, height = 6)
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$vervet_c ~ dc$crop_std , col=col.alpha(colpal[3], 0.1) , pch=19 , ylab=ylabels[i] , xlab="crop density" ,  xaxt='n' , cex.lab=1.3)
    }
  
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at= ( seq(from=0 , to=1 , by=0.1) - mean(dc$crop))/sd(dc$crop) , labels= seq(from=0 , to=1 , by=0.1) )
  dev.off()
}

##settlement distance
precis(mc_sd_min)
plot_seq <- seq(from=min(dc$settle_dist_km_std) , to=max(dc$settle_dist_km_std) , length=30)

for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(mc_sd_min, data=dpred , replace=list(village_index=av_z) )
  par( mar=c(4,4,1,1)+.1 )
    
    if(i==1){
      pdf(file = "plots/settle_dist_crop_min_conflict_bab.pdf",   width = 6, height = 6)
      par( mar=c(4,4,1,1)+.1 )
      plot(dc$baboon_c ~ dc$settle_dist_km_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="distance into settlement (km)" , xaxt='n',  cex.lab=1.3)}
    
    if(i==2){
      pdf(file = "plots/settle_dist_crop_min_conflict_ele.pdf",   width = 6, height = 6)
      par( mar=c(4,4,1,1)+.1 )
      plot(dc$elephant_c ~ dc$settle_dist_km_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="distance into settlement (km)" , xaxt='n' ,  cex.lab=1.3) }
    
    if(i==3){
      pdf(file = "plots/settle_dist_crop_min_conflict_verv.pdf",   width = 6, height = 6)
      par( mar=c(4,4,1,1)+.1 )
      plot(dc$vervet_c ~ dc$settle_dist_km_std , col=col.alpha(colpal[3], 0.1) , pch=19 , ylab=ylabels[i] , xlab="distance into settlement (km)" , xaxt='n' ,  cex.lab=1.3) }
    
    pred_mean <- apply(link2 , 2 , mean)
    lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
    for (j in sample( c(1:1000) , 100) ){
      lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
    }
    axis( 1 , at= ( c(0:12) - mean(dc$settle_dist_km))/sd(dc$settle_dist_km) , labels=c(0:12))
    
    dev.off()
}

###rivers
precis(mc_riv_min)
plot_seq <- seq(from=min(dc$river_std) , to=max(dc$river_std) , length=30)

for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,30),
    gse_slope30m_std = rep(0,30) ,
    river_std= plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(mc_riv_min, data=dpred , replace=list(village_index=av_z) )
  if(i==1){
    pdf(file = "plots/river_crop_min_conflict_bab.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$river_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="river density" ,  xaxt='n' , cex.lab=1.3)}
  if(i==2){
    pdf(file = "plots/river_crop_min_conflict_ele.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$river_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="river density" ,  xaxt='n' , cex.lab=1.3)}
  if(i==3){
    pdf(file = "plots/river_crop_min_conflict_ver.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$vervet_c ~ dc$river_std , col=col.alpha(colpal[3], 0.1) , pch=19 , ylab=ylabels[i] , xlab="river density" ,  xaxt='n' , cex.lab=1.3)}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at= ( seq(from=0 , to=0.12 , by=0.02) - mean(dc$river))/sd(dc$river) , labels= seq(from=0 , to=0.12 , by=0.02) )
  dev.off()
}

##building density

plot_seq <- seq(from=min(dc$build_dens_std) , to=max(dc$build_dens_std) , length=30)

for (i in 1:3){
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=rep(0,30),
    build_dens_std= plot_seq ,
    gse_slope30m_std= rep(0,30),
    species_index=rep(i,30)
  )
  
  link2 <- link(mc_bd_min, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "plots/build_dens_crop_min_conflict_bab.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$build_dens_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="building density" ,  xaxt='n' , cex.lab=1.3)}
  if(i==2){
    pdf(file = "plots/build_dens_crop_min_conflict_ele.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$build_dens_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="building density" ,  xaxt='n' , cex.lab=1.3)}
  if(i==3){
    pdf(file = "plots/build_dens_crop_min_conflict_ver.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$vervet_c ~ dc$build_dens_std , col=col.alpha(colpal[3], 0.1) , pch=19 , ylab=ylabels[i] , xlab="building density" ,  xaxt='n' , cex.lab=1.3)}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at= ( seq(from=0 , to=425 , by=25) - mean(dc$build_dens))/sd(dc$build_dens) , labels= seq(from=0 , to=425 , by=25) )
  dev.off()
}

####months planted
plot_seq <- seq(from=min(dc$months_planted_std) , to=max(dc$months_planted_std) , length=30)

for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,30),
    months_planted_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(mc_mp_min, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "plots/monthsplanted_crop_min_conflict_bab.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$months_planted_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="months planted",  xaxt='n' , cex.lab=1.3 )}
  
  if(i==2){
    pdf(file = "plots/monthsplanted_crop_min_conflict_ele.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$months_planted_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="months planted" ,  xaxt='n' , cex.lab=1.3)}
  
  if(i==3){
    pdf(file = "plots/monthsplanted_crop_min_conflict_ver.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$vervet_c ~ dc$months_planted_std , col=col.alpha(colpal[3], 0.1) , pch=19 , ylab=ylabels[i] , xlab="months planted" ,  xaxt='n' , cex.lab=1.3)}  
  
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at= ( seq(from=1 , to=12 , by=1) - mean(dc$months_planted))/sd(dc$months_planted) , labels= seq(from=1 , to=12 , by=1) )
  dev.off()
}

#####see field
dpred <- list(
  village_index=rep(1,6),
  see_field=c(0,1,0,1,0,1),
  species_index=c(1,1,2,2,3,3)
)

link2 <- link(mc_see_min, data=dpred , replace=list(village_index=av_z) )
pdf(file = "plots/seefield_crop_min_conflict_bab.pdf",   width = 6, height = 6) 
  dens(link2[,1] , lty=2 , col="blue" , ylim=c(0,15) , xlim=c(0,0.5) , main="probability baboon crop conflict")
  abline(v=mean(link2[,1]) , col="blue" , lty=2)
  dens(link2[,2] , add=TRUE , col="darkblue")
  abline(v=median(link2[,2]) , col="darkblue" , lty=1)
  legend('topright' , c("can't see field" , "see field") , col=c("blue" , "darkblue") , lty=c(2,1))
dev.off()

pdf(file = "plots/seefield_crop_min_conflict_ele.pdf",   width = 6, height = 6) 
  dens(link2[,3] , lty=2 , col="grey" , ylim=c(0,15) , xlim=c(0.2,1) , main="probability elephant crop conflict")
  abline(v=mean(link2[,3]) , col="grey" , lty=2)
  dens(link2[,4] , add=TRUE , col="black" )
  abline(v=median(link2[,4]) , col="black" , lty=1)
legend('topleft' , c("can't see field" , "see field") , col=c("grey" , "black") , lty=c(2,1))
dev.off()

pdf(file = "plots/seefield_crop_min_conflict_verv.pdf",   width = 6, height = 6) 
  dens(link2[,5] , lty=2 , col="green" , ylim=c(0,15) , xlim=c(0,0.6) , main="probability vervet crop conflict")
  abline(v=mean(link2[,5]) , col="green" , lty=2)
  dens(link2[,6] , add=TRUE , col="darkgreen" )
  abline(v=median(link2[,6]) , col="darkgreen" , lty=1)
  legend('topleft' , c("can't see field" , "see field") , col=c("green" , "darkgreen") , lty=c(2,1))
dev.off()

###household size
post <- extract.samples(mc_hh_min)
plot_seq <- seq(from=min(dc$household_size_std ,  na.rm = TRUE) , to=max(dc$household_size_std, na.rm = TRUE) , length=18)
hhsimp <- apply(post$household_size_std_impute , 2 , mean) #look at imputed values

for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,18),
    household_size_std=plot_seq,
    species_index=rep(i,18)
  )
  
  link2 <- link(mc_hh_min, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "plots/hhsize_crop_min_conflict_bab.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$household_size_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size",  xaxt='n' , cex.lab=1.3)}
    points(hhsimp , dc$baboon_c[is.na(dc$household_size_std)==TRUE], pch=1)
  
  if(i==2){
    pdf(file = "plots/hhsize_crop_min_conflict_ele.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$household_size_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size",  xaxt='n' , cex.lab=1.3)}
    points(hhsimp , dc$ele_c[is.na(dc$household_size_std)==TRUE], pch=1)
    
  if(i==3){
    pdf(file = "plots/hhsize_crop_min_conflict_ver.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$vervet_c ~ dc$household_size_std , col=col.alpha(colpal[3], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size",  xaxt='n' , cex.lab=1.3)}
    points(hhsimp , dc$vervet_c[is.na(dc$household_size_std)==TRUE], pch=1)
    
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at= ( seq(from=0 , to=30 , by=5) - mean(dc$household_size ,  na.rm = TRUE))/sd(dc$household_size ,  na.rm =TRUE) , labels= seq(from=0 , to=30 , by=5) )
  dev.off()
}

###household size trial bug fixin

#extract posterior
# post <- extract.samples(mc_hh_min)
# 
# tracerplot(mc_hh_min , pars=c("b_HH"))
# tracerplot(mc_hh_min , pars=c("b_HHs"))
# 
# hhsimp <- apply(post$household_size_std_impute , 2 , mean) #look at imputed values
# 
# plot_seq <- seq(from=min(dc$household_size_std ,  na.rm = TRUE) , to=max(dc$household_size_std, na.rm = TRUE) , length=30)
# 
# for (i in 1:3){
#   
#   dpred <- list(
#     village_index=rep(1,30),
#     household_size_std=plot_seq,
#     species_index=rep(i,30)
#   )
#   
#   link2 <- link(mc_hh_min, data=dpred , replace=list(village_index=av_z) )
#   mu.link <- function(household_size_std) logistic(post$a + post$as[,i] + (post$b_HH + post$b_HHs[,i])*household_size_std)
#   
#   if(i==1){
#     par( mar=c(4,4,1,1)+.1 )
#     pdf(file = "plots/hhsize_crop_min_conflict_bab2.pdf",   width = 6, height = 6) 
#     plot(dc$baboon_c ~ dc$household_size_std, col=col.alpha(colpal[i], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size",  xaxt='n' , cex.lab=1.3)}
#     points(hhsimp , dc$baboon_c[is.na(dc$household_size_std)==TRUE], pch=1)
#   
#   if(i==2){
#     par( mar=c(4,4,1,1)+.1 )
#     pdf(file = "plots/hhsize_crop_min_conflict_ele2.pdf",   width = 6, height = 6) 
#     plot(dc$elephant_c ~ dc$household_size_std , col=col.alpha(colpal[i], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size",  xaxt='n' , cex.lab=1.3)}
#     points(hhsimp , dc$elephant_c[is.na(dc$household_size_std)==TRUE], pch=1)
#     
#   if(i==3){
#     par( mar=c(4,4,1,1)+.1 )
#     pdf(file = "plots/hhsize_crop_min_conflict_verv2.pdf",   width = 6, height = 6) 
#     plot(dc$vervet_c ~ dc$household_size_std , col=col.alpha(colpal[i], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size",  xaxt='n' , cex.lab=1.3)}
#     points(hhsimp , dc$vervet_c[is.na(dc$household_size_std)==TRUE], pch=1)
#     
#   mu <- sapply( plot_seq  , mu.link )
#   mu.mean <- apply( mu , 2 , mean)
#   
#   pred_mean <- apply(link2 , 2 , mean)
#   lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=2)
#   lines( mu.mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
#   
#   # for (j in sample( c(1:1000) , 100) ){
#   #   lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
#   # }
#   
#   for (j in sample( c(1:1000) , 100) ){
#     lines( mu[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
#   }
#   
#   axis( 1 , at= ( seq(from=0 , to=30 , by=5) - mean(dc$household_size ,  na.rm = TRUE))/sd(dc$household_size ,  na.rm =TRUE) , labels= seq(from=0 , to=30 , by=5) )
#  dev.off()
# }

#########diagnosis attempts

for (i in 1:3){
  n_sim <- 18
  plot_seq <- seq(from=min(dc$household_size_std ,  na.rm = TRUE) , to=max(dc$household_size_std, na.rm = TRUE) , length=n_sim)
  
  dpred <- list(
    village_index=rep(1,n_sim),
    household_size_std=plot_seq,
    species_index=rep(i,n_sim)
  )
  
  link2 <- link(mc_hh_min, data=dpred , replace=list(village_index=av_z) )
  mu.link <- function(household_size_std) logistic(post$a + post$as[,i] + (post$b_HH + post$b_HHs[,i])*household_size_std)
  
if(i==1){
  par( mar=c(4,4,1,1)+.1 )
  plot(dc$baboon_c ~ dc$household_size_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size std",   cex.lab=1.3)}

if(i==2){
  par( mar=c(4,4,1,1)+.1 )
  plot(dc$elephant_c ~ dc$household_size_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size std", cex.lab=1.3)}

if(i==3){
  par( mar=c(4,4,1,1)+.1 )
  plot(dc$vervet_c ~ dc$household_size_std , col=col.alpha(colpal[3], 0.1) , pch=19 , ylab=ylabels[i] , xlab="household size", cex.lab=1.3)}

pred_mean <- apply(link2 , 2 , mean)
lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
}


## farm size
plot_seq <- seq(from=min(dc$farm_size_std) , to=max(dc$farm_size_std) , length=30)

for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,30),
    farm_size_std=plot_seq,
    household_size_std=rep(0,30),
    species_index=rep(i,30)
  )
  
  link2 <- link(mc_fs_min, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "plots/farmsize_crop_min_conflict_bab.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$farm_size_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="farm size" ,  xaxt='n' , cex.lab=1.3)
    }
  
  if(i==2){
    pdf(file = "plots/farmsize_crop_min_conflict_ele.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$farm_size_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="farm size",  xaxt='n' , cex.lab=1.3)
    }
  
  if(i==3){
    pdf(file = "plots/farmsize_crop_min_conflict_ver.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$vervet_c ~ dc$farm_size_std , col=col.alpha(colpal[3], 0.1) , pch=19 , ylab=ylabels[i] , xlab="farm size (ha)",  xaxt='n' , cex.lab=1.3)
    }
  
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at= ( seq(from=0 , to=70 , by=5) - mean(dc$farm_size))/sd(dc$farm_size) , labels= seq(from=0 , to=70 , by=5) )
  dev.off()
}


# slope
plot_seq <- seq(from=min(dc$gse_slope30m_std) , to=max(dc$gse_slope30m_std) , length=30)

for (i in 1:3){
  
  dpred <- list(
    village_index=rep(1,30),
    gse_slope30m_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(mc_slope_min, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "plots/slope_crop_min_conflict_bab.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$baboon_c ~ dc$gse_slope30m_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="slope 30m density" ,  xaxt='n' , cex.lab=1.3)}
  
  if(i==2){
    pdf(file = "plots/slope_crop_min_conflict_ele.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$elephant_c ~ dc$gse_slope30m_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="slope 30m density",  xaxt='n' , cex.lab=1.3)}
  
  if(i==3){
    pdf(file = "plots/slope_crop_min_conflict_ver.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dc$vervet_c ~ dc$gse_slope30m_std , col=col.alpha(colpal[3], 0.1) , pch=19 , ylab=ylabels[i] , xlab="slope 30m density",  xaxt='n' , cex.lab=1.3)}
  
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at= ( seq(from=0 , to=18 , by=2) - mean(dc$gse_slope30m))/sd(dc$gse_slope30m) , labels= seq(from=0 , to=18 , by=2) )
  dev.off()
}

##########################################
#############raster preds################
#########################################

###############baboons
#ras_bab<-  read.csv("~/Dropbox/tza_wildlife_conflict/baboonRasterstacktopoints_survext.csv")
ras_bab<-  read.csv("baboonRasterstacktopoints_survext.csv")

ras_bab$settle_dist_km <- ras_bab$settle_dist/1000
ras_bab$crop_std <- (ras_bab$crop-mean(dc$crop) )/sd(dc$crop) 
ras_bab$c70_std <- (ras_bab$c70 -mean(dc$c70 ) )/ sd(dc$c70 ) 
ras_bab$c2070_std <- (ras_bab$c2070 -mean(dc$c2070 ) )/ sd(dc$c2070 ) 
ras_bab$river_std <- (ras_bab$river -mean(dc$river ) )/ sd(dc$river) 
ras_bab$road_std <- (ras_bab$road -mean(dc$road ) )/ sd(dc$road) 
ras_bab$build_dens_std <- (ras_bab$build_dens-mean(dc$build_dens ) )/ sd(dc$build_dens) 
ras_bab$settle_dist_km_std <- (ras_bab$settle_dist_km-mean(dc$settle_dist_km ) )/ sd(dc$settle_dist_km)
ras_bab$gse_slope30m_std <- (ras_bab$gse_slope30m-mean(dc$gse_slope30m ) )/ sd(dc$gse_slope30m) 
ras_bab$species_index <- 1

p <- extract.samples(mc_landscape)

dpred <- list(
  village_index=rep(1,nrow(ras_bab)),
  settle_dist_km_std=ras_bab$settle_dist_km_std,
  c70_std = ras_bab$c70_std,
  c2070_std = ras_bab$c2070_std,
  river_std= ras_bab$river_std,
  road_std= ras_bab$road_std,
  build_dens_std=ras_bab$build_dens_std,
  crop_std= ras_bab$crop_std,
  gse_slope30m_std= ras_bab$gse_slope30m_std,
  species_index=ras_bab$species_index
)

##note this is with other variables at mean, excluded b/c standardized so mean in 0, conputationally kwiker
ras_bab$pred_bab_crop_conflict <- logistic( mean(p$a + p$as[,1]) + 
                                              mean(p$b_SD + p$b_SDs[,1])*dpred$settle_dist_km_std  + 
                                              mean(p$b_C70 + p$b_C70s[,1])*dpred$c70_std +
                                              mean(p$b_C2070 + p$b_C2070s[,1])*dpred$c2070_std +
                                              mean(p$b_BD + p$b_BDs[,1])*dpred$build_dens_std +
                                              mean(p$b_RD + p$b_RDs[,1])*dpred$road_std  + 
                                              mean(p$b_RIV + p$b_RIVs[,1])*dpred$river_std  + 
                                              mean(p$b_CR + p$b_CRs[,1])*dpred$crop_std  + 
                                              mean(p$b_SL + p$b_SLs[,1])*dpred$gse_slope30m_std 
)

dens(ras_bab$pred_bab_crop_conflict)
ras_bab_sub <- cbind( ras_bab[1:3] , ras_bab$pred_bab_crop_conflict)
write.csv(ras_bab_sub , file="ras_baboon_crop_preds_03052021.csv")


###elephants###################

ras_ele <-  read.csv("~/Dropbox/tza_wildlife_conflict/elephantRasterstacktopoints_survext.csv")
ras_ele <-  read.csv("elephantRasterstacktopoints_survext.csv")

ras_ele$settle_dist_km <- ras_ele$settle_dist/1000
ras_ele$crop_std <- (ras_ele$crop-mean(dc$crop) )/sd(dc$crop) 
ras_ele$c70_std <- (ras_ele$c70 -mean(dc$c70 ) )/ sd(dc$c70 ) 
ras_ele$c2070_std <- (ras_ele$c2070 -mean(dc$c2070 ) )/ sd(dc$c2070 ) 
ras_ele$river_std <- (ras_ele$river -mean(dc$river ) )/ sd(dc$river) 
ras_ele$road_std <- (ras_ele$road -mean(dc$road ) )/ sd(dc$road) 
ras_ele$build_dens_std <- (ras_ele$build_dens-mean(dc$build_dens ) )/ sd(dc$build_dens) 
ras_ele$settle_dist_km_std <- (ras_ele$settle_dist_km-mean(dc$settle_dist_km ) )/ sd(dc$settle_dist_km)
ras_ele$gse_slope30m_std <- (ras_ele$gse_slope30m-mean(dc$gse_slope30m ) )/ sd(dc$gse_slope30m) 
ras_ele$species_index <- 2

p <- extract.samples(mc_landscape)

dpred <- list(
  village_index=rep(1,nrow(ras_ele)),
  settle_dist_km_std=ras_ele$settle_dist_km_std,
  c70_std = ras_ele$c70_std,
  c2070_std = ras_ele$c2070_std,
  river_std= ras_ele$river_std,
  road_std= ras_ele$road_std,
  build_dens_std=ras_ele$build_dens_std,
  crop_std= ras_ele$crop_std,
  gse_slope30m_std= ras_ele$gse_slope30m_std,
  species_index=ras_ele$species_index
)

ras_ele$pred_ele_crop_conflict <- logistic( mean(p$a + p$as[,2]) + 
                                              mean(p$b_SD + p$b_SDs[,2])*dpred$settle_dist_km_std  + 
                                              mean(p$b_C70 + p$b_C70s[,2])*dpred$c70_std +
                                              mean(p$b_C2070 + p$b_C2070s[,2])*dpred$c2070_std +
                                              mean(p$b_BD + p$b_BDs[,2])*dpred$build_dens_std +
                                              mean(p$b_RD + p$b_RDs[,2])*dpred$road_std  + 
                                              mean(p$b_RIV + p$b_RIVs[,2])*dpred$river_std  + 
                                              mean(p$b_CR + p$b_CRs[,2])*dpred$crop_std  + 
                                              mean(p$b_SL + p$b_SLs[,2])*dpred$gse_slope30m_std 
)
  

dens(ras_ele$pred_ele_crop_conflict)
ras_ele_sub <- cbind( ras_ele[1:3] , ras_ele$pred_ele_crop_conflict)
write.csv(ras_ele_sub , file="ras_elephant_crop_preds_03052021.csv")

#####Vervets baby#####
# ras_ver<-  read.csv("~/Dropbox/tza_wildlife_conflict/vervetRasterstacktopoints_survext.csv")
ras_ver<-  read.csv("vervetRasterstacktopoints_survext.csv")

ras_ver$settle_dist_km <- ras_ver$settle_dist/1000
ras_ver$crop_std <- (ras_ver$crop-mean(dc$crop) )/sd(dc$crop) 
ras_ver$c70_std <- (ras_ver$c70 -mean(dc$c70 ) )/ sd(dc$c70 ) 
ras_ver$c2070_std <- (ras_ver$c2070 -mean(dc$c2070 ) )/ sd(dc$c2070 ) 
ras_ver$river_std <- (ras_ver$river -mean(dc$river ) )/ sd(dc$river) 
ras_ver$road_std <- (ras_ver$road -mean(dc$road ) )/ sd(dc$road) 
ras_ver$build_dens_std <- (ras_ver$build_dens-mean(dc$build_dens ) )/ sd(dc$build_dens) 
ras_ver$settle_dist_km_std <- (ras_ver$settle_dist_km-mean(dc$settle_dist_km ) )/ sd(dc$settle_dist_km) 
ras_ver$gse_slope30m_std <- (ras_ver$gse_slope30m-mean(dc$gse_slope30m ) )/ sd(dc$gse_slope30m) 
ras_ver$species_index <- 3

p <- extract.samples(mc_landscape)

dpred <- list(
  village_index=rep(1,nrow(ras_ver)),
  settle_dist_km_std=ras_ver$settle_dist_km_std,
  c70_std = ras_ver$c70_std,
  c2070_std = ras_ver$c2070_std,
  river_std= ras_ver$river_std,
  road_std= ras_ver$road_std,
  build_dens_std=ras_ver$build_dens_std,
  crop_std= ras_ver$crop_std,
  gse_slope30m_std=ras_ver$gse_slope30m_std,
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
                                              mean(p$b_RD + p$b_RDs[,3])*dpred$crop_std  + 
                                              mean(p$b_RIV + p$b_RIVs[,3])*dpred$crop_std +
                                              mean(p$b_SL + p$b_SLs[,3])*dpred$crop_std 
)

dens(ras_ver$pred_ver_crop_conflict)
ras_ver_sub <- cbind( ras_ver[1:3] , ras_ver$pred_ver_crop_conflict)
write.csv(ras_ver_sub , file="ras_vervet_crop_preds_03052021.csv")


##############other graphs of imporatnce#####

#lanscape model dotplot, perhaps for SI
p <- extract.samples(mc_landscape)

p_crop_landscape <- list(
  b_BD_baboon = p$b_BD + p$b_BDs[,1],
  b_CR_baboon = p$b_CR + p$b_CRs[,1],
  b_C70_baboon = p$b_C70 + p$b_C70s[,1],
  b_C2070_baboon = p$b_C2070 + p$b_C2070s[,1],
  b_RD_baboon = p$b_RD + p$b_RDs[,1],
  b_RIV_baboon = p$b_RIV + p$b_RIVs[,1],
  b_SD_baboon = p$b_SD + p$b_SDs[,1],
  b_SL_baboon = p$b_SL + p$b_SLs[,1],
  b_BD_elephant = p$b_BD + p$b_BDs[,2],
  b_CR_elephant = p$b_CR + p$b_CRs[,2],
  b_C70_elephant = p$b_C70 + p$b_C70s[,2],
  b_C2070_elephant = p$b_C2070 + p$b_C2070s[,2],
  b_RD_elephant = p$b_RD + p$b_RDs[,2],
  b_RIV_elephant = p$b_RIV + p$b_RIVs[,2],
  b_SD_elephant = p$b_SD + p$b_SDs[,2],
  b_SL_elephant = p$b_SL + p$b_SLs[,2],
  b_BD_vervet = p$b_BD + p$b_BDs[,3],
  b_CR_vervet = p$b_CR + p$b_CRs[,3],
  b_C70_vervet = p$b_C70 + p$b_C70s[,3],
  b_C2070_vervet = p$b_C2070 + p$b_C2070s[,3],
  b_RD_vervet = p$b_RD + p$b_RDs[,3],
  b_RIV_vervet = p$b_RIV + p$b_RIVs[,3],
  b_SD_vervet = p$b_SD + p$b_SDs[,3],
  b_SL_vervet = p$b_SL + p$b_SLs[,3]
)
 
pdf(file = "plots/crop_conflict_species_parameter_dotplots_landscape_model.pdf",   width = 7, height = 7) 
  plot(precis(p_crop_landscape))
  points( precis(p_crop_landscape)[[1]] , length(precis(p_crop_landscape)[[1]]):1  , col=rep(brewer.pal(8,"Spectral"), 3) , pch=19 , cex=1)
dev.off()

#write.csv( precis(mc17 , depth=2) , file="crop_global_model_medium_paramsv.csv" )
#write.csv( precis(mc17 , depth=3) , file="crop_global_model_long_paramsv.csv" )

#####wirefence
dpred <- list(
  village_index=rep(1,6),
  crop_prot_w_fence=c(0,1,0,1,0,1),
  species_index=c(1,1,2,2,3,3),
  farm_size_std=rep(0,6)
)

link2 <- link(mc_cpwf_min, data=dpred , replace=list(village_index=av_z) )
pdf(file = "plots/wirefence_crop_min_conflict_bab.pdf",   width = 6, height = 6) 
dens(link2[,1] , lty=2 , col="blue" , ylim=c(0,15) , xlim=c(0,0.5) , main="probability baboon crop conflict")
abline(v=mean(link2[,1]) , col="blue" , lty=2)
dens(link2[,2] , add=TRUE , col="darkblue")
abline(v=median(link2[,2]) , col="darkblue" , lty=1)
legend('topright' , c("no wire fence" , "yes wire fence") , col=c("blue" , "darkblue") , lty=c(2,1))
dev.off()

pdf(file = "plots/wirefence_crop_min_conflict_ele.pdf",   width = 6, height = 6) 
dens(link2[,3] , lty=2 , col="grey" , ylim=c(0,15) , xlim=c(0.2,1) , main="probability elephant crop conflict")
abline(v=mean(link2[,3]) , col="grey" , lty=2)
dens(link2[,4] , add=TRUE , col="black" )
abline(v=median(link2[,4]) , col="black" , lty=1)
legend('topleft' , c("no wire fence" , "yes wire fence") , col=c("grey" , "black") , lty=c(2,1))
dev.off()

pdf(file = "plots/wirefence_crop_min_conflict_verv.pdf",   width = 6, height = 6) 
dens(link2[,5] , lty=2 , col="green" , ylim=c(0,15) , xlim=c(0,0.6) , main="probability vervet crop conflict")
abline(v=mean(link2[,5]) , col="green" , lty=2)
dens(link2[,6] , add=TRUE , col="darkgreen" )
abline(v=median(link2[,6]) , col="darkgreen" , lty=1)
legend('topleft' , c("no wire fence" , "yes wire fence") , col=c("green" , "darkgreen") , lty=c(2,1))
dev.off()


##sisal

#####sisal
dpred <- list(
  village_index=rep(1,6),
  crop_prot_sisal=c(0,1,0,1,0,1),
  species_index=c(1,1,2,2,3,3),
  farm_size_std=rep(0,6)
)

link2 <- link(mc_cpsf_min, data=dpred , replace=list(village_index=av_z) )
pdf(file = "plots/sisal_crop_min_conflict_bab.pdf",   width = 6, height = 6) 
dens(link2[,1] , lty=2 , col="blue" , ylim=c(0,15) , xlim=c(0,0.5) , main="probability baboon crop conflict")
abline(v=mean(link2[,1]) , col="blue" , lty=2)
dens(link2[,2] , add=TRUE , col="darkblue")
abline(v=median(link2[,2]) , col="darkblue" , lty=1)
legend('topright' , c("no sisal" , "yes sisal") , col=c("blue" , "darkblue") , lty=c(2,1))
dev.off()

pdf(file = "plots/sisal_crop_min_conflict_ele.pdf",   width = 6, height = 6) 
dens(link2[,3] , lty=2 , col="grey" , ylim=c(0,15) , xlim=c(0.2,1) , main="probability elephant crop conflict")
abline(v=mean(link2[,3]) , col="grey" , lty=2)
dens(link2[,4] , add=TRUE , col="black" )
abline(v=median(link2[,4]) , col="black" , lty=1)
legend('topleft' , c("no sisal" , "yes sisal") , col=c("grey" , "black") , lty=c(2,1))
dev.off()

pdf(file = "plots/sisal_crop_min_conflict_verv.pdf",   width = 6, height = 6) 
dens(link2[,5] , lty=2 , col="green" , ylim=c(0,15) , xlim=c(0,0.6) , main="probability vervet crop conflict")
abline(v=mean(link2[,5]) , col="green" , lty=2)
dens(link2[,6] , add=TRUE , col="darkgreen" )
abline(v=median(link2[,6]) , col="darkgreen" , lty=1)
legend('topleft' , c("no sisal" , "yes sisal") , col=c("green" , "darkgreen") , lty=c(2,1))
dev.off()


