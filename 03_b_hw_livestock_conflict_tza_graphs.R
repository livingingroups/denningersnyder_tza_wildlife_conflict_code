require(rethinking)
require(RColorBrewer)
###########dotplots of parameters

pbd <- extract.samples(ml_bd_min)
pc70 <- extract.samples(ml_c70_min)
pc2070 <- extract.samples(ml_c2070_min)
priv <- extract.samples(ml_riv_min)
psd <- extract.samples(ml_sd_min)
psl <- extract.samples(ml_sl_min)
plsh <- extract.samples(ml_lsh_min)
pgua <- extract.samples(ml_guard_min)
pguaXhh <- extract.samples(ml_lshXguard_min)

p_livestock_params <- list(
  b_BD_hyena = pbd$b_BD + pbd$b_BDs[,1],
  b_C70_hyena = pc70$b_C70 + pc70$b_C70s[,1],
  b_C2070_hyena = pc2070$b_C2070 + pc2070$b_C2070s[,1],
  b_RIV_hyena = priv$b_RIV + priv$b_RIVs[,1],
  b_SD_hyena = psd$b_SD + psd$b_SDs[,1],
  b_SL_hyena = psl$b_SL + psl$b_SLs[,1],
  b_LSH_hyena = plsh$b_LSH + plsh$b_LSHs[,1],
  b_GU_hyena = pguaXhh$b_GU + pguaXhh$b_GUs[,1], #highlight collider potential
  b_GUxLSH_hyena = pguaXhh$b_GUxLSH + pguaXhh$b_GUxLSHs[,1],
  b_BD_lion = pbd$b_BD + pbd$b_BDs[,2],
  b_C70_lion = pc70$b_C70 + pc70$b_C70s[,2],
  b_C2070_lion = pc2070$b_C2070 + pc2070$b_C2070s[,2],
  b_RIV_lion = priv$b_RIV + priv$b_RIVs[,2],
  b_SD_lion = psd$b_SD + psd$b_SDs[,2],
  b_SL_lion = psl$b_SL + psl$b_SLs[,2],
  b_LSH_lion = plsh$b_LSH + plsh$b_LSHs[,2],
  b_GU_lion = pgua$b_GU + pgua$b_GUs[,2], #highlight collider potential
  b_GUxLSH_lion = pguaXhh$b_GUxLSH + pguaXhh$b_GUxLSHs[,2]
)

colpal=c("red" , "orange")

pdf(file = "plots/livestock_conflict_species_parameter_dotplots_old.pdf",   width = 7, height = 7) 
plot(precis(p_livestock_params))
points( precis(p_livestock_params)[[1]] , length(precis(p_livestock_params)[[1]]):1  , col=rep(brewer.pal(9,"Spectral"), 3) , pch=19 , cex=1)
dev.off()

pdf(file = "plots/livestock_conflict_species_parameter_dotplots.pdf",   width = 7, height = 3.5) 
  par(mfrow = c(1, 2) , mar=c(2,0,2,0) + 0.2 , oma=c(2,5,0,0) + 0.2)
  plot(precis(p_livestock_params[1:9]),  labels='' , main="a. hyena" , xlim=c(-2.5,2.5 ) , col=colpal[1] )
  axis(2, at=1:9, labels=str_remove(names(p_livestock_params)[1:9] , "_hyena") ,las=2, tck=0 , lty=0 , cex=1.2)
  plot(precis(p_livestock_params[10:18]) , labels='', main="b. lion", xlim=c(-2.5,2.5 ), col=colpal[2])
  mtext('parameter estimate', at=0.5 , side=1, outer=T,cex=1.2,line=0.75)
dev.off()

####lets plot per species effects from minimal models
av_z <- matrix(0,1000,length(unique(dl$village_index))) #need to add zeros in VE to plot main effect
ylabels=c("probability hyena livestock conflict" , "probability lion livestock conflict")

###building density
precis(ml_bd_min)

plot_seq <- seq(from=min(dl$build_dens_std) , to=max(dl$build_dens_std) , length=30)

for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    build_dens_std=plot_seq,
    settle_dist_km_std=rep(0,30),
    gse_slope30m_std=rep(0,30),
    species_index=rep(i,30)
  )
  
  link2 <- link(ml_bd_min, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "plots/build_dens_livestock_min_conflict_hyena.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$hyena_l ~ dl$build_dens_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="building density", xaxt='n', cex.lab=1.3)}
  if(i==2){
    pdf(file = "plots/build_dens_livestock_min_conflict_lion.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$lion_l ~ dl$build_dens_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="building density", xaxt='n', cex.lab=1.3)}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at= ( seq(from=0 , to=120 , by=10) - mean(dl$build_dens))/sd(dl$build_dens) , labels= seq(from=0 , to=120 , by=10) )
  dev.off()
}

#####cover 70
precis(ml_c70_min)
plot_seq <- seq(from=min(dl$c70_std) , to=max(dl$c70_std) , length=30)
av_z <- matrix(0,1000,length(unique(dl$village_index))) #need to add zeros in VE to plot main effect

for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    c70_std=plot_seq,
    river_std=rep(0,30),
    build_dens_std=rep(0,30),
    species_index=rep(i,30)
  )
  
  link2 <- link(ml_c70_min, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "plots/c70_livestock_min_conflict_hyena.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$hyena_l ~ dl$c70_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="proportion densely wooded cover", xaxt='n', cex.lab=1.3)}
  if(i==2){
    pdf(file = "plots/c70_livestock_min_conflict_lion.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$lion_l ~ dl$c70_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="proportion densely wooded cover", xaxt='n', cex.lab=1.3)}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at= ( seq(from=0 , to=0.8 , by=0.1) - mean(dl$c70))/sd(dl$c70) , labels= seq(from=0 , to=0.8 , by=0.1) )
  dev.off()
}

#cover 2070
precis(ml_c2070_min)
plot_seq <- seq(from=min(dl$c2070_std) , to=max(dl$c2070_std) , length=30)

for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    c2070_std=plot_seq,
    build_dens_std=rep(0,30),
    gse_slope30m_std=rep(0,30),
    species_index=rep(i,30)
  )
  
  link2 <- link(ml_c2070_min, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "plots/c2070_livestock_min_conflict_hyena.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$hyena_l ~ dl$c2070_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="proportion moderately wooded cover", xaxt='n', cex.lab=1.3)}
  if(i==2){
    pdf(file = "plots/c2070_livestock_min_conflict_lion.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$lion_l ~ dl$c2070_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="proportion moderately wooded cover" , xaxt='n', cex.lab=1.3)}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at= ( seq(from=0 , to=0.8 , by=0.1) - mean(dl$c2070) )/sd(dl$c2070) , labels= seq(from=0 , to=0.8 , by=0.1) )
  dev.off()
}

#river
precis(ml_riv_min)
plot_seq <- seq(from=min(dl$river_std) , to=max(dl$river_std) , length=30)

for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    river_std=plot_seq,
    gse_slope30m_std=rep(0,30),
    species_index=rep(i,30)
  )
  
  link2 <- link(ml_riv_min, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "plots/river_livestock_min_conflict_hyena.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$hyena_l ~ dl$river_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="river density", xaxt='n', cex.lab=1.3)
    }
  if(i==2){
    pdf(file = "plots/river_livestock_min_conflict_lion.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$lion_l ~ dl$river_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="river density", xaxt='n', cex.lab=1.3)
    }
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at=(seq(from=0,to=0.06,by=0.01) - mean(dl$river) )/sd(dl$river) , labels= seq(from= 0 , to=0.06 , by=0.01) )
  dev.off()
}

#settlement distance
precis(ml_sd_min)
plot_seq <- seq(from=min(dl$settle_dist_km_std) , to=max(dl$settle_dist_km_std) , length=30)

for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,30),
    settle_dist_km_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(ml_sd_min, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "plots/settle_dist_livestock_min_conflict_hyena.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$hyena_l ~ dl$settle_dist_km_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="distance into settlements (km)" , xaxt='n', cex.lab=1.3)}
  if(i==2){
    pdf(file = "plots/settle_dist_livestock_min_conflict_lion.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$lion_l ~ dl$settle_dist_km_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="distance into settlement (km)", xaxt='n', cex.lab=1.3)}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at= ( c(0:12) - mean(dl$settle_dist_km))/sd(dl$settle_dist_km) , labels=c(0:12))
  
  dev.off()
}

#slope
precis(ml_sl_min)
plot_seq <- seq(from=min(dl$gse_slope30m_std) , to=max(dl$gse_slope30m_std) , length=30)

for (i in 1:2){

  dpred <- list(
    village_index=rep(1,30),
    gse_slope30m_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(ml_sl_min, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "plots/slope30m_livestock_min_conflict_hyena.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$hyena_l ~ dl$gse_slope30m_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="slope 30m density", xaxt='n', cex.lab=1.3)}
  if(i==2){
    pdf(file = "plots/slope30m_livestock_min_conflict_lion.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$lion_l ~ dl$gse_slope30m_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="slope 30m density", xaxt='n', cex.lab=1.3)}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at= ( seq(from=1 , to=12 , by=1) - mean(dl$gse_slope30m))/sd(dl$gse_slope30m) , labels= seq(from=1 , to=12 , by=1) )
  
  dev.off()
}

##lsh
precis(ml_lsh_min)

plot_seq <- seq(from=min(dl$log_livestock_head_std) , to=max(dl$log_livestock_head_std) , length=30)

for (i in 1:2){
  dpred <- list(
    village_index=rep(1,30),
    log_livestock_head_std=plot_seq,
    species_index=rep(i,30)
  )
  
  link2 <- link(ml_lsh_min, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "plots/log_livestock_head_livestock_min_conflict_hyena.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$hyena_l ~ dl$log_livestock_head_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="log(number of livestock head)", xaxt='n', cex.lab=1.3)}
  if(i==2){
    pdf(file = "plots/log_livestock_head_livestock_min_conflict_lion.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$lion_l ~ dl$log_livestock_head_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="log(number of livestock head)", xaxt='n', cex.lab=1.3)}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at= ( seq(from=1 , to=7 , by=1) - mean(dl$log_livestock_head))/sd(dl$log_livestock_head) , labels= seq(from=1 , to=7 , by=1 ))
  dev.off()
}

###guards
precis(ml_guard_min)

plength <- 15
plot_seq <- seq(from=min(dl$guard_ave_day_std) , to=max(dl$guard_ave_day_std) , length=plength )

for (i in 1:2){
  
  dpred <- list(
    village_index=rep(1,plength ),
    guard_ave_day_std=plot_seq,
    household_size_std=rep(0,plength ),
    log_livestock_head_std=rep(0,plength ),
    species_index=rep(i,plength )
  )
  
  link2 <- link(ml_guard_min, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "plots/num_guards_livestock_min_conflict_hyena.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$hyena_l ~ dl$guard_ave_day_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="average number of guards per day", xaxt='n', cex.lab=1.3)}
  if(i==2){
    pdf(file = "plots/num_guards_livestock_min_conflict_lion.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$lion_l ~ dl$guard_ave_day_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="average number of guards per day", xaxt='n', cex.lab=1.3)}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at=(c(0:4) - mean(dl$guard_ave_day))/sd(dl$guard_ave_day) , labels=c(0:4))
  dev.off()
}

###guard X lsh
colpal1=brewer.pal(7,"Reds")
colpal2=brewer.pal(7,"Oranges")
colpal1=colpal1[3:7]
colpal2=colpal2[3:7]
plot_seq <- seq(from=min(dl$log_livestock_head_std) , to=max(dl$log_livestock_head_std) , length=plength)

pdf(file = "plots/num_guardsXlsh_livestock_min_conflict_hyena.pdf",   width = 15, height = 3)
par(mfrow=c(1,5))
par( mar=c(4,4,1,1)+.1 )

for(j in 1:5){
  i <- 1
  dpred <- list(
    village_index=rep(1,plength),
    log_livestock_head_std=plot_seq,
    species_index=rep(i,plength),
    household_size_std=rep(0,plength), 
    guard_ave_day_std=rep( sort(unique(dl$guard_ave_day_std))[j] , plength)
  )
  
  link2 <- link(ml_lshXguard_min, data=dpred , replace=list(village_index=av_z) )
  
    plot(dl$hyena_l[dl$guard_ave_day==j-1] ~ dl$log_livestock_head_std[dl$guard_ave_day==j-1], col=col.alpha(colpal1[j], 0.1) , pch=19 , ylab=ylabels[i] , xlab="log number livestock head", xaxt='n', cex.lab=1.3 , xlim=range(dl$log_livestock_head_std) , ylim=c(0,1))
    title(main=paste("number guards = ", j-1) )
    pred_mean <- apply(link2 , 2 , mean)
    lines(pred_mean ~ plot_seq , lw=2, col=colpal1[j] , lty=1)
    for (k in sample( c(1:1000) , 100) ){
      lines( link2[k,] ~ plot_seq , lw=3, col=col.alpha(colpal1[j], alpha=0.1) , lty=1)
    }
    axis( 1 , at= ( seq(from=1 , to=7 , by=1) - mean(dl$log_livestock_head))/sd(dl$log_livestock_head) , labels= seq(from=1 , to=7 , by=1 ))
}
dev.off()

pdf(file = "plots/num_guardsXlsh_livestock_min_conflict_lion.pdf",   width = 15, height = 3)
par(mfrow=c(1,5))
par( mar=c(4,4,1,1)+.1 )

for(j in 1:5){
  i <- 2
  dpred <- list(
    village_index=rep(1,plength),
    log_livestock_head_std=plot_seq,
    species_index=rep(i,plength),
    household_size_std=rep(0,plength), 
    guard_ave_day_std=rep( sort(unique(dl$guard_ave_day_std))[j] , plength)
  )
  
  link2 <- link(ml_lshXguard_min, data=dpred , replace=list(village_index=av_z) )
  
    plot(dl$lion_l[dl$guard_ave_day==j-1] ~ dl$log_livestock_head_std[dl$guard_ave_day==j-1] , col=col.alpha(colpal2[j], 0.1) , pch=19 , ylab=ylabels[i] , xlab="log number livestock head", xaxt='n', cex.lab=1.1 , xlim=range(dl$log_livestock_head_std), ylim=c(0,1))
    title(main=paste("number guards = ", j-1) )
    pred_mean <- apply(link2 , 2 , mean)
    lines(pred_mean ~ plot_seq , lw=2, col=colpal2[j] , lty=1)
    for (k in sample( c(1:1000) , 100) ){
      lines( link2[k,] ~ plot_seq , lw=3, col=col.alpha(colpal2[j], alpha=0.1) , lty=1)
    }
    axis( 1 , at= ( seq(from=1 , to=7 , by=1) - mean(dl$log_livestock_head))/sd(dl$log_livestock_head) , labels= seq(from=1 , to=7 , by=1 ))
    
  }
    
  
  dev.off()
  
############################################
##########RASTER PREDS######################
############################################
  
######################hyena
ras_hyena <-  read.csv("~/R/tzawc_bigfiles/hyenaRasterstacktopoints_survext2.csv")

ras_hyena$settle_dist_km <- ras_hyena$settle_dist/1000
ras_hyena$c70_std <- (ras_hyena$c70 -mean(dl$c70 ) )/ sd(dl$c70 ) 
ras_hyena$road_std <- (ras_hyena$road -mean(dl$road ) )/ sd(dl$road) 
ras_hyena$build_dens_std <- (ras_hyena$build_dens-mean(dl$build_dens ) )/ sd(dl$build_dens) 
ras_hyena$gse_slope30m_std <- (ras_hyena$gse_slope30m-mean(dl$gse_slope30m) )/sd(dl$gse_slope30m) 
ras_hyena$settle_dist_km_std <- (ras_hyena$settle_dist_km-mean(dl$settle_dist_km ) )/ sd(dl$settle_dist_km) 
ras_hyena$c2070_std <- (ras_hyena$c2070 -mean(dl$c2070 ) )/ sd(dl$c2070 ) 
ras_hyena$river_std <- (ras_hyena$river -mean(dl$river ) )/ sd(dl$river) 
ras_hyena$species_index <- 1

samples_landscape <- round(2000*livestock_waic_tab_lscape@.Data[[6]][1])#samples of preds for landscape model based on waic values
samples_bd <- round(2000*livestock_waic_tab_lscape@.Data[[6]][2])#samples of preds for building density model based on waic values
samples_sd <- 1 +round(2000*livestock_waic_tab_lscape@.Data[[6]][3])#samples of preds for building density model based on waic values

pl <- extract.samples(ml_landscape, n=samples_landscape)
pbd <- extract.samples(ml_bd_min, n=samples_bd)
psd <- extract.samples(ml_sd_min, n=samples_sd)

str(pl)
str(pbd)

dpred <- list(
  village_index=rep(1,nrow(ras_hyena)),
  settle_dist_km_std=ras_hyena$settle_dist_km_std,
  c70_std = ras_hyena$c70_std,
  road_std= ras_hyena$road_std,
  c2070_std = ras_hyena$c2070_std,
  river_std= ras_hyena$river_std,
  build_dens_std=ras_hyena$build_dens_std,
  gse_slope30m_std= ras_hyena$gse_slope30m_st,
  species_index=ras_hyena$species_index
)

##predict conflict using all raster data and model

##note this is with other variables at mean, excluded b/c standardized so mean in 0, conputationally kwiker
str(pl)
preds_l_hyena <- matrix(data=NA , nrow=length(dpred$settle_dist_km_std ) , ncol=samples_landscape )

for(i in 1:samples_landscape){
  preds_l_hyena[,i] <- logistic( (pl$a[i] + pl$as[i,1]) + 
                                 (pl$b_SD[i] + pl$b_SDs[i,1])*dpred$settle_dist_km_std  + 
                                 (pl$b_C70[i] + pl$b_C70s[i,1])*dpred$c70_std +
                                 (pl$b_C2070[i] + pl$b_C2070s[i,1])*dpred$c2070_std +
                                 (pl$b_BD[i] + pl$b_BDs[i,1])*dpred$build_dens_std +
                                 (pl$b_RD[i] + pl$b_RDs[i,1])*dpred$road_std  + 
                                 (pl$b_RIV[i] + pl$b_RIVs[i,1])*dpred$river_std  + 
                                 (pl$b_SL[i] + pl$b_SLs[i,1])*dpred$gse_slope30m_std 
  )
}

preds_bd_hyena <- matrix(data=NA , nrow=length(dpred$settle_dist_km_std ) , ncol=samples_bd )

for(i in 1:samples_bd){
  preds_bd_hyena[,i] <- logistic( (pbd$a[i] + pbd$as[i,1]) + 
                                  (pbd$b_SD[i] + pbd$b_SDs[i,1])*dpred$settle_dist_km_std  + 
                                  (pbd$b_BD[i] + pbd$b_BDs[i,1])*dpred$build_dens_std +
                                  (pbd$b_SL[i] + pbd$b_SLs[i,1])*dpred$gse_slope30m_std 
  )
}

str(preds_bd_hyena)

preds_sd_hyena <- matrix(data=NA , nrow=length(dpred$settle_dist_km_std ) , ncol=samples_sd )

for(i in 1:samples_sd){
  preds_sd_hyena[,i] <- logistic( (psd$a[i] + psd$as[i,1]) + 
                                  (psd$b_SD[i] + psd$b_SDs[i,1])*dpred$settle_dist_km_std 
  )
}

str(preds_sd_hyena)

preds_hyena <- cbind(preds_l_hyena,preds_bd_hyena,preds_sd_hyena) # binds in70 a 200 column prediction jaun

str(preds_hyena)

preds_hyena_mean <- preds_hyena_med <- rep(NA,nrow(preds_hyena))
preds_hyena_mean <- apply(preds_hyena , 1, mean)
preds_hyena_med <- apply(preds_hyena , 1 , median)
#preds_hyena_med <- apply(preds_hyena , 1 , HPDI)

dens(preds_hyena_med)
dens(preds_hyena_mean)

ras_hyena_sub <- cbind( ras_hyena[1:3] , preds_hyena_mean , preds_hyena_med)
str(ras_hyena_sub)
write.csv(ras_hyena_sub , file="ras_hyena_crop_preds_01082022.csv")

######################leo aka lyonz
ras_leo <-  read.csv("~/R/tzawc_bigfiles/lionRasterstacktopoints_survext21.csv")

ras_leo$settle_dist_km <- ras_leo$settle_dist/1000
ras_leo$c70_std <- (ras_leo$c70 -mean(dl$c70 ) )/ sd(dl$c70 ) 
ras_leo$road_std <- (ras_leo$road -mean(dl$road ) )/ sd(dl$road) 
ras_leo$build_dens_std <- (ras_leo$build_dens-mean(dl$build_dens ) )/ sd(dl$build_dens) 
ras_leo$gse_slope30m_std <- (ras_leo$gse_slope30m-mean(dl$gse_slope30m) )/sd(dl$gse_slope30m) 
ras_leo$settle_dist_km_std <- (ras_leo$settle_dist_km-mean(dl$settle_dist_km ) )/ sd(dl$settle_dist_km) 
ras_leo$c2070_std <- (ras_leo$c2070 -mean(dl$c2070 ) )/ sd(dl$c2070 ) 
ras_leo$river_std <- (ras_leo$river -mean(dl$river ) )/ sd(dl$river) 
ras_leo$species_index <- 1

samples_landscape <- round(2000*livestock_waic_tab_lscape@.Data[[6]][1])#samples of preds for landscape model based on waic values
samples_bd <- round(2000*livestock_waic_tab_lscape@.Data[[6]][2])#samples of preds for building density model based on waic values
samples_sd <- round(2000*livestock_waic_tab_lscape@.Data[[6]][3]) + 1#samples of preds for building density model based on waic values

pl <- extract.samples(ml_landscape, n=samples_landscape)
pbd <- extract.samples(ml_bd_min, n=samples_bd)
psd <- extract.samples(ml_sd_min, n=samples_sd)

str(pl)
str(pbd)
str(psd)

dpred <- list(
  village_index=rep(1,nrow(ras_leo)),
  settle_dist_km_std=ras_leo$settle_dist_km_std,
  c70_std = ras_leo$c70_std,
  road_std= ras_leo$road_std,
  c2070_std = ras_leo$c2070_std,
  river_std= ras_leo$river_std,
  build_dens_std=ras_leo$build_dens_std,
  gse_slope30m_std= ras_leo$gse_slope30m_st,
  species_index=ras_leo$species_index
)

##predict conflict using all raster data and model

##note this is with other variables at mean, excluded b/c standardized so mean in 0, conputationally kwiker
str(pl)
preds_l_leo <- matrix(data=NA , nrow=length(dpred$settle_dist_km_std ) , ncol=samples_landscape )

for(i in 1:samples_landscape){
  preds_l_leo[,i] <- logistic( (pl$a[i] + pl$as[i,2]) + 
                                 (pl$b_SD[i] + pl$b_SDs[i,2])*dpred$settle_dist_km_std  + 
                                 (pl$b_C70[i] + pl$b_C70s[i,2])*dpred$c70_std +
                                 (pl$b_C2070[i] + pl$b_C2070s[i,2])*dpred$c2070_std +
                                 (pl$b_BD[i] + pl$b_BDs[i,2])*dpred$build_dens_std +
                                 (pl$b_RD[i] + pl$b_RDs[i,2])*dpred$road_std  + 
                                 (pl$b_RIV[i] + pl$b_RIVs[i,2])*dpred$river_std  + 
                                 (pl$b_SL[i] + pl$b_SLs[i,2])*dpred$gse_slope30m_std 
  )
}

preds_bd_leo <- matrix(data=NA , nrow=length(dpred$settle_dist_km_std ) , ncol=samples_bd )

for(i in 1:samples_bd){
  preds_bd_leo[,i] <- logistic( (pbd$a[i] + pbd$as[i,2]) + 
                                  (pbd$b_SD[i] + pbd$b_SDs[i,2])*dpred$settle_dist_km_std  + 
                                  (pbd$b_BD[i] + pbd$b_BDs[i,2])*dpred$build_dens_std +
                                  (pbd$b_SL[i] + pbd$b_SLs[i,2])*dpred$gse_slope30m_std 
  )
}

str(preds_bd_leo)

preds_sd_leo <- matrix(data=NA , nrow=length(dpred$settle_dist_km_std ) , ncol=samples_sd )

for(i in 1:samples_sd){
  preds_sd_leo[,i] <- logistic( (psd$a[i] + psd$as[i,2]) + 
                                  (psd$b_SD[i] + psd$b_SDs[i,2])*dpred$settle_dist_km_std 
  )
}

str(preds_sd_leo)

preds_leo <- cbind(preds_l_leo,preds_bd_leo,preds_sd_leo) # binds in70 a 200 column prediction jaun

str(preds_leo)

preds_leo_mean <- preds_leo_med <- rep(NA,nrow(preds_leo))
preds_leo_mean <- apply(preds_leo , 1, mean)
preds_leo_med <- apply(preds_leo , 1 , median)

dens(preds_leo_med)
dens(preds_leo_mean)

ras_leo_sub <- cbind( ras_leo[1:3] , preds_leo_mean , preds_leo_med)
str(ras_leo_sub)
write.csv(ras_leo_sub , file="ras_leo_crop_preds_02082022.csv")
