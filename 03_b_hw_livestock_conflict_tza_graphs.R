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

plot(precis(p_livestock_params))

pdf(file = "plots/livestock_conflict_species_parameter_dotplots.pdf",   width = 7, height = 7) 
plot(precis(p_livestock_params))
points( precis(p_livestock_params)[[1]] , length(precis(p_livestock_params)[[1]]):1  , col=rep(brewer.pal(9,"Spectral"), 3) , pch=19 , cex=1)
dev.off()

####lets plot per species effects from minimal models
av_z <- matrix(0,1000,length(unique(dc$village_index))) #need to add zeros in VE to plot main effect
ylabels=c("probability hyena livestock conflict" , "probability lion livestock conflict")
colpal=c("red" , "orange")
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
    # settle_dist_km_std=rep(0,30),
    # c2070_std=plot_seq,
    c70_std=plot_seq,
    river_std=rep(0,30),
    # road_std=rep(0,30),
    build_dens_std=rep(0,30),
    # gse_slope30m_std=rep(0,30),
    # log_livestock_head_std=rep(0,30),
    # household_size_std=rep(0,30), 
    # guard_ave_day_std=rep(0,30), 
    species_index=rep(i,30)
  )
  
  link2 <- link(ml_c70_min, data=dpred , replace=list(village_index=av_z) )
  
  if(i==1){
    pdf(file = "plots/c70_livestock_min_conflict_hyena.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$hyena_l ~ dl$c70_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="forest/thicket density", xaxt='n', cex.lab=1.3)}
  if(i==2){
    pdf(file = "plots/c70_livestock_min_conflict_lion.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$lion_l ~ dl$c70_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="forest/thicket density", xaxt='n', cex.lab=1.3)}
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
    plot(dl$hyena_l ~ dl$c2070_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="woodland/open thicket/shrubland density", xaxt='n', cex.lab=1.3)}
  if(i==2){
    pdf(file = "plots/c2070_livestock_min_conflict_lion.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$lion_l ~ dl$c2070_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="woodland/open thicket/shrubland density" , xaxt='n', cex.lab=1.3)}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at= ( seq(from=0 , to=0.8 , by=0.1) - mean(dl$c2070))/sd(dl$c2070) , labels= seq(from=0 , to=0.8 , by=0.1) )
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
    plot(dl$hyena_l ~ dl$river_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="river density", xaxt='n', cex.lab=1.3)}
  if(i==2){
    pdf(file = "plots/river_livestock_min_conflict_lion.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$lion_l ~ dl$river_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="river density", xaxt='n', cex.lab=1.3)}
  pred_mean <- apply(link2 , 2 , mean)
  lines(pred_mean ~ plot_seq , lw=2, col=colpal[i] , lty=1)
  for (j in sample( c(1:1000) , 100) ){
    lines( link2[j,] ~ plot_seq , lw=3, col=col.alpha(colpal[i], alpha=0.1) , lty=1)
  }
  axis( 1 , at= ( seq(from=0 , to=0.70 , by=0.1) - mean(dl$river))/sd(dl$river) , labels= seq(from=0 , to=0.70 , by=0.1) )
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
    plot(dl$hyena_l ~ dl$settle_dist_km_std , col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="distance to settlement edge (km)" , xaxt='n', cex.lab=1.3)}
  if(i==2){
    pdf(file = "plots/settle_dist_livestock_min_conflict_lion.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$lion_l ~ dl$settle_dist_km_std , col=col.alpha(colpal[2], 0.1) , pch=19 , ylab=ylabels[i] , xlab="distance to settlement edge (km)", xaxt='n', cex.lab=1.3)}
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
    pdf(file = "plots/log_livestock head_livestock_min_conflict_hyena.pdf",   width = 6, height = 6) 
    par( mar=c(4,4,1,1)+.1 )
    plot(dl$hyena_l ~ dl$log_livestock_head_std, col=col.alpha(colpal[1], 0.1) , pch=19 , ylab=ylabels[i] , xlab="log(number of livestock head)", xaxt='n', cex.lab=1.3)}
  if(i==2){
    pdf(file = "plots/log_livestock head_livestock_min_conflict_lion.pdf",   width = 6, height = 6) 
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
#}

##############other graphs of imporatnce#####
# 
# #lanscape model dotplot, perhaps for SI
# p <- extract.samples(mc_landscape)
# 
# p_crop_landscape <- list(
#   b_BD_baboon = p$b_BD + p$b_BDs[,1],
#   b_CR_baboon = p$b_CR + p$b_CRs[,1],
#   b_C70_baboon = p$b_C70 + p$b_C70s[,1],
#   b_C2070_baboon = p$b_C2070 + p$b_C2070s[,1],
#   b_RD_baboon = p$b_RD + p$b_RDs[,1],
#   b_RIV_baboon = p$b_RIV + p$b_RIVs[,1],
#   b_SD_baboon = p$b_SD + p$b_SDs[,1],
#   b_SL_baboon = p$b_SL + p$b_SLs[,1],
#   b_BD_elephant = p$b_BD + p$b_BDs[,2],
#   b_CR_elephant = p$b_CR + p$b_CRs[,2],
#   b_C70_elephant = p$b_C70 + p$b_C70s[,2],
#   b_C2070_elephant = p$b_C2070 + p$b_C2070s[,2],
#   b_RD_elephant = p$b_RD + p$b_RDs[,2],
#   b_RIV_elephant = p$b_RIV + p$b_RIVs[,2],
#   b_SD_elephant = p$b_SD + p$b_SDs[,2],
#   b_SL_elephant = p$b_SL + p$b_SLs[,2],
#   b_BD_vervet = p$b_BD + p$b_BDs[,3],
#   b_CR_vervet = p$b_CR + p$b_CRs[,3],
#   b_C70_vervet = p$b_C70 + p$b_C70s[,3],
#   b_C2070_vervet = p$b_C2070 + p$b_C2070s[,3],
#   b_RD_vervet = p$b_RD + p$b_RDs[,3],
#   b_RIV_vervet = p$b_RIV + p$b_RIVs[,3],
#   b_SD_vervet = p$b_SD + p$b_SDs[,3],
#   b_SL_vervet = p$b_SL + p$b_SLs[,3]
# )
#  
# pdf(file = "plots/crop_conflict_species_parameter_dotplots_landscape_model.pdf",   width = 7, height = 7) 
#   plot(precis(p_crop_landscape))
#   points( precis(p_crop_landscape)[[1]] , length(precis(p_crop_landscape)[[1]]):1  , col=rep(brewer.pal(8,"Spectral"), 3) , pch=19 , cex=1)
# dev.off()
# 
# #write.csv( precis(mc17 , depth=2) , file="crop_global_model_medium_paramsv.csv" )
# #write.csv( precis(mc17 , depth=3) , file="crop_global_model_long_paramsv.csv" )
# 
  #####livestock conflict crop predictions
  ras_leo <-  read.csv("lionRasterstacktopoints_survext2.csv")
  
  ras_leo$settle_dist_km <- ras_leo$settle_dist/1000
  ras_leo$c70_std <- (ras_leo$c70 -mean(dl$c70 ) )/ sd(dl$c70 ) 
  ras_leo$road_std <- (ras_leo$road -mean(dl$road ) )/ sd(dl$road) 
  ras_leo$build_dens_std <- (ras_leo$build_dens-mean(dl$build_dens ) )/ sd(dl$build_dens) 
  ras_leo$gse_slope30m_std <- (ras_leo$gse_slope30m-mean(dl$gse_slope30m) )/sd(dl$gse_slope30m) 
  ras_leo$settle_dist_km_std <- (ras_leo$settle_dist_km-mean(dl$settle_dist_km ) )/ sd(dl$settle_dist_km) 
  ras_leo$c2070_std <- (ras_leo$c2070 -mean(dl$c2070 ) )/ sd(dl$c2070 ) 
  ras_leo$river_std <- (ras_leo$river -mean(dl$river ) )/ sd(dl$river) 
  ras_leo$species_index <- 2
  
  
  p <- extract.samples(ml_landscape)
  
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
  ras_leo$pred_leo_crop_conflict <-0
  ras_leo$pred_leo_crop_conflict <- 
    logistic( mean(p$a + p$as[,2]) + 
                mean(p$b_SD + p$b_SDs[,2])*dpred$settle_dist_km_std  + 
                mean(p$b_C70 + p$b_C70s[,2])*dpred$c70_std +
                mean(p$b_RD + p$b_RDs[,2])*dpred$road_std +
                mean(p$b_C2070 + p$b_C2070s[,2])*dpred$c2070_std +
                mean(p$b_RIV + p$b_RIVs[,2])*dpred$river_std +
                mean(p$b_BD + p$b_BDs[,2])*dpred$build_dens_std +
                mean(p$b_SL + p$b_SLs[,2])*dpred$gse_slope30m_std   
    )
  
  par(mfrow=c(1,1))
  dens(ras_leo$pred_leo_crop_conflict)
  
  ras_leo_sub <- cbind( ras_leo[1:3] , ras_leo$pred_leo_crop_conflict)
  write.csv(ras_leo_sub , file="ras_leo_deadstock_preds_03052021.csv")
  
  
  ########hyena
  # ras_hyena<-  read.csv("~/Dropbox/tza_wildlife_conflict/hyenaRasterstacktopoints_survext2.csv")
  ras_hyena<-  read.csv("hyenaRasterstacktopoints_survext2.csv")
  
  ras_hyena$settle_dist_km <- ras_hyena$settle_dist/1000
  ras_hyena$c70_std <- (ras_hyena$c70 -mean(dl$c70 ) )/ sd(dl$c70 ) 
  ras_hyena$road_std <- (ras_hyena$road -mean(dl$road ) )/ sd(dl$road) 
  ras_hyena$build_dens_std <- (ras_hyena$build_dens-mean(dl$build_dens ) )/ sd(dl$build_dens) 
  ras_hyena$gse_slope30m_std <- (ras_hyena$gse_slope30m-mean(dl$gse_slope30m) )/sd(dl$gse_slope30m) 
  ras_hyena$settle_dist_km_std <- (ras_hyena$settle_dist_km-mean(dl$settle_dist_km ) )/ sd(dl$settle_dist_km) 
  ras_hyena$c2070_std <- (ras_hyena$c2070 -mean(dl$c2070 ) )/ sd(dl$c2070 ) 
  ras_hyena$river_std <- (ras_hyena$river -mean(dl$river ) )/ sd(dl$river) 
  ras_hyena$species_index <- 1
  
  p <- extract.samples(ml_landscape)
  
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
  ras_hyena$pred_hyena_crop_conflict <-0
  ras_hyena$pred_hyena_crop_conflict <- 
    logistic( mean(p$a + p$as[,1]) + 
                mean(p$b_SD + p$b_SDs[,1])*dpred$settle_dist_km_std  + 
                mean(p$b_C70 + p$b_C70s[,1])*dpred$c70_std +
                mean(p$b_RD + p$b_RDs[,1])*dpred$road_std +
                mean(p$b_C2070 + p$b_C2070s[,1])*dpred$c2070_std +
                mean(p$b_RIV + p$b_RIVs[,1])*dpred$river_std +
                mean(p$b_BD + p$b_BDs[,1])*dpred$build_dens_std +
                mean(p$b_SL + p$b_SLs[,1])*dpred$gse_slope30m_std   
    )
  
  par(mfrow=c(1,1))
  dens(ras_hyena$pred_hyena_crop_conflict)
  
  ras_hyena_sub <- cbind( ras_hyena[1:3] , ras_hyena$pred_hyena_crop_conflict)
  write.csv(ras_hyena_sub , file="ras_hyena_deadstock_preds_03052021.csv")
#########jusr sd