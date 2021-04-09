require(rethinking)
require(dagitty)

###add DAG

ls_conf_yes_guard <- 
  dagitty('dag {
  c2070 -> conflict
  bd -> conflict 
  bd <-> road 
  c70 -> conflict 
  hh_size -> guards 
  hh_size -> lsh 
  lsh -> conflict 
  river -> c70 
  river -> conflict 
  sd -> bd 
  sd -> conflict
  bd -> c70
  bd -> c2070
  guards <-> conflict 
  slope -> bd 
  slope -> conflict
  slope -> c2070 
  slope -> river 
  slope -> road 
}')

plot(ls_conf_yes_guard)

#c2070
adjustmentSets( ls_conf_yes_guard , exposure="c2070" , outcome="conflict" , type="minimal") #independent of others should be invariant

ml_c2070_min <- map2stan(
  alist(
    conflict ~ binomial(1,p),
    logit(p) <- a + av[village_index] + as[species_index] +  
      (b_C2070 + b_C2070s[species_index])*c2070_std +
      (b_BD + b_BDs[species_index])*build_dens_std +
      (b_SL + b_SLs[species_index])*gse_slope30m_std  ,
    c(a,b_C2070,b_BD,b_SL) ~ normal( 0 , 1 ),
    av[village_index] ~ dnorm(0,sigma_v),
    c(as,b_C2070s,b_BDs,b_SLs)[species_index] ~ dmvnormNC(sigma_s,Rho) ,
    c(sigma_v,sigma_s) ~ dexp(1),
    Rho ~ dlkjcorr(3)
    
  ), data=dl , chains=4 , cores=4 , iter=3000 , log_lik=TRUE )

precis(ml_c2070_min, depth=2)
#bd
#c70
#lsh
#river
#sd
#guards
#slope
