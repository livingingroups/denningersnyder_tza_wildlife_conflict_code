library("dagitty")
#https://cran.r-project.org/web/packages/dagitty/dagitty.pdf

##below is dag for livestock no dougle guard causality
##sample code below, including unobserved confound

dag_6.1 <- dagitty( "dag {
    U [unobserved]
    X -> Y
    X <- U <- A -> C -> Y
    U -> B <- C
}")


#check for colliders
g1 <- dagitty("dag{X -> Y -> Z}")
stopifnot( isTRUE(!isCollider( g1, "X", "Y", "Z" )) )
g2 <- dagitty("dag{X -> Y <- Z }")
stopifnot( isTRUE(isCollider( g2, "X", "Y", "Z" )) )

####here is relevant code to DAGs and grumeti
##imply all relationships
ls_conf_no_guard <- 
  dagitty('dag {
  c2070 -> conflict
  bd -> conflict
  bd <-> road
  c70 -> conflict
  hh_size -> lsh
  hh_size -> guards
  lsh -> conflict
  lsh -> guards
  River -> c70
  River -> conflict
  sd -> bd
  sd -> conflict
  guards -> conflict
  slope -> bd
  slope -> conflict
  slope -> road
}'
          )
plot(ls_conf_no_guard)
###this will tell us what are the minimal things we need to condition on to make inferences about the relationship between items
adjustmentSets( ls_conf_no_guard , exposure="c2070" , outcome="conflict" , type="canonical") #independent of others should be invariant
adjustmentSets( ls_conf_no_guard , exposure="c2070" , outcome="conflict" , type="minimal") #independent of others should be invariant

adjustmentSets( ls_conf_no_guard , exposure="c70" , outcome="conflict" ) #account for river
adjustmentSets( ls_conf_no_guard , exposure="c70" , outcome="conflict", type="canonical" ) #account for river

adjustmentSets( ls_conf_no_guard , exposure="lsh" , outcome="conflict" ) #account for hh_size
adjustmentSets( ls_conf_no_guard , exposure="lsh" , outcome="conflict" , type="canonical") #account for hh_size

adjustmentSets( ls_conf_no_guard , exposure="river" , outcome="conflict" )
adjustmentSets( ls_conf_no_guard , exposure="river" , outcome="conflict" , type="canonical")

adjustmentSets( ls_conf_no_guard , exposure="sd" , outcome="conflict" )
adjustmentSets( ls_conf_no_guard , exposure="sd" , outcome="conflict" , type="canonical")

adjustmentSets( ls_conf_no_guard , exposure="slope" , outcome="conflict" )
adjustmentSets( ls_conf_no_guard , exposure="slope" , outcome="conflict", type="canonical" )

adjustmentSets( ls_conf_no_guard , exposure="guards" , outcome="conflict" )
adjustmentSets( ls_conf_no_guard , exposure="guards" , outcome="conflict", type="canonical" )

##identifing colliders
isCollider(ls_conf_no_guard , "hh_size" , "guards","conflict") #shows not a collider on path from hh_size to conflict thru guards
isCollider(ls_conf_no_guard , "hh_size" , "guards","lsh") #shows guards is collider on path from hh_size to LSH

ls_conf_yes_guard <- 
  dagitty('dag {
  c2070 -> conflict
  bd -> conflict
  bd <-> road
  c70 -> conflict
  hh_size -> lsh
  hh_size -> guards
  lsh -> conflict
  lsh -> guards
  river -> c70
  river -> conflict
  sd -> bd
  sd -> conflict
  guards <-> conflict
  slope -> bd
  slope -> conflict
  slope -> road
}')

plot(ls_conf_yes_guard)
###this will tell us what are the minimal things we need to condition on to make inferences about the relationship between items
adjustmentSets( ls_conf_yes_guard , exposure="c2070" , outcome="conflict" , type="canonical") #independent of others should be invariant
adjustmentSets( ls_conf_yes_guard , exposure="c2070" , outcome="conflict" , type="minimal") #independent of others should be invariant

adjustmentSets( ls_conf_yes_guard , exposure="c70" , outcome="conflict" ) #account for river
adjustmentSets( ls_conf_yes_guard , exposure="c70" , outcome="conflict", type="canonical" ) #account for river

adjustmentSets( ls_conf_yes_guard , exposure="lsh" , outcome="conflict" ) #account for hh_size
adjustmentSets( ls_conf_yes_guard , exposure="lsh" , outcome="conflict" , type="canonical") #account for hh_size

adjustmentSets( ls_conf_yes_guard , exposure="river" , outcome="conflict" )
adjustmentSets( ls_conf_yes_guard , exposure="river" , outcome="conflict" , type="canonical")

adjustmentSets( ls_conf_yes_guard , exposure="sd" , outcome="conflict" )
adjustmentSets( ls_conf_yes_guard , exposure="sd" , outcome="conflict" , type="canonical")

adjustmentSets( ls_conf_yes_guard , exposure="slope" , outcome="conflict" )
adjustmentSets( ls_conf_yes_guard , exposure="slope" , outcome="conflict", type="canonical" )

adjustmentSets( ls_conf_yes_guard , exposure="guards" , outcome="conflict" )
adjustmentSets( ls_conf_yes_guard , exposure="guards" , outcome="conflict", type="canonical" )

##identifing colliders
isCollider(ls_conf_yes_guard , "hh_size" , "guards","conflict") #shows not a collider on path from hh_size to conflict thru guards
isCollider(ls_conf_yes_guard , "hh_size" , "guards","lsh") #shows guards is collider on path from hh_size to LSH


###crop damage
crop_damage_dag <- 
  dagitty('dag {
  c2070 -> crop_damage
  c70 -> crop_damage
  river -> c70
  river -> c2070
  river -> crop_damage
  months_planted -> crop_damage
  farm_size -> crop_damage
  farm_size -> num_protect
  num_protect -> crop_damage
  crop_damage -> num_protect
  hh_size -> num_protect
  hh_size -> farm_size
  hh_size -> crop_damage
  see_field -> num_protect
  see_field -> crop_damage
  road -> bd
  bd -> crop_damage
  bd -> c2070
  bd -> c70
  sd -> bd
  sd -> crop_damage
  cd -> c70
  cd -> c2070
  cd -> crop_damage
  }')

plot(crop_damage_dag)

adjustmentSets( crop_damage_dag , exposure="c2070" , outcome="crop_damage" , type="canonical") #independent of others should be invariant
adjustmentSets( crop_damage_dag , exposure="c2070" , outcome="crop_damage" , type="minimal") #independent of others should be invariant

adjustmentSets( crop_damage_dag , exposure="c70" , outcome="crop_damage" ) #account for river
adjustmentSets( crop_damage_dag , exposure="c70" , outcome="crop_damage", type="canonical" ) #account for river

adjustmentSets( crop_damage_dag , exposure="cd" , outcome="crop_damage" ) #account for hh_size
adjustmentSets( crop_damage_dag , exposure="cd" , outcome="crop_damage" , type="canonical") #account for hh_size

adjustmentSets( crop_damage_dag , exposure="river" , outcome="crop_damage" )
adjustmentSets( crop_damage_dag , exposure="river" , outcome="crop_damage" , type="canonical")

adjustmentSets( crop_damage_dag , exposure="sd" , outcome="crop_damage" )
adjustmentSets( crop_damage_dag , exposure="sd" , outcome="crop_damage" , type="canonical")

adjustmentSets( crop_damage_dag , exposure="bd" , outcome="crop_damage" )
adjustmentSets( crop_damage_dag , exposure="bd" , outcome="crop_damage" , type="canonical")

adjustmentSets( crop_damage_dag , exposure="months_planted" , outcome="crop_damage" )
adjustmentSets( crop_damage_dag , exposure="months_planted" , outcome="crop_damage" , type="canonical")

adjustmentSets( crop_damage_dag , exposure="see_field" , outcome="crop_damage" )
adjustmentSets( crop_damage_dag , exposure="see_field" , outcome="crop_damage" , type="canonical")

adjustmentSets( crop_damage_dag , exposure="num_protect" , outcome="crop_damage" )
adjustmentSets( crop_damage_dag , exposure="num_protect" , outcome="crop_damage" , type="canonical")

adjustmentSets( crop_damage_dag , exposure="hh_size" , outcome="crop_damage" )
adjustmentSets( crop_damage_dag , exposure="hh_size" , outcome="crop_damage" , type="canonical")

adjustmentSets( crop_damage_dag , exposure="farm_size" , outcome="crop_damage" )
adjustmentSets( crop_damage_dag , exposure="farm_size" , outcome="crop_damage" , type="canonical")

