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


#check for collider
g1 <- dagitty("dag{X -> Y -> Z}")
stopifnot( isTRUE(!isCollider( g1, "X", "Y", "Z" )) )
g2 <- dagitty("dag{X -> Y <- Z }")
stopifnot( isTRUE(isCollider( g2, "X", "Y", "Z" )) )

##imply all relationships
ls_conf_no_guard <- 
  dagitty('dag {
  C2070 -> Conflict
  BD -> Conflict
  BD <-> road
  C70 -> Conflict
  HHsize -> LSHs
  HHsize -> guards
  LSHs -> Conflict
  LSHs -> guards
  River -> C70
  River -> Conflict
  SD -> BD
  SD -> Conflict
  guards -> Conflict
  slope -> BD
  slope -> Conflict
  slope -> road
}'
          )
plot(ls_conf_no_guard)
###this will tell us what are the minimal things we need to condition on to make inferences about the relationship between items
adjustmentSets( ls_conf_no_guard , exposure="C2070" , outcome="Conflict" , type="canonical") #independent of others should be invariant
adjustmentSets( ls_conf_no_guard , exposure="C2070" , outcome="Conflict" , type="minimal") #independent of others should be invariant

adjustmentSets( ls_conf_no_guard , exposure="BD" , outcome="Conflict" ) #account for SD and slope
adjustmentSets( ls_conf_no_guard , exposure="BD" , outcome="Conflict" , type="canonical") 

adjustmentSets( ls_conf_no_guard , exposure="C70" , outcome="Conflict" ) #account for river
adjustmentSets( ls_conf_no_guard , exposure="C70" , outcome="Conflict", type="canonical" ) #account for river

adjustmentSets( ls_conf_no_guard , exposure="LSHs" , outcome="Conflict" ) #account for HHsize
adjustmentSets( ls_conf_no_guard , exposure="LSHs" , outcome="Conflict" , type="canonical") #account for HHsize

adjustmentSets( ls_conf_no_guard , exposure="River" , outcome="Conflict" )
adjustmentSets( ls_conf_no_guard , exposure="River" , outcome="Conflict" , type="canonical")

adjustmentSets( ls_conf_no_guard , exposure="SD" , outcome="Conflict" )
adjustmentSets( ls_conf_no_guard , exposure="SD" , outcome="Conflict" , type="canonical")

adjustmentSets( ls_conf_no_guard , exposure="slope" , outcome="Conflict" )
adjustmentSets( ls_conf_no_guard , exposure="guards" , outcome="Conflict" )
isCollider(ls_conf_no_guard , "HHsize" , "guards","Conflict") #shows not a collider on path from HHsize to Conflict thru guards
isCollider(ls_conf_no_guard , "HHsize" , "guards","LSHs") #shows guards is collider on path from HHsize to LSH

##to make inference about C2070, 