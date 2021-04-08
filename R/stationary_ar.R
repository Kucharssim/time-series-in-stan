# check stationarity
is.stationary <- function(ar){
  minroots <- min(Mod(polyroot(c(1, -ar))))
  if (minroots <= 1) 
    return(FALSE)
  else 
    return(TRUE)
}

# generate stationary AR(p) process
stationary_ar <- function(p, mean = rep(0, p), sd = rep(1, p)){
  if(p == 0) return(0)
  
  stationary <- FALSE
  while(!stationary){
    ar <- rnorm(p, mean, sd)
    stationary <- is.stationary(ar)
  }
  
  return(ar)
}
# Author: Simon Kucharsky
