myFun_nYrSum <- function(r_in,n_yearSum,maxTime,FUN) {
  # FUN = 'sum'
  # n_yearSum=3
  # r_in <- r_netMigration
  # maxTime = 20
  r_in3yr <- subset(r_in,1)
  for (iTime in 1: (ceiling(nlyr(r_in)/n_yearSum))) {
    # define timesteps
    iSteps <- ((iTime-1)*3+1):(iTime*3)
    iSteps <- iSteps[iSteps<(maxTime+1)] 
    
    if(FUN == "sum"){
      tempIn <- sum(subset(r_in,iSteps),na.rm=T)
    } else {
      tempIn <- mean(subset(r_in,iSteps),na.rm=T)
    }
    r_in3yr <- c(r_in3yr,tempIn)
    
  }
  r_in3yr <- subset(r_in3yr,2: (ceiling(nlyr(r_in)/n_yearSum)+1))
  
}