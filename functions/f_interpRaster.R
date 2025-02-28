f_interpRaster <- function(r_in,existingYears,newYears) {
  # r_in <- stack(avgAge)
  # existingYears <- seq(2000,2020,5)
  # nClusters <- 6
  
  # NA to zero
  r_in[is.na(r_in)] <- 0
  
  # int_sN <- 1
  # int_eN <- existingYears[length(existingYears)] - existingYears[1] + 1
  
  #newYears <- existingYears[1]:existingYears[length(existingYears)]
  
  rasInterp <- things::interpolate_linear(stack(r_in),existingYears,newYears,tempdir(), 
                                          'example', return_stack=TRUE,overwrite=TRUE)
  
  
  # f3 <- function(y) approx(existingYears, y, int_sN:int_eN, rule=2)$y
  # 
  # beginCluster(nClusters)
  # rasInterp <- clusterR(r_in, calc, args=list(fun=f3))
  # endCluster()
  
  names(rasInterp) <- paste0('Year',existingYears[1]:existingYears[length(existingYears)]) 
  
  return(rasInterp)
}