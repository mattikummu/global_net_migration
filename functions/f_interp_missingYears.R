f_interp_missingYears <- function(inRast = avgAgeColl_sub, nameInd = 'avgAge') {
  inRast <- extend(inRast,ref_rast)
  
  ext(inRast) <- ext(ref_rast)
  
  r_stack <- c(inRast[[1]],rep(ref_rast,4),
               inRast[[2]],rep(ref_rast,4),
               inRast[[3]],rep(ref_rast,4),
               inRast[[4]],rep(ref_rast,4),
               inRast[[5]])
  
  
  names(r_stack) <- paste0(nameInd,seq(2000,2020,1))
  
  ## interpolate missing years
  
  r_stack_interp <- terra::approximate(r_stack, method="linear")
  #plot(r_stack_interp[[6]])
  r_stack_interp[r_stack_interp==0] = NA
  
  return(r_stack_interp)
  
} 