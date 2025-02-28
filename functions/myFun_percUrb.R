myFun_percUrb <- function(r_pop, r_zones, admLevel){
  
  pop <- r_pop[[20]]
  pop_sum <- terra::global(pop, fun = 'sum', na.rm=T)
  
  names(r_zones) <- 'impact_zone'
  
  r_zonal <- terra::zonal(pop, r_zones, fun = sum, na.rm =T) %>% as_tibble() %>% 
    mutate(shr_pop = cntryPop2019 / sum(cntryPop2019)) %>% #as.numeric(pop_sum)) %>% 
    dplyr::select(impact_zone, shr_pop)
  
  names(r_zonal) <- c('impact_zone', paste0('shr_pop','_', admLevel))
  
  return(r_zonal)
  
}