myFunMgrImpactPop <- function(r_adm, r_popData, r_mgrData, nameOutput) {  
  
  #r_adm <- r_gadm_lev0_5arcmin
  adm_popChange <- as_tibble(terra::zonal(subset(r_popData,nlyr(r_popData)),r_adm, fun = sum, na.rm=T)) %>% 
    left_join(as_tibble(terra::zonal(subset(r_popData,1),r_adm, fun = sum, na.rm=T))) %>% 
    mutate(popChange = cntryPop2020 - cntryPop2000) 
  
  # then net-migration over the whole study period
  adm_netMgr <- as_tibble(terra::zonal(r_mgrData,r_adm, fun = sum, na.rm=T)) %>% 
    rowwise() %>% 
    mutate(netMgrSum = sum(c_across(contains("net")))) %>% 
    ungroup() %>% 
    select(1,netMgrSum)
  
  # combine
  adm_popChngMgr <- adm_popChange %>% 
    left_join(adm_netMgr) %>% 
    # calculate population change without migration
    mutate(popChng_woMgr = popChange - netMgrSum) %>% 
    # calculate whether natural growth or migration impacts more on pop change
    mutate(rolePopMgr = 1-popChng_woMgr/popChange) %>% 
    # check whether 
    # migration increases pop growth: 3
    # migration decreases pop growth: 2
    # migration turns pop growth to declined pop: 1
    # migration exalarate pop decline: -3
    # migration slows pop decline: -2
    # migration turns pop decline to growth: -1
    mutate(popMgr_classif = if_else((popChng_woMgr > 0 & netMgrSum > 0), 3,
                                    if_else((popChange > 0 & netMgrSum < 0), 2,
                                            if_else((popChng_woMgr > 0 & popChange < 0), 1,
                                                    if_else((popChng_woMgr < 0 & netMgrSum < 0), -3,
                                                            if_else((popChange < 0 & netMgrSum > 0), -2,
                                                                    if_else((popChng_woMgr < 0 & popChange > 0), -1,#0)))))))
                                                                            0
                                                                    ))))))) 
  
  
  adm_popChngMgr_summary <- adm_popChngMgr %>% 
    group_by(popMgr_classif) %>% 
    summarise_at(vars(contains('cntryPop2020')),list(sum))%>%
    ungroup()
  
  
  # put the classification to a raster
  r_adm_class <- classify(r_adm,
                          cbind(adm_popChngMgr[,1], adm_popChngMgr$popMgr_classif))
  
  r_adm_roleMgrInPopChange <- classify(r_adm,
                                       cbind(adm_popChngMgr[,1], adm_popChngMgr$rolePopMgr))
  #plot(r_adm_class)
  
  writeRaster(r_adm_class,paste0('../results/r_',nameOutput,'.tif'),  gdal="COMPRESS=LZW",overwrite=TRUE)
  return(adm_popChngMgr)
}