myFun_mgrZonalStats <- function(r_adm, r_popTotal, r_urbanPop, r_ruralPop, r_mgrTotal, r_mgrUrban, r_mgrRural) { #r_popPos, r_popNeg, r_mgrPos, r_mgrNeg
  #r_admName <- names(r_adm)
  temp <- unique(values(r_adm)) %>% sort() %>% as_tibble %>% drop_na() %>% rename("admZone" = "value") #%>% select(cntry_raster_masked) %>% rename("country_id" = "cntry_raster_masked")
  for (i in 1:length(names(r_mgrTotal))){
    pop_temp <- terra::zonal(r_popTotal[[i]], r_adm, fun = sum, na.rm = T) %>% rename_at(2, ~ gsub("^.*?Pop","totPop_", .x),"_")
    pop_urban <- terra::zonal(r_urbanPop[[i]], r_adm, fun = sum, na.rm = T) %>% rename_at(2, ~gsub("^.*?Pop","UrbanPop_", .x),"_")
    pop_rural <- terra::zonal(r_ruralPop[[i]], r_adm, fun = sum, na.rm = T) %>% rename_at(2, ~ gsub("^.*?Pop","RuralPop_", .x),"_")
    #pos_pop <- terra::zonal(r_popPos[[i]], r_adm, fun = sum, na.rm =T) %>% rename_at(2, ~ gsub("^.*?Pop", "PosPop_", .x), "_")
    #neg_pop <- terra::zonal(r_popNeg[[i]], r_adm, fun = sum, na.rm =T) %>% rename_at(2, ~ gsub("^.*?Pop", "NegPop_", .x), "_")
    net_migr <- terra::zonal(r_mgrTotal[[i]], r_adm, fun = sum, na.rm = T)  %>% rename_at(2, ~ gsub("^.*?netMgr","TotMgr_", .x),"_")
    net_urban <- terra::zonal(r_mgrUrban[[i]], r_adm, fun = sum, na.rm =T) %>% rename_at(2, ~ gsub("^.*?netMgr", "UrbanMgr_", .x), "_")
    net_rural <- terra::zonal(r_mgrRural[[i]], r_adm, fun = sum, na.rm =T) %>% rename_at(2, ~ gsub("^.*?netMgr", "RuralMgr_", .x), "_")
    #pos_migr <- terra::zonal(r_mgrPos[[i]], r_adm, fun = sum, na.rm =T) %>% rename_at(2, ~ gsub("^.*?netMgr", "PosMgr_", .x), "_")
    #neg_migr <- terra::zonal(r_mgrNeg[[i]], r_adm, fun = sum, na.rm =T) %>% rename_at(2, ~ gsub("^.*?netMgr", "NegMgr_", .x), "_")
    
    temp <- bind_cols(temp, pop_temp[2],pop_urban[2],pop_rural[2],  net_migr[2], net_urban[2], net_rural[2]) #pos_pop[2], neg_pop[2], pos_migr[2], neg_migr[2]
  }
  
  zonalStatsMgr <- temp %>% 
    pivot_longer(!admZone, names_to = "variable", values_to = "value") %>% 
    mutate(year = variable) %>% 
    mutate(split = str_split(year, "_")) %>% 
    mutate(variable = sapply(split, "[[", 1),
           year = sapply(split, "[[", 2)) %>% 
    dplyr::select(-split) %>% 
    mutate(year = as.numeric(year))
  
  return(zonalStatsMgr)
}