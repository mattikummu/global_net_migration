myFun_migrStats <- function(r_migr_df){ #r_adm_sf, r_cntry_sf, filter_var_vec, mybreaks, titleVar, colorpal, titleLabel
  mydata <- r_migr_df %>% 
    #filter(variable %in% c("cntryPopTot","cntryUrbanPop", "cntryRuralPop", "netMgr","netUrban","netRural")) %>% 
    pivot_wider(names_from = variable, values_from = value) %>% 
    group_by(admZone, year) %>% 
    # Calculate NM per population
    summarise(totNMpp = TotMgr/totPop, #total net-migraiton per pop
              urbanNMpp = (UrbanMgr/UrbanPop)*1000, #total urban net-migr per urban pop
              ruralNMpp = (RuralMgr/RuralPop)*1000) %>%  #total rural net-migr per rural pop
    # Then calculate cumulative sum over the time period
    summarise(totSumPp = sum(totNMpp, na.rm=T),
              urbanSumPp = sum(urbanNMpp, na.rm=T),
              ruralSumPp = sum(ruralNMpp, na.rm=T)) %>% 
    pivot_longer(!admZone, names_to = "variable", values_to = "value")
  
  return(mydata)
  
}
