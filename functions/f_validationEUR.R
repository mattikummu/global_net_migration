f_validationEUR <- function(rastRaster, oecdData, oecdPolyDown, year) {
  
  rastSel <- subset(rastRaster,(year-1999))
  popSel <- subset(population,(year-1999))
  rastSelTot <- rastSel*popSel
  popSel <- popSel*1
  
  # Calculate vector of mean December precipitation amount for each municipality
  tempRastTot <- exact_extract(rastSelTot, oecdPolyDown, 'sum')
  tempPop <- exact_extract(popSel, oecdPolyDown, 'sum')
  
  # tempRastTot <- terra::extract(rast(rastSelTot), vect(oecdPolyDown), 'sum')
  # tempPop <- terra::extract(rast(popSel), vect(oecdPolyDown), 'sum')
  
  
  # join with polygon data
  oecdPolyDown[['valueDown']] <- ( tempRastTot/tempPop )
  oecdPolyDown[['pop']] <- ( tempPop )
  
  ######## join the obsrved data -------------------
  
  nYear <- as.character(year)
  
  cntrySelData <- cntryData %>% 
    dplyr::select(c(iso3,as.character(year))) %>% 
    rename(gdpCntry = as.character(year))
  
  oecdPolyData <- oecdData %>% 
    select(tl3_id, iso3,as.character(year)) %>% 
    rename(valueObs = as.character(year)) %>% 
    right_join(oecdPolyDown) #%>% 
  # # from 2015 to 2017 USD
  # mutate(valueObs = as.numeric(valueObs) * rate_2015 ) %>% 
  #select(-c(name_en )) %>% 
  #filter(valueObs<300000) %>% # remove huge values
  #drop_na() 
  
  oecdPolyData_forRatio <- oecdPolyData  %>% 
    mutate(totObsValue = valueObs * pop) 
  
  cntryValue_fromObs <- oecdPolyData_forRatio %>% 
    group_by(iso3) %>% 
    summarise(sumTotalValue = sum(totObsValue), sumPop = sum(pop)) %>% # total value and population in that subnat area
    mutate(cntryFromSubnatGDP_percapita = sumTotalValue / sumPop) %>% # calculate ratio in that subnat area
    left_join(cntrySelData) %>%  # add ratio
    mutate(GDPratio = gdpCntry / cntryFromSubnatGDP_percapita) %>% # ratio between value based on grid scale and value based on observations
    ungroup()
  
  valueObsScaled <- oecdPolyData_forRatio %>% 
    left_join(cntryValue_fromObs[,c('iso3','GDPratio')]) %>% 
    mutate(valueObs_scaled = GDPratio*valueObs) %>% 
    mutate(!!paste0('valueObs_',nYear) := valueObs_scaled) %>%  # caluclate final value with the ratio
    mutate(!!paste0('valueDown_',nYear) := valueDown) %>%  # caluclate final value with the ratio
    mutate(!!paste0('valueDiff_',nYear) := (valueDown-valueObs_scaled) / valueObs_scaled) %>%  # caluclate final value with the ratio
    select(-c(iso3,valueObs,pop,totObsValue,GDPratio,valueDown)) 
  
  
  return(valueObsScaled)
}