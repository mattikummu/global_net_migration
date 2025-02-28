f_validation <- function(population, year) {
  
  popSel <- subset(population,(year-1999))
  popSel <- popSel*1
  
  # Calculate vector of mean December precipitation amount for each municipality
  tempPop <- exact_extract(popSel, oecdPolyDown, 'sum')
  
  # tempRastTot <- terra::extract(rast(rastSelTot), vect(oecdPolyDown), 'sum')
  # tempPop <- terra::extract(rast(popSel), vect(oecdPolyDown), 'sum')
  
  
  # join with polygon data
  oecdPolyDown[['pop']] <- ( tempPop )
  
  ######## join the obsrved data -------------------
  
  nYear <- as.character(year)
  
  cntrySelData <- cntryData %>% 
    dplyr::select(c(iso3,as.character(year))) %>% 
    rename(valueCntry = as.character(year))
  
  oecdPolyData <- oecdData %>% 
    select(tl3_id, iso3,as.character(year)) %>% 
    rename(valueObs = as.character(year)) %>% 
    right_join(oecdPolyDown) %>% 
    # # from 2015 to 2017 USD
    # mutate(valueObs = as.numeric(valueObs) * rate_2015 ) %>% 
    select(-c(name_en )) %>% 
    #filter(valueObs<300000) %>% # remove huge values
    drop_na() 
  
  oecdPolyData_forRatio <- oecdPolyData  
  
  cntryValue_fromObs <- oecdPolyData_forRatio %>% 
    group_by(iso3) %>% 
    summarise(sumPop = sum(pop)) %>% # total value and population in that subnat area
    #mutate(cntryFromSubnatValue = sumTotalValue / sumPop) %>% # calculate ratio in that subnat area
    left_join(cntrySelData) %>%  # add ratio
    mutate(ValueRatio = valueCntry / sumPop) # ratio between value based on grid scale and value based on observations
  
  valueObsScaled <- oecdPolyData_forRatio %>% 
    left_join(cntryValue_fromObs[,c('iso3','ValueRatio')]) %>% 
    mutate(valueObs_scaled = ValueRatio*valueObs) %>% 
    mutate(!!paste0('valueObs_',nYear) := valueObs_scaled) %>%  # caluclate final value with the ratio
    mutate(!!paste0('valueDown_',nYear) := pop) %>%  # caluclate final value with the ratio
    mutate(!!paste0('gdpDiff_',nYear) := (pop-valueObs_scaled) / valueObs_scaled) %>%  # caluclate final value with the ratio
    select(-c(iso3,valueObs,pop,ValueRatio)) #%>% 
  
  
  return(valueObsScaled)
}