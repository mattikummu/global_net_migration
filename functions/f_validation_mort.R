f_validation_mort <- function(rastRaster, year) {
  
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
    rename(ValueCntry = as.character(year))
  
  oecdPolyData <- oecdData %>% 
    select(tl3_id, iso3,as.character(year)) %>% 
    rename(valueObs = as.character(year)) %>% 
    right_join(oecdPolyDown) %>% 
    # # from 2015 to 2017 USD
    # mutate(valueObs = as.numeric(valueObs) * rate_2015 ) %>% 
    select(-c(name_en )) %>% 
    #filter(valueObs<300000) %>% # remove huge values
    drop_na() 
  
  oecdPolyData_forRatio <- oecdPolyData  %>% 
    mutate(totObsValue = valueObs * pop) %>% 
    mutate(totDownValue = valueDown * pop)
  
  cntryValue_fromObs <- oecdPolyData_forRatio %>% 
    group_by(iso3) %>% 
    # total value and population in that subnat area
    summarise(sumTotalValue = sum(totObsValue), sumTotalValueDown = sum(totDownValue), sumPop = sum(pop)) %>% 
    mutate(cntryFromSubnatValue = sumTotalValue / sumPop) %>% # calculate ratio in that subnat area
    mutate(cntryFromSubnatValueDown = sumTotalValueDown / sumPop) %>% # calculate ratio in that subnat area
    
    left_join(cntrySelData) %>%  # add ratio
    mutate(ValueRatioObs = ValueCntry / cntryFromSubnatValue) %>%  # ratio between value based on grid scale and value based on observations
    mutate(ValueRatioDown = ValueCntry / cntryFromSubnatValueDown) # ratio between value based on grid scale and value based on observations
  
  
  
  valueObsScaled <- oecdPolyData_forRatio %>% 
    left_join(cntryValue_fromObs[,c('iso3','ValueRatioObs','ValueRatioDown')]) %>% 
    mutate(valueObs_scaled = ValueRatioObs*valueObs) %>% 
    mutate(valueDown_scaled = ValueRatioDown*valueDown) %>% 
    mutate(!!paste0('valueObs_',nYear) := valueObs_scaled) %>%  # caluclate final value with the ratio
    mutate(!!paste0('valueDown_',nYear) := valueDown_scaled) %>%  # caluclate final value with the ratio
    mutate(!!paste0('valueDiff_',nYear) := (valueDown-valueObs_scaled) / valueObs_scaled) %>%  # caluclate final value with the ratio
    select(-c(iso3,valueObs,totObsValue,ValueRatioObs,ValueRatioDown)) #%>% 
  
  
  return(valueObsScaled)
}