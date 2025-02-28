
myFun_avgAge80plus <-function(xlsWithPopByAge){
  
  popByAgeFemale <- read.xlsx(xlsWithPopByAge,"ESTIMATES", colNames = T) %>%
    # unselect non-wanted columns
    select(-c(Index, Variant,Notes,`0-4`,`5-9`,`10-14`,`15-19`,`20-24`, `25-29`, `30-34`, `35-39`, 
              `40-44`, `45-49`, `50-54`, `55-59`, `60-64`, `65-69`, `70-74`, `75-79`)) %>%
    # rename columns
    rename(age80 = `80-84`,age85 = `85-89`,age90 = `90-94`,age95 = `95-99`,age100 = `100+` ) %>% 
    # calculate population weighted age for each age group
    mutate(ageWeightedPop80_100 = 82.5 * as.numeric(age80)  + 87.5 * as.numeric(age85) + 92.5 * as.numeric(age90) + 97.5*as.numeric( age95) + 102.5 * as.numeric(age100)) %>% 
    # calculate total pop for age 80+
    mutate(totalPop80_100 = as.numeric(age80)  + as.numeric(age85) + as.numeric(age90) + as.numeric( age95) + as.numeric(age100)) %>% 
    # calculate average age for age 80+
    mutate(avgAge80_100 = ageWeightedPop80_100/totalPop80_100) %>% 
    #unselect non-wanted columns
    select(-c(age80, age85, age90, age95, age100, ageWeightedPop80_100, totalPop80_100)) %>% 
    as_tibble() %>%
    # select only country data or world data
    filter(Type == 'Country/Area' | Type == 'World' )
  
  popByAgeFemaleWorld_wide <- popByAgeFemale %>% 
    pivot_wider(names_from = "Year", values_from = 'avgAge80_100') %>% 
    # add iso3 code
    left_join(.,cntryUNmeta,by=c('cntry_id' = 'UN_id')) %>% 
    mutate(Type = ifelse(is.na(Type), 'Country/Area',Type)) %>% 
    mutate(iso3 = ifelse(Type == 'World', 'WWW', iso3)) %>% 
    filter(Type == 'World') %>% 
    select(c(Region, cntry_id, iso3,Type,'2000','2005','2010','2015','2020')) 
  
  popByAgeFemale_wide <- popByAgeFemale %>% 
    pivot_wider(names_from = "Year", values_from = 'avgAge80_100') %>% 
    # add iso3 code
    right_join(.,cntryUNmeta,by=c('cntry_id' = 'UN_id')) %>% 
    mutate(Type = ifelse(is.na(Type), 'Country/Area',Type)) %>% 
    mutate(iso3 = ifelse(Type == 'World', 'WWW', iso3)) %>% 
    # filter out the countries where no iso3 is available
    filter(!is.na(iso3)) %>% 
    select(c(Region, cntry_id, iso3,Type,'2000','2005','2010','2015','2020')) %>%
    # add GADM id
    right_join(.,cntryID)
  
  
  # use world average for countries where no data available
  popByAgeFemale_wide_NA <- popByAgeFemale_wide %>% 
    filter(is.na(Region)) 
  
  popByAgeFemale_wide_NA[,5:9] = popByAgeFemaleWorld_wide[,5:9]
  
  popByAgeFemale_wide_filled <- popByAgeFemale_wide %>% 
    filter(!is.na(Region)) %>% 
    rbind(.,popByAgeFemale_wide_NA) 
  
  
  #### put to map 
  
  # unique ids
  temp_id = as.data.frame( unique(cntry_raster_5arcmin))
  # initialise data
  rAvgAgeOver80 <- cntry_raster_5arcmin
  
  # go through each timestep (2000,2005, ..., 2020)
  for (i in 1:5) {
    
    temp_v = as.data.frame(popByAgeFemale_wide_filled[,c(10,i+4)]) # select id and value to which this is replaced
    temp_replace <- temp_id %>% 
      rename(GADM_code = layer) %>% 
      left_join(temp_v) # join id in the raster and the value it should be replaced with
    
    # reclassify raster
    temp_raster <- classify(cntry_raster_5arcmin,
                            temp_replace)
    # store to stack
    rAvgAgeOver80 <- c(rAvgAgeOver80,temp_raster)
  }
  # select only the parts of stack where data is given
  rAvgAgeOver80 <- subset(rAvgAgeOver80,2:6)
  return(rAvgAgeOver80)
}
