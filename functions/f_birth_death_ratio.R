
f_birth_death_ratio <- function(variableName) {
  
  ### Country level interpolation ------------------------------
  
  
  national_data <- read_csv(paste0('../results/national_',variableName,'_ratio_interp.csv'))
  
  # macro-region trend
  
  cntry_population_pop <- read_csv( '../results/cntry_pop.csv', col_names = T) 
  # as_tibble() %>% 
  # rename(year = Years) %>% 
  # rename(pop = Population) %>% 
  #select(cntry_code, year, pop) 
  
  cntry_data_interp_t_id <- national_data
  
  # melt national into a long form
  cntry_data_long_interpol <- national_data %>% 
    pivot_longer(!c("Country", "iso3","GADM_code","Country.code"), 
                 names_to = "year", 
                 values_to = 'ratio') 
  
  
  # sub-national interpolation and extrapolation -----------------------------------------------------
  
  subnat_gis <- st_read(paste0('../results/test_',variableName,'_combined.gpkg')) %>% 
    st_drop_geometry(.) %>% 
    as_tibble()
  
  subnat_data <- read_csv(paste0('../results/',variableName,'_ratio_filled.csv')) %>% 
    select(-c(Country,iso3,Subnat,GID_1,NumRate)) %>% 
    select(GID_nmbr,cntry_code,everything())
  
  
  #transposing data
  subnat_data_for_interp <-subnat_data %>% 
    t(.) %>% 
    as_tibble() 
  
  subnat_data_interp <-  subnat_data_for_interp 
  
  # go through each subnat unit
  for (i in 1:ncol(subnat_data_interp)) {
    temp_indicator <- subnat_data_for_interp[3:nrow(subnat_data_for_interp),i] # store the data
    
    # if only one value, then use that
    if(length(temp_indicator[temp_indicator > 0]) == 1) {
      temp_indicator[is.na(temp_indicator)] <- as.numeric( temp_indicator[temp_indicator > 0] )
      temp_indicator_interp <- temp_indicator
    } else {
      # otherwise interpolate; rule = 2 means that no extrapolation, last value will be used
      temp_indicator_interp <-  na.approx(temp_indicator, rule = 2) %>% 
        as_tibble()
    }
    
    
    # store the interpolated value
    subnat_data_interp[3:nrow(subnat_data_for_interp),i] <- temp_indicator_interp
    
  }
  
  subnat_data_interp_t <- subnat_data_interp %>% 
    t(.) %>% 
    as_tibble()
  
  names(subnat_data_interp_t) <- c('GID_nmbr','cntry_code',as.character( timestep))
  
  
  # melt dataframe into a long form
  subnat_data_interp_ID_long <- subnat_data_interp_t %>% 
    pivot_longer(!c("GID_nmbr", "cntry_code"), 
                 names_to = "year", 
                 values_to = 'ratio') 
  
  # # bring subnat population data
  # subnat_data_pop <- left_join(HDI_data, subnat_gis, by = c("GDLCODE")) %>%  #join region ids with gdp values
  #   as_tibble() %>% 
  #   filter(level == 'Subnat') %>% 
  #   select(GDLid,year,pop) 
  
  # combine pop with interpolated data
  
  if(variableName == "births") {
    subnat_pop <- read_csv("../results/test_births_combined_pop_ratio_v2.csv") %>% 
      select(-c(Country,iso3,Subnat,GID_1,NumRate,paste0("ratio",timestep),pop2020)) %>% 
      select(GID_nmbr,cntry_code,everything())
  } else {
    subnat_pop <- read_csv("../results/test_deaths_combined_pop_ratio_v2.csv") %>% 
      select(-c(Country,iso3,Subnat,GID_1,NumRate,paste0("ratio",timestep),pop2020)) %>% 
      select(GID_nmbr,cntry_code,everything()) 
  }
  
  names(subnat_pop) <- c("GID_nmbr","cntry_code",paste0(timestep))
  
  subnat_population_long <- subnat_pop %>% 
    pivot_longer(!c("GID_nmbr", "cntry_code"), 
                 names_to = "year", 
                 values_to = 'pop')
  
  
  subnat_data_interp_ID_long_pop <- left_join(subnat_data_interp_ID_long, subnat_population_long) %>% 
    mutate(weightedVar = ratio *pop)
  
  # calculate country values from subnat data
  cntry_values_from_subnat_data <- subnat_data_interp_ID_long_pop %>% 
    drop_na() %>%
    group_by(cntry_code, year) %>% 
    summarise(sumWeightedVar = sum(weightedVar), pop = sum(pop)) %>% 
    mutate(cntryVar = sumWeightedVar / pop) %>% 
    ungroup()
  
  
  subnat_long_comb_cntry <- left_join(cntry_values_from_subnat_data, subnat_data_interp_ID_long_pop, by = c('cntry_code','year')) %>% 
    mutate(ratioVar = ratio/cntryVar) %>% 
    dplyr::select(c('cntry_code','GID_nmbr','year','ratioVar'))
  
  
  #casting data into a data-frame
  
  subnat_data_for_ratio_calc <- subnat_long_comb_cntry %>% 
    #mutate(rn = rowid(GID_nmbr,cntry_code, year)) %>%  # to avoid error
    pivot_wider(names_from = year, values_from = ratioVar, values_fill = list(Number = '0')) 
  #select(-rn) %>% 
  
  # transpose
  
  subnat_data_for_ratio_calc.t <- subnat_data_for_ratio_calc %>%
    tibble::rownames_to_column() %>%  
    pivot_longer(-rowname) %>% 
    pivot_wider(names_from=rowname, values_from=value) %>% 
    select(-name)
  
  # subnat_data_for_ratio_calc <- subnat_data_for_ratio_calc %>% 
  #   t(.)
  
  
  subnat_data_from_ratio <-  subnat_data_for_ratio_calc.t 
  
  # go through each country
  for (i in 1:ncol(subnat_data_for_ratio_calc.t)) {
    temp_ratio <- subnat_data_for_ratio_calc.t[3:nrow(subnat_data_for_ratio_calc.t),i] # store the ratio
    
    #find cntry id for the subnational unit
    cntry_code =  as.numeric(subnat_data_for_ratio_calc.t[1,i])
    # get national GDP for that country
    tempCntryVar <- filter(cntry_data_interp_t_id, GADM_code ==  !!cntry_code ) %>% 
      dplyr::select(as.character( c(timestep))) %>% 
      t(.) %>% 
      as_tibble()
    
    
    
    if(length(tempCntryVar) == 0){ # if no country data, then we use the subnat data as such
      # subnational id
      subnat_id =  as.numeric(subnat_data_from_ratio[2,i])
      tempSubnatVar <- filter(subnat_data_interp_t,GID_nmbr ==  !!subnat_id) %>% 
        dplyr::select(as.character( c(timestep))) %>% 
        t(.) %>% 
        as_tibble()
      subnat_data_from_ratio[3:nrow(subnat_data_from_ratio),i] <- tempSubnatVar
      
    } else { # if country data exist
      subnat_data_from_ratio[3:nrow(subnat_data_from_ratio),i] <- tempCntryVar * temp_ratio
    }
  }
  
  subnat_data_from_ratio_t <- subnat_data_from_ratio %>% 
    t(.) %>% 
    as_tibble()
  
  names(subnat_data_from_ratio_t) <- c('GADM_code','GID_nmbr',as.character( c(timestep)))
  
  subnat_data_from_ratio_t_id <- right_join( subnat_gis,subnat_data_from_ratio_t) %>%
    as_tibble() %>%
    dplyr::select(c('Country','iso3','GADM_code','Subnat', 'GID_1', 'GID_nmbr',as.character( c(timestep)) ))
  
  subnat_data_long_interpol <- subnat_data_from_ratio_t_id %>% 
    pivot_longer(!c('Country','iso3','GADM_code','Subnat', 'GID_1', 'GID_nmbr'), 
                 names_to = "year", 
                 values_to = 'ratio') 
  
  
  #### combine subnational and national data -----------------------------------------------------
  
  comb_data <- cntry_data_long_interpol %>% 
    mutate(GID_nmbr = GADM_code) %>%  # for country level, GDLid is equal to cntry_code
    mutate(Subnat = NA) %>% 
    mutate(GID_1 = NA) %>% 
    #rename(ratio = varName) %>% 
    select(-Country.code) %>% 
    select(Country, iso3, GADM_code, Subnat, GID_1, GID_nmbr, everything()) %>% 
    rbind(.,subnat_data_long_interpol) %>% # combine cntry and subnational data
    mutate(across(ratio, \(x) round(x, 3)))
    #mutate(across(ratio, round, 3)) # round the variable columnt to three digits
  
  write_csv(comb_data,paste0('../results/subnat_cntry_ratio_',variableName,'.csv'))
  
  
  ### put data to raster -----------------------------------------------------
  #HDI_data_id <- left_join(HDI_data,GDLids, by = c("GDLCODE" = "value"))
  
  adminBoundary_raster_5arcmin <- rast(paste0('../results/adminBoundary_',variableName,'_raster_5arcmin.tif'))
  st_ratio <-  adminBoundary_raster_5arcmin
  iYear = 1990
  for (iYear in timestep) {
    temp <-  comb_data %>% 
      filter(year == iYear) %>% 
      dplyr::select(c(ratio,'GID_nmbr'))
    temp_id <-  as.numeric(temp$GID_nmbr)
    temp_v <- as.numeric(temp$ratio)
    
    # reclassify
    temp_raster <- classify(adminBoundary_raster_5arcmin,
                            cbind(temp_id, temp_v))
    
    # countries without data to NA
    temp_raster[temp_raster>max(temp_v)] <- NA
    
    st_ratio <- c(st_ratio,temp_raster)
  } 
  
  # remove first layer with ids
  st_ratio_data <- subset(st_ratio,2:nlyr(st_ratio))
  names(st_ratio_data) <- paste0(variableName,timestep)
  
  writeRaster(st_ratio_data,paste0('../results/ratioRaster_',variableName,'.tif'),  gdal="COMPRESS=LZW",overwrite=TRUE)
  
  return(st_ratio_data)
}