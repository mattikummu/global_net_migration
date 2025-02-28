myFun_interpolateCntryValues <- function(variable, dataPP, dataWB) {
  
  #### data from UN PP -----
  
  ratioNational_PP <- myFun_nationalData(dataPP)
  names(ratioNational_PP) = c("Country","Country.code","Type",'1987','1992','1997','2002','2007','2012','2017')
  
  ratioNational_PP_ip <- f_interpolate_years(ratioNational_PP, 4) %>% 
    select(-c(paste0(1987:1989), Type)) %>% 
    left_join(.,cntryID,by=c('Country.code' = 'cntry_code')) %>% 
    drop_na() %>% 
    arrange(GADM_code) %>% 
    select(Country,iso3,GADM_code,everything())
  
  
  #### data from world bank -----
  
  
  ratioNational_WB <- myFun_nationalDataWB(dataWB)
  names(ratioNational_WB) = c("Country","iso3",paste0(1990:2019))
  
  ratioNational_WB_ip <- f_interpolate_yearsWB(ratioNational_WB,3) %>% 
    # remove rows without any data
    rowwise() %>% mutate(sumVar = sum(c_across('1990':'2019'),na.rm=T)) %>% 
    filter(sumVar != 0) %>% 
    select(-sumVar)%>% 
    mutate(iso3 = ifelse(iso3=="XKX","XKO",iso3)) %>% # correct iso3 for Kosovo
    left_join(.,cntryID) %>% 
    arrange(GADM_code) %>% 
    select(Country,iso3,GADM_code,everything())
  
  
  
  # combine the data --------------------------------------------------------
  
  # check which data is missing from world bank data
  
  gadm_countries <- read_sf( "../data_in_gpkg/gadm_level0.gpkg" ) %>% 
    st_drop_geometry() %>% 
    rename(iso3 = GID_0) %>% 
    left_join(cntryID) %>% 
    filter(iso3 != "ALA") # remove Aland
  
  
  countries_missing_WB <- gadm_countries %>% 
    filter(!iso3 %in% ratioNational_WB_ip$iso3)
  
  ratio_missing_WB_but_in_PP <- ratioNational_PP_ip %>% 
    filter(iso3 %in% countries_missing_WB$iso3)
  
  ratioNational_combined <- ratioNational_WB_ip %>% 
    bind_rows(ratio_missing_WB_but_in_PP) %>% 
    select(-c(cntry_code,Country.code ))%>% 
    filter(iso3 %in% cntryID$iso3) # select only countries, exlude regions
  
  countries_missing_combined <- gadm_countries %>% 
    filter(!iso3 %in% ratioNational_combined$iso3) %>% 
    rename(Country=NAME_0) %>% 
    select(Country,iso3,GADM_code)
  
  write_csv(countries_missing_combined, "../results/countries_missing_cntryData.csv")
  
  ratioNational_missing <- countries_missing_combined %>% 
    filter(!is.na(GADM_code))
  # add year columns  
  ratioNational_missing[paste0(timestep)] = NA
  
  # add missing countries
  ratioNational_combined <-  ratioNational_combined %>% 
    bind_rows(ratioNational_missing)
  
  
  # extrapolate with regional values ----------------------------------------
  
  cntry_population <- read_csv( '../results/cntry_pop.csv', col_names = T) %>% 
    select(-pop2020, -ID)
  
  names(cntry_population) <- c('iso3',paste0(1990:2019)) 
  
  cntry_population_long <- cntry_population %>% 
    pivot_longer(!c("iso3"), 
                 names_to = "year", 
                 values_to = 'pop') 
  
  
  
  cntry_info <- read_csv("../data_in/countries_codes_and_coordinates.csv") %>% 
    distinct(GADM_code, iso3, cntry_code, .keep_all = T) %>% 
    select(GADM_code,RegionID) 
  
  # check for which countries full dataset exists
  macro_reg_full <- left_join(ratioNational_combined, cntry_info) %>%  #join region ids with gdp values
    drop_na() %>% # remove countries with some holes in data
    pivot_longer(!c("Country","iso3","GADM_code","RegionID"), 
                 names_to = "year", 
                 values_to = 'ratio') 
  
  
  # collect data for those countries that full data exists
  macro_reg <- macro_reg_full %>% 
    left_join(cntry_population_long)
  
  # calculate regional values for selected variable
  macroTrend <- macro_reg %>% 
    mutate(totalIndic = ratio * pop) %>%  # multiply with population, to weight with population
    group_by(RegionID, year) %>%  # group by regions
    #summarise(sumIndic = sum(totalIndic)) %>%  # sum the regional values
    summarise(sumIndic = sum(totalIndic), sumPop = sum(pop)) %>%  # sum the regional values
    ungroup() %>% 
    mutate(weightedMeanIndic = sumIndic / sumPop) %>%  # calculate weighted mean
    rename(ratio = weightedMeanIndic) %>%  
    select(-c(sumIndic, sumPop))
  
  # country data for all countries
  cntry_data <- left_join(ratioNational_combined, cntry_info) %>% 
    select(c(RegionID,GADM_code,paste0(1990:2019)))
  
  
  #interpolate and extrapolate cntry data
  
  temp_cntry_data <- cntry_data %>% 
    t(.) %>% 
    as_tibble()
  
  cntry_data_interp <- temp_cntry_data
  
  #casting macro-regional trend into a data-frame
  macroTrend_wider <- macroTrend %>% 
    pivot_wider(names_from = year, values_from = ratio)
  i=43
  # go through each country
  for (i in 1:ncol(temp_cntry_data)) {
    temp_data <- temp_cntry_data[3:nrow(temp_cntry_data),i] # store the data
    
    # regional values for that country
    reg_id <- temp_cntry_data[1,i] %>% 
      as.numeric()
    temp_macro <- macroTrend_wider[reg_id,2:ncol(macroTrend_wider)]
    
    # if all data is NA, use regional values
    checkNA <- which(!is.na(temp_data))
    if(length(which(!is.na(temp_data))) == 0) {
      cntry_data_interp[3:nrow(cntry_data_interp),i] = t(temp_macro)
      
    } else { # continue to interpolation
      
      # # interpolate
      # v_temp_xout <- seq(min(which(!is.na(temp_data))),max(which(!is.na(temp_data)))) # define the location of where extrapolation should start
      # temp_data_interp <- na.approx(temp_data[v_temp_xout,], na.rm = F) # interpolate, no extrapolation
      # cntry_data_interp[v_temp_xout+2,i] <- temp_data_interp # store back
      # 
      # extrapolate
      
      
      # start tail
      start_tail <- min(which(!is.na(temp_data)))
      if( start_tail == 1) {
        # do nothing
      } else {
        # extrapolate
        temp_start <- seq(1, start_tail)
        temp_macro_start <- temp_macro[temp_start] / as.numeric( temp_macro[start_tail] )
        temp_cntry_indic <- as.numeric( temp_data[start_tail,] ) *temp_macro_start %>% 
          t(.) %>% 
          as_tibble()
        cntry_data_interp[temp_start+2,i] <- temp_cntry_indic
      }
      
      # end tail
      end_tail <- max(which(!is.na(temp_data)))
      if( end_tail == length(timestep)) {
        # do nothing
      } else {
        # extrapolate
        temp_end <- seq(end_tail,(ncol(macroTrend_wider)-1))
        temp_macro_end <- temp_macro[temp_end] / as.numeric( temp_macro[end_tail] ) 
        temp_cntry_indic <- as.numeric( temp_data[end_tail,] ) *temp_macro_end %>% 
          t(.) %>% 
          as_tibble()
        cntry_data_interp[temp_end+2,i] <- temp_cntry_indic
      }
    }
  }
  
  cntry_data_interp_t <- cntry_data_interp %>% 
    t(.) %>% 
    as_tibble()
  
  names(cntry_data_interp_t) <- c('RegionID','GADM_code',as.character(timestep))
  
  cntry_names <- read_csv("../data_in/countries_codes_and_coordinates.csv") %>% 
    select(GADM_code,Country)
  
  cntry_data_interp_t_id <- left_join(cntry_data_interp_t,cntryID) %>% 
    rename(Country.code = cntry_code) %>% 
    left_join(cntry_names) %>% 
    select(Country,iso3,GADM_code,Country.code,as.character(timestep))
  
  
  
  
  
  
  return(cntry_data_interp_t_id)
}