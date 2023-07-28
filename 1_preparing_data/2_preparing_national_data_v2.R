### preparing migration data - national data
library(sf)
library(raster)
library(terra)
library(fasterize)
library(exactextractr)
library(Rfast)

library(openxlsx) #
library(readxl)
library(tidyverse)
library(dplyr) 
library(zoo)


# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# set time steps
timestep <- c(seq(1990, 2019))
step <- c(seq(1,length(timestep)))

#### load data -----


cntryID <- read_csv("data_in/countries_codes_and_coordinates.csv") %>% 
  select(GADM_code,iso3,cntry_code)



# national population -----------------------------------------------------


national_gis <- read_sf( "data_in/gadm_level0.gpkg" )
temp <- national_gis %>% 
  st_drop_geometry()

popHyde_1990_2020 <- rast("data_in/pop_1990_2020_hyde32.tif")
popWorldPop_2000_2020 <- rast("data_in/ppp_2000_2020_5arcmin_corrected_fixed.tif")

#names(subnat_gis_combined) <- c("Country", "iso3","cntry_code","Subnat","GID_1","GID_nmbr","NumRate",paste0("ratio",1990:2019),"geom")

# 
# # use HYDE to extrapolate WorldPop to 1990-2000 
# 
# # project to same extent
# popHyde_1990_2020_proj <- terra::project(popHyde_1990_2020,subset(popWorldPop_2000_2020,1))
# 
# deltaPop <- subset(popWorldPop_2000_2020,1) / subset(popHyde_1990_2020_proj,11) 
# 
# # in case of zero population in popHyde, keep delta as 1 (i.e. no change)
# deltaPop[is.infinite(deltaPop)] <- 1
# 
# popWorldPop_1990_2020 <- c(deltaPop * subset(popHyde_1990_2020_proj,1:10), popWorldPop_2000_2020)
# 
# # global(subset(popWorldPop_1990_2020,5),"sum", na.rm=TRUE)
# # global(subset(popHyde_1990_2020_proj,5) ,"sum", na.rm=TRUE)
# 

popWorldPop_1990_2020 <- rast('results/popRaster_1990_2020.tif')

temp_nat_pop <- terra::extract(popWorldPop_1990_2020,vect(national_gis),'sum') %>%
  as_tibble()


colnames(temp_nat_pop) <- c('ID',paste0("pop",as.character(1990:2020)))



# create population table
national_pop <- national_gis %>% 
  select(GID_0) %>% 
  st_drop_geometry() %>% 
  bind_cols(temp_nat_pop) %>% 
  select(GID_0,everything()) 

write_csv(national_pop, "results/cntry_pop.csv")



#  functions------------------------------------------

# functions for data from population prospects

myFun_nationalData <-function(xlsIn){
  
  origData <- read.xlsx(xlsIn,"ESTIMATES", colNames = T, startRow = 17) %>%
    # unselect non-wanted columns
    select(-c(Index, Variant,Notes,Parent.code,paste0(seq(1950,1980,5),"-",seq(1955,1985,5)))) %>%
    
    as_tibble() %>%
    
    filter(Type == 'Country/Area')
  
}
# dataIn = data frame of births/deaths with startCol meta columns and then rest
# of the columns yearly values, years as column headers
interpolate_years <- function(dataIn, startCol) {
  
  #dataIn=birthsNational
  #startCol=4
  dataCols <- startCol:ncol(dataIn)
  cNames <- colnames(dataIn)[dataCols]
  
  # values to numeric
  dataIn <- dataIn %>%
    mutate(across(all_of(dataCols), ~ as.numeric(.x))) 
  
  # interpolate chunks one by one
  for (i in 1:(length(dataCols) - 1)) {
    
    ystart <- cNames[i]
    yend <- cNames[i+1]
    tsLength <- as.numeric(yend) - as.numeric(ystart)
    
    # compute linear change per year
    ipData <- dataIn %>%
      select(Country, Country.code, Type, dataCols[i], dataCols[i+1]) %>%
      mutate(deltaTotal = !!sym(yend) - !!sym(ystart),
             deltaYear = deltaTotal / tsLength)
    
    # interpolate
    for (y in 1:(tsLength - 1)) {
      
      ipCol <- as.character(as.numeric(ystart) + y)
      ipData <- ipData %>%
        mutate(!!ipCol := !!sym(ystart) + y * deltaYear)
      
    }
    
    if(i == (length(dataCols) - 1)) {
      # extrapolate two years 
      ipData <- ipData %>%
        mutate('2018' := !!sym(ystart) + 6 * deltaYear) %>% 
        mutate('2019' := !!sym(ystart) + 7 * deltaYear)
      
    } else{}
    
    # clean up and join back
    ipData <- ipData %>%
      select(-c(deltaTotal, deltaYear, !!sym(ystart), !!sym(yend)))
    dataIn <- dataIn %>%
      left_join(ipData, by = c("Country", "Country.code", "Type"))
    
  }
  
  # fix ordering; first meta, then years ordered
  dataIn <- dataIn %>%
    select(order(colnames(.))) %>%
    select(Country, Country.code, Type, everything())
  
  return (dataIn)
  
}

# functions for data from world bank
myFun_nationalDataWB <-function(xlsIn){
  #xlsIn <- "data_in/API_SP.DYN.CBRT.IN_DS2_en_excel_v2_2595037.xlsx"
  origData <- read.xlsx(xlsIn,"Data", colNames = T, startRow = 4) %>%
    # unselect non-wanted columns
    select(-c(Indicator.Name, Indicator.Name,Indicator.Code,paste0(c(1960:1989,2020)))) %>%
    
    as_tibble() 
  
  #names(origData) = c("Country","Country.code","Type",'1992','1997','2002','2007','2012','2017')
}
# dataIn = data frame of births/deaths with startCol meta columns and then rest
# of the columns yearly values, years as column headers
interpolate_yearsWB <- function(dataIn, startCol) {
  
  #dataIn=birthsNational
  #startCol=3
  dataCols <- startCol:ncol(dataIn)
  cNames <- colnames(dataIn)[dataCols]
  
  # values to numeric
  dataIn <- dataIn %>%
    mutate(across(all_of(dataCols), ~ as.numeric(.x))) 
  
  # interpolate chunks one by one
  for (i in 1:nrow(dataIn)) {
    ipData <- as.numeric(dataIn[i,dataCols] )
    
    if(is.na(sum(ipData,na.rm = T))) {
      # do nothing
      ipData_interp <-  ipData
    }else{
      ipData_interp <- zoo::na.approx(ipData,na.rm=F)
    }
    
    dataIn[i,dataCols] <- t(as_tibble(as.numeric(ipData_interp)))
  }
  
  return (dataIn)
}


#### annual values for births and deaths -----
# variable <- "births" / "deaths"
# dataPP <- "data_in/WPP2019_FERT_F03_CRUDE_BIRTH_RATE.xlsx" / 
# dataWB <- "data_in/API_SP.DYN.CBRT.IN_DS2_en_excel_v2_2595037.xlsx" / 

myFun_interpolateCntryValues <- function(variable, dataPP, dataWB) {
  
  #### data from UN PP -----
  
  ratioNational_PP <- myFun_nationalData(dataPP)
  names(ratioNational_PP) = c("Country","Country.code","Type",'1987','1992','1997','2002','2007','2012','2017')
  
  ratioNational_PP_ip <- interpolate_years(ratioNational_PP, 4) %>% 
    select(-c(paste0(1987:1989), Type)) %>% 
    left_join(.,cntryID,by=c('Country.code' = 'cntry_code')) %>% 
    drop_na() %>% 
    arrange(GADM_code) %>% 
    select(Country,iso3,GADM_code,everything())
  
  
  #### data from world bank -----
  
  
  ratioNational_WB <- myFun_nationalDataWB(dataWB)
  names(ratioNational_WB) = c("Country","iso3",paste0(1990:2019))
  
  ratioNational_WB_ip <- interpolate_yearsWB(ratioNational_WB,3) %>% 
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
  
  gadm_countries <- read_sf( "data_in/gadm_level0.gpkg" ) %>% 
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
  
  ratioNational_missing <- countries_missing_combined %>% 
    filter(!is.na(GADM_code))
  # add year columns  
  ratioNational_missing[paste0(timestep)] = NA
  
  # add missing countries
  ratioNational_combined <-  ratioNational_combined %>% 
    bind_rows(ratioNational_missing)
  
  
  # extrapolate with regional values ----------------------------------------
  
  cntry_population <- read_csv( 'results/cntry_pop.csv', col_names = T) %>% 
    select(-pop2020)
  
  names(cntry_population) <- c('iso3',paste0(1990:2019))
  
  cntry_population_long <- cntry_population %>% 
    pivot_longer(!c("iso3"), 
                 names_to = "year", 
                 values_to = 'pop') 
  
  
  
  cntry_info <- read_csv("data_in/countries_codes_and_coordinates.csv") %>% 
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
  
  cntry_names <- read_csv("data_in/countries_codes_and_coordinates.csv") %>% 
    select(GADM_code,Country)
  
  cntry_data_interp_t_id <- left_join(cntry_data_interp_t,cntryID) %>% 
    rename(Country.code = cntry_code) %>% 
    left_join(cntry_names) %>% 
    select(Country,iso3,GADM_code,Country.code,as.character(timestep))
  
  
  
  
  
  
  return(cntry_data_interp_t_id)
}



# apply function for births and deaths ------------------------------------


births_data_interp_t_id <- myFun_interpolateCntryValues("births",
                                                        "data_in/WPP2019_FERT_F03_CRUDE_BIRTH_RATE.xlsx",
                                                        "data_in/API_SP.DYN.CBRT.IN_DS2_en_excel_v2_2595037.xlsx")
deaths_data_interp_t_id <- myFun_interpolateCntryValues("deaths",
                                                        "data_in/WPP2019_MORT_F02_CRUDE_DEATH_RATE.xlsx",
                                                        "data_in/API_SP.DYN.CDRT.IN_DS2_en_excel_v2_2593323.xlsx")


write_csv(births_data_interp_t_id, "results/national_births_ratio_interp.csv")
write_csv(deaths_data_interp_t_id, "results/national_deaths_ratio_interp.csv")

write_csv(countries_missing_combined, "results/countries_missing_cntryData.csv")


#### population count ----


## data from UN PP --


cntryID <- read_csv("data_in/countries_codes_and_coordinates.csv") %>% 
  #dplyr::select(-cntry_code) %>% 
  #rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  select(GADM_code,iso3,cntry_code)


popNational_PP <- read.xlsx('data_in/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx',"ESTIMATES", colNames = T, startRow = 17) %>%
  # unselect non-wanted columns
  select(-c(Index, Variant,Notes,Parent.code,paste0(seq(1950,1989,1)))) %>%
  
  as_tibble() %>%
  
  filter(Type == 'Country/Area')

# multiply by 1000
popNational_PP[paste0(seq(1990,2020,1))] = popNational_PP[paste0(seq(1990,2020,1))] * 1000

names(popNational_PP) = c("Country","Country.code","Type",paste0(1990:2020))

popNational_PP_ip <- popNational_PP %>% 
  select(-c(Type)) %>% 
  left_join(.,cntryID,by=c('Country.code' = 'cntry_code')) %>% 
  drop_na() %>% 
  arrange(GADM_code) %>% 
  select(Country,iso3,GADM_code,everything())


## data from world bank --

#  for data from world bank
#xlsIn <- "data_in/API_SP.DYN.CBRT.IN_DS2_en_excel_v2_2595037.xlsx"
popNational_WB <- read.xlsx('data_in/Data_Extract_From_Population_estimates_and_projections.xlsx')  %>% #,"Data", colNames = T, startRow = 1) %>%
  # unselect non-wanted columns
  select(-c(Series.Name, Series.Code,paste0(c(1960:1989,2021:2050),".[YR",c(1960:1989,2021:2050),"]"))) %>%
  
  as_tibble() 

names(popNational_WB) = c("Country","Country.code",paste0(1990:2020))

popNational_WB_ip <- popNational_WB %>% 
  rename(iso3 = Country.code) %>% 
  rowwise() %>% mutate(sumVar = sum(c_across('1990':'2019'),na.rm=T)) %>% 
  filter(sumVar != 0) %>% 
  select(-sumVar)%>% 
  mutate(iso3 = ifelse(iso3=="XKX","XKO",iso3)) %>% # correct iso3 for Kosovo
  left_join(.,cntryID) %>% 
  arrange(GADM_code) %>% 
  select(Country,iso3,GADM_code,everything())




# combine the pop data --------------------------------------------------------

# check which data is missing from world bank data

gadm_countries <- read_sf( "data_in/gadm_level0.gpkg" ) %>% 
  st_drop_geometry() %>% 
  rename(iso3 = GID_0) %>% 
  left_join(cntryID) %>% 
  filter(iso3 != "ALA") # remove Aland

countries_missing_PP <- gadm_countries %>% 
  filter(!iso3 %in% popNational_PP_ip$iso3) 

pop_missing_PP_but_in_WB <- popNational_WB_ip %>% 
  filter(iso3 %in% countries_missing_PP$iso3)

popNational_combined <- popNational_PP_ip %>% 
  bind_rows(pop_missing_PP_but_in_WB) %>% 
  select(-c(cntry_code,Country.code ))%>% 
  filter(iso3 %in% cntryID$iso3) # select only countries, exlude regions

countries_missing_combined <- gadm_countries %>% 
  filter(!iso3 %in% popNational_combined$iso3) %>% 
  rename(Country=NAME_0) %>% 
  select(Country,iso3,GADM_code)

popNational_missing <- countries_missing_combined %>% 
  filter(!is.na(GADM_code))
# add year columns  
popNational_missing[paste0(timestep)] = NA

# add missing countries
popNational_combined <-  popNational_combined %>% 
  bind_rows(popNational_missing)



write_csv(popNational_combined, "results/national_pop_total.csv")

