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


cntryID <- read_csv("../data_in/countries_codes_and_coordinates.csv") %>% 
  select(GADM_code,iso3,cntry_code) %>% 
  distinct(GADM_code, iso3, cntry_code, .keep_all = T)

# temp_gadm <- vect('/Users/mkummu/R/migration_data_bee/data_in/gadm_lev1.gpkg') %>% 
#   as_tibble()

# national population -----------------------------------------------------


national_gis <- read_sf( "../data_in_gpkg/gadm_level0.gpkg" )
temp <- national_gis %>% 
  st_drop_geometry()

popHyde_1990_2020 <- rast("../data_in_rast/pop_1990_2020_hyde32.tif")
popWorldPop_2000_2020 <- rast("../data_in_rast/ppp_2000_2020_5arcmin_corrected_fixed.tif")

#names(subnat_gis_combined) <- c("Country", "iso3","cntry_code","Subnat","GID_1","GID_nmbr","NumRate",paste0("ratio",1990:2019),"geom")


# use HYDE to extrapolate WorldPop to 1990-2000

# Define the file path
file_path <- "../results/popRaster_1990_2020.tif"

# Check if the file exists
if (!file.exists(file_path)) {
  # The file does not exist, so you can add your code here
  # project to same extent
  popHyde_1990_2020_proj <- terra::project(popHyde_1990_2020,subset(popWorldPop_2000_2020,1))
  
  deltaPop <- subset(popWorldPop_2000_2020,1) / subset(popHyde_1990_2020_proj,11)
  
  # in case of zero population in popHyde, keep delta as 1 (i.e. no change)
  deltaPop[is.infinite(deltaPop)] <- 1
  
  popWorldPop_1990_2020 <- c(deltaPop * subset(popHyde_1990_2020_proj,1:10), popWorldPop_2000_2020)
  
  # global(subset(popWorldPop_1990_2020,5),"sum", na.rm=TRUE)
  # global(subset(popHyde_1990_2020_proj,5) ,"sum", na.rm=TRUE)
  writeRaster(popWorldPop_1990_2020,'../results/popRaster_1990_2020.tif',  gdal="COMPRESS=LZW",overwrite=TRUE)
  
} else {
  message("File '", file_path, "' already exists, reading file.")
  # read data
  popWorldPop_1990_2020 <- rast('../results/popRaster_1990_2020.tif')
}



temp_nat_pop <- terra::extract(popWorldPop_1990_2020,vect(national_gis),'sum') %>%
  as_tibble()


colnames(temp_nat_pop) <- c('ID',paste0("pop",as.character(1990:2020)))



# create population table
national_pop <- national_gis %>% 
  select(GID_0) %>% 
  st_drop_geometry() %>% 
  bind_cols(temp_nat_pop) %>% 
  select(GID_0,everything()) 

write_csv(national_pop, "../results/cntry_pop.csv")

# temp_cuw_cok <- national_pop %>% 
#   filter(GID_0 %in% c('CUW', 'COK', 'FIN'))

#  functions------------------------------------------

# functions for data from population prospects
source('../functions/myFun_nationalData.R')

# dataIn = data frame of births/deaths with startCol meta columns and then rest
# of the columns yearly values, years as column headers
source('../functions/f_interpolate_years.R')

# functions for data from world bank
source('../functions/myFun_nationalDataWB.R')

# dataIn = data frame of births/deaths with startCol meta columns and then rest
# of the columns yearly values, years as column headers
source('../functions/f_interpolate_yearsWB.R')

#### annual values for births and deaths -----
# variable <- "births" / "deaths"
# dataPP <- "../data_in/WPP2019_FERT_F03_CRUDE_BIRTH_RATE.xlsx" / 
# dataWB <- "../data_in/API_SP.DYN.CBRT.IN_DS2_en_excel_v2_2595037.xlsx" / 

source('../functions/myFun_interpolateCntryValues.R')



# apply function for births and deaths ------------------------------------


births_data_interp_t_id <- myFun_interpolateCntryValues(variable = "births",
                                                        dataPP = "../data_in/WPP2019_FERT_F03_CRUDE_BIRTH_RATE.xlsx",
                                                        dataWB = "../data_in/API_SP.DYN.CBRT.IN_DS2_en_excel_v2_2595037.xlsx")
deaths_data_interp_t_id <- myFun_interpolateCntryValues("deaths",
                                                        "../data_in/WPP2019_MORT_F02_CRUDE_DEATH_RATE.xlsx",
                                                        "../data_in/API_SP.DYN.CDRT.IN_DS2_en_excel_v2_2593323.xlsx")


write_csv(births_data_interp_t_id, "../results/national_births_ratio_interp.csv")
write_csv(deaths_data_interp_t_id, "../results/national_deaths_ratio_interp.csv")




#### population count ----


## data from UN PP --


# cntryID <- read_csv("../data_in/countries_codes_and_coordinates.csv") %>% 
#   #dplyr::select(-cntry_code) %>% 
#   #rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
#   select(GADM_code,iso3,cntry_code)


popNational_PP <- read.xlsx('../data_in/WPP2019_POP_F01_1_TOTAL_POPULATION_BOTH_SEXES.xlsx',"ESTIMATES", colNames = T, startRow = 17) %>%
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
#xlsIn <- "../data_in/API_SP.DYN.CBRT.IN_DS2_en_excel_v2_2595037.xlsx"
popNational_WB <- read.xlsx('../data_in/Data_Extract_From_Population_estimates_and_projections.xlsx')  %>% #,"Data", colNames = T, startRow = 1) %>%
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

gadm_countries <- read_sf( "../data_in_gpkg/gadm_level0.gpkg" ) %>% 
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



write_csv(popNational_combined, "../results/national_pop_total.csv")

