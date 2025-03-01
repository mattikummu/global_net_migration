#### preparing migration data
library(sf)
library(e1071)
library(fasterize)
library(magrittr)
library(raster)
library(terra)

library(snow)
library(foreach)
library(openxlsx) #
library(readxl)
library(tidyverse)
library(dplyr) 



# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### load general data -----


cntryID <- read_csv("../data_in/countries_codes_and_coordinates.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  select(cntry_code,iso2,iso3,Country)%>% 
  distinct(iso3, cntry_code, .keep_all = T)



##### OECD ----

oecdTL2_3 <- read.xlsx('../data_in/OECD_db_cude_death_rate_TL2_TL3.xlsx') %>% 
  as_tibble() %>% 
  filter(TL == 2) %>% 
  filter(Gender == 'Total') %>% 
  filter(VAR == 'DEATH_RA')


oecdTL2_3_sel <- oecdTL2_3 %>% 
  select(c(TL,REG_ID,Region,Year, Value)) %>% 
  pivot_wider(names_from = Year, values_from = Value, values_fill = list(Number = '0')) %>% 
  rename(tl2_id = REG_ID) %>% 
  mutate(iso2 = substr(tl2_id,1,2)) %>%
  left_join(cntryID,by='iso2') %>%
  select(TL,tl2_id,Region,iso2,iso3,Country,cntry_code,everything()) %>%
  arrange(tl2_id) 


oecdPoly <- read_sf('../data_in_gpkg/oecd_tl2.gpkg') %>% 
  st_drop_geometry() %>% 
  right_join(oecdTL2_3_sel[,c(1,2)]) %>% 
  mutate(iso2 = substr(tl2_id,1,2)) %>% 
  left_join(cntryID,by='iso2') %>% 
  select(c(tl2_id,name_or,iso2,iso3.x,Country,cntry_code)) %>% 
  rename(iso3=iso3.x) %>% 
  mutate(gisData = ifelse(is.na(iso3),'OECD_notGIS','OECD'))

# add information about the gis data
oecdTL2_3_sel <- oecdTL2_3_sel %>% 
  left_join(oecdPoly[,c(1,7)])


unique_countriesData <- unique(c(oecdPoly$iso2)) %>% 
  as_tibble() %>% 
  arrange(value) %>% 
  rename(iso2 = value) %>% 
  left_join(cntryID) %>% 
  filter(iso2 != 'CHN') # for china only one value 

unique_countriesGIS <- unique(c(oecdPoly$iso3)) %>% 
  as_tibble() %>% 
  arrange(value) %>% 
  rename(iso3 = value) %>% 
  mutate(gis = 1) %>% 
  right_join(unique_countriesData) %>% 
  select(iso2,cntry_code,iso3,Country,gis) %>% 
  filter(iso3 != 'CHN') # for china only one value 


# create data_out folder, if it does not exist

# Define the folder name
folder_name <- "../data_out"

# Check if the folder exists
if (!dir.exists(folder_name)) {
  # Create the folder if it doesn't exist
  dir.create(folder_name)
  message("Folder '", folder_name, "' created.")
} else {
  message("Folder '", folder_name, "' already exists.")
}

write_csv(unique_countriesGIS,'../data_out/oecdMeta_deaths_tl2_dataANDgis.csv')

write_csv(oecdTL2_3_sel,'../data_out/oecd_deaths_tl2_data.csv')


# OECD level 3 for US and Australia 

oecdTL3 <- read.xlsx('../data_in/OECD_db_cude_death_rate_TL2_TL3.xlsx') %>% 
  as_tibble() %>% 
  filter(TL == 3) %>% 
  filter(Gender == 'Total') %>% 
  filter(VAR == 'DEATH_RA') 

oecdTL3_sel <- oecdTL3 %>% 
  select(c(TL,REG_ID,Region,Year, Value)) %>% 
  pivot_wider(names_from = Year, values_from = Value, values_fill = list(Number = '0')) %>% 
  rename(tl3_id = REG_ID) %>% 
  mutate(iso2 = substr(tl3_id,1,2)) %>%
  filter(iso2 == 'AU' | iso2 == 'US') %>% 
  left_join(cntryID,by='iso2') %>%
  select(TL,tl3_id,Region,iso2,iso3,Country,cntry_code,everything()) %>%
  arrange(tl3_id) 



write_csv(oecdTL3_sel,'../data_out/oecd_deaths_tl3_data.csv')

# OECD level 3 for all countries

oecdTL3 <- read.xlsx('../data_in/OECD_db_cude_death_rate_TL2_TL3.xlsx') %>% 
  as_tibble() %>% 
  filter(TL == 3) %>% 
  filter(Gender == 'Total') %>% 
  filter(VAR == 'DEATH_RA') 

oecdTL3_sel <- oecdTL3 %>% 
  select(c(TL,REG_ID,Region,Year, Value)) %>% 
  pivot_wider(names_from = Year, values_from = Value, values_fill = list(Number = '0')) %>% 
  rename(tl3_id = REG_ID) %>% 
  mutate(iso2 = substr(tl3_id,1,2)) %>%
  #filter(iso2 == 'AU' | iso2 == 'US') %>% 
  left_join(cntryID,by='iso2') %>%
  select(TL,tl3_id,Region,iso2,iso3,Country,cntry_code,everything()) %>%
  arrange(tl3_id) 

oecdPoly_tl3 <- read_sf('../data_in_gpkg/oecd_tl3.gpkg') %>% 
  st_drop_geometry() %>% 
  right_join(oecdTL3_sel[,c(1,2)]) %>% 
  mutate(iso2 = substr(tl3_id,1,2)) %>% 
  left_join(cntryID,by='iso2') %>% 
  select(c(tl3_id,name_or,iso2,iso3.x,Country,cntry_code)) %>% 
  rename(iso3=iso3.x) 



##### STAT compiler ----


STATcompiler_data <- read.xlsx('../data_in/STATcompiler_births_subnational.xlsx') %>% 
  as_tibble() %>% 
  filter(Characteristic == "Region") %>% 
  select(Country,Region,Year,Crude.birth.rate)


STATcompiler_poly <- read_sf( '../data_in_gpkg/STATcompiler_areas.gpkg' ) %>% 
  st_drop_geometry() %>% 
  select(ISO,CNTRYNAMEE,DHSREGEN) %>% 
  rename(iso2 = ISO) %>% 
  rename(Region = DHSREGEN) %>% 
  left_join(cntryID,by='iso2') 

STATcompiler_data_sel <- STATcompiler_data  %>% 
  mutate(Year = as.numeric( substr(Year,1,4)) ) %>% 
  distinct(Region, Year, .keep_all = TRUE) %>% 
  tidyr::pivot_wider(names_from = Year, values_from = Crude.birth.rate) %>% 
  left_join(STATcompiler_poly[,c(3:6)], by = c('Region','Country')) %>% 
  select(Country,Region,iso3,cntry_code,as.character(c(1990:2019))) 



STATcompiler_data_selGIS <- STATcompiler_data_sel  %>% 
  #left_join(STATcompiler_poly[,c(3,4,5)], by = 'Region') %>% 
  select(Region, iso3, Country, cntry_code, everything()) 


unique_STATcompiler_countriesData <- unique(c(STATcompiler_data_sel$Country)) %>% 
  as_tibble() %>% 
  arrange(value) %>% 
  rename(Country = value)  %>% 
  left_join(cntryID) %>% 
  select(iso2,cntry_code,iso3,Country)


readr::write_csv(STATcompiler_data_sel,'../data_out/STATcompiler_birth_dataWide.csv')

readr::write_csv(unique_STATcompiler_countriesData,'../data_out/STATcompilerMeta_birth_data.csv')


#### EUROSTAT -------

eurostatBirths_data_nuts3 <- read_csv('../data_in/eurostat_births_nuts2.csv') %>% 
  filter(age=='TOTAL') %>% 
  rename(NUTS = region) %>% 
  select(-c(unit,age))

eurostatBirths_data <- read_csv('../data_in/eurostat_births_nuts2.csv') %>% 
  filter(age=='TOTAL') %>% 
  rename(NUTS = region) %>% 
  select(-c(unit,age))

eurostat_poly <- read_sf('../data_in_gpkg/nuts2_corGeom.gpkg') %>% 
  st_drop_geometry() %>% 
  select(NUTS,NAME,NUTS0)

eurostatBirths_data_sel <- eurostatBirths_data %>% 
  left_join(eurostat_poly) %>% 
  select(NUTS,NAME,NUTS0,as.character(c(1990:2019)))

unique_Eurostat_countriesData <- unique(c(eurostatBirths_data_sel$NUTS0)) %>% 
  as_tibble() %>% 
  drop_na() %>% 
  rename(iso2 = value) %>% 
  mutate(iso2 = ifelse(iso2 == 'EL','GR',iso2)) %>% 
  mutate(iso2 = ifelse(iso2 == 'UK','GB',iso2)) %>% 
  left_join(cntryID) %>% 
  arrange(iso2) 

unique_Eurostat_NUTS2 <- unique(c(eurostatBirths_data_sel$NUTS)) %>% 
  as_tibble() %>% 
  drop_na() %>% 
  mutate(rowID = row_number()) %>% 
  rename(NUTS = value)

write_csv(eurostatBirths_data_sel,'../data_out/eurostat_birth_dataWide.csv')

write_csv(unique_Eurostat_countriesData,'../data_out/eurostatMeta_birth_data.csv')



# eurostat_gis <- read_sf('../data_in/ref-nuts-2016-01m.shp/NUTS_RG_01M_2016_3035_LEVL_2.shp/NUTS_RG_01M_2016_3035_LEVL_2.shp') %>% 
#   select(NUTS_ID,CNTR_CODE,NUTS_NAME)
# 
# eurostat_gis <- read_sf('../data_in/Europe_NUTS2_Boundaries.lpk')
# 
# write_sf(eurostat_gis, '../data_in/NUTS_level2.gpkg')


#### EUROSTAT as a ratio -----

# from number of deaths to ratio


eurostat_poly_sf <- read_sf('../data_in_gpkg/nuts2_corGeom.gpkg') %>%
  select(NUTS,NAME,NUTS0)
# now read in the fixed population data
pop_worldPop <- rast('../data_in_rast/ppp_2000_2020_5arcmin_corrected_fixed.tif')



# combine some areas  for which data is for larger entity

UK_innerLondon <- c('UKI3', 'UKI4','UKI5','UKI6','UKI7')
UK_Sscot <- c('UKM7','UKM8','UKM9')
DE_Sachsen <- c('DED2','DED4','DED5')

# st_is_valid(eurostat_poly_sf)
# st_make_valid(eurostat_poly_sf)
# st_is_valid(eurostat_poly_sf)

eurostat_poly_sf_mod <- eurostat_poly_sf %>% 
  mutate(NUTS = ifelse(NUTS %in% UK_innerLondon,'UKI',NUTS)) %>% 
  mutate(NUTS = ifelse(NUTS %in% UK_Sscot,'UKM789',NUTS)) %>% 
  mutate(NUTS = ifelse(NUTS %in% DE_Sachsen,'DED',NUTS)) %>% 
  select(NUTS,geom) %>% 
  # merge UK_innerLondon and UK_Sscot
  group_by(NUTS) %>% # group by the adm ids
  summarise(geometry = sf::st_union(geom)) %>% # dissolve the same admIDs
  ungroup() 

# read eurostat data
EuS_deaths_nuts2 <- read_excel('../data_in/eurostat_deaths_nuts2.xls',skip = 8) %>% 
  as_tibble() 
EuS_deaths_nuts1 <- read_excel('../data_in/eurostat_deaths_nuts1.xls',skip = 8) %>% 
  as_tibble() 

EuS_births_nuts2 <- read_excel('../data_in/eurostat_births_nuts2.xls',skip = 8) %>% 
  as_tibble() 
EuS_births_nuts1 <- read_excel('../data_in/eurostat_births_nuts1.xls',skip = 8) %>% 
  as_tibble() 

## DEATHS
# remove the deaths from UKM (nuts1) that are reported in UKM5 and UKM6 (nuts2)
row_UKM <- EuS_deaths_nuts1[which(EuS_deaths_nuts1$GEO == "UKM"), ]
row_UKM5_6 <- EuS_deaths_nuts2[which(EuS_deaths_nuts2$GEO %in% c("UKM5","UKM6")), ]
row_UKM_mod <- row_UKM
row_UKM_mod[,3:ncol(row_UKM_mod)] <- row_UKM_mod[,3:ncol(row_UKM_mod)] - 
  rowsum(row_UKM5_6[,3:ncol(row_UKM5_6)],c(1,1))

#row_UKI <- EuS_deaths_nuts1[which(EuS_deaths_nuts1$GEO == "UKI"), ]

EuS_deaths_nuts2_mod <- EuS_deaths_nuts2 %>% 
  # filter out the UK_innerlondon data and add larger area UKI from nuts1
  dplyr::filter(!GEO %in% UK_innerLondon) %>% 
  bind_rows(EuS_deaths_nuts1[which(EuS_deaths_nuts1$GEO == "UKI"), ]) %>% 
  # filter out the DED level2 data and add larger area DED from nuts1
  dplyr::filter(!GEO %in% DE_Sachsen) %>% 
  bind_rows(EuS_deaths_nuts1[which(EuS_deaths_nuts1$GEO == "DED"), ]) %>% 
  # filter out the UK_Sscot data and add larger area UKM from nuts1
  dplyr::filter(!GEO %in% UK_Sscot) %>% 
  bind_rows(row_UKM_mod)%>% 
  mutate(GEO = ifelse(GEO == "UKM",'UKM789',GEO)) 

## BIRTHS
# remove the deaths from UKM (nuts1) that are reported in UKM5 and UKM6 (nuts2)
row_UKM <- EuS_births_nuts1[which(EuS_births_nuts1$GEO == "UKM"), ]
row_UKM5_6 <- EuS_births_nuts2[which(EuS_births_nuts2$GEO %in% c("UKM5","UKM6")), ]
row_UKM_mod <- row_UKM
row_UKM_mod[,3:ncol(row_UKM_mod)] <- row_UKM_mod[,3:ncol(row_UKM_mod)] - 
  rowsum(row_UKM5_6[,3:ncol(row_UKM5_6)],c(1,1))

#row_UKI <- EuS_births_nuts1[which(EuS_births_nuts1$GEO == "UKI"), ]

EuS_births_nuts2_mod <- EuS_births_nuts2 %>% 
  # filter out the UK_innerlondon data and add larger area UKI from nuts1
  dplyr::filter(!GEO %in% UK_innerLondon) %>% 
  bind_rows(EuS_births_nuts1[which(EuS_births_nuts1$GEO == "UKI"), ]) %>% 
  # filter out the DED level2 data and add larger area DED from nuts1
  dplyr::filter(!GEO %in% DE_Sachsen) %>% 
  bind_rows(EuS_deaths_nuts1[which(EuS_deaths_nuts1$GEO == "DED"), ]) %>% 
  # filter out the UK_Sscot data and add larger area UKM from nuts1
  dplyr::filter(!GEO %in% UK_Sscot) %>% 
  bind_rows(row_UKM_mod) %>% 
  mutate(GEO = ifelse(GEO == "UKM",'UKM789',GEO)) 

# remove the years with partial data in terms of poland
EuS_births_nuts2_mod[189:205,3:25] <- NA

# calculate population for each year for each NUTS region
eurostat_pop_ee <- terra::extract(pop_worldPop,vect(eurostat_poly_sf_mod), 'sum',na.rm=T )
head(eurostat_pop_ee)

eurostat_poly_sf_pop <- cbind(eurostat_poly_sf_mod,eurostat_pop_ee) %>% 
  st_drop_geometry() %>% 
  as_tibble()


# calculate ratio of births and deaths per 1000 people

# BIRHTS
EuS_births_nuts2_N_pop <- EuS_births_nuts2_mod %>% 
  select(-c(as.character(1990:1999))) %>% 
  filter(!is.na(GEO)) %>% 
  left_join(eurostat_poly_sf_pop,by=c("GEO" = "NUTS"))

# calculate ratio per 1000 people
temp_ratio <- EuS_births_nuts2_N_pop[,paste0(2000:2019)] / EuS_births_nuts2_N_pop[,paste0('ppp_',2000:2019)] * 1000 
temp_ratio <- temp_ratio %>% 
  as_tibble()
head(temp_ratio)

EuS_births_nuts2_ratio <- EuS_births_nuts2_N_pop %>% 
  select(GEO,`GEO(L)/TIME`) %>% 
  bind_cols(temp_ratio)

# DEATHS

EuS_deaths_nuts2_N_pop <- EuS_deaths_nuts2_mod %>% 
  select(-c(as.character(1990:1999))) %>% 
  filter(!is.na(GEO)) %>% 
  left_join(eurostat_poly_sf_pop,by=c("GEO" = "NUTS"))

# calculate ration per 1000 people
temp_ratio <- EuS_deaths_nuts2_N_pop[,paste0(2000:2019)] / EuS_births_nuts2_N_pop[,paste0('ppp_',2000:2019)] * 1000 
temp_ratio <- temp_ratio %>% 
  as_tibble()
head(temp_ratio)

EuS_deaths_nuts2_ratio <- EuS_deaths_nuts2_N_pop %>% 
  select(GEO,`GEO(L)/TIME`) %>% 
  bind_cols(temp_ratio)

#view(EuS_deaths_nuts2_ratio)


## rasterise

eurostat_poly_birth_data <- eurostat_poly_sf_mod %>% 
  left_join(EuS_births_nuts2_ratio,by=c("NUTS" = "GEO")) 

vect_eurostat_poly_birth_data <- eurostat_poly_birth_data%>% 
  as("Spatial") %>%
  vect()

eurostat_poly_death_data <- eurostat_poly_sf_mod %>% 
  left_join(EuS_deaths_nuts2_ratio,by=c("NUTS" = "GEO")) 

vect_eurostat_poly_death_data <- eurostat_poly_death_data %>% 
  as("Spatial") %>%
  vect()


# create results folder, if it does not exist

# Define the folder name
folder_name <- "../results"

# Check if the folder exists
if (!dir.exists(folder_name)) {
  # Create the folder if it doesn't exist
  dir.create(folder_name)
  message("Folder '", folder_name, "' created.")
} else {
  message("Folder '", folder_name, "' already exists.")
}

# write results

r_EuS_births <- terra::rasterize(vect_eurostat_poly_birth_data,pop_worldPop,field = 'X2015')
writeRaster(r_EuS_births,'../results/euroStatBirths_2015.tif',  gdal="COMPRESS=LZW",overwrite=TRUE)

r_EuS_deaths <- terra::rasterize(vect_eurostat_poly_death_data,pop_worldPop,field = 'X2015')
writeRaster(r_EuS_deaths,'../results/euroStatDeaths_2015.tif',  gdal="COMPRESS=LZW",overwrite=TRUE)

st_write(eurostat_poly_birth_data, "../results/eurostat_poly_birth_data.gpkg",delete_dsn = TRUE)
st_write(eurostat_poly_death_data, "../results/eurostat_poly_death_data.gpkg", delete_dsn = TRUE)

# some checking

eurostat_poly_birth_data %>% filter(NUTS == 'UKM789')
eurostat_poly_birth_data %>% filter(NUTS == 'RO31')





