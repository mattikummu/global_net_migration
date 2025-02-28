### processing migration data
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


# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#### load data -----

cntryID <- read_csv("../data_in/countries_codes_and_coordinates.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  select(cntry_code,iso2,iso3,Country) %>% 
  mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2))

cntry_metaData_births <- read.xlsx("../data_in/cntry_metaData.xlsx",sheet = 'cntry_meta_births') %>% 
  as_tibble()

cntry_metaData_deaths <- read.xlsx("../data_in/cntry_metaData.xlsx",sheet = 'cntry_meta_deaths') %>% 
  as_tibble()
# 
# popHyde_1990_2020 <- rast("../data_in/pop_1990_2020_hyde32.tif")
# popWorldPop_2000_2020 <- rast("../data_in/ppp_2000_2020_5arcmin_corrected_fixed.tif")


#### births ----

# general

GIS_layers_births <- unique(cntry_metaData_births$GIS_data)

# number or ratio
NumRat <- cntry_metaData_births %>% 
  select(GID_0,NUMBER_RATE) %>% 
  rename(iso3 = GID_0) %>% 
  rename(NumRate = NUMBER_RATE)


### STATcompiler ----

cntryList <- cntry_metaData_births %>% 
  filter(GIS_data == GIS_layers_births[1]) %>% 
  select(GID_0,ID_0,NAME_0,NUMBER_RATE,Notes) 

id_cntryList <- unique(cntryList$GID_0)

# number or ratio
temp_NumRat <- NumRat %>% 
  dplyr::filter(iso3 %in% cntryList$GID_0)

# read data

subnat_data <- read.xlsx('../data_in/STATdata_2_wide_mk_v2.xlsx') %>% 
  as_tibble() %>% 
  rename(iso2 = ISO) %>% 
  rename(Subnat = Region) %>% 
  mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2)) %>% 
  #filter(!is.na(RegID)) %>% 
  left_join(cntryID,by='iso2') %>% 
  dplyr::filter(iso3 %in% cntryList$GID_0) %>% 
  arrange(iso3) %>% 
  select(-c(Subnat,Region2,Country.x,iso2,GADM_country,Country.y,cntry_code))

names(subnat_data) <- c('RegID',as.character(1990:2019),'iso3')

id_subnat <- unique(subnat_data$iso3)

gis_data <- read_sf( '../data_in_gpkg/STATcompiler_areas.gpkg' ) %>% 
  #st_drop_geometry() %>% 
  select(ISO,CNTRYNAMEE,DHSREGEN,REG_ID) %>% 
  rename(iso2 = ISO) %>% 
  rename(Subnat = DHSREGEN) %>% 
  rename(RegID = REG_ID) %>% 
  mutate(iso2 = ifelse(CNTRYNAMEE == 'Namibia','NB',iso2)) %>% 
  left_join(cntryID,by='iso2') %>% 
  select(Country,Subnat,iso3,cntry_code,RegID) %>% 
  dplyr::filter(iso3 %in% cntryList$GID_0) %>% 
  arrange(iso3)

id_gis <- unique(gis_data$iso3)

compare::compare(id_subnat,id_gis)

subnat_gis_data <- gis_data %>% 
  left_join(subnat_data,by=c('RegID','iso3')) %>% 
  #st_drop_geometry() %>% 
  arrange(iso3,Subnat) %>% 
  group_by(iso3) %>% 
  mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
  ungroup() %>% 
  mutate(GID_1 = paste0(iso3,".",rowNmbr,"_1")) %>% 
  mutate(GID_nmbr = (1000+cntry_code)*100+rowNmbr) %>% 
  select(-rowNmbr)%>% 
  left_join(temp_NumRat)  %>% 
  select(Country,iso3,cntry_code,Subnat,GID_1,GID_nmbr,NumRate,as.character(1990:2019))

subnat_gis_data_STATcomp <- subnat_gis_data



#### GADM ----

cntryList <- cntry_metaData_births %>% 
  filter(GIS_data == GIS_layers_births[4]) %>% 
  select(GID_0,ID_0,NAME_0,NUMBER_RATE,Notes) 

# number or ratio
temp_NumRat <- NumRat %>% 
  dplyr::filter(iso3 %in% cntryList$GID_0)


# read data 

subnat_data <- read.xlsx('../data_in/birth_rate_v3.xlsx', sheet='birth_rate', startRow = 8) %>% 
  as_tibble() %>% 
  rename(iso3=GID_0) %>% 
  select(-GID_1) %>% 
  rename(GID_1 = GID_1_combined) %>% 
  dplyr::filter(iso3 %in% cntryList$GID_0) %>% 
  rename(Subnat = NAME_1) %>% 
  rename(Country = NAME_0) %>% 
  select('iso3','GID_1','Subnat',as.character(1990:2019)) %>% 
  arrange(iso3)

# all data columns to numeric
cols.num <- c(as.character(1990:2019))
subnat_data[cols.num] <- sapply(subnat_data[cols.num],as.numeric)

id_subnat <- unique(subnat_data$iso3)

gis_data <- read_sf( '../data_in_gpkg/gadm_lev1.gpkg' ) %>% 
  #st_drop_geometry() %>% 
  select(GID_0,NAME_0,NAME_1,GID_1) %>% 
  rename(iso3 = GID_0) %>% 
  rename(Subnat = NAME_1) %>% 
  left_join(cntryID,by='iso3') %>% 
  select(Country,iso3,cntry_code,GID_1) %>% 
  dplyr::filter(iso3 %in% cntryList$GID_0) %>% 
  arrange(iso3)

id_gis <- unique(gis_data$iso3)

compare::compare(id_subnat,id_gis)


subnat_gis_data <- gis_data %>% 
  left_join(subnat_data,by=c('iso3','GID_1')) %>% 
  #st_drop_geometry() %>% 
  arrange(iso3,GID_1) %>% 
  group_by(iso3) %>% 
  mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
  ungroup() %>% 
  mutate(GID_nmbr = (1000+cntry_code)*100+rowNmbr) %>% 
  select(-rowNmbr) %>% 
  left_join(temp_NumRat)%>% 
  select(Country,iso3,cntry_code,Subnat,GID_1,GID_nmbr,NumRate,as.character(1990:2019))

subnat_gis_data_GADM <- subnat_gis_data

### EUROSTAT ----
# these were combined from smaller entities due to lack of data
temp_addedNUTS <- tibble(
  NUTS = c("UKM789","UKI","DED"), 
  NUTS0 = c("GB","GB","DE")
) 

eurostat_cntry_NUTS2 <- read_sf('../data_in_gpkg/nuts2_corGeom.gpkg') %>% 
  select(NUTS,NUTS0) %>% 
  st_drop_geometry() %>% 
  bind_rows(temp_addedNUTS)


cntryList <- cntry_metaData_births %>% 
  filter(GIS_data %in% GIS_layers_births[3]) %>% 
  select(GID_0,ID_0,NAME_0,NUMBER_RATE,Notes) %>% 
  mutate(NUMBER_RATE = "R") # already modified to ratio in previous code

# done in a separate script (preparing_data.R)
temp_eurostat_gis <- read_sf("../results/eurostat_poly_birth_data.gpkg") 
#st_is_valid(temp_eurostat_gis)

names(temp_eurostat_gis) <- as.character(c("GID_1","Subnat",as.character(2000:2019),"geom"))

subnat_gis_data <-  temp_eurostat_gis %>% 
  left_join(eurostat_cntry_NUTS2, by=c("GID_1" = "NUTS")) %>% 
  rename(iso2 = NUTS0) %>% 
  left_join(cntryID,by='iso2') %>% 
  dplyr::filter(iso3 %in% cntryList$GID_0) %>% 
  mutate(NumRate = "R") %>% 
  arrange(iso3,GID_1) %>% 
  group_by(iso3) %>% 
  mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
  ungroup() %>% 
  mutate(GID_nmbr = (1000+cntry_code)*100+rowNmbr) %>% 
  select(-rowNmbr) %>% 
  select(Country,iso3,cntry_code,Subnat,GID_1,GID_nmbr,NumRate,everything()) %>% 
  select(-iso2)

subnat_gis_data_EUROSTAT <- subnat_gis_data

subnat_gis_data %>% filter(GID_1 == 'UKM789')



### individual countries ----

cntryList <- cntry_metaData_births %>% 
  filter(GIS_data %in% GIS_layers_births[5:9]) %>% 
  select(GID_0,ID_0,NAME_0,NUMBER_RATE,Notes) 

# number or ratio
temp_NumRat <- NumRat %>% 
  dplyr::filter(iso3 %in% cntryList$GID_0)

# read data

subnat_data <- read.xlsx('../data_in/birth_rate_v3.xlsx', sheet='birth_rate', startRow = 8) %>% 
  as_tibble() %>% 
  rename(iso3=GID_0) %>% 
  #select(-GID_1)  %>% 
  #rename(GID_1 = GID_1_combined) %>% 
  dplyr::filter(iso3 %in% cntryList$GID_0) %>% 
  rename(Subnat = NAME_1) %>% 
  rename(Country = NAME_0)  %>% 
  select('iso3','GID_1','Subnat',as.character(1990:2019)) %>% 
  arrange(iso3,GID_1)

cols.num <- c(as.character(1990:2019))
subnat_data[cols.num] <- sapply(subnat_data[cols.num],as.numeric)

temp_gis_JAM <- read_sf( '../data_in_gpkg/GIS extra/Jamaica_mod.gpkg') %>% 
  select(ISO,NAME_0,NAME_1) %>% 
  rename(iso3 = ISO) %>% 
  rename(Subnat = NAME_1) %>% 
  arrange(Subnat) %>% 
  mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
  mutate(GID_1 = rowNmbr) %>% 
  mutate(GID_1 = as.character(GID_1)) %>% 
  left_join(cntryID,by='iso3') %>% 
  select(Country,Subnat,iso3,cntry_code,GID_1)

temp_gis_LBN <- read_sf( '../data_in_gpkg/GIS extra/Lebanon.gpkg') %>% 
  select(GID_0,NAME_0,NAME_1,GID_1) %>% 
  rename(iso3 = GID_0) %>% 
  rename(Subnat = NAME_1) %>% 
  mutate(GID_1 = as.character(GID_1)) %>% 
  left_join(cntryID,by='iso3') %>%
  arrange(GID_1) %>% 
  select(Country,Subnat,iso3,cntry_code,GID_1)
#


names(temp_gis_LBN) <- c("Country","Subnat","iso3","cntry_code", "GID_1","geom" )

temp_gis_MAR <- read_sf( '../data_in_gpkg/GIS extra/Morocco_mod.gpkg') %>% 
  select(ISO,NAME_0,NAME_1,admID) %>% 
  rename(iso3 = ISO) %>% 
  rename(Subnat = NAME_1) %>% 
  rename(GID_1 = admID) %>% 
  mutate(GID_1 = as.character(GID_1)) %>% 
  left_join(cntryID,by='iso3') %>% 
  select(Country,Subnat,iso3,cntry_code,GID_1)

temp_gis_PRK <- read_sf( '../data_in_gpkg/GIS extra/NorthKorea.gpkg') %>% 
  select(GID_0,NAME_0,NAME_1,GID_1) %>% 
  rename(iso3 = GID_0) %>% 
  rename(Subnat = NAME_1) %>%  
  mutate(GID_1 = as.character(GID_1)) %>% 
  left_join(cntryID,by='iso3') %>% 
  select(Country,Subnat,iso3,cntry_code,GID_1)



temp_gis_SLE <- read_sf( '../data_in_gpkg/GIS extra/SierraLeone.gpkg') %>% 
  select(admin2Pcod,admin2Name) %>% 
  mutate(iso3 = 'SLE') %>% 
  rename(Subnat = admin2Name) %>% 
  rename(GID_1 = admin2Pcod) %>% 
  mutate(GID_1 = as.character(GID_1)) %>% 
  left_join(cntryID,by='iso3') %>%
  select(Country,Subnat,iso3,cntry_code,GID_1)


gis_data <- bind_rows(temp_gis_JAM,
                      temp_gis_LBN,
                      temp_gis_MAR,
                      temp_gis_PRK,
                      temp_gis_SLE) %>% 
  select(Country,iso3,cntry_code,GID_1) %>% 
  mutate(GID_1 = as.character(GID_1)) %>% 
  arrange(iso3,GID_1) #%>% 
#st_drop_geometry()


temp <- gis_data %>% 
  st_drop_geometry()

temp_subnat <-subnat_data 


subnat_gis_data <- gis_data %>% 
  #select(-c(iso3,GID_1)) %>% 
  #bind_cols(subnat_data) %>% # left_join did not work as GID_1 did not match in SLE and LBN
  left_join(subnat_data,by=c('iso3','GID_1')) %>% 
  #filter(iso3 == "LBN") %>% 
  #as_tibble() %>% 
  #st_drop_geometry() %>% 
  arrange(iso3,GID_1) %>% 
  group_by(iso3) %>% 
  mutate(rowNmbr = row_number()) %>%  # for each subnat in a country a running number from 1...
  ungroup() %>% 
  mutate(GID_nmbr = (1000+cntry_code)*100+rowNmbr) %>% 
  select(-rowNmbr)%>% 
  left_join(temp_NumRat) %>% 
  select(Country,iso3,cntry_code,Subnat,GID_1,GID_nmbr,NumRate,as.character(1990:2019))

subnat_gis_data_individuals <- subnat_gis_data


subnat_gis_combined <- bind_rows(subnat_gis_data_EUROSTAT,
                                 subnat_gis_data_GADM,
                                 subnat_gis_data_individuals,
                                 subnat_gis_data_STATcomp) %>% 
  select(Country,iso3,cntry_code,Subnat,GID_1,GID_nmbr,NumRate,as.character(1990:2019))

st_write(subnat_gis_combined, "../results/test_births_combined.gpkg",delete_dsn = TRUE)

temp <- subnat_gis_combined %>% 
  st_drop_geometry() %>%
  arrange(cntry_code,Subnat)


write_csv(temp, "../results/births_subnat_combined_v2.csv")



### Population for each subnat ----
#temp_subnat_gis <- terra::vect(as(subnat_gis_combined, "Spatial"))  

#tmap::qtm(subnat_gis_data_STATcomp@geom )

subnat_gis_combined <- read_sf( "../results/test_births_combined.gpkg" )

names(subnat_gis_combined) <- c("Country", "iso3","cntry_code","Subnat","GID_1","GID_nmbr","NumRate",paste0("ratio",1990:2019),"geom")



popWorldPop_1990_2020 <- rast('../results/popRaster_1990_2020.tif')

# get pop for each subnat area
temp_subnat_pop <- terra::extract(popWorldPop_1990_2020,vect(subnat_gis_combined),'sum',na.rm=T) %>%
  as_tibble()


colnames(temp_subnat_pop) <- c('ID',paste0("pop",as.character(1990:2020)))

# create population table
subnat_pop <- subnat_gis_combined %>% 
  select(GID_1) %>% 
  st_drop_geometry() %>% 
  bind_cols(temp_subnat_pop) %>% 
  select(-ID) %>% 
  select(GID_1,everything()) 


subnat_data_combined <- read_csv("../results/births_subnat_combined_v2.csv")
names(subnat_data_combined) <- c("Country", "iso3","cntry_code","Subnat","GID_1","GID_nmbr","NumRate",paste0("ratio",1990:2019))

# join tables together
subnat_gis_combined_pop <- subnat_data_combined %>% 
  left_join(subnat_pop)

# calculate ratio for those entries with total births/deaths
years <- c(1990:2019)
for (i in 1:length(years)) {
  
  col1name <- paste0("ratio",years[i])
  col2name <- paste0("pop", years[i])
  
  subnat_gis_combined_pop <- subnat_gis_combined_pop %>%
    mutate(!!col1name := ifelse(NumRate=="N", !!sym(col1name) / ( !!sym(col2name) / 1000) , !!sym(col1name)))
  
}


temp <- subnat_gis_combined_pop %>% 
  #st_drop_geometry() %>%
  arrange(cntry_code,Subnat)
#filter(NumRate=="N")

write_csv(temp, "../results/test_births_combined_pop_ratio_v2.csv")


### fill missing data ----

subnat_gis_combined_pop <- read.csv( "../results/test_births_combined_pop_ratio_v2.csv" )


# GID1_missing <- 'BOL.6_1'
# GID1_data <- 'BOL.5_1'
# 
# mode <- "split"

# f_fillMissing <- function(GID1_missing,GID1_data,mode) {

# select only ratio
subnat_gis_combined_ratio <-subnat_gis_combined_pop %>% 
  as_tibble() %>% 
  select(Country,iso3,cntry_code,Subnat,GID_1,GID_nmbr,NumRate,paste0("ratio",1990:2019))

# select only pop
subnat_pop <-subnat_gis_combined_pop %>% 
  as_tibble() %>% 
  select(Country,iso3,cntry_code,Subnat,GID_1,GID_nmbr,NumRate,paste0("pop",1990:2019))

# read missing data
listMissingData <- read_csv("../data_in/missing_data_list_births.csv") %>% 
  filter(mode != "population") %>% 
  as_tibble()

# mode can be 
modeOptions <- unique(listMissingData$mode)

# go through each missing data entry

i = 19
for (i in 1:nrow(listMissingData)) {
  if(is.na(listMissingData[i,2])) {
    GID1_missing <- listMissingData[i,1]
  }else{
    GID1_missing <- listMissingData[i,1:2]
  }
  GID1_data <- listMissingData[i,3]
  mode <- listMissingData[i,4]
  
  dataSel_missing <- subnat_gis_combined_ratio %>% 
    filter(GID_1 %in% c(GID1_missing)) %>% 
    # mutate(order=ifelse(GID_1 == !!GID1_data,1,2)) %>% # arrange so that column with data first
    # arrange(order) %>% 
    select(-c(Country,iso3,cntry_code,Subnat,GID_nmbr, NumRate)) 
  
  dataSel_data <- subnat_gis_combined_ratio %>% 
    filter(GID_1 %in% c(as.character(GID1_data))) %>% 
    # mutate(order=ifelse(GID_1 == !!GID1_data,1,2)) %>% # arrange so that column with data first
    # arrange(order) %>% 
    select(-c(Country,iso3,cntry_code,Subnat,GID_nmbr, NumRate)) 
  
  NumOrRate <- subnat_gis_combined_ratio %>% 
    filter(GID_1 %in% c(GID1_missing)) %>% 
    # mutate(order=ifelse(GID_1 == !!GID1_data,1,2)) %>% # arrange so that column with data first
    # arrange(order) %>% 
    select(NumRate)
  
  dataSel <- dataSel_data %>% 
    bind_rows(dataSel_missing)
  
  popSel_missing <- subnat_pop %>% 
    filter(GID_1 %in% c(GID1_missing)) %>% 
    select(-c(Country,iso3,cntry_code,Subnat,GID_nmbr, NumRate)) 
  popSel_data <- subnat_pop %>% 
    filter(GID_1 %in% c(GID1_data)) %>% 
    select(-c(Country,iso3,cntry_code,Subnat,GID_nmbr, NumRate)) 
  popSel <- popSel_data %>% 
    bind_rows(popSel_missing)
  
  # calculate total 
  
  if(mode == "cntryAverage") {
    # skip this part
  }else{
    
    dataSel_Num <- dataSel[,2:ncol(dataSel)] * popSel[,2:ncol(popSel)]/1000
    dataRatioSel <- dataSel[,2:ncol(dataSel)]
    
    # totals for combined area
    
    # if numeric, then we calculate the ratio for combined area
    if(NumOrRate[1,1] == "N") {
      dataTotal <- colSums(as.matrix (dataSel_Num),na.rm=T)
      popTotal <- colSums(as.matrix (popSel[,2:ncol(popSel)]/1000),na.rm=T)
      dataRatioTotal <- dataTotal / popTotal # ratio for combined area
      
      # ratio between combined area and subnat areas
      dataRatio_shr <- dataRatioSel / rbind(dataRatioTotal,dataRatioTotal) 
      
    }else {
      # if ratio & mode == combine, then we calculate the ratio for combined area
      if(mode == "combine"){
        dataTotal <- colSums(as.matrix (dataSel_Num),na.rm=T)
        popTotal <- colSums(as.matrix (popSel[,2:ncol(popSel)]/1000),na.rm=T)
        dataRatioTotal <- dataTotal / popTotal # ratio for combined area
        
        # ratio between combined area and subnat areas
        dataRatio_shr <- dataRatioSel / rbind(dataRatioTotal,dataRatioTotal) 
      } else {
        
        # if ratio & mode == missing OR split modes, it is the one reported in data
        dataRatioTotal <- dataSel_data[,2:ncol(dataSel)]
        dataRatio_shr <- dataRatioSel / rbind(dataRatioTotal,dataRatioTotal) 
      }
    }
    
  }
  
  # go through each mode (combine, cntryAverage, split, missing)
  
  if(mode == "combine") { # if combine, then it means that two subnats will have same ratio
    if(is.na(listMissingData[i,2])) { # only one to combine
      dataRatio_filled <- rbind(dataRatioTotal,dataRatioTotal) %>% 
        as_tibble()
    }else{ # two to combine
      dataRatio_filled <- rbind(dataRatioTotal,dataRatioTotal,dataRatioTotal) %>% 
        as_tibble()
    }
    
  }else if(mode == "cntryAverage") { 
    cntryCode <- subnat_gis_combined_ratio %>% 
      filter(GID_1 %in% c(GID1_missing)) %>% 
      select(cntry_code) 
    
    cntryData <- subnat_gis_combined_ratio %>% 
      filter(cntry_code %in% !!cntryCode) %>%  # select all entries in  that country
      filter(GID_1 != c(GID1_missing)) %>% # deselect the missing value
      select(-c(Country,iso3,cntry_code,Subnat,GID_nmbr, NumRate)) 
    
    popData <- subnat_pop %>% 
      filter(GID_1 %in% !!cntryData$GID_1) %>%  # select all entries in  that country
      #filter(GID_1 != c(GID1_missing)) %>% # deselect the missing value
      select(-c(Country,iso3,cntry_code,Subnat,GID_nmbr, NumRate)) 
    
    # country average ratio (pop weighted average)
    cntryAvg_ratio <- colSums(cntryData[,2:ncol(cntryData)] * popData[,2:ncol(popData)],na.rm = T) /
      colSums(popData[,2:ncol(popData)],na.rm = T)
    
    dataRatio_filled <- rbind(dataSel[1,2:ncol(dataSel)],cntryAvg_ratio) %>% 
      as_tibble()
    
    
  }else {
    # for missing and split
    dataRatio_filled <- dataRatioSel
    # use the ratio in last common timestep to estimate the ratio to the timesteps with missing data
    
    # for 'start tail'
    start_tail_data <- min(which(!is.na(dataRatioSel[1,])))
    start_tail_missing <- min(which(!is.na(dataRatioSel[2,])))
    if( start_tail_missing == start_tail_data ) {
      # do nothing
    } else {
      
      if(mode == "split") { # if split, then it means that subnat has been split to two (or more) entities 
        # then we use the ratio of last common timestep to the total ratio to scale the previous timesteps 
        # for both subnats
        
        # if only one subnat splitted from original
        if(is.na(listMissingData[i,2])) {
          dataRatio_filled[,1:(start_tail_missing-1)] <- 
            dataRatio_shr[,start_tail_missing] * 
            rbind(dataRatioTotal[1:(start_tail_missing-1)],
                  dataRatioTotal[1:(start_tail_missing-1)]) 
        }else{
          dataRatio_filled[,1:(start_tail_missing-1)] <- 
            dataRatio_shr[,start_tail_missing] * 
            rbind(dataRatioTotal[1:(start_tail_missing-1)],
                  dataRatioTotal[1:(start_tail_missing-1)],
                  dataRatioTotal[1:(start_tail_missing-1)]) 
        }
        
        
      }else if(mode == "missing"){ # meaning that data is missing and we need to scale the value using data from other subnat unit
        # then we use the ratio between the subnat unit ratios to scale the previous timesteps
        
        dataRatio_filled[2:nrow(dataRatio_filled),1:(start_tail_missing-1)] <- 
          as.numeric((dataRatioSel[2,start_tail_missing]/dataRatioSel[1,start_tail_missing])) *
          dataRatioSel[1,1:(start_tail_missing-1)]
        
        
      }
    }
    
    # for 'end tail'
    end_tail_data <-  max(which(!is.na(dataRatioSel[1,])))
    end_tail_missing <-  max(which(!is.na(dataRatioSel[2,])))
    
    if(end_tail_missing == end_tail_data) { 
      # if no end tail, do nothing
    }else {
      
      if(mode == "split") { # if split, then it means that subnat has been split to two (or more) entities 
        # then we use the ratio of last common timestep to the total ratio to scale the previous timesteps 
        # for both subnats
        # if only one subnat splitted from original
        if(is.na(listMissingData[i,2])) {
          dataRatio_filled[,(end_tail_missing+1):ncol(dataRatio_filled)] <- 
            dataRatio_shr[,end_tail_missing] * 
            rbind(dataRatioTotal[(end_tail_missing+1):ncol(dataRatio_filled)],
                  dataRatioTotal[(end_tail_missing+1):ncol(dataRatio_filled)]) 
        }else {
          dataRatio_filled[,(end_tail_missing+1):ncol(dataRatio_filled)] <- 
            dataRatio_shr[,end_tail_missing] * 
            rbind(dataRatioTotal[(end_tail_missing+1):ncol(dataRatio_filled)],
                  dataRatioTotal[(end_tail_missing+1):ncol(dataRatio_filled)],
                  dataRatioTotal[(end_tail_missing+1):ncol(dataRatio_filled)]) 
        }
      }else if(mode == "missing") { # meaning that data is missing and we need to scale the value using data from other subnat unit
        # then we use the ratio between the subnat unit ratios to scale the previous timesteps 
        
        
        dataRatio_filled[2:nrow(dataRatio_filled),(end_tail_missing+1):ncol(dataRatio_filled)] <- 
          as.numeric((dataRatioSel[2,end_tail_missing]/dataRatioSel[1,end_tail_missing])) *
          dataRatioSel[1,(end_tail_missing+1):ncol(dataRatio_filled)]
        
      }
      
    }
    
    
    # some missing values are not in the tails
    
    if(mode == "missing") { # meaning that data is missing and we need to scale the value using data from other subnat unit
      # then we use the ratio between the subnat unit ratios to scale the previous timesteps 
      
      # identify column that is missing
      colMissing <- which(is.na(dataRatio_filled[2,]) & 
                            !is.na(dataRatio_filled[1,]))
      # identify columns where data is available
      colBoth <- which(!is.na(dataRatio_filled[2,]) & 
                         !is.na(dataRatio_filled[1,]))
      
      if(length(colMissing)==0) {
        # not nothing
      } else{
        # closest column where data for both
        closestBoth <- colBoth[which.min(abs(colBoth - colMissing[1]))]
        
        dataRatio_filled[2:nrow(dataRatio_filled),(colMissing)] <- 
          as.numeric((dataRatioSel[2,closestBoth]/dataRatioSel[1,closestBoth])) *
          dataRatioSel[1,closestBoth]
      }
      
    }
    
    
    
    
  }
  
  # combine
  
  dataRatio_filled[dataRatio_filled == 0] = NA
  
  if(mode %in% c("combine","split")){
    # in case of combine and split, also the subnat with the data will change
    rowFilling <- which(subnat_gis_combined_ratio$GID_1 %in% c(as.character(GID1_data),as.character(GID1_missing)) )
    
    subnat_gis_combined_ratio[rowFilling,8:ncol(subnat_gis_combined_ratio)] <- 
      dataRatio_filled[1:nrow(dataRatio_filled),]
  }else{
    # in case of missing and country average, only the subnat with missing data will be changed
    rowFilling <- which(subnat_gis_combined_ratio$GID_1 %in% as.character(GID1_missing) )
    
    subnat_gis_combined_ratio[rowFilling,8:ncol(subnat_gis_combined_ratio)] <- 
      dataRatio_filled[2:nrow(dataRatio_filled),]
  }
}

# write output

subnat_gis_combined_ratio <-  subnat_gis_combined_ratio %>% 
  arrange(cntry_code,Subnat)

write_csv(subnat_gis_combined_ratio, "../results/births_ratio_filled.csv")













