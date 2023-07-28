
#library(raster)
library(terra)
library(sf)
library(dplyr)
library(tmap)
library(tidyr)
library(RCurl)
library(openxlsx)
library(Kendall)
library(scico)
library(rnaturalearth)
library(rmapshaper)
library(readr)



# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# read data

cntryID <- read_csv("data_in/countries_codes_and_coordinates.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  select(cntry_code,iso2,iso3,Country) %>% 
  mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2))

sf_cntry <- read_sf("data_in/gadm_level0.gpkg")%>%
  #st_drop_geometry() %>% 
  select(GID_0,NAME_0) %>% 
  rename(iso3 = GID_0) %>% 
  # merge Aland to Finland
  mutate(iso3 = ifelse(iso3 == 'ALA', 'FIN', iso3)) %>% 
  mutate(NAME_0 = ifelse(NAME_0 == 'Åland', 'Finland', NAME_0)) %>% 
  left_join(cntryID,by='iso3') %>% 
  select(Country,iso3,cntry_code) %>% 
  arrange(iso3)


# # simplify the shapefile
# sf_cntrySml <- rmapshaper::ms_simplify(sf_cntry, keep = 0.05, keep_shapes = T) %>%
#   st_as_sf() 


sf_gadm_lev1 <- read_sf( 'data_in/gadm_lev1.gpkg' ) %>% 
  #st_drop_geometry() %>% 
  select(GID_0,NAME_0,NAME_1,GID_1) %>% 
  rename(iso3 = GID_0) %>% 
  rename(Subnat = NAME_1) %>% 
  mutate(nmbr = row_number()) %>%
  #filter(iso3 != 'ALA') %>% # remove Aland
  left_join(cntryID,by='iso3') %>% 
  select(Country,iso3,cntry_code,GID_1,nmbr) %>% 
  arrange(iso3)

# sf_gadm_lev1 <- st_read("data_in/gadm_lev1.gpkg") %>% 
#   mutate(nmbr = row_number()) %>%
#   st_as_sf() 

sf_gadm_lev2 <- read_sf( 'data_in/gadm_lev2.gpkg' ) %>% 
  #st_drop_geometry() %>% 
  select(GID_0,NAME_0,NAME_1,GID_1, NAME_2,GID_2) %>% 
  rename(iso3 = GID_0) %>% 
  #rename(Subnat = NAME_1) %>% 
  mutate(nmbr2 = row_number()) %>%
  #filter(iso3 != 'ALA') %>% # remove Aland
  left_join(cntryID,by='iso3') %>% 
  select(Country,iso3,cntry_code,GID_1,nmbr2, GID_2) %>% 
  arrange(iso3)

noGeom_gadm_lev2 <- sf_gadm_lev2 %>% 
  st_drop_geometry()

write_csv(noGeom_gadm_lev2,"data_out/noGeom_gadm_lev2.csv")

sf_gadm_lev2 <- sf_gadm_lev2 %>% 
  select(-GID_2) # this was added for file above but not needed in rest of the code


# check if national raster is already created, and if not create it

if(file.exists('data_in/gadm_lev0_5arcmin.tif')){
  # load file
  r_adm_lev0_5arcmin <- rast('data_in/gadm_lev0_5arcmin.tif')
}else{
  # create it
  #create ref raster
  ref_raster_5arcmin <- rast(ncol=360*12, nrow=180*12)
  ref_raster_1arcmin <- rast(ncol=360*60, nrow=180*60)
  
  # to terra polygon format
  v_adm_lev0 <- terra::vect(as(sf_cntry, "Spatial")) 
  
  # rasterise polygon file 
  r_adm_lev0_1arcmin <- terra::rasterize(v_adm_lev0,ref_raster_1arcmin,field="cntry_code")
  
  #plot(adminBoundary_raster_1arcmin)
  
  # aggregate to 5 arc-min
  r_adm_lev0_5arcmin <- terra::aggregate(r_adm_lev0_1arcmin,fact=5,fun=modal,na.rm=T)
  
  r_adm_lev0_5arcmin[is.nan(r_adm_lev0_5arcmin)] = NA
  
  writeRaster(r_adm_lev0_5arcmin,'data_in/gadm_lev0_5arcmin.tif',  gdal="COMPRESS=LZW",overwrite=TRUE)
}

# check if subnational raster is already created, and if not create it

if(file.exists('data_in/gadm_lev1_5arcmin.tif')){
  # load file
  r_gadm_lev1_5arcmin <- rast('data_in/gadm_lev1_5arcmin.tif')
}else{
  # create it
  
  #create ref raster
  ref_raster_5arcmin <- rast(ncol=360*12, nrow=180*12)
  ref_raster_1arcmin <- rast(ncol=360*60, nrow=180*60)
  
  # to terra polygon format
  v_gadm_lev1 <- terra::vect(as(sf_gadm_lev1, "Spatial")) 
  
  # rasterise polygon file 
  r_gadm_lev1_1arcmin <- terra::rasterize(v_gadm_lev1,ref_raster_1arcmin,field="nmbr")
  
  #plot(adminBoundary_raster_1arcmin)
  
  # aggregate to 5 arc-min
  r_gadm_lev1_5arcmin <- terra::aggregate(r_gadm_lev1_1arcmin,fact=5,fun=modal,na.rm=T)
  
  r_gadm_lev1_5arcmin[is.nan(r_gadm_lev1_5arcmin)] = NA
  
  writeRaster(r_gadm_lev1_5arcmin,'data_in/gadm_lev1_5arcmin.tif',  gdal="COMPRESS=LZW",overwrite=TRUE)
}

if(file.exists('data_in/gadm_lev2_5arcmin.tif')){
  # load file
  r_gadm_lev2_5arcmin <- rast('data_in/gadm_lev2_5arcmin.tif')
}else{
  # create it
  
  #create ref raster
  ref_raster_5arcmin <- rast(ncol=360*12, nrow=180*12)
  ref_raster_1arcmin <- rast(ncol=360*60, nrow=180*60)
  
  # to terra polygon format
  v_gadm_lev2 <- terra::vect(as(sf_gadm_lev2, "Spatial")) 
  
  # rasterise polygon file 
  r_gadm_lev2_1arcmin <- terra::rasterize(v_gadm_lev2,ref_raster_1arcmin,field="nmbr2")
  
  #plot(adminBoundary_raster_1arcmin)
  
  # aggregate to 5 arc-min
  r_gadm_lev2_5arcmin <- terra::aggregate(r_gadm_lev2_1arcmin,fact=5,fun=modal,na.rm=T)
  
  r_gadm_lev2_5arcmin[is.nan(r_gadm_lev2_5arcmin)] = NA
  
  writeRaster(r_gadm_lev2_5arcmin,'data_in/gadm_lev2_5arcmin.tif',  gdal="COMPRESS=LZW",overwrite=TRUE)
}

#### functions ----


# function to fill NA areas with closest value
fillmode <- function(v, na.rm) {  
  if (is.na(v[[5]])) {
    uniqv <- unique(v)
    uniqv <- uniqv[!is.na(uniqv)]
    fillval <- uniqv[which.max(tabulate(match(v, uniqv)))]
    return (fillval)
  } else {
    return (v[[5]])
  }
  
}

myFun_cntryData2raster <- function(data_in,variableName){
  # data_in <- ratio_r_pop
  # predefine raster stack
  st_ratio <-  r_GADM0
  # data in long format
  names(data_in) <- c('GADM_code',timestep)
  data_in_long <- data_in %>% 
    pivot_longer(!c('GADM_code'), 
                 names_to = "year", 
                 values_to = 'inData') 
  
  iYear = 2000
  for (iYear in timestep) {
    temp <-  data_in_long %>% 
      filter(year == iYear) %>% 
      dplyr::select(c(inData,'GADM_code'))
    temp_id <-  as.numeric(temp$GADM_code)
    temp_v <- as.numeric(temp$inData)
    
    # reclassify
    temp_raster <- classify(r_GADM0,
                            cbind(temp_id, temp_v))
    
    # countries without data to NA
    temp_raster[temp_raster>max(temp_v)] <- NA
    
    st_ratio <- c(st_ratio,temp_raster)
  } 
  
  # remove first layer with ids
  st_ratio_data <- subset(st_ratio,2:nlyr(st_ratio))
  names(st_ratio_data) <- paste0(variableName,timestep)
  
  #writeRaster(st_ratio_data,paste0('results/ratioRaster_',variableName,'.tif'),  gdal="COMPRESS=LZW",overwrite=TRUE)
  
  return(st_ratio_data)
  
}



#### LandScan population -----

pathLS <- "/Volumes/Kummu_GIS/datasets/LandScan"
zipList <- list.files(path = pathLS, pattern='.zip$', all.files=TRUE, full.names=T)
ref_raster_30arcsec <- rast(ncol=360*120, nrow=180*120)
# read year 2000
iYear = 2000
fileLS <- zipList[i+1]
fLS2000 <- utils::unzip(fileLS, 
                    exdir = paste0(pathLS,'/exdir'))
rastLS <- rast(paste0(pathLS,'/exdir/LandScan Global ',as.character(iYear),'/lspop',as.character(iYear),'/w001001.adf')) 

r_LScollection <- rastLS

# read others and add to rast
for (i in 1:(length(zipList)-1)) {
#  for (i in 17:19) {
  iYear = 2000 + i
  
  fileLS <- zipList[i+1]
  
  #fnames = as.character(utils::unzip(fileLS, list = TRUE)$Name)
  #LandScan Global 2000/lspop2000/w001001.adf
  
  fLS <- utils::unzip(fileLS, 
                      #paste0('LandScan Global ',as.character(iYear),'/lspop',as.character(iYear),'/w001001.adf'),
                      exdir = paste0(pathLS,'/exdir'))
  
  if(iYear %in% c(2016,2019)){
    rastLS <- rast(paste0(pathLS,'/exdir','/lspop',as.character(iYear),'/w001001.adf')) 
  } else {
    rastLS <- rast(paste0(pathLS,'/exdir/LandScan Global ',as.character(iYear),'/lspop',as.character(iYear),'/w001001.adf')) 
  }
  #rastLS <- rast('/Volumes/Kummu_GIS/datasets/LandScan/landScan2016.tif')
  # crop to min extent
  rastLS_crop <- crop(rastLS, subset(r_LScollection,1))
  r_LScollection <- crop(r_LScollection,rastLS)
  #global(rastLS_crop,fun='sum',na.rm=T)
  r_LScollection <- c(r_LScollection,rastLS_crop)
}

# r_LScollection <- c(subset(rast(paste0(pathLS,'/LandScan30arcsec_2000-2019.tif')),1:16),
#                            rastLS_crop,
#                            subset(rast(paste0(pathLS,'/LandScan30arcsec_2000-2019.tif')),18:20))

#write raster
names(r_LScollection) <- paste0('popCount',2000:2019)
writeRaster(r_LScollection,paste0(pathLS,'/LandScan30arcsec_2000-2019_upd.tif'),  gdal="COMPRESS=LZW",overwrite=TRUE)

#global(subset(r_LScollection,17),fun='sum',na.rm=T)

# delete unzipped files to save disk space
do.call(file.remove, list(list.files(paste0(pathLS,'/exdir'), full.names = TRUE, all.files=T)))

# aggregate to 5 arc-min
r_LScollection_5arcmin <- terra::aggregate(r_LScollection,fact=10,fun=sum,na.rm=T)

#global(r_LScollection_5arcmin,fun='sum',na.rm=T)

writeRaster(r_LScollection_5arcmin,paste0(pathLS,'/LandScan5arcmin_2000-2019.tif'),  gdal="COMPRESS=LZW",overwrite=TRUE)


#### harmonise  data with national scale data -----

# read country tabulated data
t_cntryPop <- read_csv("results/national_pop_total.csv") 
names(t_cntryPop) <- c('Country',  'iso3',  'GADM_code',paste0('yr',1990:2020))
t_cntryBirthRatio <- read_csv("results/national_births_ratio_interp.csv")
t_cntryDeathRatio <- read_csv("results/national_deaths_ratio_interp.csv")

# read cntry IDs
cntryID <- read_csv("data_in/countries_codes_and_coordinates.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  select(cntry_code,iso2,iso3,Country) %>% 
  mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2))

# expand to NA areas, as land mask different in pop file
# run focal with the fillmode function
r_GADM0 <- terra::focal(r_adm_lev0_5arcmin, w = 3, fun = fillmode, na.rm = FALSE)


#####  population data -----

r_birthRatio <- rast('results/Brths_downscaled_2000_2019_UPDATED.tif')

# worldpop
r_pop <- rast('data_in/ppp_2000_2020_5arcmin_corrected_fixed.tif') 

r_popExt <- extend(r_pop,ext(r_birthRatio))
# 
# writeRaster(r_pop,paste0('results/r_r_popTEST.tif'),  gdal="COMPRESS=LZW",overwrite=TRUE)
# 
# qtm(subset(r_pop,1))

zonal_r_pop <- terra::zonal(r_popExt,r_GADM0,fun = sum, na.rm=T) %>% 
  as_tibble()%>% 
  rename(GADM_code = cntry_code) %>% 
  left_join(t_cntryPop)

# divide country population based on UN and WB stats with the one based on gridded data
ratio_r_pop <- zonal_r_pop[paste0('yr',2000:2020)] / zonal_r_pop[paste0('ppp_',2000:2020)] 
# add GADM code to the table
ratio_r_pop <-  as_tibble(cbind(zonal_r_pop['GADM_code'],ratio_r_pop))

# ratio to raster
timestep <- c(2000:2020)
r_ratio_r_pop <- myFun_cntryData2raster(ratio_r_pop,'cntryPop')
# 
# writeRaster(r_ratio_r_pop,paste0('results/r_r_ratio_r_popTEST.tif'),  gdal="COMPRESS=LZW",overwrite=TRUE)

# multiply the pop data with ratio raster  
r_popHarm <- r_ratio_r_pop * r_popExt

#writeRaster(r_popHarm,paste0('results/r_worldpopHarmonised_v2TEST.tif'),  gdal="COMPRESS=LZW",overwrite=TRUE)
#qtm(subset(r_popHarm,1))

# if harmonised population is NA but r_pop not, then use r_pop
r_popHarm[is.na(r_popHarm) & !is.na(r_popExt)] <- r_popExt

qtm(subset(r_popHarm,1))

writeRaster(r_popHarm,paste0('results/r_worldpopHarmonised.tif'),  gdal="COMPRESS=LZW",overwrite=TRUE)

#global(r_popHarm,fun="sum",na.rm=T)

#### harmonise birth and death ratios ----

# read raster data

# births and deaths per 1000 people
r_birthRatio <- rast('results/Brths_downscaled_2000_2019_UPDATED.tif')
r_deathRatio <- rast('results/Dths_downscaled_2000_2019_UPDATED.tif')

r_popHarm <- rast('results/r_worldpopHarmonised.tif')

# land mask is not fully compatible with pop data, so we expand to NA values and then mask with pop data
#r_birthRatio_buff <- terra::focal(r_birthRatio, w = 3, fun = fillmode, na.rm = FALSE)

r_birthRatio_buff <- subset(r_birthRatio,1)
for (i in 1:terra::nlyr(r_birthRatio)) {
  i_r_birthRatio_buff <- terra::focal(subset(r_birthRatio,i), w = 3, fun = fillmode, na.rm = FALSE)
  terra::add(r_birthRatio_buff) <- i_r_birthRatio_buff
}
r_birthRatio_buff_sub <- subset(r_birthRatio_buff,2:terra::nlyr(r_birthRatio_buff))

# mask to pop data
r_birthRatio_buff_sub[is.na(subset(r_popHarm,1:terra::nlyr(r_birthRatio_buff_sub)))] <- NA

#writeRaster(r_birthRatio_buff_sub,paste0('results/test_','birthHarm','.tif'),  gdal="COMPRESS=LZW",overwrite=TRUE)

# pop weighted birth ratio
r_birthRatioPop <- r_birthRatio_buff_sub * subset(r_popHarm,1:20) 

zonal_r_birthRatioPop <- terra::zonal(r_birthRatioPop,r_GADM0,fun = sum, na.rm=T) %>% 
  as_tibble()%>% 
  rename(GADM_code = cntry_code) %>% 
  left_join(t_cntryPop) %>% 
  select(-c(Country,iso3)) %>% 
  left_join(t_cntryBirthRatio)

# divide country birth ratio based on UN and WB stats with the one based on gridded data
ratio_r_birthRatio <- zonal_r_birthRatioPop[paste0(2000:2019)] / 
  # birth ratio based on pop weighted gridded birth ratio
  (zonal_r_birthRatioPop[paste0('Brths_downscaled_2000_2019_UPDATED_',1:20)] / zonal_r_birthRatioPop[paste0('yr',2000:2019)])

# join GADM code back to the table
ratio_r_birthRatio <- as_tibble(cbind(zonal_r_birthRatioPop['GADM_code'],ratio_r_birthRatio))

# ratio to raster
timestep <- c(2000:2019)
r_ratio_r_birthRatio <- myFun_cntryData2raster(ratio_r_birthRatio,'cntryBirthRatio')

# multiply the pop data with ratio raster  
r_birthRatioHarm <- r_ratio_r_birthRatio * r_birthRatio_buff_sub

# if harmonised data is NA but original not, then use original
r_birthRatioHarm[is.na(r_birthRatioHarm)] <- r_birthRatio_buff_sub

writeRaster(r_birthRatioHarm,paste0('results/ratioRaster_','birthHarm','.tif'),  gdal="COMPRESS=LZW",overwrite=TRUE)


# r1 <- r2 <- rast(ncols=6, nrows=6)
# values(r1) <- 1:ncell(r1)
# values(r2) <- 1:ncell(r1)*2
# 
# r1[4,5] <- NA
# plot(r1)
# 
# r1[is.na(r1)] <- r2
# plot(r1)
# r3 <- cover(r1, r2, values = NA)
# plot(r3)


### pop weighted death ratio


# land mask is not fully compatible with pop data, so we expand to NA values and then mask with pop data

r_deathRatio_buff <- subset(r_deathRatio,1)
for (i in 1:terra::nlyr(r_deathRatio)) {
  i_r_deathRatio_buff <- terra::focal(subset(r_deathRatio,i), w = 3, fun = fillmode, na.rm = FALSE)
  terra::add(r_deathRatio_buff) <- i_r_deathRatio_buff
}
r_deathRatio_buff_sub <- subset(r_deathRatio_buff,2:terra::nlyr(r_deathRatio_buff))

# mask to pop data
r_deathRatio_buff_sub[is.na(subset(r_popHarm,1:terra::nlyr(r_deathRatio_buff_sub)))] <- NA


# pop weighted ratio
r_deathRatioPop <- r_deathRatio_buff_sub * r_popHarm 

# zonal
zonal_r_deathRatioPop <- terra::zonal(r_deathRatioPop,r_GADM0,fun = sum, na.rm=T) %>% 
  as_tibble()%>% 
  rename(GADM_code = cntry_code) %>% 
  left_join(t_cntryPop) %>% 
  select(-c(Country,iso3)) %>% 
  left_join(t_cntryDeathRatio)

# divide country birth ratio based on UN and WB stats with the one based on gridded data
ratio_r_deathRatio <- zonal_r_deathRatioPop[paste0(2000:2019)] / 
  # birth ratio based on pop weighted gridded birth ratio
  (zonal_r_deathRatioPop[paste0('lyr',1:20)] / zonal_r_deathRatioPop[paste0('yr',2000:2019)])

# join GADM code back to the table
ratio_r_deathRatio <- as_tibble(cbind(zonal_r_deathRatioPop['GADM_code'],ratio_r_deathRatio))

# ratio to raster
timestep <- c(2000:2019)
r_ratio_r_deathRatio <- myFun_cntryData2raster(ratio_r_deathRatio,'cntryDeathRatio')

# multiply the pop data with ratio raster  
r_deathRatioHarm <- r_ratio_r_deathRatio * r_deathRatio

# if harmonised data is NA but original not, then use original
r_deathRatioHarm[is.na(r_deathRatioHarm)] <- r_deathRatio_buff_sub

writeRaster(r_deathRatioHarm,paste0('results/ratioRaster_','deathHarm','.tif'),  gdal="COMPRESS=LZW",overwrite=TRUE)


#### total births and deaths ----

# read harmonised raster data

# births and deaths per 1000 people
r_birthRatio <- rast('results/ratioRaster_birthHarm.tif')
r_deathRatio <- rast('results/ratioRaster_deathHarm.tif')

# worldpop
r_pop <- rast('results/r_worldpopHarmonised.tif') #%>%
  #project(.,subset(r_birthRatio,1))

# calculate total births and deaths

r_birthTotal <- r_birthRatio * subset(r_pop,1:20) / 1000
r_deathTotal <- r_deathRatio * subset(r_pop,1:20)  / 1000

# global(r_birthTotal, fun = "sum", na.rm=TRUE)
# global(r_deathTotal, fun = "sum", na.rm=TRUE)


# calculate natural pop change compared to year 2000 population

r_birthTotal_sum <- sum(r_birthTotal)
r_deathTotal_sum <- sum(r_deathTotal)

r_naturalPopChange <- (r_birthTotal_sum-r_deathTotal_sum)/subset(r_pop,1) * 1000

writeRaster(r_naturalPopChange,'results/naturalPopChange.tif',  gdal="COMPRESS=LZW",overwrite=TRUE)


# reported population change

r_reportedPopChange <- (subset(r_pop,21)-subset(r_pop,1))/subset(r_pop,1) * 1000
writeRaster(r_reportedPopChange,'results/reportedPopChange.tif',  gdal="COMPRESS=LZW",overwrite=TRUE)

#### net-migration ----

# net-migration = total population change - (births - deaths)

r_popChange <- subset(r_pop,2:21) - subset(r_pop,1:20)
r_birthMinusDeath <- subset(r_birthTotal,1:20) - subset(r_deathTotal,1:20)

r_netMigration <- r_popChange - r_birthMinusDeath
names(r_netMigration) <- c(paste0("netMgr",2001:2020))

writeRaster(r_netMigration,'results/netMgr_2001_2020_v3.tif',  gdal="COMPRESS=LZW",overwrite=TRUE)

# total net-migration per 1000 people in 2000
r_netMigration_sum <- sum(r_netMigration) / subset(r_pop,1) * 1000
writeRaster(r_netMigration_sum,'results/netMgr_sum_2001_2020_v3.tif',  gdal="COMPRESS=LZW",overwrite=TRUE)

# calculate n yr net-migration

myFun_nYrSum <- function(r_in,n_yearSum,maxTime,FUN) {
  # FUN = 'sum'
  # n_yearSum=3
  # r_in <- r_netMigration
  # maxTime = 20
  r_in3yr <- subset(r_in,1)
  for (iTime in 1: (ceiling(nlyr(r_in)/n_yearSum))) {
    # define timesteps
    iSteps <- ((iTime-1)*3+1):(iTime*3)
    iSteps <- iSteps[iSteps<(maxTime+1)] 

    if(FUN == "sum"){
    tempIn <- sum(subset(r_in,iSteps),na.rm=T)
    } else {
      tempIn <- mean(subset(r_in,iSteps),na.rm=T)
    }
    r_in3yr <- c(r_in3yr,tempIn)
    
  }
  r_in3yr <- subset(r_in3yr,2: (ceiling(nlyr(r_in)/n_yearSum)+1))
  
}

# 3yr
timeSteps <- seq(2002,2020,3)

r_netMigration3yr <- myFun_nYrSum(r_netMigration,n_yearSum=3,maxTime=20,FUN='sum')
names(r_netMigration3yr) <- c(paste0('netMgr',timeSteps))

r_pop3yr <- myFun_nYrSum(subset(r_pop,2:21),3,20,FUN='mean')
names(r_pop3yr) <- c(paste0('pop',timeSteps))

writeRaster(r_netMigration3yr,'results/netMgr_2001_2020_v3_3yrSum.tif',  gdal="COMPRESS=LZW",overwrite=TRUE)

writeRaster(r_pop3yr,'results/r_worldpopHarmonised_3yrMean.tif',  gdal="COMPRESS=LZW",overwrite=TRUE)


# 5yr
timeSteps <- seq(2003,2018,5)

r_netMigration5yr <- myFun_nYrSum(r_netMigration,n_yearSum=5,maxTime=20,FUN='sum')
names(r_netMigration5yr) <- c(paste0('netMgr',timeSteps))

r_pop5yr <- myFun_nYrSum(subset(r_pop,2:21),5,20,FUN='mean')
names(r_pop5yr) <- c(paste0('pop',timeSteps))

writeRaster(r_netMigration5yr,'results/netMgr_2001_2020_v3_5yrSum.tif',  gdal="COMPRESS=LZW",overwrite=TRUE)

writeRaster(r_pop5yr,'results/r_worldpopHarmonised_5yrMean.tif',  gdal="COMPRESS=LZW",overwrite=TRUE)




# # smoothing code
# 
# # subnat data
# subnat_births <- as_tibble(terra::zonal(r_birthTotal,r_gadm_lev1_5arcmin, fun = sum, na.rm=T))
# subnat_deaths <- as_tibble(terra::zonal(r_deathTotal,r_gadm_lev1_5arcmin, fun = sum, na.rm=T))
# subnat_pop <- as_tibble(terra::zonal(r_pop,r_gadm_lev1_5arcmin, fun = sum, na.rm=T))
# 
# # smooth the pop raster as there are some strange jumps particularly in years 2000 and 2010
# #x <- calc(stack(r_pop), function(x) movingFun(x, 3, mean, na.rm=T))
# #y <- st1 - x
# 
# # see example https://rdrr.io/cran/spatialEco/man/smooth.time.series.html
# sf_gadm0_france <- sf_cntry %>% 
#   filter(Country == 'France') 
# v_gadm0_france <- terra::vect(as(sf_gadm0_france, "Spatial")) 
# r_popFrance <- terra::mask(r_pop,v_gadm0_france)
# r_popFrance <- terra::crop(r_popFrance,v_gadm0_france)
# 
# r_popFrance$cntryPop2000 <- NA
# r_popFrance$cntryPop2010 <- NA
# 
# pop.smooth.France <- spatialEco::smooth.time.series(stack(r_popFrance), f = 1, smooth.data = T)  
# writeRaster(pop.smooth.France,'results/popFranceSmoothed.tif',  gdal="COMPRESS=LZW",overwrite=TRUE)
# 
