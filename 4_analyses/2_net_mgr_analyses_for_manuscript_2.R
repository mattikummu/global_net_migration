


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

shp_cntry <- st_read("../data_in_gpkg/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp")
# simplify the shapefile
shp_cntrySml <- rmapshaper::ms_simplify(shp_cntry, keep = 0.25, keep_shapes = T) %>%
  st_as_sf() 

# clear non-needed columns
shp_cntrySml[,2:7] = NULL

st_is_valid(shp_cntrySml)



##### function to calculate trend slope ----------

#variableName <- "births"

source('../functions/f_slopeRaster.R')


##### apply change function ----------

rasterChangeBirths <- f_slopeRaster(variableName = "birthHarm",
                                    path = '../results/ratioRaster_',
                                    startYear = 2000,
                                    endYear = 2019, 
                                    refYear = 2000)
rasterChangeDeaths <- f_slopeRaster("deathHarm",'../results/ratioRaster_',2000,2019, 2000)

rasterChangeLifeRatio <- f_slopeRaster("ratioAvgAge_lifeExp_v2",'../data_in_downscaling/', 2000,2019, 2000)


##### function to plot -------

# Draw a map of a socio-economic index of choice
#
# @param r_index: a raster to be plotted
# @param index_label: label that will be placed as the legend title
# @param colorpal: color palette to be used, ordered from low to high value
# @param breakvals: break values to be drawn in legend
# @param color_midpoint: TRUE if the color scale has a midpoint, NULL by default
#                        (no midpoint in color scale)
# @param tocrs: proj4 string of CRS to which the raster is projected if given
#               (by default, no projection is done)
#
# @return tmap object of the index given


source('../functions/f_mappingTMAP.R')


source('../functions/f_currentMap.R')


source('../functions/f_changeMap.R')




##### plot ------



# downscaling input

# non-populated areas to NA in ppp_2000_2020_5arcmin_scaledbycountry

r_popScaled <- rast('../data_in_downscaling/ppp_2000_2020_5arcmin_scaledbycountry.tif')
r_ratioAvgAge_lifeExp_v2 <- rast('../data_in_downscaling/ratioAvgAge_lifeExp_v2.tif')

r_popScaled[is.na(r_ratioAvgAge_lifeExp_v2)] <- NA

writeRaster(r_popScaled,'../data_in_downscaling/ppp_2000_2020_5arcmin_scaledbycountry.tif',  gdal="COMPRESS=LZW",overwrite=TRUE)

p_avgAge <- f_currentMap("../results/","AvgAge_v2_interp", "roma", 0, .5, -1, seq(15, 55, 5))
p_lifeExp <- f_currentMap(pathVar="../data_in_downscaling/",variableName = "hdiComponent_lifexpUpd", "roma", 0, .5, -1, seq(50, 82.5, 2.5))

p_ratioAvgAge_lifeExp <- f_currentMap(pathVar="../data_in_downscaling/","ratioAvgAge_lifeExp_v2", "roma", 0, .5, -1, seq(0.2, 0.7, 0.05))

p_fertileFemaleShr <- f_currentMap(pathVar="../data_in_downscaling/","fertileFemaleShr2000_2020_v2_interp", "roma", 0, .5, -1, seq(0.1, 0.35, 0.025))

p_HDI <- f_currentMap(pathVar="../data_in_downscaling/","hdiComponent_shdi", "roma", .5, 1, 1, seq(0.3, 1, 0.05))

p_scaledPop <- f_currentMap(pathVar="../data_in_downscaling/","ppp_2000_2020_5arcmin_scaledbycountry", "roma", .5, 1, 1, seq(0, 1, 0.05))

# combine plots
p_downscalingInput <- tmap_arrange(p_HDI, p_scaledPop, p_fertileFemaleShr, p_ratioAvgAge_lifeExp, ncol=2, nrow=2)

tmap_save(p_downscalingInput, filename = "../figures/downscalingInput_jan2022.pdf", width = 180, height = 150, units = "mm")


# current status
#variableName <- "births"

p_currentBirths <- f_currentMap("../results/","ratioRaster_births", "roma", 0.5, 1, 1, seq(0, 50, 2.5))
p_currentDeaths <- f_currentMap("../results/","ratioRaster_deaths", "roma", 0, .5, -1, seq(0, 20, 1))

# p_birthDeathResults <- tmap_arrange(p_currentBirths, p_currentDeaths, ncol=2, nrow=1)
# 
# tmap_save(p_birthDeathResults, filename = "../figures/birthDeathResults_jan2022.pdf", width = 180, height = 150, units = "mm")


# downscaled results
#variableName <- "births"

p_downScaledBirths <- f_currentMap("../results/","ratioRaster_birthHarm", "roma", 0.5, 1, 1, seq(0, 50, 2.5))
p_downScaledDeaths <- f_currentMap("../results/","ratioRaster_deathHarm", "roma", 0, .5, -1, seq(0, 20, 1))

# p_changeBirths <- f_changeMap("rasterChange_births", "roma", 0,1,1,seq(-10, 10, 2))
# p_changeDeaths <- f_changeMap("rasterChange_deaths", "roma", 0,1,1,seq(-10, 10, 2))

#p_birthDeathResults <- tmap_arrange(p_downScaledBirths, p_changeBirths, p_downScaledDeaths, p_changeDeaths, ncol=2, nrow=2)
p_ds_birthDeathResults <- tmap_arrange(p_currentBirths,p_currentDeaths,
                                       p_downScaledBirths, p_downScaledDeaths, ncol=2, nrow=2)
tmap_save(p_ds_birthDeathResults, filename = "../figures/reported_downscaled_birthDeathResults_jan2023.pdf", width = 180, height = 150, units = "mm")



# plot natural and reported population change

p_natPopChange <- f_changeMap("naturalPopChange", "roma", 0,1,1,seq(-500, 500, 50))
p_reportedPopChange <- f_changeMap("reportedPopChange", "roma", 0,1,1,seq(-500, 500, 50))

# plot grid-level net migration
p_netMgrSum <- f_changeMap("netMgr_sum_2001_2020_v3", "roma", 0,1,1,seq(-500, 500, 50))

p_ds_popChange <- tmap_arrange(p_natPopChange,p_netMgrSum, p_reportedPopChange, ncol=2, nrow=2)
tmap_save(p_ds_popChange, filename = "../figures/populationChange_jan2023.pdf", width = 180, height = 150, units = "mm")







# ratio avg age life exp

p_currentRatioAgeExp <- f_currentMap("../data_in_downscaling/","ratioAvgAge_lifeExp_v2", "roma", 0, .5, -1, seq(0.2, 0.7, 0.05))
p_changeRatioAgeExp <- f_changeMap("rasterChange_ratioAvgAge_lifeExp_v2", "roma", 0,1,-1,seq(-0.1, 0.1, .025))

p_RatioAgeExp <- tmap_arrange(p_currentRatioAgeExp, p_changeRatioAgeExp, ncol=1, nrow=2)

tmap_save(p_RatioAgeExp, filename = "../figures/p_RatioAgeExp.pdf", width = 180, height = 150, units = "mm")






##### input dataset plotting -----


# origin of data at cntry level

cntryID <- read_csv("../data_in/countries_codes_and_coordinates.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  select(cntry_code,iso2,iso3,Country) %>% 
  mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2))

GIS_dataOrigin_births <- read.xlsx("../data_in/cntry_metaData.xlsx",sheet = 'cntry_meta_births') %>% 
  as_tibble() %>% 
  mutate(birthDataOrigin = ifelse(SUBNAT_USED == "STAT","StatCompiler",NA)) %>%
  mutate(birthDataOrigin = ifelse(SUBNAT_USED == "EUROSTAT","EUROSTAT",birthDataOrigin)) %>%
  mutate(birthDataOrigin = ifelse(SUBNAT_USED == "OTHER","Census",birthDataOrigin)) %>% 
  mutate(birthDataOrigin = ifelse(is.na(SUBNAT_USED),"CntryLevel",birthDataOrigin)) %>% 
  select(GID_0,ID_0,birthDataOrigin)

GIS_dataOrigin_deaths <-  read.xlsx("../data_in/cntry_metaData.xlsx",sheet = 'cntry_meta_deaths') %>% 
  as_tibble() %>% 
  mutate(deathDataOrigin = ifelse(SUBNAT_USED == "OECD","OECD",NA)) %>%
  mutate(deathDataOrigin = ifelse(SUBNAT_USED == "EUROSTAT","EUROSTAT",deathDataOrigin)) %>%
  mutate(deathDataOrigin = ifelse(SUBNAT_USED == "OTHER","Census",deathDataOrigin)) %>%
  mutate(deathDataOrigin = ifelse(is.na(SUBNAT_USED),"CntryLevel",deathDataOrigin)) %>% 
  select(GID_0,ID_0,deathDataOrigin)

# check if simplified GADM0 gpkg exists, and if not, create it
if(!file.exists("../results/GADM0_raster.gpkg")){
  
  sfGADM_0 <- st_read("../data_in_gpkg/gadm_level0.gpkg") %>% 
    rename(iso3 = GID_0) %>% 
    filter(iso3 != "ALA") %>% 
    left_join(cntryID[,c(1,3)])
  
  # to terra polygon format
  vect_sfGADM_0 <- terra::vect(as(sfGADM_0, "Spatial")) 
  
  # rasterise polygon file 
  ref_raster_1arcmin <- rast(ncol=360*60, nrow=180*60)
  sfGADM_0_raster_1arcmin <- terra::rasterize(vect_sfGADM_0,ref_raster_1arcmin,field="cntry_code")
  # aggregate to 5 arc-min
  sfGADM_0_raster_5arcmin <- terra::aggregate(sfGADM_0_raster_1arcmin,fact=5,fun=modal,na.rm=T)
  
  sfGADM_0_raster_5arcmin[is.nan(sfGADM_0_raster_5arcmin)] = NA
  
  writeRaster(sfGADM_0_raster_5arcmin,paste0('../results/GADM0_raster.tif'),  gdal="COMPRESS=LZW",overwrite=TRUE)
  
  vectGADM_0 <- terra::as.polygons(x = sfGADM_0_raster_5arcmin, dissolve = T,
                                   values = T)
  terra::is.valid(vectGADM_0)
  #dir.create("../results/shps/", showWarnings = FALSE)
  writeVector(vectGADM_0, "../results/GADM0_raster.gpkg", overwrite=TRUE)
} 


sfVectGADM_0 <- st_read("../results/GADM0_raster.gpkg")

st_is_valid(sfVectGADM_0)



sfGADM_0_dataOrigin <- sfVectGADM_0 %>% 
  rename(ID_0 = cntry_code) %>% 
  left_join(GIS_dataOrigin_births) %>% 
  left_join(GIS_dataOrigin_deaths)  



# division of subnat units

# check if simplified GADM0 gpkg exist, and if not, create it
if(!file.exists("../results/vect_divBirths.gpkg")){
  vect_divBirths <- terra::as.polygons(x = rast("../results/adminBoundary_births_raster_5arcmin.tif"), dissolve = T,
                                       values = T)
  vect_divDeaths <- terra::as.polygons(x = rast("../results/adminBoundary_deaths_raster_5arcmin.tif"), dissolve = T,
                                       values = T)

  writeVector(vect_divBirths, "../results/vect_divBirths.gpkg")
  writeVector(vect_divDeaths, "../results/vect_divDeaths.gpkg")
}

sfVect_divBirths <- st_read("../results/vect_divBirths.gpkg")
sfVect_divDeaths <- st_read("../results/vect_divDeaths.gpkg")

# calculate number of people in each category
r_pop <- subset(rast("../results/r_worldpopHarmonised.tif"),21)
sfGADM_0_dataOrigin_pop <- sfGADM_0_dataOrigin %>% 
  st_drop_geometry() %>% 
  bind_cols( terra::extract(r_pop,vect(sfGADM_0_dataOrigin), fun=sum, na.rm=T) ) 

birthPopSource <- sfGADM_0_dataOrigin_pop %>% 
  group_by(birthDataOrigin) %>% 
  summarise(sumPop = sum(cntryPop2020, na.rm=T)) %>% 
  mutate(percOfTot = sumPop / sum(sumPop))

write_csv(birthPopSource, "../results/birthPopSource.csv")

deathPopSource <- sfGADM_0_dataOrigin_pop %>% 
  group_by(deathDataOrigin) %>% 
  summarise(sumPop = sum(cntryPop2020, na.rm=T))%>% 
  mutate(percOfTot = sumPop / sum(sumPop))

write_csv(deathPopSource, "../results/deathPopSource.csv")

# plot
plt_birthDataOrigin <- tm_shape(sfGADM_0_dataOrigin, projection = "+proj=robin") +
  tm_polygons(col = "birthDataOrigin",
          palette = "-RdYlBu",
          #labels = birthDataOrigin,
          contrast = c(0, 0.5))+
  tm_shape(sfVect_divBirths, projection = "+proj=robin") +
  tm_borders(col = "white",
             lwd = 0.1)+
  tm_layout(#main.title = "Origin of data",
    main.title.position = "center",
    legend.outside = TRUE,
    legend.outside.position = "right",
    frame = FALSE)

plt_deathDataOrigin <- tm_shape(sfGADM_0_dataOrigin, projection = "+proj=robin") +
  tm_polygons(col = "deathDataOrigin",
          palette = "-RdYlBu",
          #labels = birthDataOrigin,
          contrast = c(0, 0.5))+
  tm_shape(sfVect_divDeaths, projection = "+proj=robin") +
  tm_borders(col = "white",
             lwd = 0.1)+
  tm_layout(#main.title = "Origin of data",
    main.title.position = "center",
    legend.outside = TRUE,
    legend.outside.position = "right",
    frame = FALSE)


plt_dataOrigin <- tmap_arrange(plt_birthDataOrigin, plt_deathDataOrigin, 
                               ncol = 1 )

tmap_save(plt_dataOrigin,filename = "../figures/plt_dataOrigin_v3.pdf", width = 130, height = 95, units = "mm")









