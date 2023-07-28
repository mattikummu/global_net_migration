


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

shp_cntry <- st_read("data_in/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp")
# simplify the shapefile
shp_cntrySml <- rmapshaper::ms_simplify(shp_cntry, keep = 0.05, keep_shapes = T) %>%
  st_as_sf() 

# clear non-needed columns
shp_cntrySml[,2:7] = NULL

st_is_valid(shp_cntrySml)



##### function to calculate trend slope ----------

#variableName <- "births"

f_slopeRaster <- function(variableName,path,startYear,endYear, refYear) {
  
  start_year = startYear
  end_year = endYear
  
  subnatRaster <- stack(paste0(path,variableName,'.tif')) %>% 
    subset(.,c((start_year-refYear+1):(end_year-refYear+1)))
  
  time <- 1:nlayers(subnatRaster) 
  
  fun=function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$coefficients[2] }}
  
  # change per year
  raster.slope=rast(calc(subnatRaster, fun))
  
  # change over time period 
  nYears <- nlayers(subnatRaster) 
  raster.change = nYears * raster.slope
  
  writeRaster(raster.slope,paste0('results/rasterSlope_',variableName,'.tif'),  gdal="COMPRESS=LZW",overwrite=TRUE)
  writeRaster(raster.change,paste0('results/rasterChange_',variableName,'.tif'),  gdal="COMPRESS=LZW",overwrite=TRUE)
  
  return(raster.change)
}


##### apply change function ----------

rasterChangeBirths <- f_slopeRaster("birthHarm",'results/ratioRaster_', 2000,2019, 2000)
rasterChangeDeaths <- f_slopeRaster("deathHarm",'results/ratioRaster_',2000,2019, 2000)

rasterChangeLifeRatio <- f_slopeRaster("ratioAvgAge_lifeExp_v2",'results/', 2000,2019, 2000)


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
f_mappingTMAP <- function(r_index, index_label, colorpal, breakvals,
                          color_midpoint = NULL, tocrs = NA,
                          polygonFile){
  
  polygonFile <- terra::vect(as(polygonFile, "Spatial"))  
  
  # project to another crs
  if (!is.na(tocrs)){
    r_index <- terra::project(r_index, tocrs, mask = TRUE)
    polygonFile <- terra::project(polygonFile, tocrs) %>% 
      st_as_sf()
  }
  
  # create tmap object
  index_map <- tm_shape(r_index) +
    tm_raster(style = "fixed", # draw gradient instead of classified
              palette = colorpal,
              breaks = breakvals,
              title = index_label,
              midpoint = color_midpoint,
              #legend.reverse = TRUE,
              legend.is.portrait = FALSE) +
    tm_shape(polygonFile, projection = ) +
    tm_borders(col=NA,lwd = 0.5) +
    tm_layout(main.title.position = "left",
              legend.bg.color = TRUE,
              legend.outside = TRUE,
              legend.outside.position = "bottom",
              frame = FALSE)
  
  return (index_map)
  
}



f_currentMap <- function(variableName, 
                         colPalette = "roma",
                         colBegin = 0,
                         colEnd = 1, 
                         colDir = 1,
                         breaks){
  
  rastCurrent = rast(paste0('results/',variableName,'.tif')) %>% 
    subset(.,terra::nlyr(.)) # last layer
  
  ac_pal <- scico(n = 3, palette = colPalette, begin = colBegin, end = colEnd, direction = colDir)
  
  plt_ac_global <- f_mappingTMAP(r_index = rastCurrent,
                                 index_label = paste0(variableName," per 1000 people"),
                                 colorpal = ac_pal,
                                 breakvals = breaks,
                                 tocrs = "+proj=robin +over",
                                 polygonFile = shp_cntrySml)
  
}



f_changeMap <- function(variableName, 
                        colPalette = "roma",
                        colBegin = 0,
                        colEnd = 1, 
                        colDir = 1,
                        breaks){
  
  rastChange = rast(paste0('results/',variableName,'.tif')) %>% 
    subset(.,terra::nlyr(.)) # last layer
  
  ac_pal <- scico(n = 3, palette = colPalette, begin = colBegin, end = colEnd, direction = colDir)
  
  plt_ac_global <- f_mappingTMAP(r_index = rastChange,
                                 index_label = paste0("Change in ",variableName, " per 1000 people over 2000-2019"),
                                 colorpal = ac_pal,
                                 breakvals = breaks,
                                 color_midpoint = 0,
                                 tocrs = "+proj=robin +over",
                                 polygonFile = shp_cntrySml)
  
}


##### calculate some key figures ------






##### plot ------



# downscaling input

# non-populated areas to NA in ppp_2000_2020_5arcmin_scaledbycountry

r_popScaled <- rast('results/ppp_2000_2020_5arcmin_scaledbycountry.tif')
r_ratioAvgAge_lifeExp_v2 <- rast('results/ratioAvgAge_lifeExp_v2.tif')

r_popScaled[is.na(r_ratioAvgAge_lifeExp_v2)] <- NA

writeRaster(r_popScaled,'results/ppp_2000_2020_5arcmin_scaledbycountry.tif',  gdal="COMPRESS=LZW",overwrite=TRUE)

p_avgAge <- f_currentMap("AvgAge_v2_interp", "roma", 0, .5, -1, seq(15, 55, 5))
p_lifeExp <- f_currentMap("hdiComponent_lifexpUpd", "roma", 0, .5, -1, seq(50, 82.5, 2.5))

p_ratioAvgAge_lifeExp <- f_currentMap("ratioAvgAge_lifeExp_v2", "roma", 0, .5, -1, seq(0.2, 0.7, 0.05))

p_fertileFemaleShr <- f_currentMap("fertileFemaleShr2000_2020_v2_interp", "roma", 0, .5, -1, seq(0.1, 0.35, 0.025))

p_HDI <- f_currentMap("hdiComponent_shdi", "roma", .5, 1, 1, seq(0.3, 1, 0.05))

p_scaledPop <- f_currentMap("ppp_2000_2020_5arcmin_scaledbycountry", "roma", .5, 1, 1, seq(0, 1, 0.05))

# combine plots
p_downscalingInput <- tmap_arrange(p_HDI, p_scaledPop, p_fertileFemaleShr, p_ratioAvgAge_lifeExp, ncol=2, nrow=2)

tmap_save(p_downscalingInput, filename = "figures/downscalingInput_jan2022.pdf", width = 180, height = 150, units = "mm")


# current status
#variableName <- "births"

p_currentBirths <- f_currentMap("ratioRaster_births", "roma", 0.5, 1, 1, seq(0, 50, 2.5))
p_currentDeaths <- f_currentMap("ratioRaster_deaths", "roma", 0, .5, -1, seq(0, 20, 1))

# p_birthDeathResults <- tmap_arrange(p_currentBirths, p_currentDeaths, ncol=2, nrow=1)
# 
# tmap_save(p_birthDeathResults, filename = "figures/birthDeathResults_jan2022.pdf", width = 180, height = 150, units = "mm")


# downscaled results
#variableName <- "births"

p_downScaledBirths <- f_currentMap("ratioRaster_birthHarm", "roma", 0.5, 1, 1, seq(0, 50, 2.5))
p_downScaledDeaths <- f_currentMap("ratioRaster_deathHarm", "roma", 0, .5, -1, seq(0, 20, 1))

# p_changeBirths <- f_changeMap("rasterChange_births", "roma", 0,1,1,seq(-10, 10, 2))
# p_changeDeaths <- f_changeMap("rasterChange_deaths", "roma", 0,1,1,seq(-10, 10, 2))

#p_birthDeathResults <- tmap_arrange(p_downScaledBirths, p_changeBirths, p_downScaledDeaths, p_changeDeaths, ncol=2, nrow=2)
p_ds_birthDeathResults <- tmap_arrange(p_currentBirths,p_currentDeaths,
                                       p_downScaledBirths, p_downScaledDeaths, ncol=2, nrow=2)
tmap_save(p_ds_birthDeathResults, filename = "figures/reported_downscaled_birthDeathResults_jan2023.pdf", width = 180, height = 150, units = "mm")



# plot natural and reported population change

p_natPopChange <- f_changeMap("naturalPopChange", "roma", 0,1,1,seq(-500, 500, 50))
p_reportedPopChange <- f_changeMap("reportedPopChange", "roma", 0,1,1,seq(-500, 500, 50))

# plot grid-level net migration
p_netMgrSum <- f_changeMap("netMgr_sum_2001_2020_v3", "roma", 0,1,1,seq(-500, 500, 50))

p_ds_popChange <- tmap_arrange(p_natPopChange,p_netMgrSum, p_reportedPopChange, ncol=2, nrow=2)
tmap_save(p_ds_popChange, filename = "figures/populationChange_jan2023.pdf", width = 180, height = 150, units = "mm")







# ratio avg age life exp

p_currentRatioAgeExp <- f_currentMap("ratioAvgAge_lifeExp_v2", "roma", 0, .5, -1, seq(0.2, 0.7, 0.05))
p_changeRatioAgeExp <- f_changeMap("rasterChange_ratioAvgAge_lifeExp_v2", "roma", 0,1,-1,seq(-0.1, 0.1, .025))

p_RatioAgeExp <- tmap_arrange(p_currentRatioAgeExp, p_changeRatioAgeExp, ncol=1, nrow=2)

tmap_save(p_RatioAgeExp, filename = "figures/p_RatioAgeExp.pdf", width = 180, height = 150, units = "mm")



# rastRef = rast(paste0('results/','ratioAvgAge_lifeExp_v2','.tif')) %>%
#   subset(.,terra::nlyr(.))
# 
# rastTest = rast(paste0('results/','rasNLight_5arcmin_1990_2020','.tif')) %>%
#   #subset(.,terra::nlyr(.)) %>%
#   terra::project(.,rastRef)
# 
# firstRast = subset(rastTest,29)
# firstRast[1,1]
# rastTest[is.nan(rastTest)] = 0
# 
# writeRaster(rastTest,paste0('results/rasNLight_5arcmin_1990_2020.tif'),  gdal="COMPRESS=LZW",overwrite=TRUE)
# 
# tocrs = "+proj=robin +over"
# r_index <- terra::project(firstRast, tocrs, mask = TRUE)
# 
# qtm(r_index)
# 
# plot(r_index)


##### input dataset plotting -----


# origin of data at cntry level

cntryID <- read_csv("data_in/countries_codes_and_coordinates.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  select(cntry_code,iso2,iso3,Country) %>% 
  mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2))

GIS_dataOrigin_births <- read.xlsx("data_in/cntry_metaData.xlsx",sheet = 'cntry_meta_births') %>% 
  as_tibble() %>% 
  mutate(birthDataOrigin = ifelse(SUBNAT_USED == "STAT","StatCompiler",NA)) %>%
  mutate(birthDataOrigin = ifelse(SUBNAT_USED == "EUROSTAT","EUROSTAT",birthDataOrigin)) %>%
  mutate(birthDataOrigin = ifelse(SUBNAT_USED == "OTHER","Census",birthDataOrigin)) %>% 
  mutate(birthDataOrigin = ifelse(is.na(SUBNAT_USED),"CntryLevel",birthDataOrigin)) %>% 
  select(GID_0,ID_0,birthDataOrigin)

GIS_dataOrigin_deaths <-  read.xlsx("data_in/cntry_metaData.xlsx",sheet = 'cntry_meta_deaths') %>% 
  as_tibble() %>% 
  mutate(deathDataOrigin = ifelse(SUBNAT_USED == "OECD","OECD",NA)) %>%
  mutate(deathDataOrigin = ifelse(SUBNAT_USED == "EUROSTAT","EUROSTAT",deathDataOrigin)) %>%
  mutate(deathDataOrigin = ifelse(SUBNAT_USED == "OTHER","Census",deathDataOrigin)) %>%
  mutate(deathDataOrigin = ifelse(is.na(SUBNAT_USED),"CntryLevel",deathDataOrigin)) %>% 
  select(GID_0,ID_0,deathDataOrigin)

# check if simplified GADM0 gpkg exists, and if not, create it
if(!file.exists("results/GADM0_raster_fixGeom.gpkg")){
  
  sfGADM_0 <- st_read("data_in/gadm_level0.gpkg") %>% 
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
  
  writeRaster(sfGADM_0_raster_5arcmin,paste0('results/GADM0_raster.tif'),  gdal="COMPRESS=LZW",overwrite=TRUE)
  
  vectGADM_0 <- terra::as.polygons(x = sfGADM_0_raster_5arcmin, dissolve = T,
                                   values = T)
  
  dir.create("results/shps/", showWarnings = FALSE)
  writeVector(vectGADM_0, "results/shps/GADM0_raster.shp")
} 

# NOTE: MANUAL WORK IN QGIS HERE: then read in QGIS, fixed geometries and saved to gpkg
sfVectGADM_0 <- st_read("results/GADM0_raster_fixGeom.gpkg")

st_is_valid(sfVectGADM_0)

# sfVectGADM_0_v2 <- rasterToPolygons(raster(sfGADM_0_raster_5arcmin), dissolve = T) %>% 
#   st_as_sf()

# sfGADM_0_sml <-  sfGADM_0 %>% 
#   st_buffer(., -1000, nQuadSegs = 1) %>%
#   st_buffer(1000, nQuadSegs = 1)

# sfGADM_0_sml2 <- rmapshaper::ms_simplify(input = as(sfGADM_0,'Spatial'), keep = 0.05, keep_shapes = T) %>%
#   st_as_sf() 
# 
# sfGADM_0_sml3 <- st_simplify(sfGADM_0, preserveTopology = T, dTolerance = 1000)
# 
# qtm(sfGADM_0_sml3$geom)
# 
# #sfGADM_0_sml4 <- rgeos::gSimplify(as(sfGADM_0,'Spatial'),tol = 0.05, topologyPreserve = T)

sfGADM_0_dataOrigin <- sfVectGADM_0 %>% 
  rename(ID_0 = cntry_code) %>% 
  left_join(GIS_dataOrigin_births) %>% 
  left_join(GIS_dataOrigin_deaths)  

# qtm(sfGADM_0_dataOrigin, fill="birthDataOrigin")
# qtm(sfGADM_0_dataOrigin, fill="deathDataOrigin")


# division of subnat units

# check if simplified GADM0 gpkg exist, and if not, create it
if(!file.exists("results/vect_divBirths_fixGeom.gpkg")){
  vect_divBirths <- terra::as.polygons(x = rast("results/adminBoundary_births_raster_5arcmin.tif"), dissolve = T,
                                       values = T)
  vect_divDeaths <- terra::as.polygons(x = rast("results/adminBoundary_deaths_raster_5arcmin.tif"), dissolve = T,
                                       values = T)
  # create folder if not exist
  dir.create("results/shps/", showWarnings = FALSE)
  writeVector(vect_divBirths, "results/shps/vect_divBirths.shp")
  writeVector(vect_divDeaths, "results/shps/vect_divDeaths.shp")
}

# NOTE: MANUAL WORK IN QGIS HERE: then read in QGIS, fixed geometries and saved to gpkg
sfVect_divBirths <- st_read("results/vect_divBirths_fixGeom.gpkg")
sfVect_divDeaths <- st_read("results/vect_divDeaths_fixGeom.gpkg")

# calculate number of people in each category
r_pop <- subset(rast("results/r_worldpopHarmonised.tif"),21)
sfGADM_0_dataOrigin_pop <- sfGADM_0_dataOrigin %>% 
  st_drop_geometry() %>% 
  bind_cols( terra::extract(r_pop,vect(sfGADM_0_dataOrigin), fun=sum, na.rm=T) ) 

birthPopSource <- sfGADM_0_dataOrigin_pop %>% 
  group_by(birthDataOrigin) %>% 
  summarise(sumPop = sum(cntryPop2020, na.rm=T)) %>% 
  mutate(percOfTot = sumPop / sum(sumPop))

write_csv(birthPopSource, "results/birthPopSource.csv")

deathPopSource <- sfGADM_0_dataOrigin_pop %>% 
  group_by(deathDataOrigin) %>% 
  summarise(sumPop = sum(cntryPop2020, na.rm=T))%>% 
  mutate(percOfTot = sumPop / sum(sumPop))

write_csv(deathPopSource, "results/deathPopSource.csv")

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

tmap_save(plt_dataOrigin,filename = "figures/plt_dataOrigin_v3.pdf", width = 130, height = 95, units = "mm")


#### GNI per capita, worldbank ----

worldBank_cntryMeta <- read.xlsx("data_in/API_NY.GNP.PCAP.PP.CD_DS2_en_excel_v2_2763990.xlsx","Metadata - Countries") %>%
  
  
  GNI <- read.xlsx("data_in/API_NY.GNP.PCAP.PP.CD_DS2_en_excel_v2_2763990.xlsx","Data",startRow = 3) %>%
  select(Country.Name, Country.Code, as.character(c(2000:2019))) %>%
  rename(iso3=Country.Code) %>% 
  mutate(iso3 = ifelse(iso3=="XKX", "XKO", iso3)) %>% # for kosovo correct code
  as_tibble() %>% 
  left_join(cntryID[,c(1,3)]) %>% 
  filter(!is.na(cntry_code)) %>% 
  select(Country.Name,iso3,cntry_code,everything())

cntryStatus <- read.xlsx("data_in/OGHIST.xlsx","Country Analytical History",
                         rows = c(6,11:229)) %>% 
  rename(iso3=X1) %>% 
  rename(Country.Name = "Data.for.calendar.year.:") %>% 
  mutate(iso3 = ifelse(iso3=="XKX", "XKO", iso3)) %>% # for kosovo correct code
  as_tibble() %>% 
  left_join(cntryID[,c(1,3)]) %>% 
  select(Country.Name,iso3,cntry_code,everything())






