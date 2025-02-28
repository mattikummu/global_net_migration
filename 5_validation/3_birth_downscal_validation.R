library(sf)
library(magrittr)
library(raster)
library(terra)
library(snow)
library(foreach)
library(openxlsx) #

library(exactextractr)
library("ggpubr")
library(wCorr)
library(scico)
library(tmap)

library(tidyverse)
library(dplyr) 

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

refRaster_5arcmin <- raster(ncol=360*12, nrow=180*12)
r_refRaster_5arcmin <- rast(ncol=360*12, nrow=180*12)
r_refRaster_1arcmin <- rast(ncol=5*360*12, nrow=5*180*12)
###### read data in -------------------------------

nuts3Poly <- st_read('../data_in_gpkg/NUTS_RG_20M_2016_4326.shp/NUTS_RG_20M_2016_4326.shp') %>% 
  mutate(LEVL_CODE = as.numeric(LEVL_CODE)) %>% 
  filter(LEVL_CODE == 3) %>% 
  rename(tl3_id = NUTS_ID)

areaNuts <- expanse(vect(nuts3Poly), unit = 'km') %>% 
  as_tibble() %>% 
  bind_cols(nuts3Poly) %>% 
  rename(areaExp_km2='value') %>% 
  select(tl3_id,areaExp_km2)

summary(st_is_valid(nuts3Poly))

countyPoly <- st_read('../data_in_gpkg/tl_2022_us_county/tl_2022_us_county.shp') %>% 
  select(GEOID) %>% 
  st_transform(4326) %>% 
  rename(tl3_id = GEOID)

summary(st_is_valid(countyPoly))

nutsID <- nuts3Poly %>% 
  select(tl3_id, CNTR_CODE)

countryID <- read_csv("../data_in/cntry_fao_id.csv") %>% 
  select(c(iso_a2, iso_a3)) %>% 
  rename(CNTR_CODE = iso_a2) %>% 
  rename(iso3 = iso_a3)


# load population and area
nuts3area <- readxl::read_xls ("../data_in/demo_r_d3area.xls",skip = 9) %>%
  as_tibble() %>% 
  rename(tl3_id = `GEO/TIME`) %>%  #%>% 
  left_join(nutsID) %>% 
  filter(!is.na(CNTR_CODE)) %>% 
  select(c(tl3_id, '2015', CNTR_CODE)) %>% 
  rename(area_km2 = '2015')

nuts3popCount <- readxl::read_xls ("../data_in/demo_r_d3dens.xls",skip = 8) %>%
  as_tibble() %>%
  rename(tl3_id = `GEO/TIME`) %>%  #%>%
  left_join(nutsID) %>%
  filter(!is.na(CNTR_CODE)) %>%
  left_join(nuts3area) %>%
  left_join(areaNuts) %>% 
  mutate(area_km2 = ifelse(is.na(area_km2),areaExp_km2,area_km2)) %>% 
  rename(dens2015 = '2015') %>%
  rename(dens2017 = '2017') %>%
  mutate(pop2015 = dens2015 * area_km2) %>%
  mutate(pop2017 = as.numeric(dens2017) * area_km2) %>%
  select(c(tl3_id, pop2015, pop2017,CNTR_CODE))

# nuts3popCountRaster <- exact_extract(rast('../results/r_worldpopHarmonised.tif'), nuts3Poly, 'sum')  %>%
#   dplyr::bind_cols(nuts3Poly) %>%
#   as_tibble() %>%
#   filter(!is.na(CNTR_CODE)) %>%
#   rename(pop2015r = sum.cntryPop2015) %>%
#   rename(pop2017r = sum.cntryPop2017) %>%
#   select(c(tl3_id, pop2015r, pop2017r,CNTR_CODE))
# 
# nuts3popCountComb <- nuts3popCount %>% 
#   left_join(nuts3popCountRaster) %>% 
#   mutate(pop2015 = ifelse(is.na(pop2015),pop2015r,pop2015)) %>% 
#   mutate(pop2017 = ifelse(is.na(pop2017),pop2017r,pop2017)) %>% 
#   select(c(tl3_id, pop2015, pop2017,CNTR_CODE))


# load subnat subnat data
nuts3Data <- readxl::read_xls ("../data_in/eurostat_births_nuts0-3.xls",skip = 7) %>%
  as_tibble() %>% 
  rename(tl3_id = GEO) %>%  #%>% 
  left_join(nutsID) %>% 
  filter(!is.na(CNTR_CODE)) %>% 
  left_join(nuts3popCount) %>% 
  rename(births2015 = '2015') %>% 
  rename(births2017 = '2017') %>% 
  mutate(ratio2015 = births2015 / pop2015 * 1000) %>% 
  mutate(ratio2017 = births2017 / pop2017 * 1000) %>% 
  left_join(countryID) %>% 
  select(tl3_id,iso3,ratio2015, ratio2017) %>% 
  rename('2015' = ratio2015) %>% 
  rename('2017' = ratio2017) 


#mutate(rowID = row_number())


# 
# countyData <- readxl::read_xlsx ("../data_in/US_births_2007-2019.xlsx") %>%
#   as_tibble() %>% 
#   select(c(`County Code`,Year,`Birth Rate`)) %>% 
#   rename(GEOID = `County Code`) %>% 
#   rename(BRate= `Birth Rate`) %>% 
#   filter(Year > 0) %>% 
#   pivot_wider(names_from = Year,
#               values_from = BRate) %>% 
#   mutate(iso3 = 'USA')%>% 
#   rename(tl3_id = GEOID)


# country data
cntryData <- read_csv("../results/national_births_ratio_interp.csv") %>%
  as_tibble() %>% 
  dplyr::select(-c(Country))




# oecdPolyID <- oecdData %>% 
#   right_join(oecdPoly)  %>% 
#   select(-c(name_en,geometry))


# download raster data

#downscaled_mortality <- stack('../results/Dths_downscaled_2000_2019_UPDATED.tif')
downscaled_births <- rast('../results/ratioRaster_birthHarm.tif')

population <- rast('../results/r_worldpopHarmonised.tif')



######## function for validation ------
#rastRaster <- downscaled_births
#year = 2017
#oecdPolyDown <- nuts3Poly
#oecdData <- nuts3Data
source('../functions/f_validationEUR.R')

#rastRaster <- downscaled_births
#year = 2019
#oecdPolyDown <- countyPoly
#oecdData <- countyData
source('../functions/f_validationUS.R')


###### run validation script downscaled data - log10 -----------------------


oecdPolyDataEUR <- f_validationEUR(downscaled_births,nuts3Data,nuts3Poly, 2017) %>% 
  select(tl3_id,valueObs_2017,valueDown_2017,valueDiff_2017) %>% 
  drop_na()

correlation_birth_EUR <- c(cor(oecdPolyDataEUR$valueObs_2017,oecdPolyDataEUR$valueDown_2017,method='pearson') )

validation <- ggscatter(oecdPolyDataEUR, x = "valueObs_2017", y = "valueDown_2017", 
                        add = "reg.line", conf.int = TRUE, 
                        cor.coef = TRUE, cor.method = "pearson",
                        xlab = "Observed Value", ylab = "Downscaled Value") + 
  geom_abline(intercept = 0, slope = 1) + 
  xlim(0, 35) + ylim(0, 35)


ggsave('../figures/validation_births_EUR_n1504.pdf', validation, width = 90, height = 90, units = 'mm')

# 
# oecdPolyDataUS <- f_validationUS(downscaled_births,countyData,countyPoly, 2017) %>% 
#   drop_na() %>% 
#   select(tl3_id,valueObs_2017,valueDown_2017, valueDiff_2017)
# 
# oecdPolyDataComb <- oecdPolyDataEUR %>% 
#   bind_rows(oecdPolyDataUS)
# 
# 
# correlation_birth <- c(cor(oecdPolyDataComb$valueObs_2019,oecdPolyDataComb$valueDown_2019,method='pearson') )
# 
# validation <- ggscatter(oecdPolyDataComb, x = "valueObs_2019", y = "valueDown_2019", 
#           add = "reg.line", conf.int = TRUE, 
#           cor.coef = TRUE, cor.method = "pearson",
#           xlab = "Observed Value", ylab = "Downscaled Value") + 
#   geom_abline(intercept = 0, slope = 1) + 
#   xlim(0, 35) + ylim(0, 35)
# 
# 
# ggsave('../figures/validation_births.pdf', validation, width = 90, height = 90, units = 'mm')


#oecdPolyData_log2 <- f_validation(downscaledGDP_log2)
# oecdPolyData_subnat <- f_validation(subnatGDP)


# put to maps the diff between modelled and observed
# 
# nuts3PolyDiff_map_2017 <- nuts3Poly %>% 
#   left_join(oecdPolyDataEUR)
# 
# writeVector(vect(nuts3PolyDiff_map_2017),'../results/nuts3PolyDiff_map_2017.gpkg', overwrite=TRUE)
# 


####### plot maps ----------------
source('../functions/f_create_map_valid.R')


# plot difference between modelled and observed GDP

# oecdPolyDiff_map <- st_read('../results/oecdPolyDiff_map2010.gpkg') %>% 
#   st_set_crs(4326)

oecdPolyDiff_map <- nuts3PolyDiff_map_2017  

#crs(oecdPolyDiff_map)
# in_polygonEqearth <- st_transform(oecdPolyDiff_map, 
#                                   crs = '+proj=eqearth')

diffPal <- scico(n = 9, palette = "roma") # palette
diffBreaks <- c(-.6,-.4, -.2,-.1,.1,.2,.4,.6) # breaks

# create bounding boxes

#bbox_USA <- st_bbox(c(xmin = -125, xmax = -70, ymax = 25, ymin = 50), crs = st_crs(4326))
bbox_EUR <- st_bbox(c(xmin = -10, xmax = 40, ymax = 35, ymin = 70), crs = st_crs(4326))
#bbox_JPN <- st_bbox(c(xmin = 122.5, xmax = 150, ymax = 30, ymin = 50), crs = st_crs(4326))


# apply plotting function
# plt_gdpDiff2005_USA <- create_map(in_polygon = oecdPolyDiff_map,
#                                   inBbox = bbox_USA,
#                                   cMeridian = -95,
#                                   var_name = "valueDiff_2010",
#                                   maptitle = "GDP diff 2010",
#                                   colorpal = diffPal,
#                                   breakvals = diffBreaks,
#                                   color_midpoint = 0) # give color midpoint

plt_birthDiff2017_EUR <- f_create_map_valid(in_polygon = oecdPolyDiff_map,
                                  inBbox = bbox_EUR,
                                  cMeridian = 0,
                                  var_name = "valueDiff_2017",
                                  maptitle = "birth rate diff 2017",
                                  colorpal = diffPal,
                                  breakvals = diffBreaks,
                                  color_midpoint = 0) # give color midpoint
# 
# plt_gdpDiff2005_JPN <- create_map(in_polygon = oecdPolyDiff_map,
#                                   inBbox = bbox_JPN,
#                                   cMeridian = 135,
#                                   var_name = "valueDiff_2010",
#                                   maptitle = "GDP diff 2010",
#                                   colorpal = diffPal,
#                                   breakvals = diffBreaks,
#                                   color_midpoint = 0) # give color midpoint


# plt_health <- tmap_arrange(plt_status_BMI, plt_trend_BMI, 
#                            plt_status_CHOL,plt_trend_CHOL, 
#                            ncol = 2)
tmap_save(tm = plt_birthDiff2017_EUR, filename = "../figures/plt_birthDiff2017_EUR.pdf",width = 180,height = 140, units = "mm")


