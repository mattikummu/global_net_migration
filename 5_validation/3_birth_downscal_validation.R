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

nuts3Poly <- st_read('data_in/NUTS_RG_20M_2016_4326.shp/NUTS_RG_20M_2016_4326.shp') %>% 
  mutate(LEVL_CODE = as.numeric(LEVL_CODE)) %>% 
  filter(LEVL_CODE == 3) %>% 
  rename(tl3_id = NUTS_ID)

areaNuts <- expanse(vect(nuts3Poly), unit = 'km') %>% 
  as_tibble() %>% 
  bind_cols(nuts3Poly) %>% 
  rename(areaExp_km2='value') %>% 
  select(tl3_id,areaExp_km2)

summary(st_is_valid(nuts3Poly))

countyPoly <- st_read('data_in/tl_2022_us_county/tl_2022_us_county.shp') %>% 
  select(GEOID) %>% 
  st_transform(4326) %>% 
  rename(tl3_id = GEOID)

summary(st_is_valid(countyPoly))

nutsID <- nuts3Poly %>% 
  select(tl3_id, CNTR_CODE)

countryID <- read_csv("data_in/cntry_fao_id.csv") %>% 
  select(c(iso_a2, iso_a3)) %>% 
  rename(CNTR_CODE = iso_a2) %>% 
  rename(iso3 = iso_a3)


# load population and area
nuts3area <- readxl::read_xls ("data_in/demo_r_d3area.xls",skip = 9) %>%
  as_tibble() %>% 
  rename(tl3_id = `GEO/TIME`) %>%  #%>% 
  left_join(nutsID) %>% 
  filter(!is.na(CNTR_CODE)) %>% 
  select(c(tl3_id, '2015', CNTR_CODE)) %>% 
  rename(area_km2 = '2015')

nuts3popCount <- readxl::read_xls ("data_in/demo_r_d3dens.xls",skip = 8) %>%
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

# nuts3popCountRaster <- exact_extract(rast('results/r_worldpopHarmonised.tif'), nuts3Poly, 'sum')  %>%
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
nuts3Data <- readxl::read_xls ("data_in/eurostat_births_nuts0-3.xls",skip = 7) %>%
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
# countyData <- readxl::read_xlsx ("data_in/US_births_2007-2019.xlsx") %>%
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
cntryData <- read_csv("results/national_births_ratio_interp.csv") %>%
  as_tibble() %>% 
  dplyr::select(-c(Country))




# oecdPolyID <- oecdData %>% 
#   right_join(oecdPoly)  %>% 
#   select(-c(name_en,geometry))


# download raster data

#downscaled_mortality <- stack('results/Dths_downscaled_2000_2019_UPDATED.tif')
downscaled_births <- rast('results/ratioRaster_birthHarm.tif')

population <- rast('results/r_worldpopHarmonised.tif')



######## function for validation ------
#rastRaster <- downscaled_births
#year = 2017
#oecdPolyDown <- nuts3Poly
#oecdData <- nuts3Data
f_validationEUR <- function(rastRaster, oecdData, oecdPolyDown, year) {
  
  rastSel <- subset(rastRaster,(year-1999))
  popSel <- subset(population,(year-1999))
  rastSelTot <- rastSel*popSel
  popSel <- popSel*1
  
  # Calculate vector of mean December precipitation amount for each municipality
  tempRastTot <- exact_extract(rastSelTot, oecdPolyDown, 'sum')
  tempPop <- exact_extract(popSel, oecdPolyDown, 'sum')
  
  # tempRastTot <- terra::extract(rast(rastSelTot), vect(oecdPolyDown), 'sum')
  # tempPop <- terra::extract(rast(popSel), vect(oecdPolyDown), 'sum')
  
  
  # join with polygon data
  oecdPolyDown[['valueDown']] <- ( tempRastTot/tempPop )
  oecdPolyDown[['pop']] <- ( tempPop )
  
  ######## join the obsrved data -------------------
  
  nYear <- as.character(year)
  
  cntrySelData <- cntryData %>% 
    dplyr::select(c(iso3,as.character(year))) %>% 
    rename(gdpCntry = as.character(year))
  
  oecdPolyData <- oecdData %>% 
    select(tl3_id, iso3,as.character(year)) %>% 
    rename(valueObs = as.character(year)) %>% 
    right_join(oecdPolyDown) #%>% 
  # # from 2015 to 2017 USD
  # mutate(valueObs = as.numeric(valueObs) * rate_2015 ) %>% 
  #select(-c(name_en )) %>% 
  #filter(valueObs<300000) %>% # remove huge values
  #drop_na() 
  
  oecdPolyData_forRatio <- oecdPolyData  %>% 
    mutate(totObsValue = valueObs * pop) 
  
  cntryValue_fromObs <- oecdPolyData_forRatio %>% 
    group_by(iso3) %>% 
    summarise(sumTotalValue = sum(totObsValue), sumPop = sum(pop)) %>% # total value and population in that subnat area
    mutate(cntryFromSubnatGDP_percapita = sumTotalValue / sumPop) %>% # calculate ratio in that subnat area
    left_join(cntrySelData) %>%  # add ratio
    mutate(GDPratio = gdpCntry / cntryFromSubnatGDP_percapita) %>% # ratio between value based on grid scale and value based on observations
    ungroup()
  
  valueObsScaled <- oecdPolyData_forRatio %>% 
    left_join(cntryValue_fromObs[,c('iso3','GDPratio')]) %>% 
    mutate(valueObs_scaled = GDPratio*valueObs) %>% 
    mutate(!!paste0('valueObs_',nYear) := valueObs_scaled) %>%  # caluclate final value with the ratio
    mutate(!!paste0('valueDown_',nYear) := valueDown) %>%  # caluclate final value with the ratio
    mutate(!!paste0('valueDiff_',nYear) := (valueDown-valueObs_scaled) / valueObs_scaled) %>%  # caluclate final value with the ratio
    select(-c(iso3,valueObs,pop,totObsValue,GDPratio,valueDown)) 
  
  
  return(valueObsScaled)
}

#rastRaster <- downscaled_births
#year = 2019
#oecdPolyDown <- countyPoly
#oecdData <- countyData
f_validationUS <- function(rastRaster, oecdData, oecdPolyDown, year) {
  
  rastSel <- subset(rastRaster,(year-1999))
  popSel <- subset(population,(year-1999))
  rastSelTot <- rastSel*popSel
  popSel <- popSel*1
  
  # Calculate vector of mean December precipitation amount for each municipality
  tempRastTot <- exact_extract(rastSelTot, oecdPolyDown, 'sum')
  tempPop <- exact_extract(popSel, oecdPolyDown, 'sum')
  
  # tempRastTot <- terra::extract(rast(rastSelTot), vect(oecdPolyDown), 'sum')
  # tempPop <- terra::extract(rast(popSel), vect(oecdPolyDown), 'sum')
  
  
  # join with polygon data
  oecdPolyDown[['valueDown']] <- ( tempRastTot/tempPop )
  oecdPolyDown[['pop']] <- ( tempPop )
  
  ######## join the obsrved data -------------------
  
  nYear <- as.character(year)
  
  cntrySelData <- cntryData %>% 
    dplyr::select(c(iso3,as.character(year))) %>% 
    rename(gdpCntry = as.character(year))
  
  oecdPolyData <- oecdData %>% 
    select(tl3_id, iso3,as.character(year)) %>% 
    mutate(tl3_id = as.character(tl3_id)) %>% 
    rename(valueObs = as.character(year)) %>% 
    right_join(oecdPolyDown) %>% 
    # # from 2015 to 2017 USD
    # mutate(valueObs = as.numeric(valueObs) * rate_2015 ) %>% 
    #select(-c(name_en )) %>% 
    #filter(valueObs<300000) %>% # remove huge values
    drop_na() 
  
  oecdPolyData_forRatio <- oecdPolyData  %>% 
    mutate(totObsValue = valueObs * pop) 
  
  cntryValue_fromObs <- oecdPolyData_forRatio %>% 
    group_by(iso3) %>% 
    summarise(sumTotalValue = sum(totObsValue), sumPop = sum(pop)) %>% # total value and population in that subnat area
    mutate(cntryFromSubnatGDP_percapita = sumTotalValue / sumPop) %>% # calculate ratio in that subnat area
    left_join(cntrySelData) %>%  # add ratio
    mutate(GDPratio = gdpCntry / cntryFromSubnatGDP_percapita) # ratio between value based on grid scale and value based on observations
  
  valueObsScaled <- oecdPolyData_forRatio %>% 
    left_join(cntryValue_fromObs[,c('iso3','GDPratio')]) %>% 
    mutate(valueObs_scaled = GDPratio*valueObs) %>% 
    mutate(!!paste0('valueObs_',nYear) := valueObs_scaled) %>%  # caluclate final value with the ratio
    mutate(!!paste0('valueDown_',nYear) := valueDown) %>%  # caluclate final value with the ratio
    mutate(!!paste0('valueDiff_',nYear) := (valueDown-valueObs_scaled) / valueObs_scaled) %>%  # caluclate final value with the ratio
    select(-c(iso3,valueObs,pop,totObsValue,GDPratio,valueDown)) #%>% 
  
  
  return(valueObsScaled)
}


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


ggsave('figures/validation_births_EUR_n1504_2023-02-06.pdf', validation, width = 90, height = 90, units = 'mm')

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
# ggsave('figures/validation_births.pdf', validation, width = 90, height = 90, units = 'mm')


#oecdPolyData_log2 <- f_validation(downscaledGDP_log2)
# oecdPolyData_subnat <- f_validation(subnatGDP)


# put to maps the diff between modelled and observed

nuts3PolyDiff_map_2017 <- nuts3Poly %>% 
  left_join(oecdPolyDataEUR)

st_write(nuts3PolyDiff_map_2017,'results/nuts3PolyDiff_map_2017.gpkg', append=F)



####### plot maps ----------------
create_map <- function(in_polygon, inBbox,cMeridian,var_name, maptitle, colorpal, breakvals,
                       color_midpoint = NULL, eqearth = TRUE){
  
  
  # project to Robinson if desired (true by default)
  if (eqearth){
    in_polygon <- st_transform(in_polygon, 
                               crs = paste0('+proj=eqearth +lon_0=',cMeridian,' +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'))
  }
  # create tmap object
  plt_tmap <- tm_shape(in_polygon, bbox = inBbox) +
    tm_polygons(col = var_name,
                style = "fixed",
                breaks = breakvals,
                palette = colorpal,
                lwd = 0.01,
                textNA = "no data",
                colorNA = "grey80",
                legend.is.portrait = FALSE) +
    tm_layout(main.title = maptitle,
              frame = FALSE,
              legend.bg.color = TRUE,
              legend.outside = TRUE,
              legend.outside.position = "bottom",
              main.title.size = 0.8,
              legend.title.size = 0.6)
  
  
  return(plt_tmap)
  
}


# plot difference between modelled and observed GDP

# oecdPolyDiff_map <- st_read('results/oecdPolyDiff_map2010.gpkg') %>% 
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

plt_birthDiff2017_EUR <- create_map(in_polygon = oecdPolyDiff_map,
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
tmap_save(tm = plt_birthDiff2017_EUR, filename = "figures/plt_birthDiff2017_EUR.pdf",width = 180,height = 140, units = "mm")


