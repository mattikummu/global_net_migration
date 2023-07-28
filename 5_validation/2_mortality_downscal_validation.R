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

# file created in 'gdp_downscal_validation.R'
oecdPoly <- st_read('data_in/OECD_TL3_2020_fixed_valid.gpkg')

oecdID <- oecdPoly %>% 
  select(tl3_id, iso3)

# load subnat GDP PPP data based on OECD
oecdData <- read_csv("data_in/Age-adjusted mortality rate (deaths for 1000 population).csv",col_names = T, skip = 1) %>%
  as_tibble() %>% 
  rename(tl3_id = Code) %>%  #%>% 
  left_join(oecdID) %>% 
  filter(!is.na(iso3))
#mutate(rowID = row_number())

# country data
cntryData <- read_csv("results/national_deaths_ratio_interp.csv") %>%
  as_tibble() %>% 
  dplyr::select(-c(Country))

# oecdPolyID <- oecdData %>% 
#   right_join(oecdPoly)  %>% 
#   select(-c(name_en,geometry))


# download raster data

downscaled_mortality <- rast('results/ratioRaster_deathHarm.tif')
downscaled_births <- rast('results/ratioRaster_birthHarm.tif')

population <- rast('results/r_worldpopHarmonised.tif')



######## function for validation ------
#rastRaster <- downscaled_mortality
#year = 2017
#oecdPolyDown <- oecdPoly
f_validation <- function(rastRaster, year) {
  
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
    rename(ValueCntry = as.character(year))
  
  oecdPolyData <- oecdData %>% 
    select(tl3_id, iso3,as.character(year)) %>% 
    rename(valueObs = as.character(year)) %>% 
    right_join(oecdPolyDown) %>% 
    # # from 2015 to 2017 USD
    # mutate(valueObs = as.numeric(valueObs) * rate_2015 ) %>% 
    select(-c(name_en )) %>% 
    #filter(valueObs<300000) %>% # remove huge values
    drop_na() 
  
  oecdPolyData_forRatio <- oecdPolyData  %>% 
    mutate(totObsValue = valueObs * pop) %>% 
    mutate(totDownValue = valueDown * pop)
  
  cntryValue_fromObs <- oecdPolyData_forRatio %>% 
    group_by(iso3) %>% 
    # total value and population in that subnat area
    summarise(sumTotalValue = sum(totObsValue), sumTotalValueDown = sum(totDownValue), sumPop = sum(pop)) %>% 
    mutate(cntryFromSubnatValue = sumTotalValue / sumPop) %>% # calculate ratio in that subnat area
    mutate(cntryFromSubnatValueDown = sumTotalValueDown / sumPop) %>% # calculate ratio in that subnat area
    
    left_join(cntrySelData) %>%  # add ratio
    mutate(ValueRatioObs = ValueCntry / cntryFromSubnatValue) %>%  # ratio between value based on grid scale and value based on observations
    mutate(ValueRatioDown = ValueCntry / cntryFromSubnatValueDown) # ratio between value based on grid scale and value based on observations
  
  
  
  valueObsScaled <- oecdPolyData_forRatio %>% 
    left_join(cntryValue_fromObs[,c('iso3','ValueRatioObs','ValueRatioDown')]) %>% 
    mutate(valueObs_scaled = ValueRatioObs*valueObs) %>% 
    mutate(valueDown_scaled = ValueRatioDown*valueDown) %>% 
    mutate(!!paste0('valueObs_',nYear) := valueObs_scaled) %>%  # caluclate final value with the ratio
    mutate(!!paste0('valueDown_',nYear) := valueDown_scaled) %>%  # caluclate final value with the ratio
    mutate(!!paste0('valueDiff_',nYear) := (valueDown-valueObs_scaled) / valueObs_scaled) %>%  # caluclate final value with the ratio
    select(-c(iso3,valueObs,totObsValue,ValueRatioObs,valueDown)) #%>% 
  
  
  return(valueObsScaled)
}


###### run validation script downscaled data - log10 -----------------------

oecdPolyDown <- oecdPoly

oecdPolyData_mortality <- f_validation(downscaled_mortality,2015) %>% 
  drop_na()


correlation <- c(cor(oecdPolyData_mortality$valueObs_2015,oecdPolyData_mortality$valueDown_2015,method='pearson') )

wCorrelation <- weightedCorr(oecdPolyData_mortality$valueObs_2015, oecdPolyData_mortality$valueDown_2015,
                                      method = c("Pearson"), weights = oecdPolyData_mortality$pop)

validation <- ggscatter(oecdPolyData_mortality, x = "valueObs_2015", y = "valueDown_2015", 
                        add = "reg.line", conf.int = TRUE, 
                        cor.coef = TRUE, cor.method =  "pearson",
                        xlab = "Observed Value", ylab = "Downscaled Value") + 
  geom_abline(intercept = 0, slope = 1) + 
  xlim(0, 25) + ylim(0, 25)


ggsave('figures/validation_deaths_n1883_2023-02-06.pdf', validation, width = 90, height = 90, units = 'mm')

#oecdPolyData_log2 <- f_validation(downscaledGDP_log2)
# oecdPolyData_subnat <- f_validation(subnatGDP)


# put to maps the diff between modelled and observed

oecdPolyDiff_map <- oecdPoly %>% 
  left_join(oecdPolyData_mortality)

st_write(oecdPolyDiff_map,'results/oecdPolyDiff_map_mortality_2015.gpkg', append = F)



####### plot maps ----------------
create_map <- function(in_polygon, inBbox = NULL,cMeridian,var_name, maptitle, colorpal, breakvals,
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

oecdPolyDiff_map <- st_read('results/oecdPolyDiff_map_mortality_2015.gpkg') %>% 
  st_set_crs(4326)

#crs(oecdPolyDiff_map)
# in_polygonEqearth <- st_transform(oecdPolyDiff_map, 
#                                   crs = '+proj=eqearth')

diffPal <- scico(n = 9, palette = "roma") # palette
diffBreaks <- c(-.6,-.4, -.2,-.1,.1,.2,.4,.6) # breaks


plt_Diff_world <- create_map(in_polygon = oecdPolyDiff_map,
                           #inBbox = bbox_USA,
                           cMeridian = 0,
                           var_name = "valueDiff_2015",
                           maptitle = "valueDiff_2015",
                           colorpal = diffPal,
                           breakvals = diffBreaks,
                           color_midpoint = 0) # give color midpoint
tmap_save(tm = plt_Diff_world, filename = "figures/plt_Diff_world_2015.pdf",width = 180,height = 140, units = "mm")

# create bounding boxes

bbox_USA <- st_bbox(c(xmin = -125, xmax = -70, ymax = 50, ymin = 80), crs = st_crs(4326))
bbox_EUR <- st_bbox(c(xmin = -10, xmax = 40, ymax = 35, ymin = 70), crs = st_crs(4326))
bbox_JPN <- st_bbox(c(xmin = 122.5, xmax = 150, ymax = 30, ymin = 50), crs = st_crs(4326))


# apply plotting function
plt_Diff_USA <- create_map(in_polygon = oecdPolyDiff_map,
                                  inBbox = bbox_USA,
                                  cMeridian = -95,
                                  var_name = "valueDiff_2015",
                                  maptitle = "valueDiff_2015",
                                  colorpal = diffPal,
                                  breakvals = diffBreaks,
                                  color_midpoint = 0) # give color midpoint

plt_Diff_EUR <- create_map(in_polygon = oecdPolyDiff_map,
                                  inBbox = bbox_EUR,
                                  cMeridian = 0,
                                  var_name = "valueDiff_2015",
                                  maptitle = "valueDiff_2015",
                                  colorpal = diffPal,
                                  breakvals = diffBreaks,
                                  color_midpoint = 0) # give color midpoint

plt_gdpDiff2005_JPN <- create_map(in_polygon = oecdPolyDiff_map,
                                  inBbox = bbox_JPN,
                                  cMeridian = 135,
                                  var_name = "valueDiff_2015",
                                  maptitle = "valueDiff_2015",
                                  colorpal = diffPal,
                                  breakvals = diffBreaks,
                                  color_midpoint = 0) # give color midpoint


plt_diff <- tmap_arrange(plt_status_BMI, plt_trend_BMI, 
                           plt_status_CHOL,plt_trend_CHOL, 
                           ncol = 2)
tmap_save(tm = plt_health, filename = "figures/health_status_trend.pdf",width = 180,height = 140, units = "mm")


