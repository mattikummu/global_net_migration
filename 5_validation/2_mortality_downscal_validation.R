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
oecdPoly <- st_read('../data_in_gpkg/OECD_TL3_2020_fixed_valid.gpkg')

oecdID <- oecdPoly %>% 
  select(tl3_id, iso3)

# load subnat GDP PPP data based on OECD
oecdData <- read_csv("../data_in/Age-adjusted mortality rate (deaths for 1000 population).csv",col_names = T, skip = 1) %>%
  as_tibble() %>% 
  rename(tl3_id = Code) %>%  #%>% 
  left_join(oecdID) %>% 
  filter(!is.na(iso3))
#mutate(rowID = row_number())

# country data
cntryData <- read_csv("../results/national_deaths_ratio_interp.csv") %>%
  as_tibble() %>% 
  dplyr::select(-c(Country))

# oecdPolyID <- oecdData %>% 
#   right_join(oecdPoly)  %>% 
#   select(-c(name_en,geometry))


# download raster data

downscaled_mortality <- rast('../results/ratioRaster_deathHarm.tif')
downscaled_births <- rast('../results/ratioRaster_birthHarm.tif')

population <- rast('../results/r_worldpopHarmonised.tif')



######## function for validation ------
#rastRaster <- downscaled_mortality
#year = 2017
#oecdPolyDown <- oecdPoly

source('../functions/f_validation_mort.R')

###### run validation script downscaled data - log10 -----------------------

oecdPolyDown <- oecdPoly

oecdPolyData_mortality <- f_validation_mort(rastRaster = downscaled_mortality,year = 2015) %>% 
  drop_na()


correlation <- c(cor(oecdPolyData_mortality$valueObs_2015,oecdPolyData_mortality$valueDown_2015,method='pearson') )

# wCorrelation <- weightedCorr(oecdPolyData_mortality$valueObs_2015, oecdPolyData_mortality$valueDown_2015,
#                                       method = c("Pearson"), weights = oecdPolyData_mortality$pop)

validation <- ggscatter(oecdPolyData_mortality, x = "valueObs_2015", y = "valueDown_2015", 
                        add = "reg.line", conf.int = TRUE, 
                        cor.coef = TRUE, cor.method =  "pearson",
                        xlab = "Observed Value", ylab = "Downscaled Value") + 
  geom_abline(intercept = 0, slope = 1) + 
  xlim(0, 25) + ylim(0, 25)


ggsave('../figures/validation_deaths_n1883.pdf', validation, width = 90, height = 90, units = 'mm')

#oecdPolyData_log2 <- f_validation(downscaledGDP_log2)
# oecdPolyData_subnat <- f_validation(subnatGDP)


# put to maps the diff between modelled and observed

oecdPolyDiff_map <- oecdPoly %>% 
  left_join(oecdPolyData_mortality)

st_write(oecdPolyDiff_map,'../results/oecdPolyDiff_map_mortality_2015.gpkg', append = F)



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

oecdPolyDiff_map <- st_read('../results/oecdPolyDiff_map_mortality_2015.gpkg') %>% 
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


