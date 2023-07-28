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
oecdData <- read_csv("data_in/Resident population.csv",col_names = T, skip = 1) %>%
  as_tibble() %>% 
  rename(tl3_id = Code) %>%  #%>% 
  left_join(oecdID) %>% 
  filter(!is.na(iso3))
#mutate(rowID = row_number())

# country data
cntryData <- read_csv("results/national_pop_total.csv") %>%
  as_tibble() %>% 
  dplyr::select(-c(Country))

# oecdPolyID <- oecdData %>% 
#   right_join(oecdPoly)  %>% 
#   select(-c(name_en,geometry))


# download raster data
population <- stack('results/r_worldpopHarmonised.tif')



######## function for validation ------
#rastRaster <- downscaled_mortality
#year = 2020
#oecdPolyDown <- oecdPoly
f_validation <- function(population, year) {
  
  popSel <- subset(population,(year-1999))
  popSel <- popSel*1
  
  # Calculate vector of mean December precipitation amount for each municipality
  tempPop <- exact_extract(popSel, oecdPolyDown, 'sum')
  
  # tempRastTot <- terra::extract(rast(rastSelTot), vect(oecdPolyDown), 'sum')
  # tempPop <- terra::extract(rast(popSel), vect(oecdPolyDown), 'sum')
  
  
  # join with polygon data
  oecdPolyDown[['pop']] <- ( tempPop )
  
  ######## join the obsrved data -------------------
  
  nYear <- as.character(year)
   
  cntrySelData <- cntryData %>% 
    dplyr::select(c(iso3,as.character(year))) %>% 
    rename(valueCntry = as.character(year))
  
  oecdPolyData <- oecdData %>% 
    select(tl3_id, iso3,as.character(year)) %>% 
    rename(valueObs = as.character(year)) %>% 
    right_join(oecdPolyDown) %>% 
    # # from 2015 to 2017 USD
    # mutate(valueObs = as.numeric(valueObs) * rate_2015 ) %>% 
    select(-c(name_en )) %>% 
    #filter(valueObs<300000) %>% # remove huge values
    drop_na() 
  
  oecdPolyData_forRatio <- oecdPolyData  
  
  cntryValue_fromObs <- oecdPolyData_forRatio %>% 
    group_by(iso3) %>% 
    summarise(sumPop = sum(pop)) %>% # total value and population in that subnat area
    #mutate(cntryFromSubnatValue = sumTotalValue / sumPop) %>% # calculate ratio in that subnat area
    left_join(cntrySelData) %>%  # add ratio
    mutate(ValueRatio = valueCntry / sumPop) # ratio between value based on grid scale and value based on observations
  
  valueObsScaled <- oecdPolyData_forRatio %>% 
    left_join(cntryValue_fromObs[,c('iso3','ValueRatio')]) %>% 
    mutate(valueObs_scaled = ValueRatio*valueObs) %>% 
    mutate(!!paste0('valueObs_',nYear) := valueObs_scaled) %>%  # caluclate final value with the ratio
    mutate(!!paste0('valueDown_',nYear) := pop) %>%  # caluclate final value with the ratio
    mutate(!!paste0('gdpDiff_',nYear) := (pop-valueObs_scaled) / valueObs_scaled) %>%  # caluclate final value with the ratio
    select(-c(iso3,valueObs,pop,ValueRatio)) #%>% 
  
  
  return(valueObsScaled)
}


###### run validation script downscaled data - log10 -----------------------

oecdPolyDown <- oecdPoly

oecdPolyData_pop_2020 <- f_validation(population,2020) %>% 
  drop_na()

oecdPolyData_pop_2015 <- f_validation(population,2015) %>% 
  drop_na()

oecdPolyData_pop_2010 <- f_validation(population,2010) %>% 
  drop_na()

oecdPolyData_pop_2005 <- f_validation(population,2005) %>% 
  drop_na()

oecdPolyData_pop_2000 <- f_validation(population,2000) %>% 
  drop_na()


(correlation <- c(cor(oecdPolyData_pop_2000$valueObs_2000,oecdPolyData_pop_2000$valueDown_2000,method='pearson') ))
(correlation <- c(cor(oecdPolyData_pop_2005$valueObs_2005,oecdPolyData_pop_2005$valueDown_2005,method='pearson') ))
(correlation <- c(cor(oecdPolyData_pop_2010$valueObs_2010,oecdPolyData_pop_2010$valueDown_2010,method='pearson') ))
(correlation <- c(cor(oecdPolyData_pop_2015$valueObs_2015,oecdPolyData_pop_2015$valueDown_2015,method='pearson') ))
(correlation <- c(cor(oecdPolyData_pop_2020$valueObs_2020,oecdPolyData_pop_2020$valueDown_2020,method='pearson') ))

f_validationFig <- function(inFile, obsField, obsDownSc) {
  validationFig <- ggscatter(inFile, x = obsField, y = obsDownSc, 
                               add = "reg.line", conf.int = TRUE, 
                               cor.coef = TRUE, cor.method = "pearson",
                               xlab = "Observed Value", ylab = "Downscaled Value") + 
    geom_abline(intercept = 0, slope = 1) + 
    xlim(0, 2.5e+07) + ylim(0, 2.5e+07)
  
}

pValid2000 <- f_validationFig(oecdPolyData_pop_2000,'valueObs_2000','valueDown_2000' )
pValid2005 <- f_validationFig(oecdPolyData_pop_2005,'valueObs_2005','valueDown_2005' )
pValid2010 <- f_validationFig(oecdPolyData_pop_2010,'valueObs_2010','valueDown_2010' )
pValid2015 <- f_validationFig(oecdPolyData_pop_2015,'valueObs_2015','valueDown_2015' )
pValid2020 <- f_validationFig(oecdPolyData_pop_2020,'valueObs_2020','valueDown_2020' )

validation <- ggarrange(pValid2000, pValid2005, pValid2010,pValid2015,pValid2020,
                        labels = c("2000", "2005", "2010", "2015", "2020"),
                        ncol = 2, nrow = 3)

ggsave('figures/validation_worldPop2000_2020.pdf', validation, width = 180, height = 215, units = 'mm')


#oecdPolyData_log2 <- f_validation(downscaledGDP_log2)
# oecdPolyData_subnat <- f_validation(subnatGDP)


# put to maps the diff between modelled and observed

oecdPolyDiff_map_2017 <- oecdPoly %>% 
  left_join(oecdPolyData_log10_2010)

st_write(oecdPolyDiff_map_2010,'results/oecdPolyDiff_map2010.gpkg', append=F)


##### correlations ------

correlation_log10 <- c(cor(oecdPolyData_log10$valueObs2005,oecdPolyData_log10$valueDown2005,method='pearson')  ,
                       cor(oecdPolyData_log10$valueObs2010,oecdPolyData_log10$valueDown2010,method='pearson')  ,
                       cor(oecdPolyData_log10$valueObs2015,oecdPolyData_log10$valueDown2015,method='pearson')  )


wCorrelation_log10 <- c( weightedCorr(oecdPolyData_log10$valueObs2005, oecdPolyData_log10$valueDown2005,
                                      method = c("Pearson"), weights = oecdPolyData_log10$pop2005),
                         weightedCorr(oecdPolyData_log10$valueObs2010, oecdPolyData_log10$valueDown2010,
                                      method = c("Pearson"), weights = oecdPolyData_log10$pop2010),
                         weightedCorr(oecdPolyData_log10$valueObs2015, oecdPolyData_log10$valueDown2015,
                                      method = c("Pearson"), weights = oecdPolyData_log10$pop2015))


# correlation_log2 <- c(cor(oecdPolyData_log2$valueObs2005,oecdPolyData_log2$valueDown2005,method='pearson')  ,
#                       cor(oecdPolyData_log2$valueObs2010,oecdPolyData_log2$valueDown2010,method='pearson')  ,
#                       cor(oecdPolyData_log2$valueObs2015,oecdPolyData_log2$valueDown2015,method='pearson')  )


correlation_subnat <- c(cor(oecdPolyData_subnat$valueObs2005,oecdPolyData_subnat$valueDown2005,method='pearson')  ,
                        cor(oecdPolyData_subnat$valueObs2010,oecdPolyData_subnat$valueDown2010,method='pearson')  ,
                        cor(oecdPolyData_subnat$valueObs2015,oecdPolyData_subnat$valueDown2015,method='pearson')  )

wCorrelation_log10 <- c( weightedCorr(oecdPolyData_subnat$valueObs2005, oecdPolyData_subnat$valueDown2005,
                                      method = c("Pearson"), weights = oecdPolyData_subnat$pop2005),
                         weightedCorr(oecdPolyData_subnat$valueObs2010, oecdPolyData_subnat$valueDown2010,
                                      method = c("Pearson"), weights = oecdPolyData_subnat$pop2010),
                         weightedCorr(oecdPolyData_subnat$valueObs2015, oecdPolyData_subnat$valueDown2015,
                                      method = c("Pearson"), weights = oecdPolyData_subnat$pop2015))



# http://www.sthda.com/english/wiki/correlation-test-between-two-variables-in-r

ggscatter(oecdPolyData_log10, x = "valueObs2005", y = "valueDown2005", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Observed GDP", ylab = "Downscaled GDP") + 
  geom_abline(intercept = 0, slope = 1) + 
  xlim(0, 125000) + ylim(0, 125000)

ggscatter(oecdPolyData_log10, x = "valueObs2010", y = "valueDown2010", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Observed GDP", ylab = "Downscaled GDP") + 
  geom_abline(intercept = 0, slope = 1) + 
  xlim(0, 125000) + ylim(0, 125000)

ggscatter(oecdPolyData_log10, x = "valueObs2015", y = "valueDown2015", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Observed GDP", ylab = "Downscaled GDP") + 
  geom_abline(intercept = 0, slope = 1) + 
  xlim(0, 125000) + ylim(0, 125000)


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

oecdPolyDiff_map <- st_read('results/oecdPolyDiff_map2010.gpkg') %>% 
  st_set_crs(4326)

#crs(oecdPolyDiff_map)
# in_polygonEqearth <- st_transform(oecdPolyDiff_map, 
#                                   crs = '+proj=eqearth')

diffPal <- scico(n = 9, palette = "roma") # palette
diffBreaks <- c(-.6,-.3, -.15,-.075,-.025,.025,.075,.15,.3,.6) # breaks

# create bounding boxes

bbox_USA <- st_bbox(c(xmin = -125, xmax = -70, ymax = 25, ymin = 50), crs = st_crs(4326))
bbox_EUR <- st_bbox(c(xmin = -10, xmax = 40, ymax = 35, ymin = 70), crs = st_crs(4326))
bbox_JPN <- st_bbox(c(xmin = 122.5, xmax = 150, ymax = 30, ymin = 50), crs = st_crs(4326))


# apply plotting function
plt_gdpDiff2005_USA <- create_map(in_polygon = oecdPolyDiff_map,
                                  inBbox = bbox_USA,
                                  cMeridian = -95,
                                  var_name = "gdpDiff_2010",
                                  maptitle = "GDP diff 2010",
                                  colorpal = diffPal,
                                  breakvals = diffBreaks,
                                  color_midpoint = 0) # give color midpoint

plt_gdpDiff2005_EUR <- create_map(in_polygon = oecdPolyDiff_map,
                                  inBbox = bbox_EUR,
                                  cMeridian = 0,
                                  var_name = "gdpDiff_2010",
                                  maptitle = "GDP diff 2010",
                                  colorpal = diffPal,
                                  breakvals = diffBreaks,
                                  color_midpoint = 0) # give color midpoint

plt_gdpDiff2005_JPN <- create_map(in_polygon = oecdPolyDiff_map,
                                  inBbox = bbox_JPN,
                                  cMeridian = 135,
                                  var_name = "gdpDiff_2010",
                                  maptitle = "GDP diff 2010",
                                  colorpal = diffPal,
                                  breakvals = diffBreaks,
                                  color_midpoint = 0) # give color midpoint


plt_health <- tmap_arrange(plt_status_BMI, plt_trend_BMI, 
                           plt_status_CHOL,plt_trend_CHOL, 
                           ncol = 2)
tmap_save(tm = plt_health, filename = "figures/health_status_trend.pdf",width = 180,height = 140, units = "mm")


