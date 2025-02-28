# This script 
# 1) performs the analysis of how net-migration in each admin unit is divided between urban and rural areas.
# In other words the script classifes an admin unit as a net-received, net-sender or as a case where 
# migration follows one of the following cases: rural push - urban pull or urban push - rural pull
# 2) plots the analysis (Figure 4)
# 3) calculates the share of population living in each class (table in Fig 4)

rm(list = ls())


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
.libPaths("/Documents/R/")

## Open packages
library(dplyr)
library(stringr)
library(tidyr)
library(sf)
library(tibble)
library(tmap)
library(scico)
library(pracma)
library(terra)
library(rmapshaper)

# Load data (produced in net_migration.R file)
migr_global <- readr::read_csv("results/zonalStatsMigrGlobal.csv")
migr_regions <- readr::read_csv("results/zonalStatsRegions.csv")

migr_cntries <- readr::read_csv("results/zonalStatsCountries.csv") #%>% rename("admZone" = "cntry_code")
migr_cnties <- readr::read_csv("results/zonalStatsCnties.csv") #%>% rename("admZone" = "nmbr")
migr_commun <- readr::read_csv("results/zonalStatsCommunes.csv") #%>% rename("admZone" = "nmbr2")

countries_sf <- sf::read_sf("DATA/gadm_lev0_sf.gpkg") %>% rename("admZone" = "cntry_code")
counties_sf <- sf::read_sf("DATA/gadm_lev1_sf.gpkg") %>% rename("admZone" = "nmbr")
communes_sf <- sf::read_sf("DATA/gadm_lev2_sf.gpkg") %>% rename("admZone" = "nmbr2")

r_gadm_lev0_5arcmin <- rast('DATA/gadm_lev0_5arcmin.tif')
# remove Antarctica
r_gadm_lev0_5arcmin[r_gadm_lev0_5arcmin == 10] <- NA
r_gadm_lev1_5arcmin <- rast('DATA/gadm_lev1_5arcmin.tif')
r_gadm_lev2_5arcmin <- rast('DATA/gadm_lev2_5arcmin.tif')


#### 1) Classify adm-areas based on urban and rural migration combinations ####

myFun_migrClass <- function(r_migr_adm, myYears,r_adm, nameOutput) {
  
  migr_class <- r_migr_adm %>% 
    filter(year %in% myYears) %>% 
    group_by(admZone, variable) %>% 
    summarise(accumSum = sum(value, na.rm=T)) %>% 
    pivot_wider(names_from = variable, values_from = accumSum) %>% 
    dplyr::select(admZone,  contains("Mgr")) %>% 
    # simplified classes
  mutate(mgr_class = if_else((UrbanMgr == 0 & RuralMgr == 0), 0, # urban and rural net-mgr is zero
                             if_else((UrbanMgr >= 0 & RuralMgr >= 0),1, # ), 1, # urban and rural net-positive
                                     if_else((UrbanMgr < 0 & RuralMgr < 0 ),2, # , 3, # urban and rural net-negative
                                             if_else((UrbanMgr <= 0 & RuralMgr >= 0), 3, # urban net-negative & rural positive
                                                     if_else((UrbanMgr >= 0 & RuralMgr <= 0), 4, #urban net-positive & rural negative
                                                             5 #no data
                                                     ))))))
    # put the classification to a raster
  r_adm_class <- classify(r_adm,
                          cbind(migr_class[,1], migr_class$mgr_class))
  
  
  writeRaster(r_adm_class,paste0('results/r_rural_urban_classification_',nameOutput,'_',Sys.Date(),'.tif'),  gdal="COMPRESS=LZW",overwrite=TRUE)
  return(migr_class)
  
}


# Run function to obtain classification
migr_class_countries_decades <- myFun_migrClass(r_migr_adm=migr_cntries, 
                                                myYears=seq(2001,2020,1),
                                                r_adm=r_gadm_lev0_5arcmin,
                                                nameOutput= 'countries')

migr_class_counties_decades <- myFun_migrClass(migr_cnties, 
                                               seq(2001,2020,1),
                                               r_gadm_lev1_5arcmin,
                                               'counties')
migr_class_communes_decades <- myFun_migrClass(migr_commun, 
                                               seq(2001,2020,1),
                                               r_gadm_lev2_5arcmin,
                                               'communes')  

readr::write_csv(migr_class_countries_decades, paste0('results/migr_direction_countries_', Sys.Date(), '.csv'))
readr::write_csv(migr_class_counties_decades, paste0('results/migr_direction_counties_', Sys.Date(), '.csv'))
readr::write_csv(migr_class_communes_decades, paste0('results/migr_direction_communes_', Sys.Date(), '.csv'))


#### 2) Plot classification ####
# Function for plotting
myFun_migrClassPlot <- function(r_index, nameLabels, titleLabel, colorpal,
                                tocrs = NA){
  
  # project to another crs
  if (!is.na(tocrs)){
    r_index <- project(r_index, tocrs, mask = TRUE)
  }
  
  # create tmap object
  index_map <- tm_shape(r_index) +
    tm_raster(style = "fixed", # draw gradient instead of classified
              #breaks = c(0, 0.5,1.5,2.5,3.5,4.5,5.5,6.5,7.5), #detailed
              breaks = c(0, 0.5,1.5,2.5,3.5,4.5,5.5), #simplified
              palette = colorpal,
              labels = nameLabels,
              showNA = F,
              colorNA = 'white',
              title = titleLabel)+
    #legend.reverse = TRUE) +
    tm_shape(sf_gadm0)+
    tm_borders(col='grey',
               lwd = 0.1)+
    tm_layout(main.title.position = "left",
              legend.bg.color = TRUE,
              legend.outside = TRUE,
              frame = FALSE)
  
  return (index_map)
  
}

sf_gadm0 <- terra::as.polygons(rast('DATA/gadm_lev0_5arcmin.tif')) %>% 
  sf::st_as_sf() %>% # to sf
  rmapshaper::ms_simplify(.,keep=0.1,keep_shapes = T) # simplify

mgrLabels <- c('zero net-migration',
               'urban and rural net-positive',
               # 'urban net-pos larger than rural net-pos',
               # 'rural net-pos larger than urban net-pos',
               'urban and rural net-negative',
               # 'urban net-neg larger than rural net-neg',
               # 'rural net-neg larger than urban net-neg',
               'urban net-negative & rural net-positive',
               'urban net-positive & rural net-negative',
               'no data')

col = c("#160D19", "#7B9DE1", "#E4586F", "#C7B61A",  "#9151A0", "#FFFFFF") #simplified


plot_migr_class_countries_decades <- myFun_migrClassPlot(rast('results/r_rural_urban_classification_countries_2023-01-30.tif'), 
                                                         mgrLabels, 
                                                         'direction of rural and urban net-migration - national',
                                                         colorpal = col, #scico(9,palette = 'roma'), 
                                                         tocrs = "+proj=robin +over")
plot_migr_class_counties_decades <- myFun_migrClassPlot(rast('results/r_rural_urban_classification_counties_2023-01-30.tif'), 
                                                        mgrLabels, 
                                                        'direction of rural and urban net-migration - provincial',
                                                        colorpal = col, #scico(9,palette = 'roma'), 
                                                        tocrs = "+proj=robin +over")
plot_migr_class_communes_decades <- myFun_migrClassPlot(rast('results/r_rural_urban_classification_communes_2023-01-30.tif'), 
                                                        mgrLabels, 
                                                        'direction of rural and urban net-migration - communal',
                                                        colorpal = col, #scico(9,palette = 'roma'), 
                                                        tocrs = "+proj=robin +over")



p_colMgrClass <- tmap_arrange( plot_migr_class_countries_decades,
                               plot_migr_class_counties_decades,
                               plot_migr_class_communes_decades,
                               nrow= 3, ncol = 1)

tmap_save(p_colMgrClass,  filename = paste0("results/plots/Plot_MgrClassMaps_decadal_simplified",Sys.Date(),'.pdf'),width = 200, height=320, units='mm')


#### 3) Share of pop in each class ####

myFun <- function(r_pop, r_zones, admLevel){
  
  pop <- r_pop[[20]]
  pop_sum <- terra::global(pop, fun = 'sum', na.rm=T)
  
  names(r_zones) <- 'impact_zone'
  
  r_zonal <- terra::zonal(pop, r_zones, fun = sum, na.rm =T) %>% as_tibble() %>% 
    mutate(shr_pop = cntryPop2019 / sum(cntryPop2019)) %>% #as.numeric(pop_sum)) %>% 
    dplyr::select(impact_zone, shr_pop)
  
  names(r_zonal) <- c('impact_zone', paste0('shr_pop','_', admLevel))
  
  return(r_zonal)
  
}

# Load pop
u_pop <- rast('DATA/popUrban.tif')
r_pop <- rast('DATA/popRural.tif')
t_pop <- rast('DATA/r_worldpopHarmonised.tif')

# the percentage of people living in areas with certain directions
dir_cntry <- rast('results/r_rural_urban_classification_countries_2023-02-06.tif') # replace date when necessary
dir_prov <- rast('results/r_rural_urban_classification_counties_2023-02-06.tif')
dir_comm <- rast('results/r_rural_urban_classification_communes_2023-02-06.tif')

dir_cntry_zonal <- myFun(t_pop, dir_cntry, 'cntry')
dir_prov_zonal <-  myFun(t_pop, dir_prov, 'prov')
dir_comm_zonal <- myFun(t_pop, dir_comm, 'comm')

dir_pop_zonal <- left_join(dir_cntry_zonal, dir_prov_zonal) %>% left_join(., dir_comm_zonal) %>% 
  mutate(labels = c('zero net-migration',
                    'urban and rural net-positive',
                    'urban and rural net-negative',
                    'urban net-negative & rural net-positive',
                    'urban net-positive & rural net-negative')) %>% 
  mutate(shr_pop_cntry = round(shr_pop_cntry, digits = 2)*100,
         shr_pop_prov = round(shr_pop_prov, digits = 2)*100,
         shr_pop_comm = round(shr_pop_comm, digits = 2)*100)

readr::write_csv(dir_pop_zonal, paste0('results/share_of_population_ruralUrbanClass_', Sys.Date(),'.csv'))





