# This script 
# 1) extracts net-migration in urban and rural areas,
# 2) calculates zonal stats of total, urban and rural net-migration at global, regional, national and sub-national level, and 
# 3) plots net-migration in urban and rural areas as bar-plots in different regions of the world,
# cumulative urban and rural net-migration at national and sub-national level, and
# gridded urban and rural net-migration as global maps.

rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
.libPaths("/Documents/R/")

## Open packages
library(terra)
library(dplyr)
library(stringr)
library(tidyr)
library(sf)
library(tibble)

library(tmap)
library(scico)
library(ggplot2)
library(rmapshaper)

####  1) Extract urban and rural net-migration ####
# Load data
# Rural-urban extent (based on scaled population density and urban pop share for each country)
# 0 = rural
# 1 = urban
urbanExt <- rast('../results/r_urbExtent_FixedExt.tif') 
# Subset to correct timespan
urbanExt <- terra::subset(urbanExt, 2:21)

# Net-migration (annual) 2001-2020
netMgr <- terra::rast("../results/netMgr_2001_2020_v3.tif")
names(netMgr)

# Extract net-migration in urban areas
# urban areas = 1 in urbanExt
netMgrUrban <- netMgr*urbanExt
terra::writeRaster(netMgrUrban, "../results/netMigrUrban.tif", overwrite = T)

# Extract net-migration in rural areas
# rural areas = 0 in MGUP
netMgrRural <- netMgr
netMgrRural[urbanExt == 1] <- NA 
terra::writeRaster(netMgrRural, "../results/netMigrRural.tif", overwrite = T)


#### 2) Calulate zonal stats for each admin unit ####
# Admin units
cntryID <- read_csv("../data_in/countries_codes_and_coordinates.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  #select(cntry_code,iso2,iso3,Country) %>% 
  mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2))
# Regions

# dissolve regions
#v_adm0_reg <- vect("data_gis/ne_50m_adm0_all_ids/adm0_NatEarth_all_ids.shp") %>%  
v_adm0_reg <- vect('../results/GADM0_raster.gpkg') %>%  
  left_join(cntryID) %>% 
  
  select(RegionID) %>% 
  drop_na() %>% 
  terra::aggregate(by = 'RegionID')

# rasterise polygon file 
ref_raster_1arcmin <- rast(ncol=360*60, nrow=180*60)
sfGADM_0_raster_1arcmin <- terra::rasterize(v_adm0_reg,ref_raster_1arcmin,field="RegionID")
# aggregate to 5 arc-min
sfGADM_0_raster_5arcmin <- terra::aggregate(sfGADM_0_raster_1arcmin,fact=5,fun=modal,na.rm=T)

sfGADM_0_raster_5arcmin[is.nan(sfGADM_0_raster_5arcmin)] = NA

regions <- sfGADM_0_raster_5arcmin

writeRaster(regions,paste0('../results/regions.tif'),  gdal="COMPRESS=LZW",overwrite=TRUE)

regions_df <- cntryID %>% 
  left_join(read_csv("../data_in/countries_codes_and_coordinates_regions.csv") %>% select(RegionID, RegName) ) %>% 
  select(RegionID, RegName) %>% 
  # rename(region_id = RegionID) %>% 
  # rename(regionName = RegName) %>% 
  unique()

# Countries
countries <- terra::rast("../data_in_rast/gadm_lev0_5arcmin.tif")

# Counties/provinces
counties <- terra::rast("../data_in_rast/gadm_lev1_5arcmin.tif")

# Communes
communes <- terra::rast("../data_in_rast/gadm_lev2_5arcmin.tif")

# Pop data (annual) for 2000-2020
pop <- terra::rast("../results/r_worldpopHarmonised.tif")
urbanPop <- terra::rast('../results/popUrban.tif')
ruralPop <- terra::rast('../results/popRural.tif')

# Subset to correct timespan
timesteps <- seq(2001, 2020,1)
pop <- terra::subset(pop, 2:21)
urbanPop <- terra::subset(urbanPop, 2:21)
ruralPop <- terra::subset(ruralPop, 2:21)

# Function to calclulate zonal sums of urban and rural migration

source('../functions/myFun_mgrZonalStats.R')

zonalStatsRegions <- myFun_mgrZonalStats(regions, pop, urbanPop, ruralPop,  netMgr, netMgrUrban, netMgrRural) #posPop, negPop, , posMgr, negMgr
zonalStatsCntries <- myFun_mgrZonalStats(countries, pop, urbanPop, ruralPop, netMgr, netMgrUrban, netMgrRural) #posPop, negPop,, posMgr, negMgr
zonalStatsCnties <- myFun_mgrZonalStats(counties, pop, urbanPop, ruralPop, netMgr, netMgrUrban, netMgrRural)#posPop, negPop,, posMgr, negMgr
zonalStatsCommunes <- myFun_mgrZonalStats(communes, pop, urbanPop, ruralPop,  netMgr, netMgrUrban, netMgrRural)#posPop, negPop,, posMgr, negMgr
# Global
netMigrGlobal <- terra::global(netMgr, fun = "sum", na.rm =T) %>% as_tibble() %>% rename("netMgrGlobal" = "sum") %>% mutate(year = timesteps) %>% 
  left_join(terra::global(netMgrUrban, fun = "sum", na.rm =T) %>% as_tibble() %>% rename("netMgrUrban" = "sum") %>% mutate(year = timesteps)) %>% 
  left_join(terra::global(netMgrRural, fun = "sum", na.rm =T) %>% as_tibble() %>% rename("netMgrRural" = "sum") %>% mutate(year = timesteps)) %>% 
  left_join(terra::global(pop, fun = "sum", na.rm =T) %>% as_tibble() %>% rename("popGlobal" = "sum") %>% mutate(year = timesteps)) %>% 
  left_join(terra::global(urbanPop, fun = "sum", na.rm =T) %>% as_tibble() %>% rename("popUrban" = "sum") %>% mutate(year = timesteps)) %>% 
  left_join(terra::global(ruralPop, fun = "sum", na.rm =T) %>% as_tibble() %>% rename("popRural" = "sum") %>% mutate(year = timesteps)) %>% 
  dplyr::select(year, everything())

# Save to .csv
readr::write_csv(netMigrGlobal, "../results/zonalStatsMigrGlobal.csv")
readr::write_csv(zonalStatsRegions, '../results/zonalStatsRegions.csv')
readr::write_csv(zonalStatsCntries, '../results/zonalStatsCountries.csv')
readr::write_csv(zonalStatsCnties, '../results/zonalStatsCnties.csv')
readr::write_csv(zonalStatsCommunes, '../results/zonalStatsCommunes.csv')

#### 3) Plot bar plots and maps ####

## Open packages


#### Bar plot (Figure 3) ####
#load data
migr_global <- readr::read_csv("../results/zonalStatsMigrGlobal.csv")
migr_regions <- readr::read_csv("../results/zonalStatsRegions.csv")
migr_cntries <- readr::read_csv("../results/zonalStatsCountries.csv")
migr_cnties <- readr::read_csv("../results/zonalStatsCnties.csv")
migr_commun <- readr::read_csv("../results/zonalStatsCommunes.csv")


# Regions + global
#regions_df <- readr::read_csv('DATA/countryGroupsNames.csv')
# Combine regional and global data
migr_globalRegional <- migr_global %>% rename("totPop" = "popGlobal", "UrbanPop" = "popUrban", "RuralPop" = "popRural", 
                                              "TotMgr" = "netMgrGlobal", "UrbanMgr" = "netMgrUrban", "RuralMgr" = "netMgrRural") %>% 
  mutate(admZone = 13, regionName = "XGlobal") %>% 
  dplyr::select( admZone, year, UrbanPop, RuralPop, UrbanMgr, RuralMgr, regionName) %>% 
  bind_rows(migr_regions %>% filter(variable %in% c("UrbanPop", "RuralPop", "UrbanMgr", "RuralMgr")) %>% 
              pivot_wider(names_from = variable, values_from = value) %>% 
              left_join(., regions_df, by = c("admZone" = "RegionID"))) %>% 
  pivot_longer(UrbanPop:RuralMgr, names_to = "variable", values_to = "value")


mycolors <- RColorBrewer::brewer.pal(n=2,"Set2")

Plot_MgrRegionsGlobal <-  migr_globalRegional %>% filter(variable %in% c("UrbanPop", "RuralPop", "UrbanMgr", "RuralMgr")) %>% 
  pivot_wider(names_from = variable, values_from = value) %>% 
  #left_join(., regions_df, by = c("admZone" = "region_id")) %>% 
  group_by(admZone, year) %>% 
  mutate(urbanNMpp = (UrbanMgr/UrbanPop)*1000, #total urban net-migr per urban pop
         ruralNMpp = (RuralMgr/RuralPop)*1000) %>% 
  dplyr::select(-UrbanPop, -RuralPop, -UrbanMgr, -RuralMgr) %>% 
  pivot_longer(urbanNMpp:ruralNMpp, names_to = "variable", values_to = "value") %>% 
  #mutate(zone = as.factor(zone)) %>% 
  
  ggplot(., aes(fill=variable, y=value, x=year)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values=mycolors, labels = c('Rural','Urban')) + 
  facet_wrap(~regionName) +
  theme_minimal()+
  labs(title="Net-migration in rural and urban areas in each region (relative to pop in rural and urban areas in each region)",
       x ="Year", y = "Net-migration per 1000 pop")

ggsave(Plot_MgrRegionsGlobal,  filename = paste0("../figures/Plot_MgrRegionsGlobal",Sys.Date(),'.pdf'),width = 320, height=160, units='mm')


#### Cumulative sum of urban and rural net-migration (Figure S6) ####

source('../functions/myFun_migrStats.R')

source('../functions/myFun_create_rasterMap_2.R')

# function transfer dataframes to raster
source('../functions/myFun_create_rasterMap_3.R')


# Create country boarders from cntry raster
sf_gadm0 <- terra::as.polygons(rast('../data_in_rast/gadm_lev0_5arcmin.tif')) %>% 
  sf::st_as_sf() %>% # to sf
  rmapshaper::ms_simplify(.,keep=0.1,keep_shapes = T) # simplify

# Load data
r_gadm_lev0_5arcmin <- rast('../data_in_rast/gadm_lev0_5arcmin.tif')
r_gadm_lev1_5arcmin <- rast('../data_in_rast/gadm_lev1_5arcmin.tif')
r_gadm_lev2_5arcmin <- rast('../data_in_rast/gadm_lev2_5arcmin.tif')


# Calculate cumulative sums 
mgr_cntries <- myFun_migrStats(migr_cntries)
mgr_cnties <- myFun_migrStats(migr_cnties)
mgr_communes <- myFun_migrStats(migr_commun )

mybreaks = seq(-0.25, 0.25, 0.05)*1000
# Countries
plot_cntries_u <- myFun_create_rasterMap_3(r_index = r_gadm_lev0_5arcmin, 
                                         r_mgr_df = mgr_cntries, 
                                         variableName= 'urbanSumPp',  
                                         mybreaks = seq(-0.25, 0.25, 0.05)*1000,
                                         colorpal = scico::scico(length(mybreaks)+1,palette = 'vik', direction = -1),
                                         titleLabel = "Cumulative net-migration in urban areas (relative to pop in rural and urban areas in each country)",
                                         tocrs = "+proj=robin +over")

plot_cntries_r <- myFun_create_rasterMap_3(r_index = r_gadm_lev0_5arcmin, 
                                         r_mgr_df = mgr_cntries, 
                                         variableName= 'ruralSumPp',  
                                         mybreaks = seq(-0.25, 0.25, 0.05)*1000,
                                         colorpal = scico::scico(length(mybreaks)+1,palette = 'vik', direction = -1),
                                         titleLabel = "Cumulative net-migration in rural areas (relative to pop in rural and urban areas in each country)",
                                         tocrs = "+proj=robin +over")
# Counties
plot_counties_u <- myFun_create_rasterMap_3(r_index = r_gadm_lev1_5arcmin, 
                                          r_mgr_df = mgr_cnties, 
                                          variableName= 'urbanSumPp',  
                                          mybreaks = seq(-0.25, 0.25, 0.05)*1000,
                                          colorpal = scico::scico(length(mybreaks)+1,palette = 'vik', direction = -1),
                                          titleLabel = "Cumulative net-migration in urban areas (relative to pop in rural and urban areas in each province)",
                                          tocrs = "+proj=robin +over")

plot_counties_r <- myFun_create_rasterMap_3(r_index = r_gadm_lev1_5arcmin, 
                                          r_mgr_df = mgr_cnties, 
                                          variableName= 'ruralSumPp',  
                                          mybreaks = seq(-0.25, 0.25, 0.05)*1000,
                                          colorpal = scico::scico(length(mybreaks)+1,palette = 'vik', direction = -1),
                                          titleLabel = "Cumulative net-migration in rural areas (relative to pop in rural and urban areas in each province)",
                                          tocrs = "+proj=robin +over")

# Communes
plot_communes_u <- myFun_create_rasterMap_3(r_index = r_gadm_lev2_5arcmin, 
                                          r_mgr_df = mgr_communes, 
                                          variableName= 'urbanSumPp',  
                                          mybreaks = seq(-0.25, 0.25, 0.05)*1000,
                                          colorpal = scico::scico(length(mybreaks)+1,palette = 'vik', direction = -1),
                                          titleLabel = "Cumulative net-migration in urban areas (relative to pop in rural and urban areas in each commune/municipality)",
                                          tocrs = "+proj=robin +over")

plot_communes_r <- myFun_create_rasterMap_3(r_index = r_gadm_lev2_5arcmin, 
                                          r_mgr_df = mgr_communes, 
                                          variableName= 'ruralSumPp',  
                                          mybreaks = seq(-0.25, 0.25, 0.05)*1000,
                                          colorpal = scico::scico(length(mybreaks)+1,palette = 'vik', direction = -1),
                                          titleLabel = "Cumulative net-migration in rural areas (relative to pop in rural and urban areas in each commune/municipality)",
                                          tocrs = "+proj=robin +over")

p_colMgrPlot <- tmap_arrange(plot_cntries_u,plot_cntries_r,
                             plot_counties_u, plot_counties_r,
                             plot_communes_u, plot_communes_r,
                             ncol = 2, nrow=3)

tmap_save(p_colMgrPlot,filename = paste0("../fugures/Plots_MgrCumSum",Sys.Date(),'.pdf'),width = 160, height=160, units='mm')



#### Urban and rural net-migration maps (gridded) (Figure S5) ####

#### Plot urb & rural migration plots for 2000, 2010, 2019
urbanMgr <- rast("../results/netMigrUrban.tif")
ruralMgr <- rast("../results/netMigrRural.tif")

timesteps <- c(1,10,19)

urbanMgr <- subset(urbanMgr, timesteps)
ruralMgr <- subset(ruralMgr, timesteps)

source('../functions/myFun_create_Map.R')

mybreaks = seq(-50,50,10)

urbanMgr_map <- myFun_create_Map(r_raster = urbanMgr,
                                 mybreaks = mybreaks,
                                 colorpal = scico::scico(length(mybreaks)+1,palette = 'vik', direction = -1),
                                 titleLabel = "Net-migration in urban areas",
                                 tocrs = "+proj=robin +over")

ruralMgr_map <- myFun_create_Map(r_raster = ruralMgr,
                                 mybreaks = mybreaks,
                                 colorpal = scico::scico(length(mybreaks)+1,palette = 'vik', direction = -1),
                                 titleLabel = "Net-migration in rural areas",
                                 tocrs = "+proj=robin +over")

tmap_save(tmap_arrange(urbanMgr_map, ruralMgr_map, ncol = 2), paste0('../figures/urbanRuralMgr_grid_','.pdf'),width = 160, height=160, units='mm')








