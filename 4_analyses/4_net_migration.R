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
# Regions
regions <- terra::rast("DATA/regions.tif")
regions_df <- readr::read_csv("DATA/countriesRegionsZones.csv") %>% 
  dplyr::select(region_id, regionName) %>% 
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

myFun_mgrZonalStats <- function(r_adm, r_popTotal, r_urbanPop, r_ruralPop, r_mgrTotal, r_mgrUrban, r_mgrRural) { #r_popPos, r_popNeg, r_mgrPos, r_mgrNeg
  #r_admName <- names(r_adm)
  temp <- unique(values(r_adm)) %>% sort() %>% as_tibble %>% drop_na() %>% rename("admZone" = "value") #%>% select(cntry_raster_masked) %>% rename("country_id" = "cntry_raster_masked")
  for (i in 1:length(names(r_mgrTotal))){
    pop_temp <- terra::zonal(r_popTotal[[i]], r_adm, fun = sum, na.rm = T) %>% rename_at(2, ~ gsub("^.*?Pop","totPop_", .x),"_")
    pop_urban <- terra::zonal(r_urbanPop[[i]], r_adm, fun = sum, na.rm = T) %>% rename_at(2, ~gsub("^.*?Pop","UrbanPop_", .x),"_")
    pop_rural <- terra::zonal(r_ruralPop[[i]], r_adm, fun = sum, na.rm = T) %>% rename_at(2, ~ gsub("^.*?Pop","RuralPop_", .x),"_")
    #pos_pop <- terra::zonal(r_popPos[[i]], r_adm, fun = sum, na.rm =T) %>% rename_at(2, ~ gsub("^.*?Pop", "PosPop_", .x), "_")
    #neg_pop <- terra::zonal(r_popNeg[[i]], r_adm, fun = sum, na.rm =T) %>% rename_at(2, ~ gsub("^.*?Pop", "NegPop_", .x), "_")
    net_migr <- terra::zonal(r_mgrTotal[[i]], r_adm, fun = sum, na.rm = T)  %>% rename_at(2, ~ gsub("^.*?netMgr","TotMgr_", .x),"_")
    net_urban <- terra::zonal(r_mgrUrban[[i]], r_adm, fun = sum, na.rm =T) %>% rename_at(2, ~ gsub("^.*?netMgr", "UrbanMgr_", .x), "_")
    net_rural <- terra::zonal(r_mgrRural[[i]], r_adm, fun = sum, na.rm =T) %>% rename_at(2, ~ gsub("^.*?netMgr", "RuralMgr_", .x), "_")
    #pos_migr <- terra::zonal(r_mgrPos[[i]], r_adm, fun = sum, na.rm =T) %>% rename_at(2, ~ gsub("^.*?netMgr", "PosMgr_", .x), "_")
    #neg_migr <- terra::zonal(r_mgrNeg[[i]], r_adm, fun = sum, na.rm =T) %>% rename_at(2, ~ gsub("^.*?netMgr", "NegMgr_", .x), "_")
    
    temp <- bind_cols(temp, pop_temp[2],pop_urban[2],pop_rural[2],  net_migr[2], net_urban[2], net_rural[2]) #pos_pop[2], neg_pop[2], pos_migr[2], neg_migr[2]
  }
  
  zonalStatsMgr <- temp %>% 
    pivot_longer(!admZone, names_to = "variable", values_to = "value") %>% 
    mutate(year = variable) %>% 
    mutate(split = str_split(year, "_")) %>% 
    mutate(variable = sapply(split, "[[", 1),
           year = sapply(split, "[[", 2)) %>% 
    dplyr::select(-split) %>% 
    mutate(year = as.numeric(year))
  
  return(zonalStatsMgr)
}

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
readr::write_csv(netMigrGlobal, "results/zonalStatsMigrGlobal.csv")
readr::write_csv(zonalStatsRegions, 'results/zonalStatsRegions.csv')
readr::write_csv(zonalStatsCntries, 'results/zonalStatsCountries.csv')
readr::write_csv(zonalStatsCnties, 'results/zonalStatsCnties.csv')
readr::write_csv(zonalStatsCommunes, 'results/zonalStatsCommunes.csv')

#### 3) Plot bar plots and maps ####

## Open packages
library(tmap)
library(scico)
library(ggplot2)
library(rmapshaper)

#### Bar plot (Figure 3) ####
#load data
migr_global <- readr::read_csv("results/zonalStatsMigrGlobal.csv")
migr_regions <- readr::read_csv("results/zonalStatsRegions.csv")
migr_cntries <- readr::read_csv("results/zonalStatsCountries.csv")
migr_cnties <- readr::read_csv("results/zonalStatsCnties.csv")
migr_commun <- readr::read_csv("results/zonalStatsCommunes.csv")


# Regions + global
regions_df <- readr::read_csv('DATA/countryGroupsNames.csv')
# Combine regional and global data
migr_globalRegional <- migr_global %>% rename("totPop" = "popGlobal", "UrbanPop" = "popUrban", "RuralPop" = "popRural", 
                                              "TotMgr" = "netMgrGlobal", "UrbanMgr" = "netMgrUrban", "RuralMgr" = "netMgrRural") %>% 
  mutate(admZone = 13, regionName = "XGlobal") %>% 
  dplyr::select( admZone, year, UrbanPop, RuralPop, UrbanMgr, RuralMgr, regionName) %>% 
  bind_rows(migr_regions %>% filter(variable %in% c("UrbanPop", "RuralPop", "UrbanMgr", "RuralMgr")) %>% 
              pivot_wider(names_from = variable, values_from = value) %>% 
              left_join(., regions_df, by = c("admZone" = "regionID"))) %>% 
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

myFun_migrStats <- function(r_migr_df){ #r_adm_sf, r_cntry_sf, filter_var_vec, mybreaks, titleVar, colorpal, titleLabel
  mydata <- r_migr_df %>% 
    #filter(variable %in% c("cntryPopTot","cntryUrbanPop", "cntryRuralPop", "netMgr","netUrban","netRural")) %>% 
    pivot_wider(names_from = variable, values_from = value) %>% 
    group_by(admZone, year) %>% 
    # Calculate NM per population
    summarise(totNMpp = TotMgr/totPop, #total net-migraiton per pop
              urbanNMpp = (UrbanMgr/UrbanPop)*1000, #total urban net-migr per urban pop
              ruralNMpp = (RuralMgr/RuralPop)*1000) %>%  #total rural net-migr per rural pop
    # Then calculate cumulative sum over the time period
    summarise(totSumPp = sum(totNMpp, na.rm=T),
              urbanSumPp = sum(urbanNMpp, na.rm=T),
              ruralSumPp = sum(ruralNMpp, na.rm=T)) %>% 
    pivot_longer(!admZone, names_to = "variable", values_to = "value")
  
  return(mydata)
  
}


# function transfer dataframes to raster
myFun_create_rasterMap <- function(r_index, r_mgr_df, variableName, mybreaks,colorpal, titleLabel, #nameLabels
                                   tocrs = NA){ #
  
  classMatrix <- r_mgr_df %>% dplyr::filter(variable == variableName) %>% select(admZone, value) %>% as.matrix()
  
  r_mgr_classified <- terra::classify(r_index,
                                      classMatrix) #variable to plot
  
  # project to another crs
  if (!is.na(tocrs)){
    r_mgr_classified <- project(r_mgr_classified, tocrs, mask = TRUE)
  }
  
  # create tmap object
  index_map <- tm_shape(r_mgr_classified) +
    tm_raster(style = "fixed", # draw gradient instead of classified
              breaks = mybreaks ,
              palette = colorpal,
              #labels = nameLabels,
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

# Countries
plot_cntries_u <- myFun_create_rasterMap(r_index = r_gadm_lev0_5arcmin, 
                                         r_mgr_df = mgr_cntries, 
                                         variableName= 'urbanSumPp',  
                                         mybreaks = seq(-0.25, 0.25, 0.05)*1000,
                                         colorpal = scico::scico(length(mybreaks)+1,palette = 'vik', direction = -1),
                                         titleLabel = "Cumulative net-migration in urban areas (relative to pop in rural and urban areas in each country)",
                                         tocrs = "+proj=robin +over")

plot_cntries_r <- myFun_create_rasterMap(r_index = r_gadm_lev0_5arcmin, 
                                         r_mgr_df = mgr_cntries, 
                                         variableName= 'ruralSumPp',  
                                         mybreaks = seq(-0.25, 0.25, 0.05)*1000,
                                         colorpal = scico::scico(length(mybreaks)+1,palette = 'vik', direction = -1),
                                         titleLabel = "Cumulative net-migration in rural areas (relative to pop in rural and urban areas in each country)",
                                         tocrs = "+proj=robin +over")
# Counties
plot_counties_u <- myFun_create_rasterMap(r_index = r_gadm_lev1_5arcmin, 
                                          r_mgr_df = mgr_cnties, 
                                          variableName= 'urbanSumPp',  
                                          mybreaks = seq(-0.25, 0.25, 0.05)*1000,
                                          colorpal = scico::scico(length(mybreaks)+1,palette = 'vik', direction = -1),
                                          titleLabel = "Cumulative net-migration in urban areas (relative to pop in rural and urban areas in each province)",
                                          tocrs = "+proj=robin +over")

plot_counties_r <- myFun_create_rasterMap(r_index = r_gadm_lev1_5arcmin, 
                                          r_mgr_df = mgr_cnties, 
                                          variableName= 'ruralSumPp',  
                                          mybreaks = seq(-0.25, 0.25, 0.05)*1000,
                                          colorpal = scico::scico(length(mybreaks)+1,palette = 'vik', direction = -1),
                                          titleLabel = "Cumulative net-migration in rural areas (relative to pop in rural and urban areas in each province)",
                                          tocrs = "+proj=robin +over")

# Communes
plot_communes_u <- myFun_create_rasterMap(r_index = r_gadm_lev2_5arcmin, 
                                          r_mgr_df = mgr_communes, 
                                          variableName= 'urbanSumPp',  
                                          mybreaks = seq(-0.25, 0.25, 0.05)*1000,
                                          colorpal = scico::scico(length(mybreaks)+1,palette = 'vik', direction = -1),
                                          titleLabel = "Cumulative net-migration in urban areas (relative to pop in rural and urban areas in each commune/municipality)",
                                          tocrs = "+proj=robin +over")

plot_communes_r <- myFun_create_rasterMap(r_index = r_gadm_lev2_5arcmin, 
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

myFun_create_Map <- function(r_raster, mybreaks,colorpal, titleLabel, #nameLabels
                             tocrs = NA){ #
  
  # project to another crs
  if (!is.na(tocrs)){
    r_raster <- project(r_raster, tocrs, mask = TRUE)
  }
  
  # create tmap object
  index_map <- tm_shape(r_raster) +
    tm_raster(style = "fixed", # draw gradient instead of classified
              breaks = mybreaks ,
              palette = colorpal,
              #labels = nameLabels,
              showNA = F,
              colorNA = 'white',
              title = titleLabel) +
    tm_facets(free.scales = FALSE)
  
  return (index_map)
  
}

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

tmap_save(tmap_arrange(urbanMgr_map, ruralMgr_map, ncol = 2), paste0('../fugures/urbanRuralMgr_grid_', Sys.Date(),'.pdf'),width = 160, height=160, units='mm')








