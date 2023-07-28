# This code produces the analysis illustrated in Figure 5.
# The script 1) classifies admin units into seven classes based on the impact of urban/rural net-migration 
# on urban/rural population change (growth or decline), 
# 2) plots the results, and
# 3) calculates the share of urban and rural population in each class (Table in Fig 5)

rm(list = ls())

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(terra)
library(sf)
library(dplyr)
library(tmap)
library(tidyr)
library(scico)
library(rmapshaper)

# Load admin data
r_gadm_lev0_5arcmin <- rast('DATA/gadm_lev0_5arcmin.tif')
r_gadm_lev0_5arcmin[r_gadm_lev0_5arcmin == 10] <- NA
r_gadm_lev1_5arcmin <- rast('DATA/gadm_lev1_5arcmin.tif')
r_gadm_lev2_5arcmin <- rast('DATA/gadm_lev2_5arcmin.tif')


#### 1) Impact of migration on population growth / decline ####
# function to calculate the role of net migration in urban population change
myFunMgrImpactPop_u <- function(r_adm, r_popData, r_mgrData, nameOutput) {  
  
  #r_adm <- r_gadm_lev0_5arcmin
  adm_popChange <- as_tibble(terra::zonal(subset(r_popData,nlyr(r_popData)),r_adm, fun = sum, na.rm=T)) %>% 
    left_join(as_tibble(terra::zonal(subset(r_popData,1),r_adm, fun = sum, na.rm=T))) %>% 
    mutate(popChange = cntryPop2020 - cntryPop2000) 
  
  # then net-migration over the whole study period
  adm_netMgr <- as_tibble(terra::zonal(r_mgrData,r_adm, fun = sum, na.rm=T)) %>% 
    rowwise() %>% 
    mutate(netMgrSum = sum(c_across(contains("net")))) %>% 
    ungroup() %>% 
    dplyr::select(1,netMgrSum)
  
  # combine
  adm_popChngMgr <- adm_popChange %>% 
    left_join(adm_netMgr) %>% 
    # calculate population change without migration
    mutate(popChng_woMgr = popChange - netMgrSum) %>% 
    # calculate whether natural growth or migration impacts more on pop change
    mutate(rolePopMgr = 1-popChng_woMgr/popChange) %>% 
    # check whether 
    # migration increases pop growth:1 (3)
    # migration decreases pop growth (slows population growth): -2 (2)
    # migration turns pop growth to declined pop: -3 (1)
    # migration accelerates pop decline: -1 (-3)
    # migration slows pop decline: 2 (-2)
    # migration turns pop decline to growth: 3 (-1)
    mutate(popMgr_classif = if_else((popChng_woMgr > 0 & netMgrSum > 0), 1,
                                    if_else((popChange > 0 & netMgrSum < 0), -2,
                                            if_else((popChng_woMgr > 0 & popChange < 0), -3,
                                                    if_else((popChng_woMgr < 0 & netMgrSum < 0), -1,
                                                            if_else((popChange < 0 & netMgrSum > 0), 2,
                                                                    if_else((popChng_woMgr < 0 & popChange > 0), 3,#0)))))))
                                                                            0
                                                                    ))))))) 
  
  
  adm_popChngMgr_summary <- adm_popChngMgr %>% 
    group_by(popMgr_classif) %>% 
    summarise_at(vars(contains('cntryPop2020')),list(sum))%>%
    ungroup()
  
  
  # put the classification to a raster
  r_adm_class <- classify(r_adm,
                          cbind(adm_popChngMgr[,1], adm_popChngMgr$popMgr_classif))
  
  r_adm_roleMgrInPopChange <- classify(r_adm,
                                       cbind(adm_popChngMgr[,1], adm_popChngMgr$rolePopMgr))
  #plot(r_adm_class)
  
  writeRaster(r_adm_class,paste0('results/r_urban_',nameOutput,'_',Sys.Date(),'.tif'),  gdal="COMPRESS=LZW",overwrite=TRUE)
  return(adm_popChngMgr)
}
# function to calculate the role of net migration in rural population change
myFunMgrImpactPop_r <- function(r_adm, r_popData, r_mgrData, nameOutput) {  
  
  #r_adm <- r_gadm_lev0_5arcmin
  adm_popChange <- as_tibble(terra::zonal(subset(r_popData,nlyr(r_popData)),r_adm, fun = sum, na.rm=T)) %>% 
    left_join(as_tibble(terra::zonal(subset(r_popData,1),r_adm, fun = sum, na.rm=T))) %>% 
    mutate(popChange = cntryPop2020 - cntryPop2000) 
  
  # then net-migration over the whole study period
  adm_netMgr <- as_tibble(terra::zonal(r_mgrData,r_adm, fun = sum, na.rm=T)) %>% 
    rowwise() %>% 
    mutate(netMgrSum = sum(c_across(contains("net")))) %>% 
    ungroup() %>% 
    dplyr::select(1,netMgrSum)
  
  # combine
  adm_popChngMgr <- adm_popChange %>% 
    left_join(adm_netMgr) %>% 
    # calculate population change without migration
    mutate(popChng_woMgr = popChange - netMgrSum) %>% 
    # calculate whether natural growth or migration impacts more on pop change
    mutate(rolePopMgr = 1-popChng_woMgr/popChange) %>% 
    # check whether 
    # migration increases pop growth:1 (3)
    # migration slows population growth: -2 (2)
    # migration turns pop growth to declined pop: -3 (1)
    # migration accelerates pop decline: -1 (-3)
    # migration slows pop decline: 2 (-2)
    # migration turns pop decline to growth: 3 (-1)
    mutate(popMgr_classif = if_else((popChng_woMgr > 0 & netMgrSum > 0), 1,
                                    if_else((popChange > 0 & netMgrSum < 0), -2,
                                            if_else((popChng_woMgr > 0 & popChange < 0), -3,
                                                    if_else((popChng_woMgr < 0 & netMgrSum < 0), -1,
                                                            if_else((popChange < 0 & netMgrSum > 0), 2,
                                                                    if_else((popChng_woMgr < 0 & popChange > 0), 3,#0)))))))
                                                                            0
                                                                    ))))))) 
  
  
  adm_popChngMgr_summary <- adm_popChngMgr %>% 
    group_by(popMgr_classif) %>% 
    summarise_at(vars(contains('cntryPop2020')),list(sum))%>%
    ungroup()
  
  
  # put the classification to a raster
  r_adm_class <- classify(r_adm,
                          cbind(adm_popChngMgr[,1], adm_popChngMgr$popMgr_classif))
  
  r_adm_roleMgrInPopChange <- classify(r_adm,
                                       cbind(adm_popChngMgr[,1], adm_popChngMgr$rolePopMgr))
  #plot(r_adm_class)
  
  writeRaster(r_adm_class,paste0('results/r_rural_',nameOutput,'_',Sys.Date(),'.tif'),  gdal="COMPRESS=LZW",overwrite=TRUE)
  return(adm_popChngMgr)
}

#### Urban areas ####
r_netMigration <- rast('DATA/netMigrUrban.tif')
r_pop <- rast('DATA/popUrban.tif')
#names(r_pop) <- paste0("cntryPop", names(r_pop))
timeSteps <- c(2001:2020)

# Run the function
# communal level
comm_popChngMgr_u <- myFunMgrImpactPop_u(r_gadm_lev2_5arcmin,r_pop, r_netMigration, 'commClassRoleMgr')
# provincial level
prov_popChngMgr_u <- myFunMgrImpactPop_u(r_gadm_lev1_5arcmin,r_pop, r_netMigration,'provClassRoleMgr')
# country level
cntry_popChngMgr_u <- myFunMgrImpactPop_u(r_gadm_lev0_5arcmin,r_pop, r_netMigration,'cntryClassRoleMgr')

readr::write_csv(comm_popChngMgr_u, paste0('results/comm_popChngMgr_urban_', Sys.Date(),'.csv'))
readr::write_csv(prov_popChngMgr_u, paste0('results/prov_popChngMgr_urban_', Sys.Date(),'.csv'))
readr::write_csv(cntry_popChngMgr_u, paste0('results/cntry_popChngMgr_urban_', Sys.Date(),'.csv'))

#### Rural areas ####
r_netMigration <- rast('DATA/netMigrRural.tif')
r_pop <- rast('DATA/popRural.tif')
#names(r_pop) <- paste0("cntryPop", names(r_pop))
timeSteps <- c(2001:2020)

comm_popChngMgr_r <- myFunMgrImpactPop_r(r_gadm_lev2_5arcmin,r_pop, r_netMigration, 'commClassRoleMgr')
prov_popChngMgr_r <- myFunMgrImpactPop_r(r_gadm_lev1_5arcmin,r_pop, r_netMigration,'provClassRoleMgr')
cntry_popChngMgr_r <- myFunMgrImpactPop_r(r_gadm_lev0_5arcmin,r_pop, r_netMigration,'cntryClassRoleMgr')

readr::write_csv(comm_popChngMgr_r, 'results/comm_popChngMgr_rural.csv')
readr::write_csv(prov_popChngMgr_r, 'results/prov_popChngMgr_rural.csv')
readr::write_csv(cntry_popChngMgr_r, 'results/cntry_popChngMgr_rural.csv')

#### 2) Plot results ####

# function to plot the raster

myFun_create_rasterMap <- function(r_index, nameLabels, titleLabel, colorpal,
                                   tocrs = NA){
  
  # project to another crs
  if (!is.na(tocrs)){
    r_index <- project(r_index, tocrs, mask = TRUE)
  }
  
  # create tmap object
  index_map <- tm_shape(r_index) +
    tm_raster(style = "fixed", # draw gradient instead of classified
              breaks = c(-3.5,-2.5,-1.5,-0.5,.5,1.5,2.5,3.5),
              palette = colorpal,
              labels = nameLabels,
              showNA = F,
              colorNA = 'white',
              title = titleLabel,
              legend.reverse = TRUE) +
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

# check whether 
# migration increases pop growth: 3
# migration decreases pop growth: 2
# migration turns pop growth to declined pop: 1
# migration exalarate pop decline: -3
# migration slows pop decline: -2
# migration turns pop decline to growth: -1

mgrPopLabels <- c('migration turns pop growth to declined pop',
                  'migration decreases pop growth (slows population growth)',
                  'migration accelerates pop decline',
                  'no data',
                  'migration increases pop growth',
                  'migration slows pop decline',
                  'migration turns pop decline to growth')


mycol <- c("#c92d47","#d2748a","#ffc4d2","grey90","#b3d4ff","#718ebd","#4166b0")

#### urban plots ####
p_comm_u <- myFun_create_rasterMap(rast(paste0('results/', 'r_urban_cntryClassRoleMgr_', Sys.Date(), '.tif')),
                                 mgrPopLabels,
                                 'impact of migration on urban pop change - communal',
                                 colorpal = mycol, #scico(9,palette = 'vikO'),
                                 tocrs = "+proj=robin +over")

p_prov_u <- myFun_create_rasterMap(rast(paste0('results/','r_urban_provClassRoleMgr_', Sys.Date(), '.tif')),
                                 mgrPopLabels,
                                 'impact of migration on urban pop change - provincial',
                                 colorpal = mycol, #scico(9,palette = 'roma'),
                                 tocrs = "+proj=robin +over")

p_cntry_u <- myFun_create_rasterMap(rast(paste0('results/','r_urban_commClassRoleMgr_', Sys.Date(), '.tif')),
                                  mgrPopLabels,
                                  'impact of migration on urban pop change - country',
                                  colorpal = mycol, #scico(9,palette = 'berlin'),
                                  tocrs = "+proj=robin +over")

p_colMgrPop_u <- tmap_arrange(p_cntry_u,p_prov_u, p_comm_u,ncol = 1)
tmap_save(p_colMgrPop_u,filename = paste0('results/plots/mapsMgrImpactPopUrban',Sys.Date(),'.pdf'),width = 160, height=160, units='mm')

#### rural plots ####
p_comm_r <- myFun_create_rasterMap(rast(paste0('results/r_rural_commClassRoleMgr_',Sys.Date(), '.tif')),
                                 mgrPopLabels,
                                 'impact of migration on rural pop change - communal',
                                 colorpal = mycol, #scico(9,palette = 'roma'),
                                 tocrs = "+proj=robin +over")

p_prov_r <- myFun_create_rasterMap(rast(paste0('results/r_rural_provClassRoleMgr_',Sys.Date(), '.tif')),
                                 mgrPopLabels,
                                 'impact of migration on rural pop change - provincial',
                                 colorpal = mycol, #scico(9,palette = 'roma'),
                                 tocrs = "+proj=robin +over")

p_cntry_r <- myFun_create_rasterMap(rast(paste0('results/r_rural_cntryClassRoleMgr_',Sys.Date(), '.tif')),
                                  mgrPopLabels,
                                  'impact of migration on rural pop change - country',
                                  colorpal = mycol, #scico(9,palette = 'roma'),
                                  tocrs = "+proj=robin +over")

p_colMgrPop_r <- tmap_arrange(p_cntry_r,p_prov_r, p_comm_r,ncol = 1)

tmap_save(p_colMgrPop_r,filename = paste0('results/plots/mapsMgrImpactPopRural',Sys.Date(),'.pdf'),width = 160, height=160, units='mm')

#### 3) The share of urban and rural population in each class ####

# Calculate  the percentage of rural and urban people living in impact zones

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

# percentage of urban population living in urban impact zones
u_cntry <- rast('results/r_urban_cntryClassRoleMgr_2023-02-06.tif')
u_prov <- rast('results/r_urban_provClassRoleMgr_2023-02-06.tif')
u_comm <- rast('results/r_urban_commClassRoleMgr_2023-02-06.tif')

u_cntry_zonal <- myFun(u_pop, u_cntry, 'cntry')
u_prov_zonal <- myFun(u_pop, u_prov, 'prov')
u_comm_zonal <- myFun(u_pop, u_comm, 'comm')

u_pop_zonal <- left_join(u_cntry_zonal, u_prov_zonal) %>% left_join(., u_comm_zonal) %>% 
  mutate(shr_pop_cntry = round(shr_pop_cntry, digits = 2)*100,
         shr_pop_prov = round(shr_pop_prov, digits = 2)*100,
         shr_pop_comm = round(shr_pop_comm, digits = 2)*100) 

readr::write_csv(u_pop_zonal, paste0('results/share_of_urbanPop_impact_zones_',Sys.Date(),'.csv'))

# percentage of rural population living in rural impact zones
r_cntry <- rast('results/r_rural_cntryClassRoleMgr_2023-02-06.tif')
r_prov <- rast('results/r_rural_provClassRoleMgr_2023-02-06.tif')
r_comm <- rast('results/r_rural_commClassRoleMgr_2023-02-06.tif')

r_cntry_zonal <- myFun(r_pop, r_cntry, 'cntry')
r_prov_zonal <- myFun(r_pop, r_prov, 'prov')
r_comm_zonal <- myFun(r_pop, r_comm, 'comm')

r_pop_zonal <- left_join(r_cntry_zonal, r_prov_zonal) %>% left_join(., r_comm_zonal) %>% 
  mutate(shr_pop_cntry = round(shr_pop_cntry, digits = 2)*100,
         shr_pop_prov = round(shr_pop_prov, digits = 2)*100,
         shr_pop_comm = round(shr_pop_comm, digits = 2)*100) 

readr::write_csv(r_pop_zonal, paste0('results/share_of_ruralPop_impact_zones_',Sys.Date(),'.csv'))


