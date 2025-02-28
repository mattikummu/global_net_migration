# This code produces the analysis illustrated in Figure XX.
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
r_gadm_lev0_5arcmin <- rast('../data_in_rast/gadm_lev0_5arcmin.tif')
r_gadm_lev0_5arcmin[r_gadm_lev0_5arcmin == 10] <- NA 
r_gadm_lev1_5arcmin <- rast('../data_in_rast/gadm_lev1_5arcmin.tif')
r_gadm_lev2_5arcmin <- rast('../data_in_rast/gadm_lev2_5arcmin.tif')


#### 1) Impact of migration on population growth / decline ####
# function to calculate the role of net migration in urban population change
source('../functions/myFunMgrImpactPop_u.R')
# function to calculate the role of net migration in rural population change
source('../functions/myFunMgrImpactPop_r.R')

#### Urban areas ####
u_netMigration <- rast('../results/netMigrUrban.tif')
u_pop <- rast('../results/popUrban.tif')
#names(r_pop) <- paste0("cntryPop", names(r_pop))
timeSteps <- c(2001:2020)

# Run the function
# communal level
comm_popChngMgr_u <- myFunMgrImpactPop_u(r_gadm_lev2_5arcmin,u_pop, u_netMigration, 'commClassRoleMgr')
# provincial level
prov_popChngMgr_u <- myFunMgrImpactPop_u(r_gadm_lev1_5arcmin,u_pop, u_netMigration,'provClassRoleMgr')
# country level
cntry_popChngMgr_u <- myFunMgrImpactPop_u(r_gadm_lev0_5arcmin,u_pop, u_netMigration,'cntryClassRoleMgr')

readr::write_csv(comm_popChngMgr_u, paste0('../results/comm_popChngMgr_urban.csv'))
readr::write_csv(prov_popChngMgr_u, paste0('../results/prov_popChngMgr_urban.csv'))
readr::write_csv(cntry_popChngMgr_u, paste0('../results/cntry_popChngMgr_urban.csv'))

#### Rural areas ####
r_netMigration <- rast('../results/netMigrRural.tif')
r_pop <- rast('../results/popRural.tif')
#names(r_pop) <- paste0("cntryPop", names(r_pop))
timeSteps <- c(2001:2020)

comm_popChngMgr_r <- myFunMgrImpactPop_r(r_gadm_lev2_5arcmin,r_pop, r_netMigration, 'commClassRoleMgr')
prov_popChngMgr_r <- myFunMgrImpactPop_r(r_gadm_lev1_5arcmin,r_pop, r_netMigration,'provClassRoleMgr')
cntry_popChngMgr_r <- myFunMgrImpactPop_r(r_gadm_lev0_5arcmin,r_pop, r_netMigration,'cntryClassRoleMgr')

readr::write_csv(comm_popChngMgr_r, '../results/comm_popChngMgr_rural.csv')
readr::write_csv(prov_popChngMgr_r, '../results/prov_popChngMgr_rural.csv')
readr::write_csv(cntry_popChngMgr_r, '../results/cntry_popChngMgr_rural.csv')

#### 2) Plot results ####

# function to plot the raster
source('../functions/myFun_create_rasterMap_2.R')

sf_gadm0 <- terra::as.polygons(rast('../data_in_rast/gadm_lev0_5arcmin.tif')) %>% 
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
p_comm_u <- myFun_create_rasterMap_2(rast(paste0('../results/', 'r_urban_cntryClassRoleMgr.tif')),
                                 mgrPopLabels,
                                 'impact of migration on urban pop change - communal',
                                 colorpal = mycol, #scico(9,palette = 'vikO'),
                                 tocrs = "+proj=robin +over")

p_prov_u <- myFun_create_rasterMap_2(rast(paste0('../results/','r_urban_provClassRoleMgr.tif')),
                                 mgrPopLabels,
                                 'impact of migration on urban pop change - provincial',
                                 colorpal = mycol, #scico(9,palette = 'roma'),
                                 tocrs = "+proj=robin +over")

p_cntry_u <- myFun_create_rasterMap_2(rast(paste0('../results/','r_urban_commClassRoleMgr.tif')),
                                  mgrPopLabels,
                                  'impact of migration on urban pop change - country',
                                  colorpal = mycol, #scico(9,palette = 'berlin'),
                                  tocrs = "+proj=robin +over")

p_colMgrPop_u <- tmap_arrange(p_cntry_u,p_prov_u, p_comm_u,ncol = 1)
tmap_save(p_colMgrPop_u,filename = paste0('../figures/mapsMgrImpactPopUrban','.pdf'),width = 160, height=160, units='mm')

#### rural plots ####
p_comm_r <- myFun_create_rasterMap_2(rast(paste0('../results/r_rural_commClassRoleMgr.tif')),
                                 mgrPopLabels,
                                 'impact of migration on rural pop change - communal',
                                 colorpal = mycol, #scico(9,palette = 'roma'),
                                 tocrs = "+proj=robin +over")

p_prov_r <- myFun_create_rasterMap_2(rast(paste0('../results/r_rural_provClassRoleMgr.tif')),
                                 mgrPopLabels,
                                 'impact of migration on rural pop change - provincial',
                                 colorpal = mycol, #scico(9,palette = 'roma'),
                                 tocrs = "+proj=robin +over")

p_cntry_r <- myFun_create_rasterMap_2(rast(paste0('../results/r_rural_cntryClassRoleMgr.tif')),
                                  mgrPopLabels,
                                  'impact of migration on rural pop change - country',
                                  colorpal = mycol, #scico(9,palette = 'roma'),
                                  tocrs = "+proj=robin +over")

p_colMgrPop_r <- tmap_arrange(p_cntry_r,p_prov_r, p_comm_r,ncol = 1)

tmap_save(p_colMgrPop_r,filename = paste0('../figures/mapsMgrImpactPopRural','.pdf'),width = 160, height=160, units='mm')

#### 3) The share of urban and rural population in each class ####

# Calculate  the percentage of rural and urban people living in impact zones

source('../functions/myFun_percUrb.R')

# percentage of urban population living in urban impact zones
u_cntry <- rast('../results/r_urban_cntryClassRoleMgr.tif')
u_prov <- rast('../results/r_urban_provClassRoleMgr.tif')
u_comm <- rast('../results/r_urban_commClassRoleMgr.tif')

u_cntry_zonal <- myFun_percUrb(u_pop, u_cntry, 'cntry')
u_prov_zonal <- myFun_percUrb(u_pop, u_prov, 'prov')
u_comm_zonal <- myFun_percUrb(u_pop, u_comm, 'comm')

u_pop_zonal <- left_join(u_cntry_zonal, u_prov_zonal) %>% left_join(., u_comm_zonal) %>% 
  mutate(shr_pop_cntry = round(shr_pop_cntry, digits = 2)*100,
         shr_pop_prov = round(shr_pop_prov, digits = 2)*100,
         shr_pop_comm = round(shr_pop_comm, digits = 2)*100) 

readr::write_csv(u_pop_zonal, paste0('../results/share_of_urbanPop_impact_zones','.csv'))

# percentage of rural population living in rural impact zones
r_cntry <- rast('../results/r_rural_cntryClassRoleMgr.tif')
r_prov <- rast('../results/r_rural_provClassRoleMgr.tif')
r_comm <- rast('../results/r_rural_commClassRoleMgr.tif')

r_cntry_zonal <- myFun_percUrb(r_pop, r_cntry, 'cntry')
r_prov_zonal <- myFun_percUrb(r_pop, r_prov, 'prov')
r_comm_zonal <- myFun_percUrb(r_pop, r_comm, 'comm')

r_pop_zonal <- left_join(r_cntry_zonal, r_prov_zonal) %>% left_join(., r_comm_zonal) %>% 
  mutate(shr_pop_cntry = round(shr_pop_cntry, digits = 2)*100,
         shr_pop_prov = round(shr_pop_prov, digits = 2)*100,
         shr_pop_comm = round(shr_pop_comm, digits = 2)*100) 

readr::write_csv(r_pop_zonal, paste0('../results/share_of_ruralPop_impact_zones','.csv'))


