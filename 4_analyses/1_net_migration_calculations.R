
library(raster)
library(terra)
library(sf)
library(dplyr)
library(tmap)


library(RCurl)
library(openxlsx)
library(Kendall)
library(scico)
library(rnaturalearth)
library(rmapshaper)
library(readr)
library(ggplot2)

library(tidyterra)
library(tidyr)

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))



# read data

cntryID <- read_csv("../data_in/countries_codes_and_coordinates.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  select(cntry_code,iso2,iso3,Country) %>% 
  mutate(iso2 = ifelse(Country == 'Namibia','NB',iso2))

sf_gadm_lev0 <- read_sf("../data_in_gpkg/gadm_level0.gpkg")%>%
  st_drop_geometry() %>% 
  select(GID_0,NAME_0) %>% 
  rename(iso3 = GID_0) %>% 
  filter(iso3 != 'ALA') %>% # remove Aland
  left_join(cntryID,by='iso3') %>% 
  select(Country,iso3,cntry_code) %>% 
  arrange(iso3)


sf_gadm_lev1 <- read_sf( '../data_in_gpkg/gadm_lev1.gpkg' ) %>% 
  st_drop_geometry() %>% 
  select(GID_0,NAME_0,NAME_1,GID_1) %>% 
  rename(iso3 = GID_0) %>% 
  rename(Subnat = NAME_1) %>% 
  mutate(nmbr = row_number()) %>%
  #filter(iso3 != 'ALA') %>% # remove Aland
  left_join(cntryID,by='iso3') %>% 
  select(Country,iso3,cntry_code,GID_1,nmbr) %>% 
  arrange(iso3)

sf_gadm_lev2 <- read_sf( '../data_in_gpkg/gadm_lev2.gpkg' ) %>% 
  st_drop_geometry() %>% 
  select(GID_0,NAME_0,NAME_1,GID_1, NAME_2,GID_2) %>% 
  rename(iso3 = GID_0) %>% 
  rename(Subnat = NAME_1) %>% 
  mutate(nmbr2 = row_number()) %>%
  #filter(iso3 != 'ALA') %>% # remove Aland
  left_join(cntryID,by='iso3') %>% 
  select(Country,iso3,cntry_code,GID_1,GID_2,nmbr2) %>% 
  arrange(iso3)


# sf_gadm_lev1 <- st_read("../data_in/gadm_lev1.gpkg") %>% 
#   mutate(nmbr = row_number()) %>%
#   st_as_sf() 

r_gadm_lev0_5arcmin <- rast('../data_in_rast/gadm_lev0_5arcmin.tif')
r_gadm_lev1_5arcmin <- rast('../data_in_rast/gadm_lev1_5arcmin.tif')
r_gadm_lev2_5arcmin <- rast('../data_in_rast/gadm_lev2_5arcmin.tif')

#### setup the calculations ----

# MODE can be 'annual' or '3yr' or '5yr'
MODE = 'annual'

# CALC can be 'impactMgrPop' (calculating the impact of migration on population change)
# or 'noImpact'
CALC = 'impactMgrPop' 

if(MODE == '3yr') {
  r_netMigration <- rast('../results/netMgr_2001_2020_v3_3yrSum.tif')
  r_pop <- rast('../results/r_worldpopHarmonised_3yrMean.tif')
  timeSteps <- seq(2002,2020,3)
  TSlength <- 3
  TSlength_last <- 2
} else if(MODE == '5yr') {
  r_netMigration <- rast('../results/netMgr_2001_2020_v3_5yrSum.tif')
  r_pop <- rast('../results/r_worldpopHarmonised_5yrMean.tif')
  timeSteps <- seq(2003,2018,5)
  TSlength <- 5
  TSlength_last <- 5
} else {
  r_netMigration <- rast('../results/netMgr_2001_2020_v3.tif')
  r_pop <- rast('../results/r_worldpopHarmonised.tif')
  timeSteps <- c(2001:2020)
  TSlength <- 1
  TSlength_last <- 1
  # do nothing
}


#### define functions -----

# function to calculate slope
source('../functions/f_funSlope.R')

# function to calculate the role of net migration in population change
source('../functions/myFunMgrImpactPop.R')

# plot raster

source('../functions/myFun_create_rasterMap.R')


#### impact of migration on population growth / decline ----

if(MODE == 'annual' & CALC == 'impactMgrPop') {
  
  # communal level
  comm_popChngMgr <- myFunMgrImpactPop(r_gadm_lev2_5arcmin,r_pop, r_netMigration, 'commClassRoleMgr')
  
  # provincial level
  prov_popChngMgr <- myFunMgrImpactPop(r_gadm_lev1_5arcmin,r_pop, r_netMigration,'provClassRoleMgr')
  
  # country level
  cntry_popChngMgr <- myFunMgrImpactPop(r_gadm_lev0_5arcmin,r_pop, r_netMigration,'cntryClassRoleMgr')
  
  # plot
 
  sf_gadm0 <- terra::as.polygons(rast('../data_in_rast/gadm_lev0_5arcmin.tif')) %>% 
    sf::st_as_sf() %>% # to sf
    rmapshaper::ms_simplify(.,keep=0.1,keep_shapes = T) # simplify
  
  
  mgrPopLabels <- c('migration accelarates pop decline',
                    'migration slows pop decline',
                    'migration turns pop decline to growth',
                    'no data',
                    'migration turns pop growth to declined pop',
                    'migration slows pop growth',
                    'migration increases pop growth')
  
  p_comm <- myFun_create_rasterMap(rast('../results/r_commClassRoleMgr.tif'),
                                   mgrPopLabels,
                                   'impact of migration on pop change - communal',
                                   colorpal = scico(9,palette = 'roma'),
                                   tocrs = "+proj=robin +over")
  
  p_prov <- myFun_create_rasterMap(rast('../results/r_provClassRoleMgr.tif'),
                                   mgrPopLabels,
                                   'impact of migration on pop change - provincial',
                                   colorpal = scico(9,palette = 'roma'),
                                   tocrs = "+proj=robin +over")
  
  p_cntry <- myFun_create_rasterMap(rast('../results/r_cntryClassRoleMgr.tif'),
                                    mgrPopLabels,
                                    'impact of migration on pop change - country',
                                    colorpal = scico(9,palette = 'roma'),
                                    tocrs = "+proj=robin +over")
  
  p_colMgrPop <- tmap_arrange(p_comm,p_prov, p_cntry, ncol = 1)
  
  tmap_save(p_colMgrPop,filename = paste0('../figures/mapsMgrImpactPop.pdf'),width = 160, height=160, units='mm')
  
  
  
} else{
  # do nothing
}

#### in and out -migration ----

r_outMigration <- r_netMigration
r_outMigration[r_outMigration > 0] <- 0

r_inMigration <- r_netMigration
r_inMigration[r_inMigration < 0] <- 0

names(r_outMigration) <- c(paste0('outMgr',timeSteps))
names(r_inMigration) <- c(paste0('inMgr',timeSteps))

grid_summary <- global(r_outMigration, fun = "sum", na.rm=TRUE) %>% as_tibble()
grid_summary[,2] <- global(r_inMigration, fun = "sum", na.rm=TRUE)  %>% as_tibble()

grid_summary[,3] <- grid_summary[,2] + grid_summary[,1] %>% 
  as_tibble()

names(grid_summary) <- c("grid_outMgr","grid_inMgr", "grid_netMigr")


#### communal level analysis ----

comm_inMigr <- as_tibble(terra::zonal(r_inMigration,r_gadm_lev2_5arcmin, fun = sum, na.rm=T))
comm_outMigr <- as_tibble(terra::zonal(r_outMigration,r_gadm_lev2_5arcmin, fun = sum, na.rm=T))
comm_pop <- as_tibble(terra::zonal(r_pop,r_gadm_lev2_5arcmin, fun = sum, na.rm=T)) %>% 
  left_join(sf_gadm_lev2) 

names(comm_inMigr) <- c('nmbr2',paste0('comm_in',timeSteps))
names(comm_outMigr) <- c('nmbr2',paste0('comm_out',timeSteps))


# join in and out migrations to one dataset
comm_all <- comm_inMigr %>% 
  left_join(comm_outMigr)

# calculate net migration
comm_netMigr <- cbind( comm_all['nmbr2'],
                       comm_all[2:(length(timeSteps)+1)] + 
                         comm_all[(length(timeSteps)+2):(2*length(timeSteps)+1)]) %>% 
  as_tibble() #%>% 

names(comm_netMigr) <- c('nmbr2',paste0('commNet',timeSteps))


# calculate communal scale relative (to total population) net migration for each year

comm_popSel <- comm_netMigr %>% 
  left_join(comm_pop) 

comm_netMigr_rel <- cbind( comm_popSel['nmbr2'],
                           comm_popSel[,2:(length(timeSteps)+1)] / 
                             comm_popSel[,(length(timeSteps)+2):(2*length(timeSteps)+1)]) %>% 
  as_tibble() %>% 
  rowwise() %>%
  mutate(commNetMgrSum = sum(c_across(contains("Net")))) %>%
  ungroup() %>% 
  mutate(across(-c(nmbr2,commNet2020,commNetMgrSum), ~ . / TSlength)) %>% # three year mean; here we divide by three
  mutate(across(contains('2020'), ~ . / TSlength_last)) %>% # except the last timestep with 2 yr
  rowwise() %>% 
  mutate(commNetMgrSlope = f_funSlope(c_across(-c(nmbr2,commNetMgrSum)))) %>%
  ungroup() %>% 
  select(nmbr2,commNetMgrSum,commNetMgrSlope,everything())



##### subnational analysis -----


# collect in and out migration to each subnational unit 
# for net-migration we use the grid scale data, as not all subnat units 
# have level 2 division

subnat_inMigr <- as_tibble(terra::zonal(r_inMigration,r_gadm_lev1_5arcmin, fun = sum, na.rm=T))
subnat_outMigr <- as_tibble(terra::zonal(r_outMigration,r_gadm_lev1_5arcmin, fun = sum, na.rm=T))
subnat_pop <- as_tibble(terra::zonal(r_pop,r_gadm_lev1_5arcmin, fun = sum, na.rm=T)) %>% 
  left_join(sf_gadm_lev1) 

names(subnat_inMigr) <- c('nmbr',paste0('subnat_in',timeSteps))
names(subnat_outMigr) <- c('nmbr',paste0('subnat_out',timeSteps))

# calculate net migration

# join in and out migrations to one dataset
subnat_all <- subnat_inMigr %>% 
  left_join(subnat_outMigr)

# calculate net migration
subnat_netMigr <- cbind( subnat_all['nmbr'],
                         subnat_all[2:(length(timeSteps)+1)] + 
                           subnat_all[(length(timeSteps)+2):(2*length(timeSteps)+1)]) %>% 
  as_tibble() #%>% 
#mutate_if(is.numeric, function(x) ifelse(is.infinite(x), 0, x))

names(subnat_netMigr) <- c('nmbr',paste0('subnatNet',timeSteps))

subnat_popSel <- subnat_netMigr %>% 
  left_join(subnat_pop) 
#select(-contains('Net'))

# calculate subnational scale relative (to total population) net migration for each year
subnat_netMigr_rel <- cbind( subnat_popSel['nmbr'],
                             subnat_popSel[,2:(length(timeSteps)+1)] / 
                               subnat_popSel[,(length(timeSteps)+2):(2*length(timeSteps)+1)]) %>% 
  as_tibble() %>% 
  rowwise() %>%
  mutate(subnatNetMgrSum = sum(c_across(contains("Net")))) %>%
  ungroup() %>% 
  mutate(across(-c(nmbr,subnatNet2020,subnatNetMgrSum), ~ . / TSlength)) %>% # three year mean; here we divide by three
  mutate(across(contains('2020'), ~ . / TSlength_last)) %>% # except the last timestep with 2 yr
  rowwise() %>% 
  mutate(subnatNetMgrSlope = f_funSlope(c_across(-c(nmbr,subnatNetMgrSum)))) %>%
  ungroup() %>% 
  select(nmbr,subnatNetMgrSum,subnatNetMgrSlope,everything())


# rowwise() %>%
#   mutate(IntraProvSlope = f_funSlope(c_across(contains("Provincial")))) %>%
#   ungroup() %>% 

# cbind( subnat_netMigr['nmbr'],
#                            subnat_netMigr[2:21] / subnat_pop[2:21])


subnat_netMigr_out <- subnat_netMigr 
subnat_netMigr_out[subnat_netMigr_out > 0] <- 0

subnat_netMigr_in <- subnat_netMigr 
subnat_netMigr_in[subnat_netMigr_in < 0] <- 0

subnat_summary <- colSums(subnat_netMigr_out[2:(length(timeSteps)+1)]) %>% as_tibble()
subnat_summary[,2] <- as_tibble(colSums(subnat_netMigr_in[2:(length(timeSteps)+1)]))
subnat_summary[,3] <- subnat_summary[,2] + subnat_summary[,1]

names(subnat_summary) <- c("subnat_outMgr","subnat_inMgr", "subnat_netMigr")



## inter-communal migration within each subnat area

# then we'll calculate the balance, which is the net migration from a coomunal
# if positive, then there in-migration is larger, if negative then out-migration is larger

comm_balance <- comm_netMigr %>% 
  left_join(sf_gadm_lev2) 


# then calculate for each subnat the sum of in and out migration based on communal calculations

# Create a dataframe for "out" where positive values are set to 0
subnat_comm_out <- comm_balance %>% 
  mutate(across(contains('commNet'), ~ ifelse(. > 0, 0, .))) %>% 
  group_by(GID_1) %>% 
  summarise(across(contains('commNet'), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(sf_gadm_lev1, by = "GID_1") %>% 
  select(-c(GID_1, Country, iso3, cntry_code)) %>% 
  select(nmbr, everything())

# Create a dataframe for "in" where negative values are set to 0
subnat_comm_in <- comm_balance %>% 
  mutate(across(contains('commNet'), ~ ifelse(. < 0, 0, .))) %>% 
  group_by(GID_1) %>% 
  summarise(across(contains('commNet'), sum, na.rm = TRUE)) %>% 
  ungroup() %>% 
  left_join(sf_gadm_lev1, by = "GID_1") %>% 
  select(-c(GID_1, Country, iso3, cntry_code)) %>% 
  select(nmbr, everything())

names(subnat_comm_in) <- c('nmbr',paste0('subnat_comm_in',timeSteps))
names(subnat_comm_out) <- c('nmbr',paste0('subnat_comm_out',timeSteps))

subnat_comm_all <- subnat_comm_in %>% 
  left_join(subnat_comm_out) 

# intraprovincial based on communal scale data

# first let's create a 3D array of in and out migration absolute values
subnat_comm_array <- array( c( as.matrix(abs(subnat_comm_in)) , as.matrix(abs(subnat_comm_out)) ) , 
                            dim = c( nrow(subnat_comm_in), length(timeSteps)+1, 2 ) )
dim(subnat_comm_array)

# internal migration is then the minimum of these two datasets, and
subnat_comm_internalMigr <- apply(subnat_comm_array, c(1,2), min) %>% 
  as_tibble() %>% 
  #mutate(IntraProvMigrSum = rowSums(select_(., '-V1'))) %>% 
  rename(nmbr = V1) %>% 
  rowwise() %>%
  mutate(IntraProvMigrSum = sum(c_across(contains("V")))) %>%
  ungroup() %>% 
  mutate(across(-c(nmbr,V8,IntraProvMigrSum), ~ . / TSlength)) %>% # three year mean; here we divide by three
  mutate(across(contains('V8'), ~ . / TSlength_last)) %>% # except the last timestep with 2 yr
  rowwise() %>% 
  mutate(IntraProvSlope = f_funSlope(c_across(-c(nmbr,IntraProvMigrSum)))) %>%
  ungroup() %>% 
  left_join(sf_gadm_lev1) %>% 
  select(cntry_code,iso3,Country,GID_1,nmbr,IntraProvMigrSum,IntraProvSlope,everything())

names(subnat_comm_internalMigr) <- c('cntry_code', 'iso3','Country','GID_1','nmbr',
                                     'IntraProvMigrSum','IntraProvSlope',paste0('IntraProvincial',timeSteps))

subnat_internalMigr <- subnat_comm_internalMigr


# relative to population
subnat_internalMigr_pop <- subnat_comm_internalMigr %>% 
  select(-c("cntry_code", "iso3", "Country", "GID_1")) %>% 
  left_join(subnat_pop)


subnat_internalMigr_rel <- cbind( subnat_internalMigr_pop['nmbr'],
                                  subnat_internalMigr_pop[,4:(length(timeSteps)+3)] / 
                                    subnat_internalMigr_pop[,(length(timeSteps)+4):(2*length(timeSteps)+3)] ) %>% 
  as_tibble() %>% 
  rowwise() %>%
  mutate(IntraProvMigrSum = sum(c_across(contains("Provincial")))) %>%
  ungroup() %>% 
  mutate(across(-c(nmbr,IntraProvincial2020,IntraProvMigrSum), ~ . / TSlength)) %>% # three year mean; here we divide by three
  mutate(across(contains('2020'), ~ . / TSlength_last)) %>% # except the last timestep with 2 yr
  rowwise() %>% 
  mutate(IntraProvSlope = f_funSlope(c_across(-c(nmbr,IntraProvMigrSum)))) %>%
  ungroup() %>% 
  left_join(sf_gadm_lev1) %>% 
  select(cntry_code,iso3,Country,GID_1,nmbr,IntraProvMigrSum,IntraProvSlope,everything())

names(subnat_internalMigr_rel) <- c('cntry_code', 'iso3','Country','GID_1','nmbr',
                                    'IntraProvMigrSum','IntraProvSlope',paste0('IntraProvRel',timeSteps))


# use the provincial data for net migration instead of the one based on communal data, as not all provinces has that data

subnat_internalMigr_rel_final <- subnat_netMigr_rel[,1:3] %>% 
  left_join(sf_gadm_lev1) %>% 
  left_join(subnat_internalMigr_rel) %>% 
  select(cntry_code,iso3,Country,GID_1,nmbr,subnatNetMgrSum,subnatNetMgrSlope,IntraProvMigrSum,IntraProvSlope,everything())



#### cntry level -----

# summarise by country, and calculate slope

# population
cntry_pop <- subnat_pop %>% 
  #mutate_at(vars(contains('Pop')), function(x) sum(x)) %>% 
  group_by(cntry_code) %>% 
  summarise_at(vars(contains('Pop')),list(sum)) %>% 
  left_join(sf_gadm_lev0) %>% 
  #mutate(popAvg = rowSums(select_(., '-cntry_code','-Country','-iso3')) / 21) %>% 
  select(-c(iso3,Country)) %>% 
  select(cntry_code,everything())



# cntry_popSel <- subnat_internalMigr %>% 
#   left_join(cntry_pop)

cntry_intraProvMigr <- subnat_internalMigr %>% 
  select(-c(IntraProvMigrSum, IntraProvSlope)) %>% 
  group_by(cntry_code) %>% 
  summarise_at(vars(contains('Intra')),list(sum)) %>% 
  ungroup() 

cntry_popSel <- cntry_intraProvMigr %>% 
  left_join(cntry_pop)

# relative intra provincial migration
cntry_intraProvMigrRel <- cbind( cntry_popSel['cntry_code'],
                                 cntry_popSel[,2:(length(timeSteps)+1)] / 
                                   cntry_popSel[,(length(timeSteps)+2):(2*length(timeSteps)+1)]) %>% 
  
  as_tibble() %>% 
  rowwise() %>%
  mutate(IntraProvMigrSum = sum(c_across(contains("Provincial")))) %>%
  ungroup() %>% 
  mutate(across(-c(cntry_code,IntraProvincial2020,IntraProvMigrSum), ~ . / TSlength)) %>% # three year mean; here we divide by three
  mutate(across(contains('2020'), ~ . / TSlength_last)) %>% # except the last timestep with 2 yr
  rowwise() %>% 
  mutate(IntraProvSlope = f_funSlope(c_across(-c(cntry_code,IntraProvMigrSum)))) %>%
  ungroup() %>% 
  left_join(sf_gadm_lev0) %>% 
  select(cntry_code,iso3,Country,IntraProvMigrSum, IntraProvSlope,everything())


## inter-provincial migration within each country

# then we'll calculate the balance, which is the net migration from a subnat
# if positive, then there in-migration is larger, if negative then out-migration is larger

subnat_balance <- subnat_netMigr %>% 
  left_join(sf_gadm_lev1) 


# then calculate for each country the sum of in and out migration based on subnat calculations

# Create a data frame for "out" where positive values in 'subnatNet' columns are set to 0
cntry_subnat_out <- subnat_balance %>% 
  mutate(across(contains('subnatNet'), ~ ifelse(. > 0, 0, .))) %>%
  group_by(cntry_code) %>%
  summarise(across(contains('subnatNet'), sum, na.rm = TRUE))

# Create a data frame for "in" where negative values in 'subnatNet' columns are set to 0
cntry_subnat_in <- subnat_balance %>% 
  mutate(across(contains('subnatNet'), ~ ifelse(. < 0, 0, .))) %>%
  group_by(cntry_code) %>%
  summarise(across(contains('subnatNet'), sum, na.rm = TRUE))

names(cntry_subnat_in) <- c('cntry_code',paste0('cntry_subnat_in',timeSteps))
names(cntry_subnat_out) <- c('cntry_code',paste0('cntry_subnat_out',timeSteps))

cntry_subnat_all <- cntry_subnat_in %>% 
  left_join(cntry_subnat_out)

# 
# subnat_popSel[,2:(length(timeSteps)+1)] / 
#   subnat_popSel[,(length(timeSteps)+2):(2*length(timeSteps)+1)]) 

cntry_netMigr <- cbind( cntry_subnat_all['cntry_code'],
                        cntry_subnat_all[,2:(length(timeSteps)+1)] +
                          cntry_subnat_all[,(length(timeSteps)+2):(2*length(timeSteps)+1)]) %>% 
  as_tibble() #%>% 

names(cntry_netMigr) <- c('cntry_code',paste0('cntry_netMgr',timeSteps))

# first let's create a 3D array of in and out migration absolute values
cntry_subnat_array <- array( c( as.matrix(abs(cntry_subnat_in)) , as.matrix(abs(cntry_subnat_out)) ) , 
                             dim = c( nrow(cntry_subnat_in), ncol(cntry_subnat_in), 2 ) )



# inter-provincial migration is then the minimum of these two variables
cntry_interProvMigr <- apply(cntry_subnat_array, c(1,2), min) %>% 
  as_tibble() 

names(cntry_interProvMigr) <- c('cntry_code',paste0('interProvMgr',timeSteps))

cntry_popSel <- cntry_interProvMigr %>% 
  #rename(cntry_code = V1) %>% 
  left_join(cntry_pop)

# relative intra provincial migration
cntry_interProvMigrRel <- cbind( cntry_popSel['cntry_code'],
                                 cntry_popSel[,2:(length(timeSteps)+1)] / 
                                   cntry_popSel[,(length(timeSteps)+2):(2*length(timeSteps)+1)] ) %>% 
  as_tibble() %>% 
  left_join(sf_gadm_lev0) %>% 
  rowwise() %>%
  mutate(InterProvMigrSum = sum(c_across(contains("Mgr")))) %>%
  ungroup() %>% 
  mutate(across(-c(cntry_code,interProvMgr2020,InterProvMigrSum,Country,iso3), ~ . / TSlength)) %>% # three year mean; here we divide by three
  mutate(across(contains('2020'), ~ . / TSlength_last)) %>% # except the last timestep with 2 yr
  rowwise() %>% 
  mutate(InterProvSlope = f_funSlope(c_across(-c(cntry_code,InterProvMigrSum,Country,iso3)))) %>%
  ungroup() %>% 
  select(cntry_code,iso3,Country,InterProvMigrSum,InterProvSlope,everything())

names(cntry_interProvMigrRel) <- c('cntry_code', 'iso3','Country',
                                   'InterProvMigrSum','InterProvSlope',paste0('interProvincial',timeSteps))


## inter-national migration for each country

# then we'll calculate the cntry balance, which is the net migration from a country
# if positive, then there in-migration is larger, if negative then out-migration is larger

cntry_popSel <- cntry_netMigr %>% 
  left_join(cntry_pop)


cntry_netMigrRel <- cbind( cntry_popSel['cntry_code'],
                           cntry_popSel[,2:(length(timeSteps)+1)] / 
                             cntry_popSel[,(length(timeSteps)+2):(2*length(timeSteps)+1)]) %>% 
  as_tibble() %>% 
  left_join(sf_gadm_lev0) %>% 
  rowwise() %>%
  mutate(InterNatMigrSum = sum(c_across(contains("Mgr")))) %>%
  ungroup() %>% 
  mutate(across(-c(cntry_code,cntry_netMgr2020,InterNatMigrSum,Country,iso3), ~ . / TSlength)) %>% # three year mean; here we divide by three
  mutate(across(contains('2020'), ~ . / TSlength_last)) %>% # except the last timestep with 2 yr
  rowwise() %>% 
  mutate(InterNatSlope = f_funSlope(c_across(-c(cntry_code,InterNatMigrSum,Country,iso3)))) %>%
  ungroup() %>% 
  select(cntry_code, iso3, Country, InterNatMigrSum, InterNatSlope, everything())


names(cntry_netMigrRel) <- c('cntry_code', 'iso3','Country',
                             'InterNatMigrSum','InterNatSlope',paste0('interNational',timeSteps))

#save(cntry_balance, file = '../results/cntry_balance.R')


#### global ----

# sum together separately the out and in 
# cntry_out <- cntry_netMigr %>% 
#   mutate_at(vars(contains('cntry_netMgr')), function(x) ifelse(x>0, 0, x)) %>% 
#   summarise_at(vars(contains('cntry_netMgr')),list(sum))
# 
# cntry_in <- cntry_netMigr %>% 
#   mutate_at(vars(contains('cntry_netMgr')), function(x) ifelse(x<0, 0, x)) %>% 
#   summarise_at(vars(contains('cntry_netMgr')),list(sum))

cntry_out <- cntry_netMigr %>% 
  mutate(across(contains('cntry_netMgr'), ~ ifelse(. > 0, 0, .))) %>% 
  summarise(across(contains('cntry_netMgr'), sum, na.rm = TRUE))

# Create a dataframe for "in" where negative values are set to 0
cntry_in <- cntry_netMigr %>% 
  mutate(across(contains('cntry_netMgr'), ~ ifelse(. < 0, 0, .))) %>% 
  summarise(across(contains('cntry_netMgr'), sum, na.rm = TRUE))

global_array <- array( c( as.matrix(abs(cntry_in)) , as.matrix(abs(cntry_out)) ) , 
                       dim = c( nrow(cntry_out), ncol(cntry_out), 2 ) )

# global international migration is then the minimum of these two variables
global_internatMigr <- apply(global_array, c(1,2), min) %>% 
  as_tibble() 

# global inter-provincial migration
global_interProv <- cntry_interProvMigr %>% 
  summarise_at(vars(contains('Mgr')),list(sum))

# global intra-provincial migration
global_intraProv <- cntry_intraProvMigr %>% 
  summarise_at(vars(contains('Intra')),list(sum))

global_Collect <- rbind( as.numeric( global_internatMigr) / 10^6, 
                         as.numeric(global_interProv)  / 10^6, 
                         as.numeric(global_intraProv)  / 10^6) %>% 
  round(1) %>% 
  as_tibble() %>% 
  
  mutate(rowNames = c('InterNational (10^6)','InterProvincial (10^6)','InterCommunal (10^6)')) %>% 
  select(rowNames, everything())
#tibble::column_to_rownames(var="rowNames") 


names(global_Collect) <- c( 'Scale', paste0('globalSum',as.character(timeSteps)) )

write_csv(global_Collect,paste0('../results/globalCollect.csv'))

names(global_Collect) <- c( 'Scale', paste0(timeSteps) )

global_Collect_long <- global_Collect %>% 
  pivot_longer(!c('Scale'), 
               names_to = "year", 
               values_to = 'mgrt') 



p<-ggplot(global_Collect_long, aes(x=year, y=mgrt, group=Scale)) +
  geom_line(aes(color=Scale))+
  geom_point(aes(color=Scale))
p

ggsave(paste0('../figures/fig1_migration_timeseries.pdf'),p,width = 140, height=120, unit='mm')

#### collect results ------

# communal
comm_dataCollection <- comm_netMigr_rel %>% 
  mutate(across(contains('Slope'), ~ . * 20))  # to get slope over 20 yr


# subnat
subnat_dataCollection <- subnat_internalMigr_rel_final %>% 
  #select(c(nmbr, IntraProvMigrSum, IntraProvSlope)) %>% 
  # left_join(subnat_netMigr_rel) %>% 
  mutate(across(contains('Slope'), ~ . * 20)) %>%  # to get slope over 20 yr
  select(cntry_code,iso3,Country,GID_1, nmbr,
         IntraProvMigrSum,IntraProvSlope,
         subnatNetMgrSum,subnatNetMgrSlope)


# cntry 

# migration
cntry_dataCollection <- sf_gadm_lev0 %>% 
  left_join(cntry_netMigrRel[,c('cntry_code','InterNatMigrSum','InterNatSlope')]) %>% 
  left_join(cntry_interProvMigrRel[,c('cntry_code','InterProvMigrSum','InterProvSlope')]) %>% 
  left_join(cntry_intraProvMigrRel[,c('cntry_code','IntraProvMigrSum','IntraProvSlope')]) %>% 
  mutate(across(contains('Slope'), ~ . * 20))  # to get slope over 20 yr



#### plot -----

# create sf files for country and subnational data

v_gadm0 <- terra::as.polygons(rast('../data_in_rast/gadm_lev0_5arcmin.tif'))

#terra::writeVector(v_gadm0,"../results/test_adm0.gpkg",overwrite=T)


# remove repetition
cntry_dataCollection_mod <- cntry_dataCollection %>% 
  distinct(iso3, cntry_code, .keep_all = T)

cntry_netMigrRel_mod <- cntry_netMigrRel%>% 
  distinct(iso3, cntry_code, .keep_all = T)

sf_gadm_lev0_dataCollection <- sf::st_as_sf(v_gadm0) %>% 
  ms_simplify(.,keep=0.1,keep_shapes = T) %>% 
  left_join(cntry_dataCollection_mod) %>% 
  # from percentage to per 1000 people
  mutate(across(contains('Sum'), ~ 1000 * .))

sf_gadm_lev0_dataNetMgr <- sf_gadm_lev0_dataCollection %>% 
  select(-c("InterNatMigrSum", "InterNatSlope" , "InterProvMigrSum", "InterProvSlope", 
            "IntraProvMigrSum" , "IntraProvSlope")) %>% 
  left_join(cntry_netMigrRel_mod) %>% 
  mutate(across(contains('nter'), ~ 1000 * .)) %>% 
  select(-Country) %>% 
  left_join(cntryID[,c(3,4)]) %>% 
  select(Country, everything())
  


#write_sf(sf_gadm_lev0_dataNetMgr,"../results/sf_gadm_lev0_dataNetMgr.gpkg",overwrite=T)

v_gadm_lev0_dataNetMgr <- vect(sf_gadm_lev0_dataNetMgr)
is.valid(v_gadm_lev0_dataNetMgr)
v_gadm_lev0_dataNetMgr_valid <- terra::makeValid(v_gadm_lev0_dataNetMgr)

is.valid(v_gadm_lev0_dataNetMgr_valid)

terra::writeVector(v_gadm_lev0_dataNetMgr_valid,"../results/sf_gadm_lev0_dataNetMgr.gpkg",overwrite=T)

test_adm0 <- vect("../results/sf_gadm_lev0_dataNetMgr.gpkg") 

test_data <- sf_gadm_lev0_dataNetMgr %>% st_drop_geometry()  
# 
# 
# test_adm0 <- vect("../results/test_adm0.gpkg") %>% 
#   left_join(sf_gadm_lev0_dataNetMgr %>% st_drop_geometry())



v_gadm1 <- terra::as.polygons(rast('../data_in_rast/gadm_lev1_5arcmin.tif'))

gadm_lev1 <- st_read('../data_in_gpkg/gadm_lev1.gpkg') %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(GID_1, NAME_1)

sf_gadm_lev1_dataCollection <- sf::st_as_sf(v_gadm1) %>% 
  ms_simplify(.,keep=0.1,keep_shapes = T) %>% 
  left_join(subnat_dataCollection)%>% 
  # from percentage to per 1000 people
  mutate(across(contains('Sum'), ~ 1000 * .))

sf_gadm_lev1_dataNetMgr <- sf_gadm_lev1_dataCollection %>% 
  select(-c("IntraProvMigrSum", "IntraProvSlope" , "subnatNetMgrSum", "subnatNetMgrSlope")) %>% 
  left_join(subnat_netMigr_rel) %>% 
  mutate(across(contains('subnat'), ~ 1000 * .)) %>% 

  # add adm1 names
  left_join(gadm_lev1) %>% 
  select(nmbr, cntry_code, iso3,Country,GID_1, NAME_1, everything())



terra::writeVector(vect(sf_gadm_lev1_dataNetMgr),"../results/sf_gadm_lev1_dataNetMgr.gpkg",overwrite=T)


v_gadm2 <- terra::as.polygons(rast('../data_in_rast/gadm_lev2_5arcmin.tif'))

gadm_lev2 <- st_read('../data_in_gpkg/gadm_lev2.gpkg') %>% 
  st_drop_geometry() %>% 
  as_tibble() %>% 
  select(GID_2, NAME_2) %>% 
  left_join(sf_gadm_lev2[c('Country', 'iso3', 'GID_1', 'GID_2', 'nmbr2')])

 
sf_gadm_lev2_dataCollection <- sf::st_as_sf(v_gadm2) %>% 
  ms_simplify(.,keep=0.1,keep_shapes = T) %>% 
  left_join(comm_dataCollection)%>% 
  # from percentage to per 1000 people
  mutate(across(contains('comm'), ~ 1000 * .)) %>% 
  left_join(gadm_lev2) %>% 
  select('Country', 'iso3', 'GID_1', 'GID_2', NAME_2, everything())


terra::writeVector(vect(sf_gadm_lev2_dataCollection),"../results/sf_gadm_lev2_dataNetMgr.gpkg",overwrite=T)



source('../functions/myPlot_sfAbs.R') 

source('../functions/myPlot_sfNetMgr.R') 

sumMgrRange <- seq(0,100,by=10)
netMgrRange <- seq(-250,250,by=50)
slopeRange <- seq(-0.005,0.005,by=0.001)

p_commNetMgr <- myPlot_sfNetMgr(sf_gadm_lev2_dataCollection,'commNetMgrSum', netMgrRange )

p_commNetMgrSlope <- myPlot_sfNetMgr(sf_gadm_lev2_dataCollection,'commNetMgrSlope',slopeRange )

p_provInternal <- myPlot_sfAbs(sf_gadm_lev1_dataCollection,'IntraProvMigrSum',sumMgrRange )

p_provInternalSlope <- myPlot_sfNetMgr(sf_gadm_lev1_dataCollection,'IntraProvSlope',slopeRange )

p_provNetMgr <- myPlot_sfNetMgr(sf_gadm_lev1_dataCollection,'subnatNetMgrSum',netMgrRange )

p_provNetMgrSlope <- myPlot_sfNetMgr(sf_gadm_lev1_dataCollection,'subnatNetMgrSlope',slopeRange )

p_cntryInterProv <- myPlot_sfAbs(sf_gadm_lev0_dataCollection,'InterProvMigrSum',sumMgrRange )

p_cntryInterProvSlope <- myPlot_sfNetMgr(sf_gadm_lev0_dataCollection,'InterProvSlope',slopeRange )

p_cntryNetMgr <- myPlot_sfNetMgr(sf_gadm_lev0_dataCollection,'InterNatMigrSum',netMgrRange )

p_cntryNetMgrSlope <- myPlot_sfNetMgr(sf_gadm_lev0_dataCollection,'InterNatSlope',slopeRange )

# 
# p_col <- tmap_arrange(p_commNetMgr,p_commNetMgrSlope,
#                       p_provNetMgr,p_provNetMgrSlope,
#                       p_cntryNetMgr,p_cntryNetMgrSlope,
#                       p_provInternal,p_provInternalSlope,
#                       p_cntryInterProv,p_cntryInterProvSlope,
#                       ncol = 2)
# 
# tmap_save(p_col,filename = paste0('../figures/mapsMigrations.pdf'),width = 180, height=200, units='mm')
# 

p_fig1 <- tmap_arrange(p_commNetMgr, p_provInternal,
                       p_provNetMgr, p_cntryInterProv,
                       p_cntryNetMgr,
                       ncol = 2)

tmap_save(p_fig1,filename = paste0('../figures/fig1_mapsMigrations.pdf'),width = 180, height=200, units='mm')

