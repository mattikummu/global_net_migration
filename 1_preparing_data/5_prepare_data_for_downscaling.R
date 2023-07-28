
### prepare data for downscaling

library(sf)
library(e1071)
library(fasterize)
library(magrittr)
library(raster)
library(terra)



library(snow)
library(foreach)
library(openxlsx) #
library(readxl)
library(tidyverse)
library(dplyr) 

library(doParallel)  #Foreach Parallel Adaptor 
library(foreach)     #Provides foreach looping construct

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#### load general data -----


cntryID <- read_csv("data_in/countries_codes_and_coordinates.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  #rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  select(GADM_code,iso3)

cntryUNmeta <- read.xlsx("data_in/country_id_meta.xlsx", sheet="cntry_id_FAO_UN") %>% 
  mutate_at(vars(fao_id),as.character) %>% 
  select(-fao_id) %>% 
  as_tibble()


###### National data 80+ population  -------------------------------


# cntry_finalData_IDs <- read.xlsx("../sub_national_gdp/results/cntry_data_final.xlsx", skipEmptyRows = FALSE) %>%
#   select(cntry_id,country_code)  %>%
#   as_tibble()


# polygon to raster - needs to be done only once. 
# 
# # load national gpkg
# cntry_polyg <- read_sf('data_in/gadm_level0.gpkg') %>% 
#   st_simplify(preserveTopology = T,dTolerance = .01)
# 
# # add id
# cntry_polyg_withID <- cntry_polyg %>% 
#   rename(iso3 = GID_0) %>% 
#   mutate(iso3 = ifelse(iso3 == 'ALA','FIN',iso3)) %>%  # join Åland as a part of Finland
#   mutate(NAME_0 = ifelse(NAME_0 == 'Åland','Finland',NAME_0)) %>% 
#   left_join(cntryID) %>% 
#   filter(!is.na(GADM_code))
# 
# # multipolygon to polygon, so that fasterize(()) would work
# cntry_polyg_withID_cast <- st_cast(cntry_polyg_withID,"MULTIPOLYGON") %>% 
#   st_cast("POLYGON")
# 
# # rasterise cntry borders
# ref_raster <- raster(ncol=360*12*5, nrow=180*12*5)
# 
# cntry_raster_1arcmin <- fasterize(cntry_polyg_withID_cast,ref_raster,field="GADM_code")
# 
# # aggregate to 5 arc-min
# cntry_raster_5arcmin <- terra::aggregate(rast(cntry_raster_1arcmin),fact=5,fun=modal, na.rm = T)
# 
# writeRaster(cntry_raster_5arcmin,'data_in/GADM_level0_raster_5arcmin.tif',gdal="COMPRESS=LZW", overwrite=TRUE)



# load raster
cntry_raster_5arcmin <- rast('data_in/GADM_level0_raster_5arcmin.tif')


###### National data 80+ population 

# create function

myFun_avgAge80plus <-function(xlsWithPopByAge){
  
  popByAgeFemale <- read.xlsx(xlsWithPopByAge,"ESTIMATES", colNames = T) %>%
    # unselect non-wanted columns
    select(-c(Index, Variant,Notes,`0-4`,`5-9`,`10-14`,`15-19`,`20-24`, `25-29`, `30-34`, `35-39`, 
              `40-44`, `45-49`, `50-54`, `55-59`, `60-64`, `65-69`, `70-74`, `75-79`)) %>%
    # rename columns
    rename(age80 = `80-84`,age85 = `85-89`,age90 = `90-94`,age95 = `95-99`,age100 = `100+` ) %>% 
    # calculate population weighted age for each age group
    mutate(ageWeightedPop80_100 = 82.5 * as.numeric(age80)  + 87.5 * as.numeric(age85) + 92.5 * as.numeric(age90) + 97.5*as.numeric( age95) + 102.5 * as.numeric(age100)) %>% 
    # calculate total pop for age 80+
    mutate(totalPop80_100 = as.numeric(age80)  + as.numeric(age85) + as.numeric(age90) + as.numeric( age95) + as.numeric(age100)) %>% 
    # calculate average age for age 80+
    mutate(avgAge80_100 = ageWeightedPop80_100/totalPop80_100) %>% 
    #unselect non-wanted columns
    select(-c(age80, age85, age90, age95, age100, ageWeightedPop80_100, totalPop80_100)) %>% 
    as_tibble() %>%
    # select only country data or world data
    filter(Type == 'Country/Area' | Type == 'World' )
  
  popByAgeFemaleWorld_wide <- popByAgeFemale %>% 
    pivot_wider(names_from = "Year", values_from = 'avgAge80_100') %>% 
    # add iso3 code
    left_join(.,cntryUNmeta,by=c('cntry_id' = 'UN_id')) %>% 
    mutate(Type = ifelse(is.na(Type), 'Country/Area',Type)) %>% 
    mutate(iso3 = ifelse(Type == 'World', 'WWW', iso3)) %>% 
    filter(Type == 'World') %>% 
    select(c(Region, cntry_id, iso3,Type,'2000','2005','2010','2015','2020')) 
  
  popByAgeFemale_wide <- popByAgeFemale %>% 
    pivot_wider(names_from = "Year", values_from = 'avgAge80_100') %>% 
    # add iso3 code
    right_join(.,cntryUNmeta,by=c('cntry_id' = 'UN_id')) %>% 
    mutate(Type = ifelse(is.na(Type), 'Country/Area',Type)) %>% 
    mutate(iso3 = ifelse(Type == 'World', 'WWW', iso3)) %>% 
    # filter out the countries where no iso3 is available
    filter(!is.na(iso3)) %>% 
    select(c(Region, cntry_id, iso3,Type,'2000','2005','2010','2015','2020')) %>%
    # add GADM id
    right_join(.,cntryID)
  
  
  # use world average for countries where no data available
  popByAgeFemale_wide_NA <- popByAgeFemale_wide %>% 
    filter(is.na(Region)) 
  
  popByAgeFemale_wide_NA[,5:9] = popByAgeFemaleWorld_wide[,5:9]
  
  popByAgeFemale_wide_filled <- popByAgeFemale_wide %>% 
    filter(!is.na(Region)) %>% 
    rbind(.,popByAgeFemale_wide_NA) 
  
  
  #### put to map 
  
  # unique ids
  temp_id = as.data.frame( unique(cntry_raster_5arcmin))
  # initialise data
  rAvgAgeOver80 <- cntry_raster_5arcmin
  
  # go through each timestep (2000,2005, ..., 2020)
  for (i in 1:5) {
    
    temp_v = as.data.frame(popByAgeFemale_wide_filled[,c(10,i+4)]) # select id and value to which this is replaced
    temp_replace <- temp_id %>% 
      rename(GADM_code = layer) %>% 
      left_join(temp_v) # join id in the raster and the value it should be replaced with
    
    # reclassify raster
    temp_raster <- classify(cntry_raster_5arcmin,
                            temp_replace)
    # store to stack
    rAvgAgeOver80 <- c(rAvgAgeOver80,temp_raster)
  }
  # select only the parts of stack where data is given
  rAvgAgeOver80 <- subset(rAvgAgeOver80,2:6)
  return(rAvgAgeOver80)
}


rAvgAgeOver80_female <- myFun_avgAge80plus("data_in/WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx")
rAvgAgeOver80_male <- myFun_avgAge80plus("data_in/WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx")

names(rAvgAgeOver80_female) <- c('yr2000','yr2005','yr2010','yr2015','yr2020')
names(rAvgAgeOver80_male) <- c('yr2000','yr2005','yr2010','yr2015','yr2020')

writeRaster(rAvgAgeOver80_female,'results/rAvgAgeOver80_female.tif', gdal="COMPRESS=LZW",overwrite=TRUE)
writeRaster(rAvgAgeOver80_male,'results/rAvgAgeOver80_male.tif', gdal="COMPRESS=LZW",overwrite=TRUE)


#### Prepare explanatory variables to downscaling ----


# read total population for 2000-2020
ref_rast <- rast('/Users/mkummu/Aalto University/WDRG - Datasets/WorldPop_5arcmin_2000_2020/age_structures_2000_5arcmin.tif')

# country specific raster data for age 80+
rAvgAgeOver80_female <- rast('results/rAvgAgeOver80_female.tif')
rAvgAgeOver80_male <- rast('results/rAvgAgeOver80_male.tif')


### for years 2000, 2005 ... 2020

avgAgeColl <- ref_rast[[1]]
fertShrColl <- ref_rast[[1]]

iYear = 2000
for (iYear in seq(2000,2020,by=5)) {
  
  # age structure
  filePath <- paste0('/Users/mkummu/Aalto University/WDRG - Datasets/WorldPop_5arcmin_2000_2020/age_structures_',iYear,'_5arcmin.tif')
  ageStruct_iYear <- rast(filePath)
  
  # project to match with other raster data
  rAvgAgeOver80_female_proj <- project(rAvgAgeOver80_female[[(iYear-2000)/5+1]],ref_rast)
  rAvgAgeOver80_male_proj <- project(rAvgAgeOver80_male[[(iYear-2000)/5+1]],ref_rast)
  
  #names(ageStruct_iYear_female_reOrg)
  ageStruct_iYear_female_reOrg <-  ageStruct_iYear[[c(1:2,11,3:10,12:18)]] # select only female, organise so that goes by age groups
  ageStruct_iYear_male_reOrg <-  ageStruct_iYear[[c(19:20,29,21:28,30:36)]] # select only male, organise so that goes by age groups
  # average age
  
  avgAgeLayers <- c(0.5,3,seq(5.5,77.5,5))
  
  # calculate average age for each grid cell and raster layer (from 0 to 75)
  avgAge_iYear_female <- avgAgeLayers * ageStruct_iYear_female_reOrg[[1:17]]
  avgAge_iYear_male <- avgAgeLayers * ageStruct_iYear_male_reOrg[[1:17]]
  
  # calculate average for each grid cell for 80+ years
  avgAge_iYear_female_80plus <- rAvgAgeOver80_female_proj * ageStruct_iYear_female_reOrg[[18]]
  avgAge_iYear_male_80plus <- rAvgAgeOver80_male_proj * ageStruct_iYear_male_reOrg[[18]]
  
  # combine avgAge rasters
  avgAge_iYear_female_comb <- c(avgAge_iYear_female,avgAge_iYear_female_80plus)
  avgAge_iYear_male_comb <- c(avgAge_iYear_male,avgAge_iYear_male_80plus)
  
  
  # calculate average age for each grid cell
  #avgAge_iYear_grid <- (sum(avgAge_iYear_female_comb) + sum(avgAge_iYear_male_comb) )/ pop_total[[iYear-1999]]
  avgAge_iYear_grid <- (sum(avgAge_iYear_female_comb) + sum(avgAge_iYear_male_comb) )/ sum(ageStruct_iYear)
  avgAge_iYear_grid[is.infinite(avgAge_iYear_grid)] = 0
  
  avgAgeColl <- c(avgAgeColl,avgAge_iYear_grid)
  
  
  # share of women of age 15-49 of total population
  #female15_49_shr <- sum(ageStruct_iYear_female_reOrg[[5:11]]) / pop_total[[iYear-1999]]
  female15_49_shr <- sum(ageStruct_iYear_female_reOrg[[5:11]]) / sum(ageStruct_iYear)
  female15_49_shr[is.infinite(female15_49_shr)] = 0
  
  fertShrColl <- c(fertShrColl,female15_49_shr)
}

avgAgeColl_sub <- subset(avgAgeColl,2:6)
names(avgAgeColl_sub) <- c('avgAge2000','avgAge2005','avgAge2010','avgAge2015','avgAge2020')
writeRaster(avgAgeColl_sub ,'results/avgAge2000_2020_v2.tif', gdal="COMPRESS=LZW",overwrite=TRUE)

fertShrColl_sub <- subset(fertShrColl,2:6)
names(fertShrColl_sub) <- c('avgAge2000','avgAge2005','avgAge2010','avgAge2015','avgAge2020')
writeRaster(fertShrColl_sub ,'results/fertileFemaleShr2000_2020_v2.tif', gdal="COMPRESS=LZW",overwrite=TRUE)

plot(avgAgeColl[[6]])
# fill NA values


#### Ratio between average age and life expectance ----


lifeExp <- rast('data_in/hdiComponent_lifexpUpd.tif')
# select 2000-2019 and use 2019 to year 2020 too
lifeExp_sel <- lifeExp[[c(11:nlyr(lifeExp),nlyr(lifeExp))]]

# interpolate avgAge between the years (2000,2005,...,2020)
avgAge <- rast('results/avgAge2000_2020_v2.tif')

# define function for interpolation
interpRaster <- function(r_in,existingYears,newYears) {
  # r_in <- stack(avgAge)
  # existingYears <- seq(2000,2020,5)
  # nClusters <- 6
  
  # NA to zero
  r_in[is.na(r_in)] <- 0
  
  # int_sN <- 1
  # int_eN <- existingYears[length(existingYears)] - existingYears[1] + 1
  
  #newYears <- existingYears[1]:existingYears[length(existingYears)]
  
  rasInterp <- things::interpolate_linear(stack(r_in),existingYears,newYears,tempdir(), 
                                          'example', return_stack=TRUE,overwrite=TRUE)
  
  # f3 <- function(y) approx(existingYears, y, int_sN:int_eN, rule=2)$y
  # 
  # beginCluster(nClusters)
  # rasInterp <- clusterR(r_in, calc, args=list(fun=f3))
  # endCluster()
  
  names(rasInterp) <- paste0('Year',existingYears[1]:existingYears[length(existingYears)]) 
  
  return(rasInterp)
}

# library(whitebox)
# 
# ref_raster_5arcmin <- raster(ncol=360*12, nrow=180*12)
# whitebox::wbt_euclidean_allocation(stack(subset(avgAge,1)),ref_raster_5arcmin)

fillmode <- function(v, na.rm) {  
  if (is.na(v[[5]])) {
    uniqv <- unique(v)
    uniqv <- uniqv[!is.na(uniqv)]
    fillval <- uniqv[which.max(tabulate(match(v, uniqv)))]
    return (fillval)
  } else {
    return (v[[5]])
  }
  
}

if (file.exists('results/AvgAge_v2_interp.tif')){
  # load it
  avgAge_interp_proj <- rast('results/AvgAge_v2_interp.tif')
} else { 
  # create it
  
  # create buffer, as interpolation results very small values to coastal zone
  avgAge_buffer <- subset(avgAge,1)
  for (i in 1:terra::nlyr(avgAge)) {
    i_avgAge_buffer <- focal(subset(avgAge,i), w = 3, fun = fillmode, na.rm = FALSE)
    terra::add(avgAge_buffer) <- i_avgAge_buffer
  }
  avgAge_buffer_sub <- subset(avgAge_buffer,2:terra::nlyr(avgAge_buffer))
  #avgAge_buffer <- focal(avgAge, w = 3, fun = fillmode, na.rm = FALSE)
  
  # run funtion
  writeRaster(avgAge_buffer_sub ,'results/AvgAge_v2_buffer.tif', gdal="COMPRESS=LZW",overwrite=TRUE)
  
  avgAge_interp <- interpRaster(stack(avgAge_buffer_sub),seq(2000,2020,5),seq(2000,2020,1))
  
  # project avgAge to same extent than lifeExp
  avgAge_interp_buff_proj <- terra::project(rast(avgAge_interp),subset(lifeExp,1))
  
  # apply coastal mask of lifeExp
  
  temp_rast <- subset(avgAge,1)
  temp_rast_proj <- terra::project(temp_rast,subset(lifeExp,1))
  plot(temp_rast_proj)
  #mask_sea <- is.na(subset(lifeExp,1))
  
  avgAge_interp_buff_proj[avgAge_interp_buff_proj == 0] = NA
  
  avgAge_interp_proj <- avgAge_interp_buff_proj
  mask_sea <- is.nan(temp_rast_proj)
  plot(mask_sea)
  avgAge_interp_proj[mask_sea == 1] <- NA
  
  
  names(avgAge_interp_proj) <- paste0('avgAge',paste0(2000:2020)) 
  writeRaster(avgAge_interp_proj ,'results/AvgAge_v2_interp.tif', gdal="COMPRESS=LZW",overwrite=TRUE)
}

# buffer to lifeExp_sel to take care of slightly different land mask of the data

#Define how many cores you want to use
# UseCores <- detectCores() -1
# #Register CoreCluster
# cl <- makeCluster(UseCores)
# registerDoParallel(cl)
# 
# rTemp <- brick(lifeExp_sel)
# 
# Density_Function_1000 <- function (raster_layer){
#   
#   focal(raster_layer, w = 3, fun = fillmode, na.rm = FALSE)
# }
# 
# 
# tempLifeExp<- foreach(i = 1:nlyr(rTemp),
#                      .packages="terra",
#                      .combine = 'c') %dopar% {
#                        focal(subset(rTemp,i), w = 3, fun = fillmode, na.rm = FALSE)
#                      }
# 


lifeExp_sel_buff <- subset(lifeExp_sel,1)
for (i in 1:terra::nlyr(lifeExp_sel)) {
  i_lifeExp_sel_buff <- focal(subset(lifeExp_sel,i), w = 3, fun = fillmode, na.rm = FALSE)
  terra::add(lifeExp_sel_buff) <- i_lifeExp_sel_buff
}
lifeExp_sel_buff <- subset(lifeExp_sel_buff,2:terra::nlyr(lifeExp_sel_buff))


# create sea mask to mask out the buffer
temp_rast <- subset(avgAge,1)
temp_rast_proj <- terra::project(temp_rast,subset(lifeExp,1))
mask_sea <- is.nan(temp_rast_proj)

# store masked lifeExp
lifeExp_sel_masked <- lifeExp_sel_buff
lifeExp_sel_masked[mask_sea == 1] <- NA
writeRaster(lifeExp_sel_masked ,'results/lifeExp_sel_masked.tif', gdal="COMPRESS=LZW",overwrite=TRUE)

avgAge_interp_buff <- rast('results/AvgAge_v2_buffer.tif')
avgAge_interp <- interpRaster(stack(avgAge_interp_buff),seq(2000,2020,5),seq(2000,2020,1))

# project avgAge to same extent than lifeExp
avgAge_interp_buff_proj <- terra::project(rast(avgAge_interp),subset(lifeExp,1))


# store masked ratio
# ratio between avgAge and lifeExp
ratioAvgAge_lifeExp <- avgAge_interp_buff_proj / lifeExp_sel_buff
ratioAvgAge_lifeExp[mask_sea == 1] <- NA

#ratioAvgAge_lifeExp[ratioAvgAge_lifeExp == 0] = NA
ratioAvgAge_lifeExp[ratioAvgAge_lifeExp < 0.02] = NA # some very small values, due to different landmask of the files
names(ratioAvgAge_lifeExp) <- paste0('ratio',paste0(2000:2020)) 
writeRaster(ratioAvgAge_lifeExp ,'results/ratioAvgAge_lifeExp_v2.tif', gdal="COMPRESS=LZW",overwrite=TRUE)


# interpolate fertile women share data ------------------------------------

# define function for interpolation

# load data
fertileFemShr <- rast('results/fertileFemaleShr2000_2020_v2.tif')

# interpolate
# run funtionc
fertileFemShr_interp <- interpRaster(stack(fertileFemShr),seq(2000,2020,5),seq(2000,2020,1))

r_fertileFemShr_interp <- rast(fertileFemShr_interp)

r_fertileFemShr_interp[r_fertileFemShr_interp == 0] = NA
names(r_fertileFemShr_interp) <- paste0('ratio',paste0(2000:2020)) 


rastRef = rast(paste0('results/','ratioAvgAge_lifeExp_v2','.tif')) %>% 
  subset(.,terra::nlyr(.))

rastProj = r_fertileFemShr_interp %>% 
  project(.,rastRef)

# #buffering with parallel computing
# doParallel::registerDoParallel(makeCluster(8), cores = 8)
# 
# rTemp <- rastProj
# pTemp <- pack(rTemp)
# tempNLight<- foreach(i = 1:length(pTemp), 
#                      .packages="terra",
#                      .combine = 'c') %dopar% {
#                        focal(subset(pTemp,i), w = 3, fun = fillmode, na.rm = FALSE)
#                      }

r_fertileFemShr_interp_buff <- subset(rastProj,1)
for (i in 1:terra::nlyr(rastProj)) {
  i_r_fertileFemShr_interp_buff <- focal(subset(rastProj,i), w = 3, fun = fillmode, na.rm = FALSE)
  terra::add(r_fertileFemShr_interp_buff) <- i_r_fertileFemShr_interp_buff
}
r_fertileFemShr_interp_buff <- subset(r_fertileFemShr_interp_buff,2:terra::nlyr(r_fertileFemShr_interp_buff))

r_fertileFemShr_masked <- r_fertileFemShr_interp_buff
r_fertileFemShr_masked[mask_sea == 1] <- NA


#r_fertileFemShr_interp[r_fertileFemShr_interp < 0.02] = NA # some very small values, due to different landmask of the files
writeRaster(r_fertileFemShr_masked ,'results/fertileFemaleShr2000_2020_v2_interp.tif', gdal="COMPRESS=LZW",overwrite=TRUE)


### plot ####

library(patchwork)
library(tmap)
library(rnaturalearth)
library(viridis)


# load raster
ratioAvgAge_lifeExp <- rast('results/ratioAvgAge_lifeExp_v2.tif')

fertShrColl <- rast('results/fertileFemaleShr2000_2020_v2.tif')


ratioAvgAge_lifeExp_robin <- 1 - terra::project(subset(ratioAvgAge_lifeExp,21), "+proj=robin",
                                                mask = TRUE)
fertShrColl_robin <- terra::project(subset(fertShrColl,5), "+proj=robin",
                                    mask = TRUE)

# load land boundary
#land <- rnaturalearth::ne_download(scale=110, type="land", category = "physical")
#land50 <- rnaturalearth::ne_download(scale=50, type="land", category = "physical")
#land10 <- rnaturalearth::ne_download(scale=10, type="land", category = "physical")
cntry50 <- rnaturalearth::ne_download(scale=50, type="countries", category = "cultural")



# # limit to 15 hours
# r_raster_fishing_robin[r_raster_fishing_robin>15] = 15


# set colour map
colPalette <- viridis(n=20,begin = 0.25, end = 1,direction=1, option="magma")



# mapping function
raster2map_v2 <- function(r_in,shape_in, colPalette, plotTitle) {
  
  
  tmapMap <- tm_shape(r_in) +
    tm_raster(palette = colPalette,
              #breaks = fishing.breaks,
              title = plotTitle,
              n = 15,
              style = "fisher",
              colorNA = "white",
              legend.is.portrait = FALSE) +
    tm_shape(shape_in,projection = "robin") +
    tm_borders(col="grey50",lwd = 0.1)+
    tm_layout(legend.bg.color = TRUE,
              legend.outside.position = "bottom",
              legend.outside = TRUE,
              frame = FALSE)
  
  
  return(tmapMap)
}



map_ratioAvgAge_lifeExp <- raster2map_v2(ratioAvgAge_lifeExp_robin,cntry50,colPalette,"percentage of life left")

map_fertShrColl <- raster2map_v2(fertShrColl_robin,cntry50,colPalette,"percentage of fertile women")


# collect maps
map_popIndicators <- tmap_arrange( map_ratioAvgAge_lifeExp,map_fertShrColl,
                                   widths = c(.5, .5))

# save map
tmap_save(map_popIndicators, "figures/map_popIndicators_2020.pdf", width=180, height=200, units="mm")

