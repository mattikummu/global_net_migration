
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


cntryID <- read_csv("../data_in/countries_codes_and_coordinates.csv") %>% 
  dplyr::select(-cntry_code) %>% 
  #rename(cntry_code = GADM_code) %>% # use GADM code instead of UN code
  select(GADM_code,iso3)

cntryUNmeta <- read.xlsx("../data_in/country_id_meta.xlsx", sheet="cntry_id_FAO_UN") %>% 
  mutate_at(vars(fao_id),as.character) %>% 
  select(-fao_id) %>% 
  as_tibble()


###### National data 80+ population  -------------------------------


## load admin0 raster

cntry_raster_5arcmin <- rast('../data_in_rast/GADM_level0_raster_5arcmin.tif')



###### National data 80+ population 

# create function
source('../functions/myFun_avgAge80plus.R')

rAvgAgeOver80_female <- myFun_avgAge80plus("../data_in/WPP2019_POP_F07_3_POPULATION_BY_AGE_FEMALE.xlsx")
rAvgAgeOver80_male <- myFun_avgAge80plus("../data_in/WPP2019_POP_F07_2_POPULATION_BY_AGE_MALE.xlsx")

names(rAvgAgeOver80_female) <- c('yr2000','yr2005','yr2010','yr2015','yr2020')
names(rAvgAgeOver80_male) <- c('yr2000','yr2005','yr2010','yr2015','yr2020')

writeRaster(rAvgAgeOver80_female,'../results/rAvgAgeOver80_female.tif', gdal="COMPRESS=LZW",overwrite=TRUE)
writeRaster(rAvgAgeOver80_male,'../results/rAvgAgeOver80_male.tif', gdal="COMPRESS=LZW",overwrite=TRUE)


#### gridded population structure ----


# ref raster
ref_rast <- rast('../data_in_rast/age_structure/age_structures_2000_5arcmin.tif')

# country specific raster data for age 80+
rAvgAgeOver80_female <- rast('../results/rAvgAgeOver80_female.tif')
rAvgAgeOver80_male <- rast('../results/rAvgAgeOver80_male.tif')

plot(subset(rAvgAgeOver80_female,3))

### for years 2000, 2005 ... 2020

avgAgeColl <- ref_rast[[1]]
fertShrColl <- ref_rast[[1]]

iYear = 2000
for (iYear in seq(2000,2020,by=5)) {
  
  # age structure
  filePath <- paste0('../data_in_rast/age_structure/age_structures_',iYear,'_5arcmin.tif')
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
writeRaster(avgAgeColl_sub ,'../results/avgAge2000_2020_v2.tif', gdal="COMPRESS=LZW",overwrite=TRUE)

fertShrColl_sub <- subset(fertShrColl,2:6)
names(fertShrColl_sub) <- c('avgAge2000','avgAge2005','avgAge2010','avgAge2015','avgAge2020')
writeRaster(fertShrColl_sub ,'../results/fertileFemaleShr2000_2020_v2.tif', gdal="COMPRESS=LZW",overwrite=TRUE)

plot(avgAgeColl_sub[[5]])
# fill NA values


#### Ratio between average age and life expectance ----


lifeExp <- rast('../data_in_downscaling/hdiComponent_lifexpUpd.tif')
# select 2000-2019 and use 2019 to year 2020 too
lifeExp_sel <- lifeExp[[c(11:nlyr(lifeExp),nlyr(lifeExp))]]

# interpolate avgAge between the years (2000,2005,...,2020)
avgAge <- rast('../results/avgAge2000_2020_v2.tif')

# define function for interpolation
source('../functions/f_interp_missingYears.R') 

# library(whitebox)
# 
ref_rast <- rast(ncol=360*12, nrow=180*12)
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

if (file.exists('../results/AvgAge_v2_interp.tif')){
  # load it
  avgAge_interp_proj <- rast('../results/AvgAge_v2_interp.tif')
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
  writeRaster(avgAge_buffer_sub ,'../results/AvgAge_v2_buffer.tif', gdal="COMPRESS=LZW",overwrite=TRUE)
  
  
  
  avgAge_interp <- f_interp_missingYears(avgAge_buffer_sub, nameInd = 'avgAge')  
  
  #avgAge_interp <- f_interpRaster(stack(avgAge_buffer_sub),seq(2000,2020,5),seq(2000,2020,1))
  
  # project avgAge to same extent than lifeExp
  avgAge_interp_buff_proj <- terra::extend(avgAge_interp,subset(lifeExp,1))
  
  ext(avgAge_interp_buff_proj) <- ext(subset(lifeExp,1))
  
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
  writeRaster(avgAge_interp_proj ,'../results/AvgAge_v2_interp.tif', gdal="COMPRESS=LZW",overwrite=TRUE)
}

# buffer to lifeExp_sel to take care of slightly different land mask of the data


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
writeRaster(lifeExp_sel_masked ,'../results/lifeExp_sel_masked.tif', gdal="COMPRESS=LZW",overwrite=TRUE)

avgAge_interp_buff <- rast('../results/AvgAge_v2_buffer.tif')

avgAge_interp <- f_interp_missingYears(avgAge_interp_buff, nameInd = 'avgAge')  

# project avgAge to same extent than lifeExp
avgAge_interp_buff_proj <- terra::extend(avgAge_interp,subset(lifeExp,1))

ext(avgAge_interp_buff_proj) <- ext(subset(lifeExp,1))


# store masked ratio
# ratio between avgAge and lifeExp
ratioAvgAge_lifeExp <- avgAge_interp_buff_proj / lifeExp_sel_buff
ratioAvgAge_lifeExp[mask_sea == 1] <- NA

#ratioAvgAge_lifeExp[ratioAvgAge_lifeExp == 0] = NA
ratioAvgAge_lifeExp[ratioAvgAge_lifeExp < 0.02] = NA # some very small values, due to different landmask of the files
names(ratioAvgAge_lifeExp) <- paste0('ratio',paste0(2000:2020)) 
writeRaster(ratioAvgAge_lifeExp ,'../data_in_downscaling/ratioAvgAge_lifeExp_v2.tif', gdal="COMPRESS=LZW",overwrite=TRUE)



# interpolate fertile women share data ------------------------------------

# define function for interpolation

# load data
fertileFemShr <- rast('../results/fertileFemaleShr2000_2020_v2.tif')

# interpolate
# run funtionc
fertileFemShr_interp <- f_interp_missingYears(fertileFemShr,'fertileFemShr')

r_fertileFemShr_interp <- fertileFemShr_interp

r_fertileFemShr_interp[r_fertileFemShr_interp == 0] = NA
names(r_fertileFemShr_interp) <- paste0('ratio',paste0(2000:2020)) 


rastRef = rast(paste0('../results/','ratioAvgAge_lifeExp_v2','.tif')) %>% 
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
writeRaster(r_fertileFemShr_masked ,'../data_in_downscaling/fertileFemaleShr2000_2020_v2_interp.tif', gdal="COMPRESS=LZW",overwrite=TRUE)

#plot(subset(r_fertileFemShr_masked,5))

### plot ####

library(patchwork)
library(tmap)
library(rnaturalearth)
library(viridis)


# load raster
ratioAvgAge_lifeExp <- rast('../results/ratioAvgAge_lifeExp_v2.tif')

fertShrColl <- rast('../results/fertileFemaleShr2000_2020_v2_interp.tif')


ratioAvgAge_lifeExp_robin <- 1 - terra::project(subset(ratioAvgAge_lifeExp,21), "+proj=robin",
                                                mask = TRUE)
fertShrColl_robin <- terra::project(subset(fertShrColl,21), "+proj=robin",
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
source('../functions/f_raster2map_v2.R')


map_ratioAvgAge_lifeExp <- f_raster2map_v2(ratioAvgAge_lifeExp_robin,cntry50,colPalette,"percentage of life left")

map_fertShrColl <- f_raster2map_v2(fertShrColl_robin,cntry50,colPalette,"percentage of fertile women")


# collect maps
map_popIndicators <- tmap_arrange( map_ratioAvgAge_lifeExp,map_fertShrColl,
                                   widths = c(.5, .5))


# Define the folder name
folder_name <- "../figures"

# Check if the folder exists
if (!dir.exists(folder_name)) {
  # Create the folder if it doesn't exist
  dir.create(folder_name)
  message("Folder '", folder_name, "' created.")
} else {
  message("Folder '", folder_name, "' already exists.")
}

# save map
tmap_save(map_popIndicators, "../figures/map_popIndicators_2020.pdf", width=180, height=200, units="mm")

