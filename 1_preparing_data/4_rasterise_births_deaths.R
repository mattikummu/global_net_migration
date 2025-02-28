
## rasterise births and deaths

library(raster)
#library(rgdal)
library(openxlsx)

library(sf)
library(fasterize)

library(smoothr)
#library(rgeos)
library(terra)

library(foreach)
library(doParallel)
library(snow)

library(zoo)

library(viridis)
library(rnaturalearth)
library(tmap)

library(data.table)
library(tidyverse)
library(dplyr) #

# set working directory the path that this script is located in
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# set time steps
timestep <- c(seq(1990, 2019))
step <- c(seq(1,length(timestep)))
# 
# #setup parallel backend to use many processors
# cores=detectCores()
# cl <- makeCluster(cores[1]-1) #not to overload your computer

#### load spatial data ----

# read cntry metadata
cntry_info <- read_csv("../data_in/countries_codes_and_coordinates.csv") #%>%
# #as_tibble() %>% 
# rename(iso_code = iso3) %>% 
# change iso_code for kosovo to match the one in data
#mutate(iso_code = ifelse(iso_code == 'XKX','KSV',iso_code))

# cntry_info_temp <- cntry_info %>% 
#   rename(country = Country) %>% 
#   select(country,iso_code)

#create ref raster
ref_raster_5arcmin <- rast(ncol=360*12, nrow=180*12)
ref_raster_1arcmin <- rast(ncol=360*60, nrow=180*60)




### Function to create birth and death admin rasters ----------------
#variableName = "births"
source('../functions/f_birth_death_adminRaster.R')

### Function to rasterise birth and death ratio ----------------
#variableName = 'births'

source('../functions/f_birth_death_ratio.R')

### run functions -----------------------------------------------------

# create admin raster layers (needs to be done only once)
f_birth_death_adminRaster("births")
f_birth_death_adminRaster("deaths")

# create the rasters with ratios for both, births and deaths
st_ratio_births <- f_birth_death_ratio("births")
st_ratio_deaths <- f_birth_death_ratio("deaths")


# check
plot(subset(st_ratio_births,30))
plot(subset(st_ratio_deaths,30))


