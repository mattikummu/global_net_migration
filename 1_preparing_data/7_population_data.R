setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(terra)
library(dplyr)
library(tidyr)
library(raster)

timesteps <- seq(2000,2020,1)

# Calculate urban and rural pop
# Load urban extent
pop <- rast('DATA/r_worldpopHarmonised.tif')
urbanExt <- rast('DATA/r_urbExtent.tif')
unique(values(urbanExt$ppp_1))
# remove any values that are not 0 or 1
urbanExt[urbanExt > 0 & urbanExt < 1] <- NA
unique(values(urbanExt$ppp_1))
urbanExt <- extend(urbanExt, ext(pop))
terra::writeRaster(urbanExt, "DATA/r_urbExtent_FixedExt.tif", overwrite = T)

# Load pop data

# Multiply urban pop with urban extent (urban areas marked with 1)
urbanPop <- pop*urbanExt
# Initiate ruralpop
ruralPop <- pop
# Set all values in urban areas to NA to get rural pop raster
ruralPop[urbanExt == 1] <- NA 

global(urbanPop, fun = sum, na.rm = T)
global(ruralPop, fun = sum, na.rm = T)
global(pop, fun=sum, na.rm=T)

terra::writeRaster(urbanPop, "DATA/popUrban.tif", overwrite = T)
terra::writeRaster(ruralPop, "DATA/popRural.tif", overwrite = T)