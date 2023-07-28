# This script creates the socio-climatic bins

rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

packages <- c("terra", "raster", "tmap", "reldist")
not_installed <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(not_installed)){install.packages(not_installed)}

library(terra)
library(raster)
library(reldist)
library(stringr)



# Load data 
population <- rast('DATA/r_worldpopHarmonised.tif')

# Create a pop mask
pop_mask <- population
# mask out zero values (i.e. areas with zero pop)
pop_mask[pop_mask == 0] <- NA
# All NA values to NaN
pop_mask[is.na(pop_mask)] <- NaN
# All areas with pop to 1
pop_mask[pop_mask > 0] <- 1

pop_mask

pop_mask_orig <- pop_mask #rast('DATA/pop_mask.tif')

# format data to have same extent and transform NAs to zero
pop_mask_orig[is.na(pop_mask_orig)] <- 0 # transform NA to zero
population[is.na(population)] <- 0 # transform NA to zero
population <- resample(population, pop_mask_orig, method = "near") # resample
population[is.na(population)] <- 0 # transform remaining NA to zero

# and into a single layer (if average is used)
pop_mask_max <- max(pop_mask_orig)
#average over time
pop_count_avg <- mean(population)


# read in human development index and aridity
hdi <- rast('DATA/hdi_buff.tif')
aridity <- rast('DATA/aridity_buff.tif')

# project to same crs as pop_mask
hdi <- project(hdi, pop_mask_max, method = "bilinear" )
aridity <- project(aridity, pop_mask_max, method = "bilinear")


####### Pop weighted bins ########

# function to create climate bins
create_bins <- function(hdi_tot, aridity_avg, pop_count, pop_mask, N) {
  
  
  # mask out non-populated areas from the data
  hdi_tot[pop_mask == 0] <- NA
  aridity_avg[pop_mask == 0] <- NA
  
  # assign NA to all pop_cells where hdi/aridity is NA
  pop_count[is.na(hdi_tot)] <- NA
  pop_count[is.na(aridity_avg)] <- NA
  
  # create mask raster from cells where population is not Na
  tot_mask <- !is.na(pop_count) # create for which any data is nan
  
  # assign NA to all hdi and aridity cells where mask is 0 (i.e. no population)
  aridity_avg[tot_mask == 0] <- NA
  hdi_tot[tot_mask == 0] <- NA
  
  
  # raster to vector, drop na also
  hdi_vec <- na.omit(c(as.matrix(hdi_tot)))
  arid_vec <- na.omit(c(as.matrix(aridity_avg)))
  pop_vec <- na.omit(c(as.matrix(pop_count)))
  
  # create hdi bins, based on N quantiles 
  hdi_wtd_quantiles <- wtd.quantile (hdi_vec, q = seq(0, 1, length.out = N+1), weight=pop_vec) #try also N+1
  hdi_wtd_quantiles[1] <- -100
  hdi_wtd_quantiles[length(hdi_wtd_quantiles)] <- 100
  #T_quantiles <- quantile (T_vec, p = seq(0, 1, length.out = N+1))
  
  # Categorize
  hdi_cut <- cut(hdi_vec, hdi_wtd_quantiles, include.lowest=TRUE)
  hdi_bins <- (as.numeric(hdi_cut)-1)*10 # multiply by ten to allow incorporating bin info on aridity too
  
  # initialize a vector for the socio-climatic bins (same lenght as T_bins, but all vals -9999)
  soc_arid_bins <- hdi_bins*0-9999
  
  # loop across the unique hdi bins, and conduct the same steps for aridity
  # (in other words, the quantiles are first taken from the hdi data, and from each hdi bin
  # aridity quantiles are calculated, and then categorized)
  for (hdi_bin in unique(hdi_bins)){
    
    # create aridity bins for each hdi bin, based on N quantiles 
    arid_bin <- arid_vec[hdi_bins == hdi_bin]
    pop_bin <- pop_vec[hdi_bins == hdi_bin]
    
    #print(sum(pop_bin) / sum(pop_vec))
    
    arid_wtd_quantiles <- wtd.quantile (arid_bin, q = seq(0, 1, length.out = N+1), weight=pop_bin) 
    arid_wtd_quantiles[1] <- -100
    arid_wtd_quantiles[length(arid_wtd_quantiles)] <- 10^9
    
    arid_cut <- cut(arid_bin, unique(arid_wtd_quantiles) , include.lowest=TRUE)
    arid_bins <- as.numeric(arid_cut)-1
    
    # for (P_bin in unique(P_bins)){
    #   print(sum(pop_bin[P_bin == P_bins])/sum(pop_vec))
    # }
    
    # add the aridity bin category info to the hdi bin variable (hdi bin is in the tens, and aridity is in the ones) 
    soc_arid_bins[hdi_bins == hdi_bin] = hdi_bins[hdi_bins == hdi_bin] + arid_bins
    
  } 
  
  
  soc_arid_bins_raster <- rast(tot_mask)
  soc_arid_bins_raster[tot_mask] <- soc_arid_bins +1
  soc_arid_bins_raster <- raster(soc_arid_bins_raster)
  
  
  
  return(soc_arid_bins_raster)
  
}

# Extract years 

# total hdi, temperature and pop (longterm average)

hdi_tot <-  hdi
aridity_avg <-  aridity

pop_count <- pop_count_avg
pop_mask <- pop_mask_max

# what size bin matrix to create (N x N)
N = 10

pop_soc_arid_bins <- create_bins(hdi_tot, aridity_avg, pop_count, pop_mask, N)

plot(pop_soc_arid_bins)

writeRaster(pop_soc_arid_bins, filename=paste0('DATA/pop_soc_arid_bins_',str_remove(names(pop_count),'ppp_'),'_', Sys.Date(),'.tif'), overwrite = T)




