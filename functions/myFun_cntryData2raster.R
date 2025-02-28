myFun_cntryData2raster <- function(data_in,variableName){
  # data_in <- ratio_r_pop
  # predefine raster stack
  st_ratio <-  r_GADM0
  # data in long format
  names(data_in) <- c('GADM_code',timestep)
  data_in_long <- data_in %>% 
    pivot_longer(!c('GADM_code'), 
                 names_to = "year", 
                 values_to = 'inData') 
  
  iYear = 2000
  for (iYear in timestep) {
    temp <-  data_in_long %>% 
      filter(year == iYear) %>% 
      dplyr::select(c(inData,'GADM_code'))
    temp_id <-  as.numeric(temp$GADM_code)
    temp_v <- as.numeric(temp$inData)
    
    # reclassify
    temp_raster <- classify(r_GADM0,
                            cbind(temp_id, temp_v))
    
    # countries without data to NA
    temp_raster[temp_raster>max(temp_v)] <- NA
    
    st_ratio <- c(st_ratio,temp_raster)
  } 
  
  # remove first layer with ids
  st_ratio_data <- subset(st_ratio,2:nlyr(st_ratio))
  names(st_ratio_data) <- paste0(variableName,timestep)
  
  #writeRaster(st_ratio_data,paste0('../results/ratioRaster_',variableName,'.tif'),  gdal="COMPRESS=LZW",overwrite=TRUE)
  
  return(st_ratio_data)
  
}