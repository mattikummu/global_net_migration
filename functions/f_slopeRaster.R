f_slopeRaster <- function(variableName,path,startYear,endYear, refYear) {
  
  start_year = startYear
  end_year = endYear
  
  subnatRaster <- rast(paste0(path,variableName,'.tif')) %>% 
    subset(.,c((start_year-refYear+1):(end_year-refYear+1)))
  
  time <- 1:nlyr(subnatRaster) 
  
  # fun=function(x) { if (is.na(x[1])){ NA } else { m = lm(x ~ time); summary(m)$coefficients[2] }}
  # 
  # # change per year
  # raster.slope=rast(calc(subnatRaster, fun))
  
  # Define the function to calculate the slope
  fun <- function(x) {
    if (is.na(x[1])) {
      return(NA)
    } else {
      m <- lm(x ~ time)
      return(summary(m)$coefficients[2])
    }
  }
  
  # Apply the function to compute the slope for each cell
  raster.slope <- app(subnatRaster, fun)
  
  # change over time period 
  nYears <- nlyr(subnatRaster) 
  raster.change = nYears * raster.slope
  
  writeRaster(raster.slope,paste0('../results/rasterSlope_',variableName,'.tif'),  gdal="COMPRESS=LZW",overwrite=TRUE)
  writeRaster(raster.change,paste0('../results/rasterChange_',variableName,'.tif'),  gdal="COMPRESS=LZW",overwrite=TRUE)
  
  return(raster.change)
}