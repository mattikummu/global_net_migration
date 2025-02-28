f_birth_death_adminRaster <- function(variableName) {
  
  
  subnat_gis <- st_read(paste0('../results/test_',variableName,'_combined.gpkg')) %>% 
    select(iso3,GID_nmbr)
  
  
  cntry_gis <- st_read('../data_in_gpkg/gadm_level0.gpkg') %>% 
    rename(iso3 = GID_0) %>% 
    filter(!iso3 %in% c(unique(subnat_gis$iso3),'ALA')) %>%   # remove countries for which subnat data exists + Åland
    left_join(cntry_info[,c(2,5)]) %>% # add country code 
    rename(GID_nmbr = GADM_code) %>% 
    select(-NAME_0)
  
  combined_gis <- subnat_gis %>% 
    bind_rows(cntry_gis)
  
  # # from mix of polygon and multipolygon to polygon
  # combined_gis_id_cast <- st_cast(combined_gis,"MULTIPOLYGON") %>% 
  #   st_cast("POLYGON")
  # rasterise polygon file 
  #adminBoundary_raster_1arcmin <- rast(fasterize(combined_gis_id_cast,raster(ref_raster_1arcmin),field="GID_nmbr"))
  
  # to terra polygon format
  vect_combined_gis <- terra::vect(as(combined_gis, "Spatial")) 
  
  # rasterise polygon file 
  adminBoundary_raster_1arcmin <- terra::rasterize(vect_combined_gis,ref_raster_1arcmin,field="GID_nmbr")
  
  #plot(adminBoundary_raster_1arcmin)
  
  # aggregate to 5 arc-min
  adminBoundary_raster_5arcmin <- terra::aggregate(adminBoundary_raster_1arcmin,fact=5,fun=modal,na.rm=T)
  
  adminBoundary_raster_5arcmin[is.nan(adminBoundary_raster_5arcmin)] = NA
  
  # write raster
  writeRaster(adminBoundary_raster_5arcmin,paste0('../results/adminBoundary_',variableName,'_raster_5arcmin.tif'), gdal="COMPRESS=LZW",overwrite=TRUE)
  
  # write id data
  st_write(combined_gis,paste0('../results/adminBoundary_',variableName,'.gpkg'), append=F)
  
  
}