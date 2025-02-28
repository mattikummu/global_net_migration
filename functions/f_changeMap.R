f_changeMap <- function(variableName, 
                        colPalette = "roma",
                        colBegin = 0,
                        colEnd = 1, 
                        colDir = 1,
                        breaks){
  
  rastChange = rast(paste0('../results/',variableName,'.tif')) %>% 
    subset(.,terra::nlyr(.)) # last layer
  
  ac_pal <- scico(n = 3, palette = colPalette, begin = colBegin, end = colEnd, direction = colDir)
  
  plt_ac_global <- f_mappingTMAP(r_index = rastChange,
                                 index_label = paste0("Change in ",variableName, " per 1000 people over 2000-2019"),
                                 colorpal = ac_pal,
                                 breakvals = breaks,
                                 color_midpoint = 0,
                                 tocrs = "+proj=robin +over",
                                 polygonFile = shp_cntrySml)
  
}