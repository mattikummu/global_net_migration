
f_currentMap <- function(pathVar, 
                         variableName, 
                         colPalette = "roma",
                         colBegin = 0,
                         colEnd = 1, 
                         colDir = 1,
                         breaks){
  
  rastCurrent = rast(paste0(pathVar,variableName,'.tif')) %>% 
    subset(.,terra::nlyr(.)) # last layer
  
  ac_pal <- scico(n = 3, palette = colPalette, begin = colBegin, end = colEnd, direction = colDir)
  
  plt_ac_global <- f_mappingTMAP(r_index = rastCurrent,
                                 index_label = paste0(variableName," per 1000 people"),
                                 colorpal = ac_pal,
                                 breakvals = breaks,
                                 tocrs = "+proj=robin +over",
                                 polygonFile = shp_cntrySml)
  
}
