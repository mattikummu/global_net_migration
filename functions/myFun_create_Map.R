
myFun_create_Map <- function(r_raster, mybreaks,colorpal, titleLabel, #nameLabels
                             tocrs = NA){ #
  
  # project to another crs
  if (!is.na(tocrs)){
    r_raster <- project(r_raster, tocrs, mask = TRUE)
  }
  
  # create tmap object
  index_map <- tm_shape(r_raster) +
    tm_raster(style = "fixed", # draw gradient instead of classified
              breaks = mybreaks ,
              palette = colorpal,
              #labels = nameLabels,
              showNA = F,
              colorNA = 'white',
              title = titleLabel) +
    tm_facets(free.scales = FALSE)
  
  return (index_map)
  
}