f_mappingTMAP <- function(r_index, index_label, colorpal, breakvals,
                          color_midpoint = NULL, tocrs = NA,
                          polygonFile){
  
  polygonFile <- terra::vect(as(polygonFile, "Spatial"))  
  
  # project to another crs
  if (!is.na(tocrs)){
    r_index <- terra::project(r_index, tocrs, mask = TRUE)
    polygonFile <- terra::project(polygonFile, tocrs) %>% 
      st_as_sf()
  }
  
  # create tmap object
  index_map <- tm_shape(r_index) +
    tm_raster(style = "fixed", # draw gradient instead of classified
              palette = colorpal,
              breaks = breakvals,
              title = index_label,
              midpoint = color_midpoint,
              #legend.reverse = TRUE,
              legend.is.portrait = FALSE) +
    tm_shape(polygonFile, projection = ) +
    tm_borders(col=NA,lwd = 0.5) +
    tm_layout(main.title.position = "left",
              legend.bg.color = TRUE,
              legend.outside = TRUE,
              legend.outside.position = "bottom",
              frame = FALSE)
  
  return (index_map)
  
}