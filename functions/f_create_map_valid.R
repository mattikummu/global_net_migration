f_create_map_valid <- function(in_polygon, inBbox,cMeridian,var_name, maptitle, colorpal, breakvals,
                               color_midpoint = NULL, eqearth = TRUE){
  
  
  # project to Robinson if desired (true by default)
  if (eqearth){
    in_polygon <- st_transform(in_polygon, 
                               crs = paste0('+proj=eqearth +lon_0=',cMeridian,' +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'))
  }
  # create tmap object
  plt_tmap <- tm_shape(in_polygon, bbox = inBbox) +
    tm_polygons(col = var_name,
                style = "fixed",
                breaks = breakvals,
                palette = colorpal,
                lwd = 0.01,
                textNA = "no data",
                colorNA = "grey80",
                legend.is.portrait = FALSE) +
    tm_layout(main.title = maptitle,
              frame = FALSE,
              legend.bg.color = TRUE,
              legend.outside = TRUE,
              legend.outside.position = "bottom",
              main.title.size = 0.8,
              legend.title.size = 0.6)
  
  
  return(plt_tmap)
  
}