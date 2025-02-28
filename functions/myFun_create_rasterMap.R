myFun_create_rasterMap <- function(r_index, nameLabels, titleLabel, colorpal,
                                   tocrs = NA){
  
  # project to another crs
  if (!is.na(tocrs)){
    r_index <- project(r_index, tocrs, mask = TRUE)
  }
  
  # create tmap object
  index_map <- tm_shape(r_index) +
    tm_raster(style = "fixed", # draw gradient instead of classified
              breaks = c(-3.5,-2.5,-1.5,-0.5,.5,1.5,2.5,3.5),
              palette = colorpal,
              labels = nameLabels,
              showNA = F,
              colorNA = 'white',
              title = titleLabel,
              legend.reverse = TRUE) +
    tm_shape(sf_gadm0)+
    tm_borders(col='grey',
               lwd = 0.1)+
    tm_layout(main.title.position = "left",
              legend.bg.color = TRUE,
              legend.outside = TRUE,
              frame = FALSE)
  
  return (index_map)
  
}