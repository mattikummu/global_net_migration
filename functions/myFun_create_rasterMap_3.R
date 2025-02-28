myFun_create_rasterMap_3 <- function(r_index, r_mgr_df, variableName, mybreaks,colorpal, titleLabel, #nameLabels
                                   tocrs = NA){ #
  
  classMatrix <- r_mgr_df %>% dplyr::filter(variable == variableName) %>% select(admZone, value) %>% as.matrix()
  
  r_mgr_classified <- terra::classify(r_index,
                                      classMatrix) #variable to plot
  
  # project to another crs
  if (!is.na(tocrs)){
    r_mgr_classified <- project(r_mgr_classified, tocrs, mask = TRUE)
  }
  
  # create tmap object
  index_map <- tm_shape(r_mgr_classified) +
    tm_raster(style = "fixed", # draw gradient instead of classified
              breaks = mybreaks ,
              palette = colorpal,
              #labels = nameLabels,
              showNA = F,
              colorNA = 'white',
              title = titleLabel)+
    #legend.reverse = TRUE) +
    tm_shape(sf_gadm0)+
    tm_borders(col='grey',
               lwd = 0.1)+
    tm_layout(main.title.position = "left",
              legend.bg.color = TRUE,
              legend.outside = TRUE,
              frame = FALSE)
  
  
  return (index_map)
  
}