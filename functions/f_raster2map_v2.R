f_raster2map_v2 <- function(r_in,shape_in, colPalette, plotTitle) {
  
  
  tmapMap <- tm_shape(r_in) +
    tm_raster(palette = colPalette,
              #breaks = fishing.breaks,
              title = plotTitle,
              n = 15,
              style = "fisher",
              colorNA = "white",
              legend.is.portrait = FALSE) +
    tm_shape(shape_in,projection = "robin") +
    tm_borders(col="grey50",lwd = 0.1)+
    tm_layout(legend.bg.color = TRUE,
              legend.outside.position = "bottom",
              legend.outside = TRUE,
              frame = FALSE)
  
  
  return(tmapMap)
}
