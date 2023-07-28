# rm(list = ls())  # clean memory
# graphics.off()   # close graphic windows
library(foreign)
library(sp)
library(sf)
library(RColorBrewer)
library(rgdal)
library(gstat)
library(MASS)
library(geoR)
library(raster)
library(reshape)
library(maptools)
library(rgeos)
library(spdep)
library(grid)


ATP_vector_gstat<-function(inShp_Area,inShp_Point,inShp_rstr,outPred=NA,field="residual",formula=residual~1,samplesize=9,norms=2,showATP=FALSE)
{

  plyg=inShp_Area
  
  inShp_rstr_big <- inShp_rstr %>% raster::aggregate(2)
  
  point_shp <- inShp_rstr_big %>% raster::rasterToPoints() %>% as.data.frame() %>% st_as_sf(coords = c(1,2)) %>% st_set_crs(crs(plyg))
  
  finegrid=point_shp # as R object
  # plot(finegrid)
  
  kr = krige(as.formula(paste(field,"~1")), plyg %>% as_Spatial(), finegrid)

  # finegrid["ATP"]=as.vector(kr$var1.pred)
  
  intersec <- st_intersects(plyg,point_shp)
  
  rst_vls <- inShp_rstr_big %>% values() %>% as.data.frame()
  rst_vls[!is.na(rst_vls)] <- 0
  
  ATP_vls <- as.vector(kr$var1.pred)
  
  for(i in 1:length(plyg[[field]])){
    centerplyg=plyg[c(i),]
    ids <- intersec[[i]]
    # finegridsub=finegrid[ids,]$ATP
    finegridsub=ATP_vls[ids]
    if(length(finegridsub)<1)
      next
    #extract ids of finegridsub
    # ids=data.frame(unlist(strsplit(rownames(fs2@coords)," ")))
    # ids=as.vector(ids[seq(1,nrow(ids),by=2),])
    # ids=as.numeric(ids)
    # finegridsub2=finegrid[ids,]#the grids within the central polygon
    
    
    if(centerplyg[[field]]==0)#### no operation for the case of value == 0
    {
      
      ATP_vls[ids]=ATP_vls[ids]*0
    }
    else{
      if(norms==1){
        ATP_vls[ids]=ATP_vls[ids]*(plyg[i,][[field]]/sum(ATP_vls[ids])) #### coherence constraint
      }
      else if(norms==2){
        if(centerplyg[[field]]>0)
        {
          #
          if(min(ATP_vls[ids])<0){
            ATP_vls[ids]=(ATP_vls[ids]-min(ATP_vls[ids]))/(max(ATP_vls[ids])-min(ATP_vls[ids]))
            
            ATP_vls[ids]=ATP_vls[ids]*(plyg[i,][[field]]/sum(ATP_vls[ids])) #### coherence constraint
          }else{
            ATP_vls[ids]=ATP_vls[ids]*(plyg[i,][[field]]/sum(ATP_vls[ids])) #### coherence constraint
          }
          
        }
        else if(centerplyg[[field]]<0)
        {
          if(max(ATP_vls[ids])>0){
            ATP_vls[ids]=(ATP_vls[ids]-max(ATP_vls[ids]))/(max(ATP_vls[ids])-min(ATP_vls[ids]))
            
            ATP_vls[ids]=ATP_vls[ids]*(plyg[i,][[field]]/sum(ATP_vls[ids])) #### coherence constraint
          }else{
            ATP_vls[ids]=ATP_vls[ids]*(plyg[i,][[field]]/sum(ATP_vls[ids])) #### coherence constraint
          }
          
        }
      }
      
    }
    p <- (i*100)/nrow(plyg)
    p <- round(p,2)
    print(paste0(p,'%'))
    # gc(rm(centerplyg,finegridsub,ids))
  }
  gc(rm(centerplyg,finegridsub,ids))
  
  rst_vls[!is.na(rst_vls)] <- ATP_vls
  values(inShp_rstr_big) <- rst_vls$.
  inShp_rstr <- inShp_rstr_big %>% raster::resample(inShp_rstr)
  
  if(is.na(outPred)){
    # writeOGR(finegrid,".",paste(inShp_Area,"a2p",sep ="_"),driver = "ESRI Shapefile",overwrite_layer = TRUE)
    writeRaster(inShp_rstr,'.',paste0('xgb_atpk.grd'))
  }
  else{
    # writeOGR(finegrid,dirname(outPred),basename(outPred),driver = "ESRI Shapefile",overwrite_layer = TRUE)
    writeRaster(inShp_rstr,outPred, overwrite =T, append=FALSE)
  }
  if(showATP)
    spplot(finegrid,"ATP")
  print("ATP_vector_gstat is completed.")
  return(inShp_rstr)
}
