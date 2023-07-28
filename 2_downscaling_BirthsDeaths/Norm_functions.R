
library(sf)
library(raster)
library(dplyr)


norm_beta <- function(Fine_grid_Dta, Area_Dta, init_dta_rstr, field_nm, Area_nm, pop_nm, pop_unit, output_dest){
  
  intersct <- st_intersects(Area_Dta,Fine_grid_Dta)
  
  vls <- Fine_grid_Dta[[field_nm]]
  pop <- Fine_grid_Dta[[pop_nm]]
  
  for (n in 1:length(Area_Dta[[Area_nm]])){
    print(n)
    ids <- intersct[[n]]
    area_n <- Area_Dta[n,]
    if (!is.na(area_n[[Area_nm]])){
      if (area_n[[Area_nm]] == 0){
        vls[ids] <- 0
      } else {
        
        tot.A_pop <- sum(pop[ids],na.rm = T)
        tot.A_value <- (tot.A_pop*pop_unit)*area_n[[Area_nm]]
        fine.vls <- pop_unit*(pop[ids]*vls[ids])
        tot.fn.vls <- sum(fine.vls, na.rm=T)
        rto <- tot.A_value/tot.fn.vls
        
        
        if(min(vls[ids],na.rm = T)>=0){
          
          vls[ids]<- vls[ids]*rto

        } else {
          
          vls_n <-  vls[ids] - min(vls[ids],na.rm = T)
          fine.vls <- pop_unit*(pop[ids]* vls_n)
          tot.fn.vls <- sum(fine.vls, na.rm=T)
          rto <- tot.A_value/tot.fn.vls
          
          new_vls <- vls_n*rto
          
          vls[ids]<- new_vls
          
          # finegrid[ids,][[Point_field]]=(finegrid[ids,][[Point_field]]-max(finegrid[ids,][[Point_field]]))/(max(finegrid[ids,][[Point_field]])-min(finegrid[ids,][[Point_field]]))
          # 
          # finegrid[ids,][[Point_field]]=finegrid[ids,][[Point_field]]/tot.fine_value*tot.A_value 
          # 
          # new_fine <- (fine.vls - min(fine.vls))/((max(fine.vls)-min(fine.vls)))
          # new_fine <- (new_fine/sum(new_fine))*tot.A_value
          # new_fine <- new_fine/pop_unit/(pop[ids])
          # 
          # vls[ids] <- new_fine
          
        }
        
      }
    }
    
  }
  
  rst_vls <- init_dta_rstr %>% values() %>% as.data.frame()
  rst_vls[!is.na(rst_vls)] <- vls
  values(init_dta_rstr) <- rst_vls$.
  
  if(is.na(output_dest)){
    # writeOGR(finegrid,dirname(outPred),basename(outPred),driver = "ESRI Shapefile",overwrite_layer = TRUE)
    writeRaster(init_dta_rstr,'.', overwrite =T, append=FALSE)
  }  else {
    # writeOGR(finegrid,dirname(outPred),basename(outPred),driver = "ESRI Shapefile",overwrite_layer = TRUE)
    writeRaster(init_dta_rstr,output_dest, overwrite =T, append=FALSE)
  }
  
  return(init_dta_rstr)
  
}
