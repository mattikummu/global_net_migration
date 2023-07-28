
source('Scripts/ATPK_function.R')
library(dplyr)

#Load residuals of admin application

XGB_area_resid <- read.table('Outputs/LM/Births/LM_101221_brths_residuals.txt')

admin_boundry <- st_read('data/UPDATED brths dths data/adminBoundary_births.gpkg')

lst_fls <- list.files('Outputs/LM/Births/Initial_apply/101221/', pattern = '.tif', full.names = T)
fine.XGB.rstrs <- stack(lst_fls)
# fine.XGB.rstrs <- stack('Outputs/LM/Births/Initial_apply/101221/')
names(fine.XGB.rstrs) <- 2000:2019

fine.XGB.pnts <- fine.XGB.rstrs %>% raster::rasterToPoints() %>% as.data.frame() %>% st_as_sf(coords = c(1,2)) %>% st_set_crs(crs(admin_boundry))

yrs <- 2000:2019
rst_xgb_lst <- list()
for(i in 1:length(yrs)){
  
  y <- yrs[i] %>% as.integer()
  XGB_yr_res <- XGB_area_resid[XGB_area_resid$yrs == y,] %>% dplyr::select(coord.x, coord.y, residual) %>% st_as_sf(coords = c(1,2)) %>% st_set_crs(crs(admin_boundry))
  
  intersct <- st_intersects(admin_boundry$geom,XGB_yr_res)%>% as.data.frame()
  
  admin_bndry.res <- admin_boundry %>% mutate('residual' = NA)
  
  admin_bndry.res$residual[intersct$row.id] <- XGB_yr_res$residual[intersct$col.id]
  admin_bndry.res <-  admin_bndry.res %>% na.omit()
  
  fine.XGB.yr <-  fine.XGB.pnts %>% dplyr::select(paste0('X',y)) %>% na.omit()
  
  nm.yr <- paste0('X',y)
  fine.XGB.rstr.yr <- fine.XGB.rstrs[[nm.yr]]
  
  XGB_ATP_yr <- ATP_vector_gstat(inShp_Area=admin_bndry.res,inShp_Point=fine.XGB.yr,inShp_rstr=fine.XGB.rstr.yr,outPred=paste0('Outputs/LM/Births/ATPK/101221/',y,'.tif'),field="residual",formula=residual~1,samplesize=9,norms=2,showATP=F)
  
  rst_xgb_lst <- c(rst_xgb_lst,XGB_ATP_yr)
  print(y)
}

lst <- list.files('Outputs/LM/Births/ATPK/101221/', full.names = T, pattern = '.tif')

stk <- stack(lst)

rst_xgb_lst <- stack(rst_xgb_lst)
writeRaster(rst_xgb_lst,'Outputs/LM/Dths/ATPK/141021/')
