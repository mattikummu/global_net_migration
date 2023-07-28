source('scripts/Norm_functions.R')
library(sf)
library(raster)
library(dplyr)
library(rgeos)

admin_boundry <- st_read('data/UPDATED brths dths data/adminBoundary_births.gpkg')

main_data <- stack('data/UPDATED brths dths data/ratioRaster_births.tif')
names(main_data) <- 1990:2019

list.atpk <- list.files('Outputs/LM/Births/ATPK/101221/',pattern = '.tif', full.names = T)
ATPK_rstrs <- stack(list.atpk)
ATPK_rstrs[is.na(ATPK_rstrs)] <- 0

init.fls <- list.files('Outputs/LM/Births/Initial_apply/101221/', pattern = '.tif', full.names = T)
initial_rstrs <- stack(init.fls[1:20])
# initial_rstrs <- stack('Outputs/LM/Dths/initial apply/141021/LM_dths_init_141021_ALL.gri')

init_ATPKmod.rstrs <- initial_rstrs + ATPK_rstrs
names(init_ATPKmod.rstrs) <- 2000:2019

pop.dta <- stack('data/ppp_2000_2020_5arcmin_corrected_fixed_extended.tif') 
# pop.dta <- stack('data/ppp_2000_2020_min1.tif')
# pop.dta[pop.dta<1] <- 0
names(pop.dta) <- 2000:2020


exrt_dths <- read.table('data/EXTRACTED/brths/main_data_brths_extrct_longVersion_2000_2019.txt')

yrs <- 2000:2019
for (i in 1:length(yrs)){
  
  y <- yrs[i] %>% as.integer()
  A_fld <- paste0('X',y)
  
  # mn_dta.point <- main_data[[A_fld]] %>% raster::rasterToPoints() %>% as.data.frame() %>% st_as_sf(coords = c(1,2)) %>% st_set_crs(crs(admin_boundry))
  # mn_dta.rstr.vls <- main_data[[A_fld]] %>% values() %>% as.data.frame() %>% na.omit()
  # mn_intrsct <- st_intersects(admin_boundry$geom,mn_dta.point)
  # mn_dta.area <- admin_boundry 
  # mn_dta_lst <- list()
  # for (x in 1:length(mn_intrsct)){
  #   dta <- mn_dta.rstr.vls[mn_intrsct[[x]],1] %>% mean(na.rm=T)
  #   mn_dta_lst <- c(mn_dta_lst,dta)
  # }
  # mn_dta_lst <- mn_dta_lst %>% unlist()
  # mn_dta.area[A_fld] <- mn_dta_lst
  
  mn_dta.area <- admin_boundry
  mn_dta.area[A_fld] <- exrt_dths[[A_fld]]
  # init_ATPKmod.rstrs[[A_fld]][init_ATPKmod.rstrs[[A_fld]]<=0] <- 1
  init_ATPK.points <- init_ATPKmod.rstrs[[A_fld]] %>% raster::rasterToPoints() %>% as.data.frame() %>% st_as_sf(coords = c(1,2)) %>% st_set_crs(crs(admin_boundry))
  init_ATPK.points <- init_ATPK.points %>% na.omit()
  
  pop.nm <- paste0('X',y)
  pop_intrsct.fine <- extract(pop.dta[[pop.nm]], init_ATPK.points %>% as_Spatial())
  pop_intrsct.fine[1:10] <- 1
  for (n in 1:(length(pop_intrsct.fine))){if(pop_intrsct.fine[[n]] %>% is.na()){pop_intrsct.fine[[n]] <- mean(c(pop_intrsct.fine[(n+1):(n+10)],pop_intrsct.fine[(n-1):(n-10)]), na.rm =T)}}
  init_ATPK.points['pop'] <- pop_intrsct.fine
  
  init_rstr=init_ATPKmod.rstrs[[A_fld]]
  
  norm_out <- norm_beta(Fine_grid_Dta=init_ATPK.points, Area_Dta=mn_dta.area,init_dta_rstr = init_rstr, field_nm=A_fld, Area_nm=A_fld, 
                        pop_nm='pop', pop_unit = (1/1000), output_dest=paste0("Outputs/LM/Births/normalised/101221/LM_brths_normalised_101221_",y,'.tif'))
  
  
  
}

out_rast_nrm <- stack('Outputs/LM/Births/normalised/101221/LM_brths_normalised_101221_ALL_popNAmain.tif')
lst.nrm <- list.files('Outputs/LM/Births/normalised/101221/', pattern = '.tif', full.names = T)
out_rast_nrm <- stack(lst.nrm[1:20])
names(out_rast_nrm) <- 2000:2019

for (y in 2000:2019){
  nm <- paste0('X',y)
  # out_rast_nrm[[nm]][is.na(out_rast_nrm[[nm]])] <- main_data[[nm]][is.na(out_rast_nrm[[nm]])]
  out_rast_nrm[[nm]][pop.dta[[nm]]<10] <- main_data[[nm]][pop.dta[[nm]]<10]
  # out_rast_nrm[[nm]][is.na(pop.dta[[nm]])] <- main_data[[nm]][is.na(pop.dta[[nm]])]
  # out_rast_nrm[[nm]][out_rast_nrm[[nm]]<0] <- 0
  # out_rast_nrm[[nm]][out_rast_nrm[[nm]]>100] <- NA
}



writeRaster(out_rast_nrm,'Outputs/LM/Births/normalised/101221/LM_brths_normalised_101221_ALL_FINAL.tif', overwrite=T)

