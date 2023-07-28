library(dplyr)
library(sf)
library(raster)

################################################
###### DATA ####################################
################################################

#admin boundaries shape file
admin_b <-read_sf('data/UPDATED brths dths data/adminBoundary_births.gpkg')

#independent variable (dths/birhts)
main_data <- stack('data/UPDATED brths dths data/ratioRaster_births.tif')
names(main_data) <- 1990:2019
main_data <- main_data[[11:30]]

#predictor variables
pred_var_hdi <- stack('data/pred vars/UPDATED_predvars/norm/pred_var_hdi_nrm.grd')
names(pred_var_hdi) <- 1990:2019
pred_var_hdi <- pred_var_hdi[[11:30]]

pred_var_NLight <- stack('data/pred vars/UPDATED_predvars/norm/pop_dens_worldpop_scaled_2000_2020.tif')
names(pred_var_NLight) <- 2000:2020
pred_var_NLight <- pred_var_NLight[[1:20]]

pred_var_fert <- stack('data/pred vars/UPDATED_predvars/norm/pred_var_fert_ext_nrm.grd')
names(pred_var_fert) <- 2000:2020
pred_var_fert <- pred_var_fert[[1:20]]

pred_var_lifeEXP <- stack('data/pred vars/UPDATED_predvars/norm/pred_var_lifeEXP_nrm.grd')
names(pred_var_lifeEXP) <- 2000:2020
pred_var_lifeEXP <- pred_var_lifeEXP[[1:20]]

incm_cunt <- raster('data/Upper_lower_incomes/modelgroups.tif')

################################################
###### EXTRACT VALUES ##########################
################################################


pred_var_NLight <- pred_var_NLight[[1]]

admin_b_rstr <- admin_b['GID_nmbr'] %>% rasterize(pred_var_NLight[[1]] %>% raster::aggregate(fact=3),fun='median', field='GID_nmbr')
# admin_b_rstr <- writeRaster(admin_b_rstr,'data/UPDATED brths dths data/adminBoundary_births_zones_births.tif')
admin_b_rstr <- admin_b_rstr %>% resample(pred_var_NLight[[1]],method='ngb')

etrct_vrs <- c('main_data','pred_var_hdi', 'pred_var_NLight','pred_var_fert','incm_cunt')

for (i in 1:length(etrct_vrs)){
  
  dta_rst <- get(etrct_vrs[[i]]) 
  
  xtrct <- zonal(dta_rst,admin_b_rstr, fun= 'mean', na.rm=T)
  
  write.table(xtrct,paste0('data/EXTRACTED/brths/',etrct_vrs[[i]],'_brths_extrct.txt'))
  
} 

yrs <- 2000:2019
for (j in 1:1){
  extrct_data <- get(etrct_vrs[[j]])
  main_data_df <- data.frame(admin_b$GID_nmbr)
  mn_dta.point <- extrct_data[['X2019']] %>% raster::rasterToPoints() %>% as.data.frame() %>% st_as_sf(coords = c(1,2)) %>% st_set_crs(crs(admin_b))
  mn_intrsct <- st_intersects(admin_b$geom,mn_dta.point)
  for (i in 1:length(yrs)){
    y <- yrs[i] %>% as.integer()
    A_fld <- paste0('X',y)
    mn_dta.rstr.vls <- extrct_data[[A_fld]] %>% values() %>% as.data.frame() %>% na.omit()
    mn_dta_lst <- list()
    for (x in 1:length(mn_intrsct)){
      dta <- mn_dta.rstr.vls[mn_intrsct[[x]],1] %>% mean(na.rm=T)
      mn_dta_lst <- c(mn_dta_lst,dta)
    }
    mn_dta_lst <- mn_dta_lst %>% unlist()
    main_data_df[A_fld] <- mn_dta_lst
  }
  write.table(main_data_df,paste0('data/EXTRACTED/brths/',etrct_vrs[[j]],'_brths_extrct_longVersion_2000_2019.txt'))
}


# etrct_vrs <- c('pred_var_NLight')
# for (i in 1:1) {
#   var_out <- data.frame(ID=admin_b$GID_nmbr) %>% dplyr::select((!ID))
#   var_i <-  get(etrct_vrs[[i]]) 
#   for (j in 1:nlayers((var_i))){
#     extrxt_var_i_vls <- var_i[[j]] %>% values() %>% as.data.frame() %>% na.omit()
#     extrxt_var_i_pnts <-  var_i[[j]] %>% raster::rasterToPoints() %>% as.data.frame() %>% st_as_sf(coords = c(1,2)) %>% st_set_crs(crs(admin_b))
#     i_intrsct <- st_intersects(admin_b$geom,extrxt_var_i_pnts)
#     out_nm_lst <- list()
#     for (x in 1:length(i_intrsct)){
#       dta <- extrxt_var_i_vls[i_intrsct[[x]],1] %>% mean(na.rm=T)
#       out_nm_lst <- c(out_nm_lst,dta)
#     }
#     out_nm_lst <- out_nm_lst %>% unlist() %>% as.data.frame()
#     names(out_nm_lst) <- names(var_i[[j]])
#     var_out <- cbind(var_out,out_nm_lst)
#   }
#   
#   
#   write.table(var_out,paste0('data/EXTRACTED/dths/',etrct_vrs[[i]],'_dths_extrct2.txt'))
# }

################################################
###### BUILD DATAFRAMES #########################
################################################

main_dta.admin <- read.table('data/EXTRACTED/brths/main_data_brths_extrct.txt')
incm_bnds <- read.table('data/EXTRACTED/brths/incm_cunt_brths_extrct.txt') %>% round()
pred_hdi.admin <- read.table('data/EXTRACTED/brths/pred_var_hdi_brths_extrct.txt')
pred_NLight.admin <- read.table('data/EXTRACTED/brths/pred_var_worldpop_brths_extrct.txt')
pred_fert.admin <- read.table('data/EXTRACTED/brths/pred_var_fert_brths_extrct.txt')

coords <- st_centroid(admin_b$geom) %>% st_coordinates()
coords <- coords %>% as.data.frame()
coords$zone <- admin_b$GID_nmbr

#Training data dataframe
yrs <- c(2000:2019)

df <- data.frame()
for (yr in yrs){
  nm_yr <- paste0('X',yr)
  df_n <-   data.frame(pred_hdi.admin$zone, yr, main_dta.admin[nm_yr], pred_hdi.admin[nm_yr], pred_NLight.admin[nm_yr], pred_fert.admin[nm_yr],incm_bnds$mean)
  names(df_n) <- c('zone', 'yrs', 'main_dta', 'hdi', 'NLight', 'fert','incm_admin')
  df_n <- merge(df_n,coords,by='zone',all=T)
  df <- rbind(df,df_n)
}

train_df <- df %>% na.omit()

write.table(train_df,'data/Training/brths/train_df_brths_norm_101221.txt')









