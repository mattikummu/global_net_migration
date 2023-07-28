library(dplyr)
library(caret)
library(raster)
library(sf)


####################################################
###### DATA ########################################
####################################################
df <- data.frame()
mdls_incm <- list()
for (i in 1:4){
  train_df1 <- read.table('data/Training/brths/train_df_brths_norm_101221.txt')
  
  train_df1 <- train_df1 %>% filter(incm_admin == i)
  train_df <- train_df1 %>% dplyr::select(main_dta,hdi, NLight, fert)
  
  
  
  lm_mdl <- lm(main_dta ~ NLight + fert + hdi, data = train_df)
  saveRDS(lm_mdl,paste0('Models/LM/101221/LM_101221_brths_',i))
  
  lm_results <- train_df
  lm_results$lm_res <- stats::predict.lm(lm_mdl)
  lm_results <- lm_results %>% mutate(residual = main_dta - lm_res)
  lm_results$coord.x <- train_df1$X
  lm_results$coord.y <- train_df1$Y
  lm_results$yrs <- train_df1$yrs
  df <- rbind(df,lm_results)
  mdls_incm <- rbind(mdls_incm,lm_mdl)
}




defaultSummary(data.frame(pred=df$lm_res,obs=df$main_dta), lev = NULL, model = NULL)

write.table(df,'Outputs/LM/Births/LM_101221_brths_residuals.txt')

plot(df$main_dta,df$lm_res)

saveRDS(mdls_incm,'Models/LM/101221/LM_brths_101221_ALL')

########################
###  Predict with LM
##########################
#admin boundaries shape file
admin_boundry  <-read_sf('data/UPDATED brths dths data/adminBoundary_deaths.gpkg')

#independent variable (dths/birhts)
main_data <- stack('data/UPDATED brths dths data/ratioRaster_births.tif')
names(main_data) <- 1990:2019

# #predictor variables
# pred_var_hdi <- stack('data/pred vars/UPDATED_predvars/norm/pred_var_hdi_nrm.grd')
# names(pred_var_hdi) <- 1990:2019
# 
# 
# pred_var_NLight <- stack('data/pred vars/UPDATED_predvars/norm/pred_var_NLight_nrm.grd')
# names(pred_var_NLight) <- 1990:2020
# 
# pred_var_fert <- stack('data/pred vars/UPDATED_predvars/norm/pred_var_fert_ext_nrm.grd')
# names(pred_var_fert) <- 2000:2020
# 
# pred_var_lifeEXP <- stack('data/pred vars/ratioAvgAge_lifeExp_v2.tif')
# names(pred_var_lifeEXP) <- 2000:2020

#Nrm pred vars
pred_var_hdi <- stack('data/pred vars/UPDATED_predvars/norm/pred_var_hdi_nrm.grd')
pred_var_NLight <- stack('data/pred vars/UPDATED_predvars/norm/pop_dens_worldpop_scaled_2000_2020.tif')
names(pred_var_NLight) <- 2000:2020
pred_var_fert_ext <- stack('data/pred vars/UPDATED_predvars/norm/pred_var_fert_ext_nrm.grd')
pred_var_lifeEXP <- stack('data/pred vars/UPDATED_predvars/norm/pred_var_lifeEXP_nrm.grd')

inmc_rstr <- stack('data/Upper_lower_incomes/modelgroups.tif')
inmc_vls <- inmc_rstr %>% values() %>% as.data.frame()

# mdls_incm <- readRDS('Models/LM/010921_incm/LM_010921_incm_all')

LM_rst_lst <- list()
for (i in 2000:2019){
  nm <- paste0('X',i)
  mn_dta_yr <- main_data[[nm]] %>% values()
  vls_rst_hdi <- pred_var_hdi[[nm]] %>% values()
  vls_rst_NLight <- pred_var_NLight[[nm]] %>% values()
  vls_fert <- pred_var_fert_ext[[nm]] %>% values()
  
  pred_df_yr <- data.frame(hdi = vls_rst_hdi, NLight = vls_rst_NLight, fert = vls_fert, inmc = inmc_vls$modelgroups)
  
  for (j in 1:4){
    nm_j <- paste0('LM_pred_',j)
    lm_mdl <- readRDS(paste0('Models/LM/101221/LM_101221_brths_',j))
    pred_df_yr[[nm_j]] <- stats::predict.lm(lm_mdl,pred_df_yr[,1:3])
  }

  lpl_lst <-as.list(as.data.frame(t( pred_df_yr[,4:8])))
  LM_pred_incm <- lapply(lpl_lst, FUN = function(x){if (x[[1]] %>% is.na()){return(NA)} else if (x[[1]] == 1) {return(x[[2]])} else if (x[[1]] == 2) {return(x[[3]])} else if (x[[1]] == 3) {return(x[[4]])} 
    else if (x[[1]] == 4) {return(x[[5]])} })
  
  LM_pred_incm <- LM_pred_incm %>% unlist()
  names(LM_pred_incm) <- NULL
  
  
  
  defaultSummary(data.frame(pred=LM_pred_incm,obs=mn_dta_yr), lev = NULL, model = NULL)
  
  tmp_rst <- main_data[[nm]]
  values(tmp_rst) <- LM_pred_incm
  tmp2_rst <-  main_data[[nm]]
  tmp2_rst[!is.na(tmp_rst)] <- tmp_rst[!is.na(tmp_rst)]
  
  LM_rst_lst <- c(LM_rst_lst,tmp2_rst)
  
  writeRaster(tmp2_rst,paste0('Outputs/LM/Births/Initial_apply/101221/LM_brths_init_101221_',i,'.tif'))
  remove(lpl_lst)
}



LM_rst_lst <- LM_rst_lst %>% stack()
writeRaster(LM_rst_lst,'Outputs/LM/Dths/initial apply/101221/LM_dths_init_101221_ALL.tif', overwrite=T)


