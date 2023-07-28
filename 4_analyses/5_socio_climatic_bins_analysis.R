# This script performs the analysis based on socio-climatic bins (Figure 6)
# The script 1) first calculates zonal sums of urban and rural net-migration in each bin, to
# 2) calulate the impact of u/r net-migration has on u/r population change (growth/decline) in each bin, and
# plots the results as 3) maps and 4) heatmaps

rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
.libPaths("/Users/nivav1/Documents/R/")

## Open packages
library(terra)
library(dplyr)
library(stringr)
library(tidyr)
library(sf)
library(tibble)
library(tmap)

# Load data
# Socio-climatic bins
soc_arid_bins <- rast('DATA/pop_soc_arid_bins_mean_2022-03-04.tif') # change date

# pop 2000-2020
pop <- terra::rast("DATA/r_worldpopHarmonised.tif")
urbanPop <- terra::rast('DATA/popUrban.tif')
ruralPop <- terra::rast('DATA/popRural.tif')

# net-migration (yearly) 2001-2020
netMgr <- terra::rast("DATA/netMgr_2001_2020_v3.tif")
netMgrUrban <- terra::rast("DATA/netMigrUrban.tif")
netMgrRural <- terra::rast("DATA/netMigrRural.tif")


#### 1) Calculate zonal sums of urban and rural migration in socio-climatic bins ####

myFun_mgrZonalStats <- function(r_zone, r_popTotal, r_urbanPop, r_ruralPop, r_mgrTotal, r_mgrUrban, r_mgrRural) {
  temp <- unique(values(r_zone[[1]])) %>% sort() %>% as_tibble %>% drop_na() %>% rename("climBin" = "value") #%>% select(cntry_raster_masked) %>% rename("country_id" = "cntry_raster_masked")
  
  pop_temp <- terra::zonal(r_popTotal, r_zone, fun = sum, na.rm = T) %>% rename_at(-1, ~ gsub("^.*?Pop","totPop_", .x),"_")
  pop_urban <- terra::zonal(r_urbanPop, r_zone, fun = sum, na.rm = T) %>% rename_at(-1, ~gsub("^.*?Pop","UrbanPop_", .x),"_")
  pop_rural <- terra::zonal(r_ruralPop, r_zone, fun = sum, na.rm = T) %>% rename_at(-1, ~ gsub("^.*?Pop","RuralPop_", .x),"_")
  
  net_migr <- terra::zonal(r_mgrTotal, r_zone, fun = sum, na.rm = T)  %>% rename_at(-1, ~ gsub("^.*?netMgr","TotMgr_", .x),"_")
  net_urban <- terra::zonal(r_mgrUrban, r_zone, fun = sum, na.rm =T) %>% rename_at(-1, ~ gsub("^.*?netMgr", "UrbanMgr_", .x), "_")
  net_rural <- terra::zonal(r_mgrRural, r_zone, fun = sum, na.rm =T) %>% rename_at(-1, ~ gsub("^.*?netMgr", "RuralMgr_", .x), "_")
  
  temp <- bind_cols(temp, pop_temp[-1], pop_urban[-1],pop_rural[-1], net_migr[-1], net_urban[-1], net_rural[-1])
  
  zonalStatsMgr <- temp %>% 
    pivot_longer(!climBin, names_to = "variable", values_to = "value") %>% 
    mutate(year = variable) %>% 
    mutate(split = str_split(year, "_")) %>% 
    mutate(variable = sapply(split, "[[", 1),
           year = sapply(split, "[[", 2)) %>% 
    dplyr::select(-split) %>% 
    mutate(year = as.numeric(year))
  
  return(zonalStatsMgr)
  
}

r_zone <- soc_arid_bins
r_popTotal <- pop
r_urbanPop <- urbanPop
r_ruralPop <- ruralPop
r_mgrTotal <- netMgr
r_mgrUrban <- netMgrUrban
r_mgrRural <- netMgrRural

zonalStats_socAridBins <- myFun_mgrZonalStats(soc_arid_bins, pop, urbanPop, ruralPop, netMgr, netMgrUrban, netMgrRural)
readr::write_csv(zonalStats_socAridBins, paste0('results/zonalStats_socAridBins_', Sys.Date(), '.csv'))

#### 2) Impact of migration on population change in each bin ####

# Calculate how much population has changed in each bin and how much migration has affected to the change

# Calculate net-migration to migration rate in each bin
# Calculate population change without migration in each bin
# Calculate whether natural growth or migration impacts more on pop change

# Function to calculate impact
myFunMgrImpactPop <- function(r_adm, nameOutput) {  
  pop_change <- r_adm %>%
    pivot_wider(names_from = c(variable,year), values_from = value) %>% 
    summarise(climBin = unique(climBin),
              totPop_change = totPop_2020 - totPop_2001,
              urbanPop_change = UrbanPop_2020 - UrbanPop_2001,
              ruralPop_change = RuralPop_2020 - RuralPop_2001)
  
  tot_pop <- filter(r_adm, variable == 'totPop', year== 2020)$value
  tot_upop <-  filter(r_adm, variable == 'UrbanPop', year== 2020)$value
  tot_rpop <- filter(r_adm, variable == 'RuralPop', year== 2020)$value
  
  accum_mgr <- r_adm %>%
    filter(., variable %in% c("TotMgr", "UrbanMgr", "RuralMgr")) %>% 
    pivot_wider(names_from = variable, values_from = value) %>% 
    group_by(climBin) %>% 
    summarise(accum_TotMgr = sum(TotMgr, na.rm = T),
              accum_UrbanMgr = sum(UrbanMgr, na.rm = T),
              accum_RuralMgr = sum(RuralMgr, na.rm = T))
  
  accum_mgr <- left_join(accum_mgr, 
                         cbind(tibble(climBin = accum_mgr$climBin,
                                      shr_TotMgr = accum_mgr$accum_TotMgr / tot_pop * 1000, 
                                      shr_UrbMgr = accum_mgr$accum_UrbanMgr / tot_upop * 1000, #urban net-migration per 1000 urban pop
                                      shr_RurMgr = accum_mgr$accum_RuralMgr / tot_rpop *1000))) %>% #rural net-migration per 1000 rural pop
    as_tibble()
  
  
  pop_change_mgr <- pop_change %>% 
    left_join(accum_mgr) %>% 
    # calculate population change without migration
    mutate(TotpopChng_woMgr = totPop_change - accum_TotMgr,
           UrbanpopChng_woMgr = urbanPop_change - accum_UrbanMgr,
           RuralpopChng_woMgr = ruralPop_change - accum_RuralMgr) %>% 
    # calculate whether natural growth or migration impacts more on pop change
    mutate(roleTotPopMgr = 1-TotpopChng_woMgr/totPop_change,
           roleUrbanPopMgr = 1-UrbanpopChng_woMgr/urbanPop_change,
           roleUrbanPopMgr = 1-RuralpopChng_woMgr/ruralPop_change) %>% 
    
    # check whether 
    
    # check whether 
    # migration increases pop growth:1 (3)
    # migration slows population growth: -2 (2)
    # migration turns pop growth to declined pop: -3 (1)
    # migration accelerates pop decline: -1 (-3)
    # migration slows pop decline: 2 (-2)
    # migration turns pop decline to growth: 3 (-1)
    
  mutate(popMgr_classif_urban = if_else((UrbanpopChng_woMgr > 0 & accum_UrbanMgr > 0), 1,
                                        if_else((urbanPop_change > 0 & accum_UrbanMgr < 0), -2,
                                                if_else((UrbanpopChng_woMgr > 0 & urbanPop_change < 0), -3,
                                                        if_else((UrbanpopChng_woMgr < 0 & accum_UrbanMgr < 0), -1,
                                                                if_else((urbanPop_change < 0 & accum_UrbanMgr > 0), 2,
                                                                        if_else((UrbanpopChng_woMgr < 0 & urbanPop_change > 0), 3,#0)))))))
                                                                                0
                                                                        ))))))) %>% 
    
    mutate(popMgr_classif_rural = if_else((RuralpopChng_woMgr > 0 & accum_RuralMgr > 0), 1,
                                          if_else((ruralPop_change > 0 & accum_RuralMgr < 0), -2,
                                                  if_else((RuralpopChng_woMgr > 0 & ruralPop_change < 0), -3,
                                                          if_else((RuralpopChng_woMgr < 0 & accum_RuralMgr < 0), -1,
                                                                  if_else((ruralPop_change < 0 & accum_RuralMgr > 0), 2,
                                                                          if_else((RuralpopChng_woMgr < 0 & ruralPop_change > 0), 3,#0)))))))
                                                                                  0
                                                                          ))))))) %>% 
    
    mutate(popMgr_classif = if_else((TotpopChng_woMgr > 0 & accum_TotMgr > 0), 1,
                                    if_else((totPop_change > 0 & accum_TotMgr < 0), -2,
                                            if_else((TotpopChng_woMgr > 0 & totPop_change < 0), -3,
                                                    if_else((TotpopChng_woMgr < 0 & accum_TotMgr < 0), -1,
                                                            if_else((totPop_change < 0 & accum_TotMgr > 0), 2,
                                                                    if_else((TotpopChng_woMgr < 0 & totPop_change > 0), 3,#0)))))))
                                                                            0
                                                                    ))))))) 
  
  temp <- pop_change_mgr #%>% dplyr::select(climBin, contains('accum'), popMgr_classif_urban, popMgr_classif_rural, popMgr_classif)
  readr::write_csv(temp, paste0('results/', nameOutput, '_impact_' ,Sys.Date(), '.csv'))
  return(temp)
  
}

# Upload zonal stats 
migr_socAridBins <- readr::read_csv('results/zonalStats_socAridBins_2023-02-06.csv') # change date when needed

socAridBin_impact <- myFunMgrImpactPop(migr_socAridBins, 'socAridBin')

#### 3) Plot results on maps ####

# function transfer impact dataframes to raster
myFun_create_rasterMap <- function(r_index, r_mgr_df, variableName, mybreaks,colorpal, nameLabels, titleLabel,
                                   tocrs = NA){ #
  
  selectVariable <- r_mgr_df %>% dplyr::select(all_of(variableName)) %>% as.matrix()
  
  r_pop_change_mgr <- classify(r_index,
                               cbind(r_mgr_df[,1], #climate bins
                                     selectVariable)) #variable to plot
  
  # project to another crs
  if (!is.na(tocrs)){
    r_pop_change_mgr <- project(r_pop_change_mgr, tocrs, mask = TRUE)
  }
  
  # create tmap object
  index_map <- tm_shape(r_pop_change_mgr) +
    tm_raster(style = "fixed", # draw gradient instead of classified
              breaks = mybreaks ,
              palette = colorpal,
              labels = nameLabels,
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


# Create country boarders from cntry raster
sf_gadm0 <- terra::as.polygons(rast('DATA/gadm_lev0_5arcmin.tif')) %>% 
  sf::st_as_sf() %>% # to sf
  rmapshaper::ms_simplify(.,keep=0.1,keep_shapes = T) # simplify

#### Impact of migration on pop change ####
# Load data
socAridBin_rast <- terra::rast('DATA/pop_soc_arid_bins_mean_2023-02-06.tif') # change date when needed
socAridBin_impact <- readr::read_csv('results/socAridBin_impact_2023-02-06.csv')

mycol <- c("#c92d47","#d2748a","#ffc4d2","grey50","#b3d4ff","#718ebd","#4166b0")
mylabels <- c('migration turns pop growth to declined pop',
              'migration decreases pop growth (slows population growth)',
              'migration accelerates pop decline',
              'no data',
              'migration increases pop growth',
              'migration slows pop decline',
              'migration turns pop decline to growth')

map_socAridBin_mgr_class <- myFun_create_rasterMap(socAridBin_rast,
                                                   socAridBin_impact,
                                                   'popMgr_classif',
                                                   c(-3.5,-2.5,-1.5,-0.5,.5,1.5,2.5,3.5),
                                                   mylabels,
                                                   colorpal = mycol,
                                                   "Impact of migration on population in each combination bin",
                                                   tocrs = "+proj=robin +over")

map_socAridBin_mgr_class_urban <- myFun_create_rasterMap(socAridBin_rast,
                                                         socAridBin_impact,
                                                         'popMgr_classif_urban',
                                                         c(-3.5,-2.5,-1.5,-0.5,.5,1.5,2.5,3.5),
                                                         mylabels,
                                                         colorpal = mycol,
                                                         "Impact of migration on urban population in each combination bin",
                                                         tocrs = "+proj=robin +over")

map_socAridBin_mgr_class_rural <- myFun_create_rasterMap(socAridBin_rast,
                                                         socAridBin_impact,
                                                         'popMgr_classif_rural',
                                                         c(-3.5,-2.5,-1.5,-0.5,.5,1.5,2.5,3.5),
                                                         mylabels,
                                                         colorpal = mycol,
                                                         "Impact of migration on rural population in each combination bin",
                                                         tocrs = "+proj=robin +over")

socAridBin_rasterMaps <- tmap_arrange(map_socAridBin_mgr_class, map_socAridBin_mgr_class_urban , map_socAridBin_mgr_class_rural, nrow = 3, ncol = 1)
tmap_save(socAridBin_rasterMaps, filename = paste0('results/plots/maps_socAridBin_mgr',Sys.Date(),'.pdf'),width = 160, height=380, units='mm')

#### Migration to pop ratio (per 1000 pop) ####

map_socAridBin_mgr_shr_pop <- myFun_create_rasterMap(socAridBin_rast,
                                                     socAridBin_impact,
                                                     'shr_TotMgr',
                                                     seq(-150,150,10),
                                                     as.character(seq(-150,150,10)),
                                                     colorpal = scico::scico(9,palette = 'vik', direction = -1),
                                                     "Net-migration per 1000 pop in each combination bin",
                                                     tocrs = "+proj=robin +over")

map_socAridBin_mgr_shr_pop_urban <- myFun_create_rasterMap(socAridBin_rast,
                                                           socAridBin_impact,
                                                           'shr_UrbMgr',
                                                           seq(-150,150,10),
                                                           as.character(seq(-150,150,10)),
                                                           colorpal = scico::scico(9,palette = 'vik', direction = -1),
                                                           "Net-migration per 1000 urban pop in each combination bin",
                                                           tocrs = "+proj=robin +over")

map_socAridBin_mgr_shr_pop_rural <- myFun_create_rasterMap(socAridBin_rast,
                                                           socAridBin_impact,
                                                           'shr_RurMgr',
                                                           seq(-150,150,10),
                                                           as.character(seq(-150,150,10)),
                                                           colorpal = scico::scico(9,palette = 'vik', direction = -1),
                                                           "Net-migration per 1000 rural pop in each combination bin",
                                                           tocrs = "+proj=robin +over")


socAridBin_rasterMaps_mgrShr <- tmap_arrange(map_socAridBin_mgr_shr_pop_urban, map_socAridBin_mgr_shr_pop_rural, nrow = 2, ncol = 1)
tmap_save(socAridBin_rasterMaps_mgrShr, filename = paste0('results/plots/maps_socAridBin_mgrShr',Sys.Date(),'.pdf'),width = 160, height=380, units='mm')









#### 4) Plot results on heatmaps ####

# load data
socAridBin_impact <- readr::read_csv('results/socAridBin_impact_2023-02-06.csv')

# Create a function for heatmaps
myFun_create_heatMap <- function(r_mgr_df, variableNames, colorpal,colNames, mybreaks, nameOutput) {
  
  #col <- colorRampPalette(colorpal)
  
  #First select variable to matrix
  temp <- r_mgr_df %>% dplyr::select(all_of(variableNames)) %>% as.matrix() 
  # mybreaks <- seq(-150,100,10)
  # temp <- cut(temp[,2], mybreaks, include.lowest = T)
  # # Any values over 150 -> convert to 1000
  # temp[temp >= 150] <- 150
  # temp[temp <= -150] <- -150
  # reshape vector (length 100) to 10 by 10 matrix
  temp <- pracma::Reshape(temp[,2], 10, 10)
  temp <- apply(t(temp), 2, rev)
  # # change row and col names
  # colnames(temp) <- paste0(colNames[1],1:10)
  # rownames(temp) <- paste0(colNames[2],1:10)
  
  temp <- raster(temp)
  
  # data <- matrix(as.numeric(data_in$shr_UrbMgr), 10, 10)
  # data <- apply(t(data),2,rev) # rotate matrix, now reads: HDI bins follow y axis (1 to 91), aridity bins follow x axis (1 to 10)
  # data <- raster::raster(data)
  # 
  # colnames(temp) <- paste0("MAT",1:10)
  # rownames(temp) <- paste0("MAP",1:10)
  
  myTheme=rasterVis::rasterTheme(region=mycol) 
  #mybreaks = seq(-150,150,10)
  mycolorKey <- list(at=mybreaks, ## where the colors change
                     labels=list(
                       at=mybreaks ## where to print labels
                     ))
  
  pdf(paste0("results/plots/heatmap_", nameOutput, '_', variableNames[2],"_", Sys.Date(), '.pdf'), height=10, width=10)
  # plot as a heatmap, no dendogram
  #myHeatmap <- lattice::levelplot(temp, col.regions = col, at = mybreaks)
  myHeatmap <- rasterVis::levelplot(temp, margin = list(FUN = 'mean'),  par.settings=myTheme, colorkey = mycolorKey, axis = T)
  print(myHeatmap)
  dev.off()
  return(myHeatmap)
}

#### Impact of migration on pop change ####

mycol <- c("#c92d47","#d2748a","#ffc4d2","grey50","#b3d4ff","#718ebd","#4166b0")

heatmap_socAridBin_mgr_class_u <- myFun_create_heatMap( r_mgr_df= socAridBin_impact, 
                                                        variableNames = c('climBin','popMgr_classif_urban'),
                                                        colorpal = mycol,
                                                        #colNames = c("hdi","arid"),
                                                        mybreaks = c(-3.5,-2.5,-1.5,-0.5,.5,1.5,2.5,3.5), #mybreaks = c(-3,-2,-1,0,1,2,3),
                                                        nameOutput = 'socAridBin')

heatmap_socAridBin_mgr_class_r <- myFun_create_heatMap( r_mgr_df = socAridBin_impact, 
                                                        variableNames= c('climBin','popMgr_classif_rural'),
                                                        colorpal = mycol,
                                                        #colNames = c("hdi","arid"),
                                                        mybreaks = c(-3.5,-2.5,-1.5,-0.5,.5,1.5,2.5,3.5),
                                                        nameOutput = 'socAridBin')

#### Migration to population ratio (per 1000 pop) ####
mycol <- scico::scico(9,palette = 'vik', direction = -1)

heatmap_socAridBin_mgrshr_pop_u <- myFun_create_heatMap(socAridBin_impact, 
                                                        c('climBin','shr_UrbMgr'),
                                                        colorpal = mycol,
                                                        colNames = c("hdi","arid"),
                                                        mybreaks = seq(-150,150,10),
                                                        nameOutput = 'socAridBin') #scico(9,palette = 'roma'))

heatmap_socAridBin_mgrshr_pop_r <- myFun_create_heatMap( socAridBin_impact, 
                                                         c('climBin','shr_RurMgr'),
                                                         colorpal = mycol,
                                                         colNames = c("hdi","arid"),
                                                         mybreaks = seq(-150,150,10),
                                                         nameOutput = 'socAridBin')







