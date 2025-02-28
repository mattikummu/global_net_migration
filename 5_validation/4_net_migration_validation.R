# This script performs the validation of net-migration data

rm(list = ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


## Open packages
library(terra)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(sf)
library(readxl)
library(ggplot2)
library(exactextractr)
#devtools::install_github('smin95/smplot2', force = TRUE)
library(smplot2)

#### 1) national level validation ####
# Upload UN estimates for net-migration stock per each country
UN_data_raw <- readxl::read_excel('UN_NM_estimates.xlsx')
glimpse(UN_data_raw)

# select years between 1999-2021
UN_estimates <- UN_data_raw %>% dplyr::filter(., !is.na(ISO3)) %>% 
  dplyr::filter(., Year %in% c(2001:2020)) %>% 
  rename(., UN_net_migration = 'net-migr_thousands') %>% 
  rename(., UN_net_migration_rate_per_thousands = 'net-migr_rate_per_thousand') %>% 
  mutate(UN_net_migration = UN_net_migration*1000) %>% 
  distinct()

# Upload cntry codes
cntry_codes <- readr::read_csv('DATA/countries_codes_and_coordinates.csv')
region_codes <-  readr::read_csv('DATA/cntry_ids.csv')
region_name <- readr::read_csv('DATA/countryGroupsNames.csv')

cntry_codes <- left_join(cntry_codes,region_codes, by = c('iso3' = 'country_code'))
cntry_codes <- left_join(cntry_codes, region_name, by = c('region_id' = 'regionID')) %>% dplyr::select(-country_name, -cntry_id)

# join cntry_codes to UN_estimates
UN_estimates <- left_join(cntry_codes,UN_estimates, by = c('iso3'='ISO3', 'iso2'='ISO2'))
#select(-Country,-Cntry, -cntry_code,-ISO2)

# Calculate zonal stats for net-migration
countries <- terra::rast('DATA/gadm_lev0_5arcmin.tif') #gadm IDs
net_mgr_rast <- terra::rast('DATA/netMgr_2001_2020_v3.tif')
population <- rast('DATA/r_worldpopHarmonised.tif')

NM_estimates <- terra::zonal(net_mgr_rast, countries, fun = sum, na.rm =T) 
NM_estimates <- NM_estimates %>% rename(., gadm_code = cntry_code) %>%  tidyr::pivot_longer(NM_estimates, cols = !gadm_code, names_to = 'Year', values_to = 'modelled_net_migration')
NM_estimates <- mutate(NM_estimates, Year = stringr::str_remove(Year, pattern = 'netMgr')) %>% mutate(., Year = as.double(Year))
pop_estim <- terra::zonal(population, countries, fun = sum, na.rm =T)
pop_estim <- rename(pop_estim, gadm_code = cntry_code) %>%  
  tidyr::pivot_longer(., cols = !gadm_code, names_to = 'Year', values_to = 'population') %>% 
  mutate(., Year = stringr::str_remove(Year, pattern = 'cntryPop')) %>% mutate(., Year = as.double(Year))

# join estimates
estimates_compare <- left_join(UN_estimates, NM_estimates, by = c('GADM_code' = 'gadm_code', 'Year' = 'Year')) %>% 
  distinct() 
estimates_compare <- na.omit(estimates_compare) %>% arrange(., modelled_net_migration) %>% left_join(., pop_estim, by = c('GADM_code' = 'gadm_code', 'Year' = 'Year'))
head(estimates_compare)                                         
tail(estimates_compare)
estimates_compare <- mutate(estimates_compare, model_nm_ratio = modelled_net_migration / population *1000)

# Scatter plot with correlation coefficient

# check histogram
ggplot(estimates_compare, aes(x=log(modelled_net_migration))) + geom_histogram()
# Slice top three values (outliers)
temp <- estimates_compare %>% arrange(modelled_net_migration) %>% slice(., -c(1:3))
# check country n
length(unique(temp$Country))

# plot
NM_validation <- ggplot(data = filter(temp,
                                      Year %in% c(2001, 2005, 2010, 2015, 2020)),
                        mapping = aes(x = UN_net_migration, y = modelled_net_migration)) + 
  geom_point(size = 1) +
  #geom_smooth() +
  facet_wrap(facets = vars(Year), ncol= 2, nrow=3) +
  coord_fixed(1)+
  sm_corr_theme() + #borders = F
  sm_statCorr(line_type = 'solid')

NM_validation_ratio <- ggplot(data = filter(temp,
                                            Year %in% c(2001, 2005, 2010, 2015, 2020)),
                              mapping = aes(x = UN_net_migration_rate_per_thousands, y = model_nm_ratio)) + 
  geom_point(size = 1) +
  #geom_smooth() +
  facet_wrap(facets = vars(Year), ncol= 2, nrow=3) +
  coord_fixed(1)+
  sm_corr_theme() + #borders = F
  sm_statCorr(line_type = 'solid')

ggsave(paste0('results/plots/NM_validation_', Sys.Date(), '.pdf'), NM_validation, dpi = 320, width = 20, height = 20, units = "cm")
ggsave(paste0('results/plots/NM_validation_ratio_', Sys.Date(), '.pdf'), NM_validation_ratio, dpi = 320, width = 20, height = 20, units = "cm")


####  2) Sub-national validation ####
population <- rast('DATA/r_worldpopHarmonised.tif')
net_mgr_rast <- terra::rast('DATA/netMgr_2001_2020_v3.tif')

#### South Korea ####
# Extract sub-national net-migration from gridded data

# read in a shp file with provinces in korea
korea_shp <- st_read("oecd_tl3.gpkg") %>%
  #st_drop_geometry() %>% 
  #as_tibble() %>% 
  filter(iso3 == 'KOR')

# extract names for provinces
korea_id <- korea_shp %>% st_drop_geometry() %>% as_tibble() #%>% tibble::rowid_to_column(., "ID")

# extract net-migration estimates for each province (zonal sum over polygon)
net_mgr_estim_south_korea <- exact_extract(net_mgr_rast, korea_shp, 'sum') %>% as_tibble %>% 
  bind_cols(korea_id) %>% 
  dplyr::select(-tl3_id, -tl2_id, -tl1_id, -iso3, -name_en, -name_fr, -continenta, -reg_name_p) %>% 
  tidyr::pivot_longer(!name_or, names_to = "Year", values_to = "net_mgr") %>% 
  mutate(Year = str_remove(Year, pattern = 'sum.netMgr')) %>% 
  mutate(Year = as.double(Year)) %>% 
  arrange(name_or) %>% 
  filter(!name_or == 'Sejong') # filter Sejong because of admin reform -> mismatch between reported and estimated values

# extract population for each province
pop_south_korea <- exact_extract(population, korea_shp, 'sum') %>% as_tibble() %>% 
  bind_cols(korea_id)  %>% 
  dplyr::select(-tl3_id, -tl2_id, -tl1_id, -iso3, -name_en, -name_fr, -continenta, -reg_name_p) %>% 
  tidyr::pivot_longer(!name_or, names_to = "Year", values_to = "population") %>% 
  mutate(Year = str_remove(Year, pattern = 'sum.cntryPop')) %>% 
  mutate(Year = as.double(Year)) %>% 
  arrange(name_or) %>% 
  filter(!Year == 2000) %>% 
  filter(!name_or == 'Sejong')

# Calculate net-migration to population ratio using gridded population data
net_mgr_estim_south_korea <- mutate(net_mgr_estim_south_korea, estim_ratio = net_mgr_estim_south_korea$net_mgr / pop_south_korea$population*1000) #%>% rename(estim_net_mgr = net_mgr)

# read in observed net-migration counts from south korea
net_mgr_obs_south_korea <- readxl::read_excel('southKorea_net_mgr_00-20.xlsx') %>% 
  filter(Year %in% c(2001:2020)) %>% 
  filter(Variable == 'Netmigration(Administrative reports) (Person)') %>% 
  dplyr::select(-Variable) %>% 
  dplyr::select(-'Whole Country') %>% 
  mutate(Sejong = as.double(Sejong)) %>% 
  tidyr::pivot_longer(!Year, names_to = "name_or", values_to = 'net_mgr') %>% 
  mutate(net_mgr = ifelse(net_mgr == 0, NA, net_mgr)) %>% 
  arrange(., name_or) %>% 
  mutate(Year = as.double(Year)) %>% 
  filter(!name_or == 'Sejong')

# Calculate net-migration to population ratio using gridded population data
net_mgr_obs_south_korea <- mutate(net_mgr_obs_south_korea, obs_ratio = net_mgr_obs_south_korea$net_mgr / pop_south_korea$population*1000) #%>% rename(obs_net_mgr = net_mgr)

# check that names match
identical(net_mgr_estim_south_korea$name_or, net_mgr_obs_south_korea$name_or)

# calculate 5 and 10 year accumulated migration / average pop

x_year_accum_mgr_ratio <- function(migration_estim, years, range, pop_estim, type){
  # 5 or ten year accumulative sum
  yr_accum_estim <- migration_estim %>% 
    group_by(name_or) %>% 
    summarise(accum_sum = zoo::rollsum(net_mgr, years, align = 'left')) %>%
    mutate(id = row_number()) %>% 
    filter(id %in% seq(1, 20, by=years)) %>% # filter every Xth year to get accumulated sum
    mutate(years = range) %>% 
    dplyr::select(-id) %>% 
    rename(Year = years)
  
  #5 or 10 year average
  yr_avg_estim <- pop_estim %>% 
    group_by(name_or) %>% 
    summarise(avg_pop = zoo::rollmean(population, years, align = 'left')) %>%
    mutate(id = row_number()) %>% 
    filter(id %in% seq(1, 20, by=years)) %>% # filter every sixth year to get accumulated sum
    mutate(years = range) %>% 
    dplyr::select(-id) %>% 
    rename(Year = years)
  
  temp <- left_join(yr_accum_estim, yr_avg_estim)
  
  # Calculate ratio
  yr_ratio <- temp %>% mutate(ratio = accum_sum / avg_pop * 1000) 
  #new_name <- paste0(type, '_ratio')
  #dplyr::rename_at(yr_ratio, new_name[1] = 'ratio')
  yr_ratio <- yr_ratio %>% rename_with(.cols = ratio, function(x){paste0(type,"_", x)})
  
  return(yr_ratio) 
}


five_yr_range <- c('2001-2005', '2006-2010', '2011-2015', '2016-2020')
ten_yr_range <- c('2001-2010', '2011-2020')

# calculate ratios (per 1000 pop) from estimated net-migration
five_yr_accum_nm_estim_ratio_sk <- x_year_accum_mgr_ratio(net_mgr_estim_south_korea, years=5, 
                                                          range=five_yr_range, pop_south_korea, type = 'estim') 

ten_yr_accum_nm_estim_ratio_sk <- x_year_accum_mgr_ratio(net_mgr_estim_south_korea,  10, 
                                                         ten_yr_range, pop_south_korea, 'estim') 
# calculate ratios from observed net-migration
five_yr_accum_nm_obs_ratio_sk <- x_year_accum_mgr_ratio(net_mgr_obs_south_korea,  5, 
                                                        five_yr_range, pop_south_korea, 'obs') 
ten_yr_accum_nm_obs_ratio_sk <- x_year_accum_mgr_ratio(net_mgr_obs_south_korea,  10, 
                                                       ten_yr_range, pop_south_korea, 'obs')
### PLOT

# Absolute
southKorea_NM_abs_validation <- ggplot(data = left_join(dplyr::select(net_mgr_obs_south_korea, name_or, Year, net_mgr) %>% 
                                                          rename(obs_net_mgr = net_mgr),
                                                        dplyr::select(net_mgr_estim_south_korea, name_or, Year, net_mgr) %>% 
                                                          rename(estim_net_mgr = net_mgr)) %>% 
                                         filter(Year %in% c(2001, 2005, 2010, 2015, 2019)),
                                       mapping = aes(x = obs_net_mgr, y = estim_net_mgr)) + 
  geom_point(size = 1) +
  #geom_smooth() +
  facet_wrap(facets = vars(Year)) + #, ncol= 5, nrow=3
  coord_fixed(1)+
  smplot2::sm_corr_theme() + #borders = F
  smplot2::sm_statCorr(corr_method = 'pearson') #line_type = 'solid', 

ggsave('results/plots/NM_abs_validation_south_korea.pdf', southKorea_NM_abs_validation)

# WITHOUT Gyeonggi-do and Seoul (very high and low observations/estimates:
# southKorea_NM_abs_validation_wooutlier <- ggplot(data = left_join(select(net_mgr_obs_south_korea, name_or, Year, net_mgr) %>%
#                                                                     rename(obs_net_mgr = net_mgr),
#                                                                   select(net_mgr_estim_south_korea, name_or, Year, net_mgr) %>%
#                                                                     rename(estim_net_mgr = net_mgr)) %>% 
#                                                    filter(Year %in% c(2001, 2005, 2010, 2015, 2019)) %>% 
#                                                    filter(!name_or == 'Gyeonggi-do') %>% 
#                                                    filter(!name_or == 'Seoul'),
#                                                  mapping = aes(x = obs_net_mgr, y = estim_net_mgr)) + 
#   geom_point(size = 1) +
#   #geom_smooth() +
#   facet_wrap(facets = vars(Year)) + #, ncol= 5, nrow=3
#   coord_fixed(1)+
#   smplot2::sm_corr_theme() + #borders = F
#   smplot2::sm_statCorr(corr_method = 'pearson')
# 
# ggsave('results/plots/NM_abs_validation_south_korea_no_outliers.pdf', southKorea_NM_abs_validation_wooutlier)




southKorea_NM_ratio_validation <- ggplot(data = left_join(dplyr::select(net_mgr_obs_south_korea, name_or, Year, obs_ratio),
                                                          #rename(obs_net_mgr = net_mgr),
                                                          dplyr::select(net_mgr_estim_south_korea, name_or, Year, estim_ratio)) %>%
                                           #rename(estim_net_mgr = net_mgr)) %>% 
                                           filter(Year %in% c(2001, 2005, 2010, 2015, 2019)),
                                         mapping = aes(x = obs_ratio, y = estim_ratio)) + 
  geom_point(size = 1) +
  #geom_smooth() +
  facet_wrap(facets = vars(Year)) + #, ncol= 5, nrow=3
  coord_fixed(1)+
  smplot2::sm_corr_theme() + #borders = F
  smplot2::sm_statCorr(corr_method = 'pearson') #line_type = 'solid', 

ggsave('results/plots/NM_ratio_validation_south_korea.pdf', southKorea_NM_ratio_validation)



southKorea_NM_rel_validation_5yr <- ggplot(data = left_join(dplyr::select(five_yr_accum_nm_obs_ratio_sk, -avg_pop, -accum_sum), 
                                                            dplyr::select(five_yr_accum_nm_estim_ratio_sk, -avg_pop, -accum_sum)) , #%>% 
                                           # ungroup() %>% 
                                           # arrange(obs_ratio) %>% 
                                           # slice(., -(1:3)) %>% 
                                           # arrange(desc(obs_ratio)),
                                           mapping = aes(x = obs_ratio, y = estim_ratio)) +
  geom_point(size = 1) +
  #geom_smooth() +
  facet_wrap(facets = vars(Year)) + #, ncol= 5, nrow=3
  coord_fixed(1)+
  smplot2::sm_corr_theme() + #borders = F
  smplot2::sm_statCorr(corr_method = 'pearson') #line_type = 'solid',

southKorea_NM_rel_validation_10yr <- ggplot(data = left_join(dplyr::select(ten_yr_accum_nm_obs_ratio_sk, -avg_pop, -accum_sum), 
                                                             dplyr::select(ten_yr_accum_nm_estim_ratio_sk, -avg_pop, -accum_sum)), # %>% 
                                            # arrange(obs_ratio) %>% 
                                            # slice_head(n=2) %>% 
                                            # slice_tail(n=2),
                                            mapping = aes(x = obs_ratio, y = estim_ratio)) +
  geom_point(size = 1) +
  #geom_smooth() +
  facet_wrap(facets = vars(Year)) + #, ncol= 5, nrow=3
  coord_fixed(1)+
  smplot2::sm_corr_theme() + #borders = F
  smplot2::sm_statCorr(corr_method = 'pearson')


ggsave('results/plots/NM_ratio_5yr_validation_south_korea.pdf', southKorea_NM_rel_validation_5yr)
ggsave('results/plots/NM_ratio_10yr_validation_south_korea.pdf', southKorea_NM_rel_validation_10yr)

southKorea_NM_5yr_validation <- ggplot(data = left_join(dplyr::select(five_yr_accum_nm_obs_ratio_sk, -avg_pop, -obs_ratio) %>% rename(accum_sum_obs = accum_sum), 
                                                        dplyr::select(five_yr_accum_nm_estim_ratio_sk, -avg_pop, -estim_ratio) %>% rename(accum_sum_estim = accum_sum)),
                                       mapping = aes(x = accum_sum_obs, y = accum_sum_estim)) +
  geom_point(size = 1) + 
  #geom_smooth() +
  facet_wrap(facets = vars(Year)) + #, ncol= 5, nrow=3
  coord_fixed(1)+
  smplot2::sm_corr_theme() + #borders = F
  smplot2::sm_statCorr(corr_method = 'pearson') #line_type = 'solid',

southKorea_NM_5yr_validation

ggsave('results/plots/NM_abs_5yr_validation_south_korea.pdf', southKorea_NM_5yr_validation)

southKorea_NM_10yr_validation <- ggplot(data = left_join(dplyr::select(ten_yr_accum_nm_obs_ratio_sk, -avg_pop, -obs_ratio) %>% rename(accum_sum_obs = accum_sum), 
                                                         dplyr::select(ten_yr_accum_nm_estim_ratio_sk, -avg_pop, -estim_ratio) %>% rename(accum_sum_estim = accum_sum)),
                                        mapping = aes(x = accum_sum_obs, y = accum_sum_estim)) +
  geom_point(size = 1) +
  #geom_smooth() +
  facet_wrap(facets = vars(Year)) + #, ncol= 5, nrow=3
  coord_fixed(1)+
  smplot2::sm_corr_theme() + #borders = F
  smplot2::sm_statCorr(corr_method = 'pearson') #line_type = 'solid',

southKorea_NM_10yr_validation
ggsave('results/plots/NM_abs_10yr_validation_south_korea.pdf', southKorea_NM_10yr_validation)


#### US ####
# read in US data
net_mgr_obs_usa_in <- read_csv('netmigration_usa/netmigration_usa.csv')
net_mgr_obs_usa <- net_mgr_obs_usa_in %>% dplyr::select(c(stname, name, fips, starts_with("m0ttt"))) %>%  # Estimated Number of Net Migrants in 2000s by age group
  arrange(., fips) %>% 
  dplyr::select(-stname, -name) %>% 
  tidyr::pivot_longer(!fips, names_to = 'age_group', values_to = 'net_mgr_obs') %>% 
  group_by(fips) %>% 
  summarise(net_mgr_obs = sum(net_mgr_obs, na.rm = T)) # sum over all age groups to get a county total net-migration


# read in US shapefile with geoinfo for counties
usa_shp <- st_read("netmigration_usa/tl_2022_us_county/tl_2022_us_county.shp") %>% arrange(., GEOID)
#st_drop_geometry() %>% 
#as_tibble() %>% 
#filter(iso3 == 'USA')

# extract net-migration estimates for each county (zonal sum over polygon)
net_mgr_estim_usa_in <- exact_extract(net_mgr_rast, usa_shp, 'sum') %>% as_tibble %>% tibble::rowid_to_column()
net_mgr_estim_usa <- net_mgr_estim_usa_in %>% 
  mutate(rowid = usa_shp$GEOID) %>% 
  rename(GEOID = rowid) %>% 
  tidyr::pivot_longer(!GEOID, names_to = 'year', values_to = 'net_mgr_estim') %>% 
  mutate(year = str_remove(year, pattern = 'sum.netMgr'))

# observed net-migration counts are decadal, i.e. a cumulative sum over 2000-2010
# calculate a cumulative sum of net-migration from modelled estimate
net_mgr_estim_usa <- net_mgr_estim_usa %>% 
  filter(year %in% c(2001:2010)) %>% 
  group_by(GEOID) %>% 
  summarise(net_mgr_estim = sum(net_mgr_estim, na.rm = T))

net_mgr_obs_usa <- filter(net_mgr_obs_usa, fips %in% net_mgr_estim_usa$GEOID)
net_mgr_estim_usa <- filter(net_mgr_estim_usa, GEOID %in% net_mgr_obs_usa$fips)

# Extract population estimates for each county
pop_usa <- exact_extract(population, usa_shp, 'sum') %>% as_tibble %>% tibble::rowid_to_column() %>% 
  mutate(rowid = usa_shp$GEOID) %>% 
  rename(GEOID = rowid) %>% 
  tidyr::pivot_longer(!GEOID, names_to = 'year', values_to = 'pop_estim') %>% 
  mutate(year = str_remove(year, pattern = 'sum.cntryPop')) %>% 
  filter(year %in% c(2001:2010)) %>% 
  filter(., GEOID %in% net_mgr_estim_usa$GEOID) %>% 
  filter(., GEOID %in% net_mgr_obs_usa$fips) %>% 
  group_by(GEOID) %>% 
  summarise(pop_10yr_avg = mean(pop_estim, na.rm = T))

# Calculate net-migration per population ratios
net_mgr_obs_usa <- net_mgr_obs_usa %>% mutate(net_mgr_obs_ratio = net_mgr_obs / pop_usa$pop_10yr_avg * 1000)
net_mgr_estim_usa <- net_mgr_estim_usa %>% mutate(net_mgr_estim_ratio = net_mgr_estim / pop_usa$pop_10yr_avg * 1000)

# plot 

# Absolute values
temp <- inner_join(net_mgr_obs_usa, net_mgr_estim_usa, by = c('fips' = 'GEOID'))
USA_NM_validation <- ggplot(data = temp,
                            mapping = aes(x = net_mgr_obs, y = net_mgr_estim)) + 
  geom_point(size = 1) +
  #geom_smooth() +
  #facet_wrap(facets = vars(Year), ncol= 2, nrow=3) +
  coord_fixed(1)+
  sm_corr_theme() + #borders = F
  sm_statCorr(corr_method = 'pearson') #line_type = 'solid', 

USA_NM_validation

ggsave('results/plots/NM_validation_usa.pdf', USA_NM_validation)

# Ratio
temp <- inner_join(net_mgr_obs_usa, net_mgr_estim_usa, by = c('fips' = 'GEOID'))
USA_NM_ratio_validation <- ggplot(data = temp,
                                  mapping = aes(x = net_mgr_obs_ratio, y = net_mgr_estim_ratio)) + 
  geom_point(size = 1) +
  #geom_smooth() +
  #facet_wrap(facets = vars(Year), ncol= 2, nrow=3) +
  coord_fixed(1)+
  sm_corr_theme() + #borders = F
  sm_statCorr(corr_method = 'pearson') #line_type = 'solid', 

USA_NM_ratio_validation

ggsave('results/plots/NM_ratio_validation_usa.pdf', USA_NM_ratio_validation)


#### EU ####

nuts3Poly <- st_read('data_in/NUTS_RG_20M_2016_4326.shp/NUTS_RG_20M_2016_4326.shp') %>% 
  mutate(LEVL_CODE = as.numeric(LEVL_CODE)) %>% 
  filter(LEVL_CODE == 3) %>% 
  dplyr::rename(tl3_id = NUTS_ID)


nutsID <- nuts3Poly %>% 
  dplyr::select(tl3_id, CNTR_CODE)


nuts3Data <- readxl::read_xlsx ("data_in/NUTS_net_migration.xlsx",sheet="Sheet 1",skip = 7) %>%
  as_tibble() %>% 
  rename(tl3_id = 'GEO (Codes)') %>%  #%>% 
  left_join(nutsID) %>% 
  filter(!is.na(CNTR_CODE)) %>% 
  #left_join(nuts3popCount) %>% 
  dplyr::select(-'GEO (Labels)', -geometry, -CNTR_CODE) %>% 
  tidyr::pivot_longer(!tl3_id, names_to = 'year', values_to = 'net_mgr_obs') %>% 
  #left_join(nuts3popAvgCount) %>% 
  #mutate(net_mgr_ratio_obs = net_mgr_obs / pop_avg) %>% 
  filter(year %in% c(2001:2020)) %>% 
  mutate(year = as.double(year))


five_yr_range <- c('2001-2005', '2006-2010', '2011-2015', '2016-2020')
ten_yr_range <- c('2001-2010', '2011-2020')

nuts3Data_5yr <- nuts3Data %>% group_by(tl3_id) %>% 
  #filter(year %in% c(2001:2020)) %>% 
  summarise(accum_sum = zoo::rollapply(net_mgr_obs, 5, sum, na.rm = T, align = 'left')) %>%
  mutate(id = row_number()) %>% 
  filter(id %in% seq(1, 20, by=5)) %>% 
  mutate(years = five_yr_range) %>% 
  #left_join(nuts3popAvgCount) %>% #join 5 yr avg pop
  group_by(tl3_id, years) #%>% 
#mutate(ratio_per_1000 = accum_sum / pop_avg *1000)

nuts3Data_10yr <- nuts3Data %>% group_by(tl3_id) %>% 
  #filter(year %in% c(2001:2020)) %>% 
  summarise(accum_sum = zoo::rollapply(net_mgr_obs, 10, sum, na.rm = T, align = 'left')) %>%
  mutate(id = row_number()) %>% 
  filter(id %in% seq(1, 20, by=10)) %>% 
  mutate(years = ten_yr_range) %>% 
  #left_join(nuts3popAvgCount) %>% #join 5 yr avg pop
  group_by(tl3_id, years) #%>% 
#mutate(ratio_per_1000 = accum_sum / pop_avg *1000)

# Calculate EU sub-nat estimates

population <- rast('DATA/r_worldpopHarmonised.tif')
net_mgr_rast <- terra::rast('DATA/netMgr_2001_2020_v3.tif')

# Extract sub-national migration and population
net_mgr_estim_EU <- exact_extract(net_mgr_rast, nuts3Poly, 'sum') %>% 
  as_tibble %>% 
  bind_cols(nutsID) %>% 
  dplyr::select(-geometry) %>% 
  #select(-tl3_id, -tl2_id, -tl1_id, -iso3, -name_en, -name_fr, -continenta, -reg_name_p) %>% 
  tidyr::pivot_longer(-c(tl3_id, CNTR_CODE), names_to = "year", values_to = "net_mgr_estim") %>% 
  mutate(year = str_remove(year, pattern = 'sum.netMgr')) %>% 
  mutate(year = as.double(year))

pop_estim_EU <- exact_extract(population, nuts3Poly, 'sum') %>% 
  as_tibble %>% 
  bind_cols(nutsID) %>% 
  dplyr::select(-geometry) %>% 
  #select(-tl3_id, -tl2_id, -tl1_id, -iso3, -name_en, -name_fr, -continenta, -reg_name_p) %>% 
  tidyr::pivot_longer(-c(tl3_id, CNTR_CODE), names_to = "year", values_to = "pop_estim") %>% 
  mutate(year = str_remove(year, pattern = 'sum.cntryPop')) %>% 
  mutate(year = as.double(year)) %>% 
  filter(year %in% c(2001:2020))

# calculate ratio for each sub-nat area for both estimates and observed
net_mgr_estim_EU <- net_mgr_estim_EU %>% left_join(., pop_estim_EU) %>%
  mutate(net_mgr_estim_ratio = net_mgr_estim / pop_estim * 1000) 

nuts3Data <- nuts3Data %>% left_join(., pop_estim_EU) %>% mutate(net_mgr_obs_ratio = net_mgr_obs / pop_estim * 1000)

mgr_EU <- left_join(net_mgr_estim_EU, nuts3Data)

# Calculate 5 and 10 year accumulated sum
net_mgr_estim_5yr_EU <- net_mgr_estim_EU %>% group_by(tl3_id) %>% 
  #filter(year %in% c(2001:2020)) %>% 
  summarise(accum_sum = zoo::rollapply(net_mgr_estim, 5, sum, na.rm = T, align = 'left')) %>%
  mutate(id = row_number()) %>% 
  filter(id %in% seq(1, 20, by=5)) %>% 
  mutate(years = five_yr_range) %>% 
  #left_join(nuts3popAvgCount) %>% #join 5 yr avg pop
  group_by(tl3_id, years) #%>% 
#mutate(ratio_per_1000 = accum_sum / pop_avg *1000)

net_mgr_estim_10yr_EU <- net_mgr_estim_EU %>% group_by(tl3_id) %>% 
  #filter(year %in% c(2001:2020)) %>% 
  summarise(accum_sum = zoo::rollapply(net_mgr_estim, 10, sum, na.rm = T, align = 'left')) %>%
  mutate(id = row_number()) %>% 
  filter(id %in% seq(1, 20, by=10)) %>% 
  mutate(years = ten_yr_range) %>% 
  #left_join(nuts3popAvgCount) %>% #join 5 yr avg pop
  group_by(tl3_id, years)  


# Calculate 5 and 10 year average population

pop_estim_5yr_avg_EU <- pop_estim_EU %>% group_by(tl3_id) %>% 
  #filter(year %in% c(2001:2020)) %>% 
  summarise(pop_avg = zoo::rollapply(pop_estim, 5, mean, na.rm = T, align = 'left')) %>%
  mutate(id = row_number()) %>% 
  filter(id %in% seq(1, 20, by=5)) %>% 
  mutate(years = five_yr_range) %>% 
  #left_join(nuts3popAvgCount) %>% #join 5 yr avg pop
  group_by(tl3_id, years)  

pop_estim_10yr_avg_EU <- pop_estim_EU %>% group_by(tl3_id) %>% 
  #filter(year %in% c(2001:2020)) %>% 
  summarise(pop_avg = zoo::rollapply(pop_estim, 10, mean, na.rm = T, align = 'left')) %>%
  mutate(id = row_number()) %>% 
  filter(id %in% seq(1, 20, by=10)) %>% 
  mutate(years = ten_yr_range) %>% 
  #left_join(nuts3popAvgCount) %>% #join 5 yr avg pop
  group_by(tl3_id, years)  


# Calculate 5 and 10 year
mgr_5yr_EU <- left_join(net_mgr_estim_5yr_EU, pop_estim_5yr_avg_EU) %>% 
  mutate(., ratio_estim = accum_sum / pop_avg *1000) %>% 
  rename(accum_sum_estim = accum_sum) %>% 
  left_join(., 
            left_join(nuts3Data_5yr, pop_estim_5yr_avg_EU) %>%
              mutate(., ratio_obs = accum_sum / pop_avg*1000) %>% 
              rename(accum_sum_obs = accum_sum) %>% 
              dplyr::select(tl3_id, years, id, ratio_obs, accum_sum_obs))

mgr_10yr_EU <- left_join(net_mgr_estim_10yr_EU, pop_estim_10yr_avg_EU) %>% 
  mutate(., ratio_estim = accum_sum / pop_avg*1000) %>% 
  rename(accum_sum_estim = accum_sum) %>% 
  left_join(., 
            left_join(nuts3Data_10yr, pop_estim_10yr_avg_EU) %>%  
              mutate(., ratio_obs = accum_sum / pop_avg*1000) %>% 
              rename(accum_sum_obs = accum_sum) %>% 
              dplyr::select(tl3_id, years, id, ratio_obs, accum_sum_obs))

### PLOT
net_mgr_abs_validate_EU <- ggplot(data = mgr_EU %>% 
                                    arrange(., desc(net_mgr_estim_ratio)) %>%
                                    #mutate(drop = ifelse(net_mgr_estim >= 1000*net_mgr_obs, 1, 0)) %>% 
                                    slice(-(1:5)) %>%  # drop outliers (1000fold difference)
                                    filter(., year %in% c(2001, 2005, 2010, 2015, 2020)) %>% 
                                    tidyr::drop_na(net_mgr_estim, net_mgr_obs),
                                  mapping = aes(x = net_mgr_obs, y = net_mgr_estim)) + 
  geom_point(size = 1) +
  #geom_smooth() +
  facet_wrap(facets = vars(year), ncol= 2, nrow=3) +
  coord_fixed(1)+
  sm_corr_theme() + #borders = F
  sm_statCorr(corr_method = 'pearson')

net_mgr_validate_EU <- ggplot(data = mgr_EU %>% 
                                arrange(., desc(net_mgr_estim_ratio)) %>%
                                #mutate(drop = ifelse(net_mgr_estim >= 1000*net_mgr_obs, 1, 0)) %>% 
                                #slice(-(1:5)) %>%  # drop outliers (1000fold difference)
                                filter(., year %in% c(2001, 2005, 2010, 2015, 2020)) %>% 
                                tidyr::drop_na(net_mgr_estim, net_mgr_obs),
                              mapping = aes(x = net_mgr_obs_ratio, y = net_mgr_estim_ratio)) + 
  geom_point(size = 1) +
  #geom_smooth() +
  facet_wrap(facets = vars(year), ncol= 2, nrow=3) +
  coord_fixed(1)+
  sm_corr_theme() + #borders = F
  sm_statCorr(corr_method = 'pearson')

ggsave('results/plots/NM_ratio_validation_EU.pdf',net_mgr_validate_EU)
ggsave('results/plots/NM_abs_validation_EU.pdf',net_mgr_abs_validate_EU)


net_mgr_abs_5yr_validate_EU <- ggplot(data = mgr_5yr_EU %>% 
                                        #filter(., year %in% c(2001, 2005, 2010, 2015, 2019)) %>% 
                                        tidyr::drop_na(accum_sum_estim, accum_sum_obs),
                                      mapping = aes(x = accum_sum_obs, y = accum_sum_estim)) + 
  geom_point(size = 1) +
  #geom_smooth() +
  facet_wrap(facets = vars(years), ncol = 4) + #, ncol= 2, nrow=3) +
  coord_fixed(1)+
  sm_corr_theme() + #borders = F
  sm_statCorr(corr_method = 'pearson')

ggsave('results/plots/NM_ratio_5yr_validation_EU.pdf',net_mgr_ratio_5yr_validate_EU)
ggsave('results/plots/NM_abs_5yr_validation_EU.pdf',net_mgr_abs_5yr_validate_EU)

net_mgr_abs_5yr_validate_EU <- ggplot(data = mgr_5yr_EU %>% 
                                        #filter(., year %in% c(2001, 2005, 2010, 2015, 2019)) %>% 
                                        tidyr::drop_na(accum_sum_estim, accum_sum_obs),
                                      mapping = aes(x = accum_sum_obs, y = accum_sum_estim)) + 
  geom_point(size = 1) +
  #geom_smooth() +
  facet_wrap(facets = vars(years), ncol = 4) + #, ncol= 2, nrow=3) +
  coord_fixed(1)+
  sm_corr_theme() + #borders = F
  sm_statCorr(corr_method = 'pearson')

ggsave('results/plots/NM_ratio_5yr_validation_EU.pdf',net_mgr_ratio_5yr_validate_EU)
ggsave('results/plots/NM_abs_5yr_validation_EU.pdf',net_mgr_abs_5yr_validate_EU)


net_mgr_abs_10yr_validate_EU <- ggplot(data = mgr_10yr_EU %>% 
                                         # arrange(desc(ratio_estim)) %>% 
                                         # slice()
                                         #filter(., year %in% c(2001, 2005, 2010, 2015, 2019)) %>% 
                                         tidyr::drop_na(accum_sum_obs, accum_sum_estim),
                                       mapping = aes(x = accum_sum_obs, y = accum_sum_estim)) + 
  geom_point(size = 1) +
  #geom_smooth() +
  facet_wrap(facets = vars(years), ncol= 2, nrow=3) +
  coord_fixed(1)+
  sm_corr_theme() + #borders = F
  sm_statCorr(corr_method = 'pearson')

ggsave('results/plots/NM_ratio_10yr_validation_EU.pdf',net_mgr_10yr_validate_EU)
ggsave('results/plots/NM_abs_10yr_validation_EU.pdf',net_mgr_abs_10yr_validate_EU)










