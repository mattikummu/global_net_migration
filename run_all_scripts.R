# This script runs all other scripts in a project.
# The code is divided into the following sections (folders):
# 1. Data preparation for births and death ratios as well as independent variables for downscaling (`1_preparing_data` -folder)
# 2. Downscaling birth and death ratios from adm1 level to gridded level (`2_downscaling_BirthsDeaths` -folder)
# 3. Calculating the net-migration from birth and death ratios and reported population change (`3_net_migration_setup` -folder)
# 4. Analyses for the paper (`4_analyses` -folder)
# 5. Validation of birth and death downscaling, used population data as well as net-migration estimates (`5_validation` -folder)

# Set working directory to where this script is located
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

source("0_install_required_packages.R")
# Installs all the required packages, in case those are not yet installed

# 1. Data preparation

# 1_preparing_data
source("1_preparing_data/1_prepare_spatial_data.R")
# Spatial data is prepared for following scripts

source("1_preparing_data/2_preparing_national_data_v2.R")
# National datasets are harmonised

source("1_preparing_data/3_1_processing_subnat_data_births.R")
# Subnational data for births are combined from several sources

source("1_preparing_data/3_2_processing_subnat_data_deaths.R")
# Subnational data for deaths are combined from several sources

source("1_preparing_data/4_rasterise_births_deaths.R")
# The subnational births and deaths datasets are put into a global raster

source("1_preparing_data/5_prepare_data_for_downscaling.R")
# Data is prepared for downscaling

# 2. Downscaling Births and Deaths

# 2_downscaling_BirthsDeaths
source("2_downscaling_BirthsDeaths/1_Build training dataset.R")
# Using the birth/death data raster file, administrative boundary shapefile and 
#the predictor variable raster files (hdi, scaled world pop data, and either fertility or 
#life expectancy data) – create a training dataset. Adds the income bands that each admin boundary 
#is in to the training data.

source("2_downscaling_BirthsDeaths/2_and_3_LM_fit_apply.R")
# Fits a separate LM for each income band classification and saves both the LM model and the residuals 
#from the model application. Saves a raster stack of predictions for each year labeled ‘init’ (initial guess).

source("2_downscaling_BirthsDeaths/4_LM_apply_ATPK.R")
# Uses the init raster stack and the residuals from the application of the LMs at the admin scale to 
# output a raster stack of corrections to be applied to the init predictions.

source("2_downscaling_BirthsDeaths/5_Apply_norm_beta.R")
# Takes the init predictions and the ATPK as inputs, then normalizes them based on the extracted births/deaths 
#from each admin boundary. Outputs the ATPK corrected, normalized downscaled results.

# 3. Net migration setup

# 3_net_migration_setup
source("3_net_migration_setup/1_net_migration_setup.R")
# Calculates the net migration at different scales, based on births and deaths rasters (from downscaling)
#as well as population count raster

source("3_net_migration_setup/2_population_data.R")
# Prepares the population data at urban and rural areas for analyses

# 4. Analyses

# 4_analyses
source("4_analyses/1_net_migration_calculations.R")
# Analyzes the impact of migration on population changes over various administrative levels and calculate
#internal, sub-national, and international migration statistics.

source("4_analyses/2_net_mgr_analyses_for_manuscript_2.R")
# Sets up an environment to read and process geographic and demographic data, calculate trends for 
#various parameters such as births, deaths, and life expectancy, and visualize these trends using thematic maps.

source("4_analyses/3_net_migration.R")
# Performs an analysis of urban and rural net migration data for the years 2001-2020. 
#Extracts net migration data, calculates zonal statistics of total, urban, and rural net-migration at different levels.

source("4_analyses/4_admin_impact_migr_pop_change.R")
# Analyzes the impact of urban and rural net migration on population change. 
#Classifies administrative units into seven classes based on how migration affects urban and rural population growth or decline.

source("4_analyses/5_socio_climatic_bins.R")
# Creates socio-climatic bins by combining human development index (HDI) and aridity data, 
#weighted by population distribution.

source("4_analyses/6_socio_climatic_bins_analysis.R")
# Performs an analysis based on socio-climatic bins to illustrate the impact of urban and
#rural net migration on population changes.

source("4_analyses/7_admin_division_urban_rural_NM.R")
# Analyzes how net-migration in each admin unit is divided between urban and rural areas, 
#classifying units based on migration patterns.

# 5. Validation

# 5_validation
source("5_validation/1_worldpop_2020_validation.R")
# Validates worldpop dataset against reported population count at detailed administrative level

source("5_validation/2_mortality_downscal_validation.R")
# Validates downscaled mortality rates against reported mortality rates at detailed administrative level

source("5_validation/3_birth_downscal_validation.R")
# Validates downscaled birth rates against reported birth rates at detailed administrative level

source("5_validation/4_net_migration_validation.R")
# Validates our net-migration estimates against reported net migration datasets
