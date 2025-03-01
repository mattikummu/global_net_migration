**Code for the global net migration dataset and analyses**

These codes were used to create the global net-migration dataset that is published in Niva et al. (2023). Please do cite this paper if using the codes. 

Niva et al. 2023. World's human migration patterns in 2000-2019 unveiled by high-resolution data. Nature Human Behaviour 7: 2023–2037. Doi: https://doi.org/10.1038/s41562-023-01689-4 

The output data is available at: 
http://doi.org/10.5281/zenodo.7997134

**Input data for the script**

The input data for the codes are available at:
http://doi.org/10.5281/zenodo.14946360

To keep the folder structure, please extract all zip files in that Zenodo repository to the same folder where the code folders (see Code structure section) are. The folder should look like this:


------ global_net_migration -folder ----------

0_install_required_packages.R

1_preparing_data
2_downscaling_BirthsDeaths
3_net_migration_setup
4_analyses
5_validation

data_in
data_in_downscaling
data_in_gpkg
data_in_rast
data_out_downscaling

functions

global_net_migration.Rproj
LICENSE
readme.txt
--------------------------------------------


**Reproducibility**

Please note: all the scripts are not yet reproducible - we are working hard to get all the used input data into Zenodo. Apologies for this.

Here are details:

- The downscaling part (section 2) is not reproducible, but the output from that is stored in `data_out_downscaling`, and thus following sections (3-5) can be run.

- Under the `analyses` folder (section 4), two scripts (`5_socio_climatic_bins.R` (aridity map is missing) and `6_socio_climatic_bins_analysis.R` (needs information from `5_socio_climatic_bins.R`)) are not reproducible yet.

- Under the `validation` folder (section 5), the `4_net_migration_validation.R` script is not reproducible yet.


**Code structure**

The code is divided into the following sections (folders):
1. Data preparation for births and death ratios as well as independent variables for downscaling (`1_preparing_data` -folder)
2. Downscaling birth and death ratios from adm1 level to gridded level (`2_downscaling_BirthsDeaths` -folder)
3. Calculating the net-migration from birth and death ratios and reported population change (`3_net_migration_setup` -folder)
4. Analyses for the paper (`4_analyses` -folder)
5. Validation of birth and death downscaling, used population data as well as net-migration estimates (`5_validation` -folder)

*Each script within a section is briefly explained below*

`0_install_required_packages.R`
Installs all the required packages, in case those are not yet installed


*1_preparing_data*
- `1_prepare_spatial_data.R`
Spatial data is prepared for following script

- `2_preparing_national_data_v2.R`
National datasets are harmonised 

- `3_1_processing_subnat_data_births.R`
Subnational data for births are combined from several sources

- `3_2_processing_subnat_data_deaths.R`
Subnational data for deaths are combined from several sources

- `4_rasterise_births_deaths.R`
The subnational births and deaths datasets are put into a global raster

- `5_prepare_data_for_downscaling.R`
Data is prepared for downscaling


*2_downscaling_BirthsDeaths*- '1_Build training dataset.R'
Using the birth/death data raster file, administrative boundary shapefile and the predictor variable raster files (hdi, scaled world pop data, and either fertility or life expectancy data) – create a training dataset. Also adds the income bands that each admin boundary is in to the training data.

- '2_and_3_LM_fit_apply.R'Fits a separate LM for each income band classification and saves both the LM model and the residuals from the model application.
This will also save a raster stack of predictions for each year labelled ‘init’ (initial guess).- '4_LM_apply_ATPK.R'
Use the init raster stack, and the residuals from the application of the LMs at the admin scale. Due to a reworking of the st_intersects function in R – this may now take too long. This might be worth re-writing the script to work using a raster input for the admin boundaries.
This script will output a raster stack of corrections to be applied to the init predictions.
- '5_Apply_norm_beta.R'
This takes the init predictions and the ATPK as inputs, then normalises them based on the extracted births/deaths from each admin boundary (as an input I’ve attached).
The output is the ATPK corrected, normalised downscaled results.


*3_net_migration_setup*- '1_net_migration_setup.R'
Calculates the net migration at different scales, based on births and deaths rasters (from downscaling) as well as population count raster- '2_population_data.R'
Prepares the population data at urban and rural areas for analyses

*4_analyses*
- '1_net_migration_calculations.R'
Analyzes the impact of migration on population changes over various administrative levels and calculate internal, sub-national, and international migration statistics. 
- '2_net_mgr_analyses_for_manuscript_2.R'This script sets up an environment to read and process geographic and demographic data, calculate trends for various parameters such as births, deaths, and life expectancy, and visualize these trends using thematic maps.
- '3_net_migration.R'
This script performs an analysis of urban and rural net migration data for the years 2001-2020. It begins by extracting net migration data for urban and rural areas based on previously defined urban extents. The script then calculates zonal statistics of total, urban, and rural net-migration at global, regional, national, and sub-national levels using specified administrative units and population data.
- '4_admin_impact_migr_pop_change.R'
This script analyzes the impact of urban and rural net migration on population change. It classifies administrative units into seven classes based on how migration affects urban and rural population growth or decline.
- '5_socio_climatic_bins.R'
This script creates socio-climatic bins by combining human development index (HDI) and aridity data, weighted by population distribution. 
- '6_socio_climatic_bins_analysis.R'This script performs an analysis based on socio-climatic bins to illustrate the impact of urban and rural net migration on population changes- '7_admin_division_urban_rural_NM.R'
performs the analysis of how net-migration in each admin unit is divided between urban and rural areas. In other words the script classifes an admin unit as a net-received, net-sender or as a case where migration follows one of the following cases: rural push - urban pull or urban push - rural pull.

*5_validation*- '1_worldpop_2020_validation.R'
Validates worldpop dataset against reported population count at detail administrative level- '2_mortality_downscal_validation.R'
Validates downscaled mortality rates against reported mortality rates at detail administrative level

- '3_birth_downscal_validation.R'
Validates downscaled birth rates against reported birth rates at detail administrative level


- '4_net_migration_validation.R'Validates our net-migration estimates against reported net migration datasets




**Contact**

For any questions, please contact:
[matti.kummu@aalto.fi](mailto:matti.kummu@aalto.fi)
