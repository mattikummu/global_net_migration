**Code for the global net migration dataset and analyses**

These codes were used to create the global net-migration dataset that is published in Niva et al. (2023). Please do cite this paper if using the codes. 

Niva et al. 2023. World's human migration patterns in 2000-2019 unveiled by high-resolution data. Nature Human Behaviour 7: 2023–2037. Doi: https://doi.org/10.1038/s41562-023-01689-4 

The output data is available at: 
http://doi.org/10.5281/zenodo.7997134

**Input data for the script**

The input data for the codes are available at:
http://doi.org/10.5281/zenodo.14946360

To keep the folder structure, please extract all zip files in that Zenodo repository to the same folder where the code folders (see Code structure section) are. The folder should look like this:

```plaintext
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

- The downscaling part (section 2) is not reproducible, but the output from that is stored in `data_out_downscaling`, and thus following sections can be run.

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

**1_preparing_data**
- `1_prepare_spatial_data.R`
- `2_preparing_national_data_v2.R`
- `3_1_processing_subnat_data_births.R`
- `3_2_processing_subnat_data_deaths.R`
- `4_rasterise_births_deaths.R`
- `5_prepare_data_for_downscaling.R`

**Contact**

For any questions, please contact:
[matti.kummu@aalto.fi](mailto:matti.kummu@aalto.fi)
