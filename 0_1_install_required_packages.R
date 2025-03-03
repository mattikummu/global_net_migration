
# script that installs all required packages

# List of all required packages 
required_packages <- c(
  "sf", "e1071", "fasterize", "magrittr", "raster", "terra",
  "snow", "foreach", "openxlsx", "readxl", "tidyverse", "dplyr",
  "doParallel", "exactextractr", "Rfast", "zoo", "smoothr",
  "viridis", "rnaturalearth", "tmap", "data.table", "caret", 
  "foreign", "sp", "RColorBrewer", "gstat",
  "MASS", "geoR", "reshape", "maptools", "spdep", "grid",
  "RCurl", "Kendall", "scico", "rmapshaper", "readr", "ggplot2",
  "tidyterra", "tibble", "reldist", "pracma", "ggpubr",
  "wCorr", "smplot2"
)

# Create a function to check and install packages
check_and_install_package <- function(package) {
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  } else {
    message(paste("Package", package, "is already installed."))
  }
}

# Check and install necessary packages
for (package in required_packages) {
  check_and_install_package(package)
}

# Load the required packages
invisible(lapply(required_packages, library, character.only = TRUE))

message("All required packages are now installed and loaded.")