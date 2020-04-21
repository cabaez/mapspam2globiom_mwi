#'========================================================================================================================================
#' Project:  crop_map
#' Subject:  Script to create synergistic crop map file that can be used in harmonization part
#' Author:   Michiel van Dijk, Yating
#' Contact:  michiel.vandijk@wur.nl, y.ru@cgiar.org
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "mapview")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(viewer = NULL) # to force mapview to open in the browser and not in RStudio



# Synergy cropland
cl_raw <- raster(file.path(proc_path, glue("maps/cropland/cropland_med_{grid_sel}_{year_sel}_{iso3c_sel}.tif"))) 
names(cl_raw) <- "cl_area"
cl_rank_raw <- raster(file.path(proc_path, glue("maps/cropland/cropland_rank_{grid_sel}_{year_sel}_{iso3c_sel}.tif"))) 
names(cl_rank_raw) <- "cl_rank"
cl_max_raw <- raster(file.path(proc_path, glue("maps/cropland/cropland_max_{grid_sel}_{year_sel}_{iso3c_sel}.tif"))) 
names(cl_max_raw) <- "cl_area_max"
