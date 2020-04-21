#'========================================================================================================================================
#' Project:  crop_map
#' Subject:  Code to select GLC2000 land cover map per country
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils")
# Additional packages
p_load("WDI", "countrycode", "plotKML", "sf")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)


### LOAD DATA
# adm
adm <- readRDS(file.path(proc_path, paste0("maps/adm/adm_", year_sel, "_", iso3c_sel, ".rds")))

### TO_UPDATE: DOES NOT WORK WITH GLC SO USE APPROACH WITH RASTER
### TO_UPDATE: turn this into a function
### CLIP COUNTRY
# temp_path <- file.path(proc_path, paste0("maps/glc2000"))
# dir.create(temp_path, recursive = T, showWarnings = F)
# 
# input <-file.path(glob_path, paste0("glc2000/glc2000_v1_1.tif"))
# input_shp <- file.path(proc_path, paste0("maps/adm/adm_", year_sel, "_", iso3c_sel, ".shp"))
# output <- file.path(temp_path, paste0("glc2000_raw_", year_sel, "_", iso3c_sel, ".tif"))
# 
# message("Clip glc2000 map")
# glc2000 <- gdalwarp(cutline =input_shp, crop_to_cutline = T, srcfile = input, dstfile = output, verbose = T, output_Raster = T, overwrite = T)


### CLEAN UP
# rm(temp_path, input, input_shp, output, glc2000, adm)


# Load global ESA map
glc_raw <- raster(file.path(glob_path, paste0("glc2000/glc2000_v1_1.tif")))
crs(glc_raw) <- crs(adm)
glc <- crop(glc_raw, adm)
glc <- mask(glc, adm)
names(glc) <- "GLC2000"

# Save map
temp_path <- file.path(proc_path, paste0("maps/glc2000"))
dir.create(temp_path, recursive = T, showWarnings = F)

writeRaster(glc, file.path(temp_path, paste0("glc2000_raw_", year_sel, "_", iso3c_sel, ".tif")), overwrite = T)


