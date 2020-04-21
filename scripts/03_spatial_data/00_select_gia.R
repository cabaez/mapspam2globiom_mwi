#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Code to process GIA
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### MESSAGE ###############
message("\nRunning 03_spatial_data\\02_select_gia.r")


############### SET UP ###############
# Install and load pacman package that automatically installs R packages if not available
if("pacman" %in% rownames(installed.packages()) == FALSE) install.packages("pacman")
library(pacman)

# Load key packages
p_load("mapspam2globiom", "tidyverse", "readxl", "stringr", "here", "scales", "glue", "gdalUtils", "raster", "sf")

# Set root
root <- here()

# R options
options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits


############### LOAD DATA ###############
# Raw gia file
gia_raw <- raster(file.path(param$raw_path, "gia/global_irrigated_areas.tif"))


############### PROCESS ###############
# The crs of the gia (WGS 84) is missing for some reason. We add and save the map
crs(gia_raw) <- "+proj=longlat +datum=WGS84 +no_defs"
if(!file.exists(file.path(param$raw_path, "gia/global_irrigated_areas_crs.tif"))){
  writeRaster(gia_raw, file.path(param$raw_path, "gia/global_irrigated_areas_crs.tif"), overwrite = T)
}

# Gia assumes full 30sec (the resolution of the map) are irrigated and uses a categorical
# variable (1-4) to indicate irrigated areas (see README.txt).
# In order to use the map at higher resolutions (e.g. 5 arcmin) we need to reclassify these into 
# 1 (100%) and use gdalwarp with "average" to calculate the share of irrigated area at larger grid cells.
# If res is 30sec, we can clip the raw map and reclassify c(1:4) values to 1.

# Set files
grid <- file.path(param$spam_path,
                  glue("processed_data/maps/grid/grid_{param$res}_{param$year}_{param$iso3c}.tif"))
mask <- file.path(param$spam_path,
                  glue("processed_data/maps/adm/adm_loc_{param$year}_{param$iso3c}.shp"))
input <- file.path(param$raw_path, "gia/global_irrigated_areas_crs.tif")
output <- file.path(param$raw_path,
                    glue("gia/gia_temp_{param$year}_{param$iso3c}.tif"))
  
# Clip to adm
# Warp and mask
# Use r = "near" for categorical values.
# Use crop to cutline to crop.
# TODO probably does not work at 30sec!!
gia_temp <- gdalUtils::gdalwarp(srcfile = input, dstfile = output,
                          cutline = mask, crop_to_cutline = T, 
                          r = "near", verbose = F, output_Raster = T, overwrite = T)
  
# Reclassify
gia_temp <- reclassify(gia_temp, cbind(1, 4, 1))
names(gia_temp) <- "gia"

if(param$res == "30sec") {
  # Save
  writeRaster(gia_temp,
    file.path(param$spam_path,
    glue("processed_data/maps/irrigated_area/gia_{param$res}_{param$year}_{param$iso3c}.tif")),
    overwrite = T)
}

if(param$res == "5min"){
  # Save temporary file with 1 for irrigated area
  writeRaster(gia_temp, file.path(param$spam_path,
                                  glue("processed_data/maps/irrigated_area/gia_temp_{param$year}_{param$iso3c}.tif")), overwrite = T)
  
  # Set files
  grid <- file.path(param$spam_path,
    glue("processed_data/maps/grid/grid_{param$res}_{param$year}_{param$iso3c}.tif"))
  mask <- file.path(param$spam_path,
    glue("processed_data/maps/adm/adm_loc_{param$year}_{param$iso3c}.shp"))
  input <- file.path(param$spam_path, 
    glue("processed_data/maps/irrigated_area/gia_temp_{param$year}_{param$iso3c}.tif"))
  output <- file.path(param$spam_path,
    glue("processed_data/maps/irrigated_area/gia_{param$res}_{param$year}_{param$iso3c}.tif"))
  
  # Warp and mask
  # Use average to calculate share of irrigated area
  gia_temp <- align_rasters(unaligned = input, reference = grid, dstfile = output,
                cutline = mask, crop_to_cutline = F, 
                r = "average", verbose = F, output_Raster = T, overwrite = T)
  plot(gia_temp)
}

############### CLEAN UP ###############
rm(gia, gia_raw, gia_temp)
