#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Code to process population maps
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### MESSAGE ###############
message("\nRunning 03_spatial_data\\06_select_worldpop.r")


############### SET UP ###############
# Install and load pacman package that automatically installs R packages if not available
if("pacman" %in% rownames(installed.packages()) == FALSE) install.packages("pacman")
library(pacman)

# Load key packages
p_load("mapspam2globiom", "tidyverse", "readxl", "stringr", "here", "scales", "glue", "gdalUtils", "sf", "raster")

# Set root
root <- here()

# R options
options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits


############### PROCESS ###############
# Worldpop presents population density per #grid cell (in this case 30 arcsec,
# the resolution of the map). 
# In order to use the map at higher resolutions (e.g. 5 arcmin) we need to resample using
# the average option and multiple by 100, the number of 30sec grid cells in 5 arcmin.

grid <- file.path(param$spam_path,
                  glue("processed_data/maps/grid/grid_{param$res}_{param$year}_{param$iso3c}.tif"))
mask <- file.path(param$spam_path,
                  glue("processed_data/maps/adm/adm_map_{param$year}_{param$iso3c}.shp"))
input <- file.path(param$raw_path, "worldpop/ppp_2010_1km_Aggregated.tif")
output <- file.path(param$spam_path,
                    glue("processed_data/maps/population/population_{param$res}_{param$year}_{param$iso3c}.tif"))

if(param$res == "30sec") {
  
  # Warp and mask
  output_map <- align_rasters(unaligned = input, reference = grid, dstfile = output,
                   cutline = mask, crop_to_cutline = F, 
                   r = "bilinear", verbose = F, output_Raster = T, overwrite = T)
  plot(output_map)
}

if(param$res == "5min") {
  
  # Warp and mask
  worldpop_temp <- align_rasters(unaligned = input, reference = grid, dstfile = output,
                              cutline = mask, crop_to_cutline = F, 
                              r = "average", verbose = F, output_Raster = T, overwrite = T)
  
  # Multiple average population with 100
  worldpop_temp <- worldpop_temp*100
  plot(worldpop_temp)
  
  # Overwrite
  writeRaster(worldpop_temp, file.path(param$spam_path,
    glue("processed_data/maps/population/population_{param$res}_{param$year}_{param$iso3c}.tif")), 
    overwrite = T)
}


############### CLEAN UP ###############
rm(grid, input, mask, output, output_map)


############### MESSAGE ###############
message("Complete")

