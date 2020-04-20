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
# Set files
grid <- file.path(param$spam_path,
                  glue("processed_data/maps/grid/grid_{param$res}_{param$year}_{param$iso3c}.tif"))
mask <- file.path(param$spam_path,
                  glue("processed_data/maps/adm/adm_{param$year}_{param$iso3c}.shp"))
input <- file.path(param$raw_path, "worldpop/ppp_2010_1km_Aggregated.tif")
output <- file.path(param$spam_path,
                    glue("processed_data/maps/accessibility/accessibility_{param$res}_{param$year}_{param$iso3c}.tif"))

# Warp and mask
output_map <- align_rasters(unaligned = input, reference = grid, dstfile = output,
                   cutline = mask, crop_to_cutline = F, 
                   r = "bilinear", verbose = F, output_Raster = T, overwrite = T)
plot(output_map)


############### CLEAN UP ###############
rm(grid, input, mask, output, output_map)


############### MESSAGE ###############
message("Complete")

