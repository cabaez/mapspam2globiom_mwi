#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Code to process travel time maps
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### MESSAGE ###############
message("\nRunning 03_spatial_data\\05_select_travel_time_2000_2015.r")


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
                  glue("processed_data/maps/adm/adm_loc_{param$year}_{param$iso3c}.shp"))
output <- file.path(param$spam_path,
                    glue("processed_data/maps/accessibility/accessibility_{param$res}_{param$year}_{param$iso3c}.tif"))

# There are two products, one for around 2000 and one for around 2015, we select on the basis of reference year
if (param$year <= 2007){
  input <- file.path(param$raw_path, "travel_time_2000/acc_50.tif")
  } else {
  input <- file.path(param$raw_path, "travel_time_2015/2015_accessibility_to_cities_v1.0.tif")
}

# Warp and mask
output_map <- align_rasters(unaligned = input, reference = grid, dstfile = output,
                            cutline = mask, crop_to_cutline = F, srcnodata = "-9999",
                            r = "bilinear", verbose = F, output_Raster = T, overwrite = T)
plot(output_map)


############### CLEAN UP ###############
rm(input, mask, output, grid, output_map)


############### MESSAGE ###############
message("Complete")
