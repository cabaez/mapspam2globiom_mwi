#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Code process SASAM global synergy cropland map
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### MESSAGE ###############
message("\nRunning 03_spatial_data\\01_select_sasam_adm.r")


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


############### PROCESS CROPRATIO (MEDIAN AREA) ###############
# Set files
grid <- file.path(param$spam_path,
                  glue("processed_data/maps/grid/grid_{param$res}_{param$year}_{param$iso3c}.tif"))
mask <- file.path(param$spam_path,
                  glue("processed_data/maps/adm/adm_{param$year}_{param$iso3c}.shp"))
input <- file.path(param$spam_path,
                   glue("raw_data/sasam/{param$continent}/cropland_ratio_{param$continent}.tif"))
output <- file.path(param$spam_path,
                    glue("processed_data/maps/cropland/cropland_med_share_{param$res}_{param$year}_{param$iso3c}.tif"))

# Warp and mask
output_map <- align_rasters(unaligned = input, reference = grid, dstfile = output,
                            cutline = mask, crop_to_cutline = F,
                            r = "bilinear", verbose = F, output_Raster = T, overwrite = T)

# Maps are in shares of area. We multiply by grid size to create an area map.
a <- area(output_map)
output_map <- output_map * a * 100
plot(output_map)
writeRaster(output_map, file.path(param$spam_path,
  glue("processed_data/maps/cropland/cropland_med_{param$res}_{param$year}_{param$iso3c}.tif")),overwrite = T)

# clean up
rm(grid, input, mask, output, output_map)


############## PROCESS CROPMAX (MAXIMUM AREA) ###############
# Set files
grid <- file.path(param$spam_path,
                  glue("processed_data/maps/grid/grid_{param$res}_{param$year}_{param$iso3c}.tif"))
mask <- file.path(param$spam_path,
                  glue("processed_data/maps/adm/adm_{param$year}_{param$iso3c}.shp"))
input <- file.path(param$spam_path,
                   glue("raw_data/sasam/{param$continent}/cropland_max_{param$continent}.tif"))
output <- file.path(param$spam_path,
                    glue("processed_data/maps/cropland/cropland_max_share_{param$res}_{param$year}_{param$iso3c}.tif"))

# warp and mask
output_map <- align_rasters(unaligned = input, reference = grid, dstfile = output,
                            cutline = mask, crop_to_cutline = F,
                            r = "bilinear", verbose = F, output_Raster = T, overwrite = T)

# Maps are in shares of area. We multiply by grid size to create an area map.
a <- area(output_map)
output_map <- output_map * a * 100
plot(output_map)
writeRaster(output_map, file.path(param$spam_path,
                             glue("processed_data/maps/cropland/cropland_max_{param$res}_{param$year}_{param$iso3c}.tif")),overwrite = T)

# clean up
rm(a, grid, input, mask, output, output_map)


############### PROCESS CROPPROB (PROBABILITY, 1 IS HIGHEST) ###############
# Set files
grid <- file.path(param$spam_path,
                      glue("processed_data/maps/grid/grid_{param$res}_{param$year}_{param$iso3c}.tif"))
mask <- file.path(param$spam_path,
                  glue("processed_data/maps/adm/adm_{param$year}_{param$iso3c}.shp"))
input <- file.path(param$spam_path,
                   glue("raw_data/sasam/{param$continent}/cropland_confidence_level_{param$continent}.tif"))
output <- file.path(param$spam_path,
                    glue("processed_data/maps/cropland/cropland_rank_{param$res}_{param$year}_{param$iso3c}.tif"))

# warp and mask
# Use r = "med" to select the median probability as probability is a categorical variable (1-32).
# Remove 0 values (no cropland) before processing as they will bias taking the medium value.
output_map <- align_rasters(unaligned = input, reference = grid, dstfile = output,
                            cutline = mask, crop_to_cutline = F, srcnodata = "0",
                            r = "med", verbose = F, output_Raster = T, overwrite = T)
plot(output_map)

# clean up
rm(grid, input, mask, output, output_map)


############### MESSAGE ###############
message("Complete")

