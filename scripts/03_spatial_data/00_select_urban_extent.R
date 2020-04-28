#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Code to process urban extent maps
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### MESSAGE ###############
message("\nRunning 03_spatial_data\\07_select_urban_extent.r")


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


############### LOAD DATA ###############
# Adm location
adm_map <- readRDS(file.path(param$spam_path,
                         glue("processed_data/maps/adm/adm_map_{param$year}_{param$iso3c}.rds")))

# urban extent, select country
grump_raw <- read_sf(file.path(param$raw_path, "grump/global_urban_extent_polygons_v1.01.shp"))


############### PROCESS ###############
grump <- grump_raw %>%
  filter(ISO3 == param$iso3c)
plot(adm_loc$geometry)
plot(grump$geometry, col = "red", add = T)


############### SAVE ###############
saveRDS(grump, file.path(param$spam_path, glue("processed_data/maps/population/urb_{param$year}_{param$iso3c}.rds")))


############### CLEAN UP ###############
rm(temp_path, adm_loc, grump, grump_raw)


############### MESSAGE ###############
message("Complete")