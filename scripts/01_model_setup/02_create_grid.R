#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Code to create mapspam grid
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### MESSAGE ###############
message("\nRunning 02_grid_and_adm\\01_create_grid.r")


############### SET UP ###############
# Install and load pacman package that automatically installs R packages if not available
if("pacman" %in% rownames(installed.packages()) == FALSE) install.packages("pacman")
library(pacman)

# Load key packages
p_load("mapspam2globiom", "tidyverse", "readxl", "stringr", "here", "scales", "glue", "raster")

# Set root
root <- here()

# R options
options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits


############### LOAD DATA ###############
# Adm map
load_data("adm_map", param)


############### CREATE COUNTRY GRID ###############
grid <- create_grid(border = adm_map, param = param)
plot(grid)


############### SAVE ###############
writeRaster(grid, file.path(param$spam_path, 
                            glue("processed_data/maps/grid/grid_{param$res}_{param$year}_{param$iso3c}.tif")),
  overwrite = T)

############### CLEAN UP ###############
rm(adm_map, grid)
