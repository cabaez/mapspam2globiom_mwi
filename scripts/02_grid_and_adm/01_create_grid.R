#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Code to create mapspam grid
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### MESSAGE ###############
message("\nRunning 02_grid_and_adm\\01_create_grid.r")


############### SET UP ###############
# Load pacman for p_load
if(!require(pacman)){
  install.packages("pacman")
  library(pacman) 
} else {
  library(pacman)
}

# Load key packages
p_load("tidyverse", "readxl", "stringr", "here", "scales", "glue", "raster")

# Set root
root <- here()

# R options
options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits


############### LOAD DATA ###############
# adm
adm <- readRDS(file.path(spam_par$spam_path,
  glue("processed_data/maps/adm/adm_{spam_par$year}_{spam_par$iso3c}.rds")))


############### CREATE COUNTRY GRID ###############
grid <- create_grid(res = spam_par$res, border = adm, crs = spam_par$crs)
plot(grid)


############### SAVE ###############
writeRaster(grid, file.path(spam$par, glue("processed_data/maps/grid/grid_{spam_par$year}_{spam_par$res}_{spam_par$iso3c}.tif"), overwrite = T))


