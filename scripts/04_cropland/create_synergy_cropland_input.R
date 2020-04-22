#'========================================================================================================================================
#' Project:  crop_map
#' Subject:  Script to create synergistic cropland map file that can be used in harmonization part
#' Author:   Michiel van Dijk, Yating
#' Contact:  michiel.vandijk@wur.nl, y.ru@cgiar.org
#'========================================================================================================================================

############### SET UP ###############
# Install and load pacman package that automatically installs R packages if not available
if("pacman" %in% rownames(installed.packages()) == FALSE) install.packages("pacman")
library(pacman)

# Load key packages
p_load("mapspam2globiom", "tidyverse", "readxl", "stringr", "here", "scales", "glue", "sf", "raster")

# Set root
root <- here()

# R options
options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits

############### CREATE SYNERGY CROPLAND INPUT ###############
prepare_cropland(param)
