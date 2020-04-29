#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Script to prepate biophysical suitability and potential yield
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### SET UP ###############
# Install and load pacman package that automatically installs R packages if not available
if("pacman" %in% rownames(installed.packages()) == FALSE) install.packages("pacman")
library(pacman)

# Load key packages
p_load("mapspam2globiom", "tidyverse", "readxl", "stringr", "here", "scales", "glue")

# Set root
root <- here()

# R options
options(scipen=999) # Surpress scientific notation
options(digits=4)


############### PREPARE BS AND YG ###############
prepare_bs_yg("biophysical_suitability", param)
prepare_bs_yg("potential_yield", param)


############### PREPARE SCORE ###############
prepare_score(param)
