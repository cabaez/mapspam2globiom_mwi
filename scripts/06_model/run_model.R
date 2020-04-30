#'========================================================================================================================================
#' Project:  mapspam2globiom_mwi
#' Subject:  
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


############### HARMONIZE INPUT DATA ###############
harmonize_inputs(param)


############### PREPARE BIOPHYSICAL SUITABILITY AND POTENTIAL YIELD ###############
prepare_bs_yg("biophysical_suitability", param)
prepare_bs_yg("potential_yield", param)


############### PREPARE SCORE ###############
prepare_score(param)


############### COMBINE MODEL INPUTS ###############
combine_inputs(param)


############### RUN MODEL ###############
run_spam(param)


############### PREPARE RESULTS ###############
prepare_results(param)


############### PREPARE RESULTS ###############
create_all_tif(param)
