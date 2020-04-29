#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Code to run model
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### SET UP ###############
# Load pacman for p_load
if(!require(pacman)){
  install.packages("pacman")
  library(pacman) 
} else {
  library(pacman)
}

# Load key packages
p_load("tidyverse", "readxl", "stringr", "here", "scales", "glue", "sf", "raster", "mapview")

# Set root
root <- here()

# R options
options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits


### SOURCE FUNCTIONS
# TO_UPDATE
source(file.path(root, "Code/general/mapspam_functions.r"))

############### LINK GAMS LIBRARIES ###############
# R will automatically find the location of GAMS if this is stored in your system (see documentation).
# If for some reason this does not work you can set it specifically, e.g. igdx("C:/Program Files/GAMS/win64/24.6")
igdx("")


############### RUN MODEL ###############
if(model_sel == "max_score") {
  run_gams("max_score")  
  } else {
  if(model_sel == "min_entropy") {
    message("This model is not implemented yet")
    stop("Stop process")
    } else {
    message("This model is not implemented yet")
    stop("Stop process")
  }
}


