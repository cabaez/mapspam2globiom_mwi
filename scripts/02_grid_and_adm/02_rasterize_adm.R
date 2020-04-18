#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Code to rasterize adm
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### MESSAGE ###############
message("\nRunning 02_grid_and_adm\\02_rasterize_adm.r")


############### SET UP ###############
# Load pacman for p_load
if(!require(pacman)){
  install.packages("pacman")
  library(pacman) 
} else {
  library(pacman)
}

# Load key packages
p_load("mapspam2globiom", "tidyverse", "readxl", "stringr", "here", "scales", "glue", "sf", "gdalUtilities")

# Set root
root <- here()

# R options
options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits


############### LOAD DATA ###############
# Adm
adm <- readRDS(file.path(spam_par$spam_path,
  glue("processed_data/maps/adm/adm_{spam_par$year}_{spam_par$iso3c}.rds")))

# Grid
grid <- raster(file.path(spam_par$spam_path,
 glue("processed_data/maps/grid/grid_{spam_par$res}_r_{spam_par$year}_{spam_par$iso3c}.tif")))
names(grid) <- "gridID"
  

############### CREATE RASTER OF ADMS WITH GRIDID ###############
# Rasterize adm
adm_r <- rasterize(adm, grid)
names(adm_r) <- "ID"
plot(adm_r)

# Get adm info
if(adm_sel == 0){
  adm_df <- levels(adm_r)[[1]] %>%
    transmute(ID, adm0_name, adm0_code)
} else if(adm_sel == 1){
  adm_df <- levels(adm_r)[[1]] %>%
    transmute(ID, adm0_name, adm0_code, adm1_name, adm1_code)
} else if(adm_sel == 2){
  adm_df <- levels(adm_r)[[1]] %>%
    transmute(ID, adm0_name, adm0_code, adm1_name, adm1_code, adm2_name, adm2_code)
}

# stack
adm_r <- stack(grid, adm_r)

# Create data.frame, remove cells outside border and add adm names
adm_r <- as.data.frame(rasterToPoints(adm_r)) %>%
  left_join(adm_df, by = "ID") %>%
  na.omit %>%
  dplyr::select(-ID, -x, -y)


############### SAVE ###############
# Save
saveRDS(adm_r, file.path(proc_path, glue("maps/adm/adm_r_{grid_sel}_{year_sel}_{iso3c_sel}.rds")))


############### CLEAN UP ###############
rm(adm, grid, adm_r, adm_df)


############### MESSAGE ###############
message("Complete")
