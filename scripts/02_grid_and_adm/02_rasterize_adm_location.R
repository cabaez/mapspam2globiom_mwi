#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Code to rasterize adm
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### MESSAGE ###############
message("\nRunning 02_grid_and_adm\\02_rasterize_adm.r")


############### SET UP ###############
# Install and load pacman package that automatically installs R packages if not available
if("pacman" %in% rownames(installed.packages()) == FALSE) install.packages("pacman")
library(pacman)


# Load key packages
p_load("mapspam2globiom", "tidyverse", "readxl", "stringr", "here", "scales", "glue",
       "sf", "gdalUtilities")

# Set root
root <- here()

# R options
options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits


############### LOAD DATA ###############
# Adm location
adm_loc <- readRDS(file.path(param$spam_path,
  glue("processed_data/maps/adm/adm_loc_{param$year}_{param$iso3c}.rds")))

# Grid
grid <- raster(file.path(param$spam_path,
 glue("processed_data/maps/grid/grid_{param$res}_{param$year}_{param$iso3c}.tif")))
names(grid) <- "gridID"


############### CREATE RASTER OF ADMS WITH GRIDID ###############
# Rasterize adm
# getCover ensures all grid cells covered are rasterized, not only where the center is covered by the polyon.
# Otherwise grid cells might get lost.
adm_loc_r <- rasterize(adm_loc, grid)
names(adm_loc_r) <- "ID"
plot(adm_loc_r)

# Get adm info
if(param$adm_level == 0){
  adm_df <- levels(adm_loc_r)[[1]] %>%
    transmute(ID, adm0_name, adm0_code)
} else if(param$adm_level == 1){
  adm_df <- levels(adm_loc_r)[[1]] %>%
    transmute(ID, adm0_name, adm0_code, adm1_name, adm1_code)
} else if(param$adm_level == 2){
  adm_df <- levels(adm_loc_r)[[1]] %>%
    transmute(ID, adm0_name, adm0_code, adm1_name, adm1_code, adm2_name, adm2_code)
}

# stack
adm_loc_r <- stack(grid, adm_loc_r)

# Create data.frame, remove cells outside border and add adm names
adm_loc_r <- as.data.frame(rasterToPoints(adm_loc_r)) %>%
  left_join(adm_df, by = "ID") %>%
  na.omit %>%
  dplyr::select(-ID, -x, -y)


############### SAVE ###############
# Save
saveRDS(adm_loc_r, file.path(param$spam_path,
  glue("processed_data/maps/adm/adm_loc_r_{param$res}_{param$year}_{param$iso3c}.rds")))


############### CLEAN UP ###############
rm(adm, grid, adm_r)


############### MESSAGE ###############
message("Complete")
