#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Code process SASAM global synergy cropland map
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### MESSAGE ###############
message("\nRunning 03_spatial_data\\01_select_sasam_adm.r")


############### SET UP ###############
# Load pacman for p_load
if(!require(pacman)){
  install.packages("pacman")
  library(pacman) 
} else {
  library(pacman)
}

# Load key packages
p_load("mapspam2globiom", "tidyverse", "readxl", "stringr", "here", "scales", "glue", "gdalUtils", "sf", "raster")

# Set root
root <- here()

# R options
options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits


### SOURCE FUNCTIONS
# TO_UPDATE
source(file.path(root, "Code/general/mapspam_functions.r"))


############### PROCESS CROPRATIO (MEDIAN AREA) ###############
# Set files
input <- file.path(spam_par$spam_path, glue("raw_data/sasam/{spam_par$continent}/cropland_ratio_{spam_par$continent}.tif"))
mask <- file.path(spam_par$spam_path, glue("processed_data/maps/adm/adm_{spam_par$year}_{spam_par$iso3c}.shp"))
output <- file.path(spam_par$spam_path, glue("processed_data/maps/cropland_med_share_{spam_par$grid}_{spam_par$year}_{spam_par$iso3c}.tif"))
ref_grid <- file.path(spam_par$spam_path, glue("processed_data/maps/grid/grid_{spam_par$grid}_r_{spam_par$year}_{spam_par$iso3c}.tif"))

# Warp and mask
align_cut_raster(srcfile, reference, dstfile, cutline, crop_to_cutline = F,
                 r = "bilinear", verbose = F, output_Raster = F)

output_map <- align_rasters(unaligned = input, reference = ref_grid, dstfile = ouput,
                            cutline = mask, crop_to_cutline = F,
                            r = "bilinear", verbose = T, output_Raster = T, overwrite = T)

# Maps are in shares of area. We multiply by grid size to create an area map.
r <- raster(dstfile)
a <- area(r)
r_upd <- r*a*100
writeRaster(r_upd, file.path(temp_path, glue("cropland_med_{grid_sel}_{year_sel}_{iso3c_sel}.tif")),
            overwrite = T)

# clean up
rm(srcfile, cutline, dstfile, reference, r, a, r_upd)


############## PROCESS CROPMAX (MAXIMUM AREA) ###############
# Set files
srcfile <- file.path(glob_raw_path, glue("sasam/{continent_sel}/cropland_max_{continent_sel}.tif"))
cutline <- file.path(proc_path, glue("maps/adm/adm_{year_sel}_{iso3c_sel}.shp"))
dstfile <- file.path(temp_path, glue("cropland_max_share_{grid_sel}_{year_sel}_{iso3c_sel}.tif"))
reference <- file.path(proc_path, glue("maps/grid/grid_{grid_sel}_r_{year_sel}_{iso3c_sel}.tif"))

# warp and mask
align_cut_raster(srcfile, reference, dstfile, cutline, crop_to_cutline = F,
                 r = "bilinear", verbose = F, output_Raster = F)

# Maps are in shares of area. We multiply by grid size to create an area map and overwrite shares map.
r <- raster(dstfile)
a <- area(r)
r_upd <- r*a*100
writeRaster(r_upd, file.path(temp_path, glue("cropland_max_{grid_sel}_{year_sel}_{iso3c_sel}.tif")), overwrite = T)

# clean up
rm(srcfile, cutline, dstfile, reference, r, a, r_upd)


############### PROCESS CROPPROB (PROBABILITY, 1 IS HIGHEST) ###############
# Set files
srcfile <- file.path(glob_raw_path, glue("sasam/{continent_sel}/cropland_confidence_level_{continent_sel}.tif"))
cutline <- file.path(proc_path, glue("maps/adm/adm_{year_sel}_{iso3c_sel}.shp"))
dstfile <- file.path(temp_path, glue("cropland_rank_{grid_sel}_{year_sel}_{iso3c_sel}.tif"))
reference <- file.path(proc_path, glue("maps/grid/grid_{grid_sel}_r_{year_sel}_{iso3c_sel}.tif"))

# warp and mask
# Use r = "med" to select the median probability as probability is a categorical variable (1-32)
align_cut_rm_raster(srcfile, reference, dstfile, cutline, crop_to_cutline = F,
                    r = "med", verbose = F, output_Raster = F, srcnodata = "0")

# clean up
rm(temp_path, srcfile, reference, dstfile, cutline)


############### MESSAGE ###############
message("Complete")

