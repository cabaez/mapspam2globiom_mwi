#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Code to process GMIA
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### MESSAGE ###############
message("\nRunning 03_spatial_data\\03_select_gmia.r")


############### SET UP ###############
# Install and load pacman package that automatically installs R packages if not available
if("pacman" %in% rownames(installed.packages()) == FALSE) install.packages("pacman")
library(pacman)

# Load key packages
p_load("mapspam2globiom", "tidyverse", "readxl", "stringr", "here", "scales", "glue", "gdalUtils", "raster", "sf")

# Set root
root <- here()

# R options
options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits


############### LOAD DATA ###############
load_data(c("adm_map"), param)

# Raw gmia file
gmia_raw <- raster(file.path(param$raw_path,"gmia/gmia_v5_aei_ha.asc"))


############### PROCESS ###############
# The crs of the gmia (WGS 84) is missing and the file is not in tif. We add and save the map
crs(gmia_raw) <- "+proj=longlat +datum=WGS84 +no_defs"
if(!file.exists(file.path(param$raw_path, "gmia/gmia_v5_aei_ha_crs.tif"))){
  writeRaster(gmia_raw, file.path(param$raw_path, "gmia/gmia_v5_aei_ha_crs.tif"), 
              overwrite = T)
}

# GMIA presents the area in ha of area equiped for irrigation at 5 arcmin resolution.
# Hence, we cannot simply warp to higher resolutions (e.g. 30 arcmin).
# We calculate the share of irrigated area first and then warp.
# In case the resolution is 5 arcmin, we only clip the map to the country borders

if(param$res == "5min") {
  # Set files
  mask <- file.path(param$spam_path,
                    glue("processed_data/maps/adm/adm_map_wsg_84_{param$year}_{param$iso3c}.shp"))
  input <- file.path(param$raw_path,
                     glue("gmia/gmia_v5_aei_ha_crs.tif"))
  output <- file.path(param$spam_path,
                      glue("processed_data/maps/irrigated_area/gmia_temp_{param$res}_{param$year}_{param$iso3c}.tif"))
  
  # Warp and mask
  # Use crop to cutline to crop.
  # TODO probably does not work at 30sec!!
  gmia_temp <- gdalUtils::gdalwarp(srcfile = input, dstfile = output,
                                  cutline = mask, crop_to_cutline = T, 
                                  r = "bilinear", verbose = F, output_Raster = T, overwrite = T)
  names(gmia_temp) <- "gmia"
  
  # Calculate share of irrigated area
  gmia_temp <- gmia_temp/(area(gmia_temp)*100)
  plot(gmia_temp)
  
  # overwrite
  writeRaster(gmia_temp, file.path(param$spam_path,
                                   glue("processed_data/maps/irrigated_area/gmia_temp_{param$res}_{param$year}_{param$iso3c}.tif"))
              ,
  overwrite = T)

  # Warp and mask to model resolution
  # Set files
  grid <- file.path(param$spam_path,
                    glue("processed_data/maps/grid/grid_{param$res}_{param$year}_{param$iso3c}.tif"))
  mask <- file.path(param$spam_path,
                    glue("processed_data/maps/adm/adm_map_{param$year}_{param$iso3c}.shp"))
  input <- file.path(param$spam_path,
                     glue("processed_data/maps/irrigated_area/gmia_temp_{param$res}_{param$year}_{param$iso3c}.tif"))
  output <- file.path(param$spam_path,
                      glue("processed_data/maps/irrigated_area/gmia_{param$res}_{param$year}_{param$iso3c}.tif"))
  
  # Warp and mask
  output_map <- align_rasters(unaligned = input, reference = grid, dstfile = output,
                              cutline = mask, crop_to_cutline = F, 
                              r = "bilinear", verbose = F, output_Raster = T, overwrite = T)
  plot(output_map)
  }

if(param$res == "30sec") {
  # Crop, making sure it also includes grid cells, which center is outside the polygon
  # by adding snap = "out")
  gmia_temp <- crop(gmia_raw, adm_loc, snap = "out")
  
  # Set 0 values to NA
  gmia_temp <- reclassify(gmia_temp, cbind(0, NA))
  
  # Calculate share of irrigated area
  gmia_temp <- gmia_temp/(area(gmia_temp)*100)
  plot(gmia_temp)
  
  # Save
  writeRaster(gmia_temp, file.path(param$spam_path, 
    glue("processed_data/maps/irrigated_area/gmia_temp_{param$year}_{param$iso3c}.tif")),
    overwrite = T)
  
  # Set files
  grid <- file.path(param$spam_path,
    glue("processed_data/maps/grid/grid_{param$res}_{param$year}_{param$iso3c}.tif"))
  mask <- file.path(param$spam_path,
    glue("processed_data/maps/adm/adm_loc_{param$year}_{param$iso3c}.shp"))
  input <- file.path(param$spam_path,
    glue("processed_data/maps/irrigated_area/gmia_temp_{param$year}_{param$iso3c}.tif"))
  output <- file.path(param$spam_path,
    glue("processed_data/maps/irrigated_area/gmia_{param$res}_{param$year}_{param$iso3c}.tif"))
  
  # Warp and mask
  output_map <- align_rasters(unaligned = input, reference = grid, dstfile = output,
                              cutline = mask, crop_to_cutline = F, 
                              r = "bilinear", verbose = F, output_Raster = T, overwrite = T)
  plot(output_map)
}


############### CLEAN UP ###############
rm(adm_loc, gmia_raw, gmia_temp)


############### MESSAGE ###############
message("Complete")

