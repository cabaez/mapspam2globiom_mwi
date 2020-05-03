#'========================================================================================================================================
#' Project:  crop_map
#' Subject:  Script to combine updated simu spam maps in one file
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "sf", "mapview")
# Additional packages
p_load("WDI", "countrycode", "plotKML", "sf", "gdxrrw", "viridis")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(viewer = NULL) # to force mapview to open in the browser and not in RStudio


### FUNCTIONS
# source gdx functions
source(file.path(root, "scripts/support/R2GDX.r"))


### GET DATA PATH
source(file.path(root, "scripts/support/get_data_path.R"))


### LINK GAMS LIBRARIES
igdx(gams_path)


### LOAD DATA
# crop distribution
mwi_cd <- readRDS(file.path(crop_map_path, "crop_area_spam_2010_MWI.rds"))
moz_cd <- readRDS(file.path(crop_map_path, "crop_area_spam_2010_MOZ.rds"))
zmb_cd <- readRDS(file.path(crop_map_path, "crop_area_spam_2010_ZMB.rds"))
zwe_cd <- readRDS(file.path(crop_map_path, "crop_area_spam_2010_ZWE.rds"))
nam_cd <- readRDS(file.path(crop_map_path, "crop_area_spam_2010_NAM.rds"))
bwa_cd <- readRDS(file.path(crop_map_path, "crop_area_spam_2010_BWA.rds"))
ago_cd <- readRDS(file.path(crop_map_path, "crop_area_spam_2010_AGO.rds"))

# landcover
mwi_lc <- readRDS(file.path(crop_map_path, "landcover_2010_MWI.rds"))
moz_lc <- readRDS(file.path(crop_map_path, "landcover_2010_MOZ.rds"))
zmb_lc <- readRDS(file.path(crop_map_path, "landcover_2010_ZMB.rds"))
zwe_lc <- readRDS(file.path(crop_map_path, "landcover_2010_ZWE.rds"))
nam_lc <- readRDS(file.path(crop_map_path, "landcover_2010_NAM.rds"))
bwa_lc <- readRDS(file.path(crop_map_path, "landcover_2010_BWA.rds"))
ago_lc <- readRDS(file.path(crop_map_path, "landcover_2010_AGO.rds"))

### COMBINE
all_cd <- bind_rows(mwi_cd, moz_cd, zmb_cd, zwe_cd, nam_cd, bwa_cd, ago_cd) %>%
  dplyr::select(-SimUID)
all_lc <- bind_rows(mwi_lc, moz_lc, zmb_lc, zwe_lc, nam_lc, bwa_lc, ago_lc)


### SAVE
# landcover
lc_all_gdx <- para_gdx(all_lc, 
                                 c("LC_TYPES_EPIC", "ALLCOUNTRY", "ColRow30", "AltiClass", "SlpClass", "SoilClass"), 
                                 "LANDCOVER", "update landcover")

wgdx(file.path(crop_map_path, paste0("landcover_2010.gdx")),
     lc_all_gdx)

write_csv(all_lc, file.path(crop_map_path, paste0("landcover_2010.csv")))


# cd
all_cd_final_gdx <- para_gdx(all_cd, 
                                 c("system", "globiom_crop", "ALLCOUNTRY", "ColRow30", "AltiClass", "SlpClass", "SoilClass"), 
                                 "CROPAREA_SPAM", "update spam")

wgdx(file.path(crop_map_path, paste0("croparea_spam_2010.gdx")),
     all_cd_final_gdx)

write_csv(all_cd, file.path(crop_map_path, paste0("crop_area_spam_2010.csv")))
