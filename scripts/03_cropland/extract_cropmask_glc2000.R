#'========================================================================================================================================
#' Project:  crop_map
#' Subject:  Script to extract GLC2000 crop cover
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "mapview")
# Additional packages
p_load("countrycode")

### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)


### SOURCE FUNCTIONS
source(file.path(root, "Code/general/support_functions.r"))


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(viewer = NULL) # to force mapview to open in the browser and not in RStudio


### LOAD DATA
# Adm
adm <- readRDS(file.path(proc_path, paste0("maps/adm/adm_", year_sel, "_", iso3c_sel, ".rds")))

# Land cover
lc_raw <- raster(file.path(proc_path, paste0("maps/glc2000/glc2000_raw_", year_sel, "_", iso3c_sel, ".tif")))
names(lc_raw) <- "GLC2000"

# Legend
lc_class <- read_csv(file.path(glob_path, "GLC2000/glc2000_legend.csv")) %>%
  dplyr::select(lc_code, lc)

# Grid
grid <- raster(file.path(proc_path, paste0("maps/grid/grid_", grid_sel, "_r_", year_sel, "_", iso3c_sel, ".tif")))
names(grid) <- "gridID"


### APROACH
#' Due to the very high resolution of the land cover maps, we developed to following approach to calculate the share of 
#' cropland in a grid cell
#' (1) We resample the grid map tot he resolution of the lc map
#' (2) We link and convert the two maps into a dataframe
#' (3) We calculate the share of cropland in a grid cell.
#' 
#' In case of GLC2000, the resolution is already at the 1x1 km so we only need to harmonize the grid and lc maps and add the area


### RESAMPLE MAP TO GRID_SEL
# Specify input and output files
lc_raw_file <- file.path(proc_path, paste0("maps/glc2000/glc2000_raw_", year_sel, "_", iso3c_sel, ".tif"))
output_file <- file.path(proc_path, paste0("maps/glc2000/glc2000_", grid_sel, "_", year_sel, "_", iso3c_sel, ".tif"))
grid_file <- file.path(proc_path, paste0("maps/grid/grid_", grid_sel, "_r_", year_sel, "_", iso3c_sel, ".tif"))

# Resample
lc_grid <- align_raster2_f(lc_raw_file, grid_file, output_file, nThreads = "ALL_CPUS", verbose = T, 
                           output_Raster = T, overwrite = TRUE, r = "near")
names(lc_grid) <- "lc_code"


### COMBINE LC AND GRID 
# Calculate area
grid_size <- area(grid)
names(grid_size) <- "grid_size"
grid_size <- stack(grid, grid_size)
grid_size <- as.data.frame(rasterToPoints(grid_size)) %>%
  na.omit

# No need to calculate shares as resolution is 1kmx1km
# Remove non-cropland gridcells
# Combine and calculate shares
lc_stack <- stack(grid, lc_grid)
lc_df <- as.data.frame(rasterToPoints(lc_stack)) %>%
  na.omit() %>% # remove approx 1000 border cells where gridID and/or lc is NA
  unique() %>%
  left_join(lc_class) %>%
  filter(lc == "crops") %>%
  left_join(grid_size) %>%
  mutate(area = grid_size*100, 
         source = "glc2000") %>% # in ha
  dplyr::select(-grid_size, -lc_code, -x, -y, -lc)
summary(lc_df)


### SAVE
temp_path <- file.path(proc_path, "synergistic_cropmask")
dir.create(temp_path, showWarnings = F, recursive = T)

saveRDS(lc_df, file.path(temp_path, paste0("cropmask_glc2000_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds")))

### CLEAN UP
rm(adm, grid, grid_size, lc_raw, lc_grid, lc_class, lc_df, lc_stack, r, temp_path)
