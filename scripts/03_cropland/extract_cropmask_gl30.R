#'========================================================================================================================================
#' Project:  crop_map
#' Subject:  Script to extract ESA crop cover
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "mapview")


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
# adm
adm <- readRDS(file.path(proc_path, paste0("maps/adm/adm_", year_sel, "_", iso3c_sel, ".rds")))

# legend
lc_class <- read_csv(file.path(glob_path, "gl30/gl30_legend.csv")) %>%
  dplyr::select(lc_code, lc)

# grid
grid <- raster(file.path(proc_path, paste0("maps/grid/grid_", grid_sel, "_r_", year_sel, "_", iso3c_sel, ".tif")))
names(grid) <- "gridID"


### APROACH
#' Due to the very high resolution of the land cover maps, we developed to following approach to calculate the share of 
#' cropland in a grid cell
#' (1) We resample the grid map tot he resolution of the lc map
#' (2) We link and convert the two maps into a dataframe
#' (3) We calculate the share of cropland in a grid cell.
#' 
#' The resolution of the GL30 map is so high (30 x 30 m) that we warp it to a 300 x 300 m (10sec) resolution first.


### CREATE 10SEC GRID
r <- raster() # 1 degree raster
r <- disaggregate(r, fact = 120)
grid_10sec <- crop(r, adm)
values(grid_10sec) <- 1:ncell(grid_10sec) # Add ID numbers
names(grid_10sec) <- "gridID" 
grid_10sec <- mask(grid_10sec, adm)
grid_10sec <- disaggregate(grid_10sec, fact = 3)
writeRaster(grid_10sec, file.path(proc_path, paste0("maps/gl30/grid_10sec_", year_sel, "_", iso3c_sel, ".tif")), overwrite = T)


### WARP GL30 TO 10SEC GRID
# Specify input and output files
lc_raw <- file.path(proc_path, paste0("maps/gl30/gl30_raw_", year_sel, "_", iso3c_sel, ".tif"))
lc_10sec <- file.path(proc_path, paste0("maps/gl30/gl30_10sec_", year_sel, "_", iso3c_sel, ".tif"))
grid_10sec <- file.path(proc_path, paste0("maps/gl30/grid_10sec_", year_sel, "_", iso3c_sel, ".tif"))

# Resample
# No need to mask grid (slow) as the grid_30sec is already masked => use align_raster2_f
lc_10sec <- align_raster2_f(lc_raw, grid_10sec, lc_10sec, nThreads = "ALL_CPUS", verbose = T, 
                            output_Raster = T, overwrite = TRUE, r = "near")
names(lc_10sec) <- "GL30"


### COMBINE LC AND GRID AND CALCULATE SHARES
# Combine and calculate shares of cropland for each gridID
# Remove is.na(griID) but keep is.na(lc_code). For the first no data is available. 
# The second are parts of a gridID which are not covered by the lc map and are needed to calculate the correct lc share
lc_stack <- stack(grid_10sec, lc_10sec)
lc_df  <- as.data.frame(rasterToPoints(lc_stack)) %>%
  set_names(c("x", "y", "gridID", "lc_code")) %>%
  filter(!is.na(gridID)) %>%
  unique() %>%
  left_join(lc_class) %>%
  group_by(gridID, lc) %>%
  summarize(n = n())  %>%
  mutate(share = n / sum(n, na.rm = T)) %>%
  dplyr::select(-n) 

# Filter out crop grid cells 
lc_df <- lc_df %>%
  filter(lc == "crops")

# Calculate area
grid_size <- area(grid)
names(grid_size) <- "grid_size"
grid_size <- stack(grid, grid_size)
grid_size <- as.data.frame(rasterToPoints(grid_size)) %>%
  na.omit

lc_df <- left_join(lc_df, grid_size) %>%
  mutate(area = grid_size*share*100) # in ha
summary(lc_df)

# Add source and select variables
lc_df <- lc_df %>%
  mutate(source = "gl30") %>%
  dplyr::select(gridID, area, source) 


### SAVE
temp_path <- file.path(proc_path, "synergistic_cropmask")
dir.create(temp_path, recursive = T, showWarnings = F)

saveRDS(lc_df, file.path(temp_path, paste0("cropmask_gl30_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds")))


### CLEAN UP
rm(adm, grid, grid_size, grid_10sec, lc_10sec, lc_raw, lc_class, lc_df, lc_stack, r, temp_path)
