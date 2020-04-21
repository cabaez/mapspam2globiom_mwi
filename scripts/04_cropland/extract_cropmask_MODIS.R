#'========================================================================================================================================
#' Project:  crop_map
#' Subject:  Script to extract MODIS crop cover
#' Author:   Yating, Michiel van Dijk
#' Contact:  y.ru@cgiar.org, michiel.vandijk@wur.nl
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


# ###Script to preprocess the MODIS files: transforming into GeoTiff and merge into a global map (Only need to run once)
# #convert hdf files to GeoTiff files
# files <- list.files(path=file.path(glob_path, paste0("MODIS/MODIS_", year_sel)), pattern="*.hdf", full.names=T) 
# files_names <- paste0(substr(files, 1, 85 ), "tif")
# for (i in 1:length(files)){
#   sds <- get_subdatasets(files[i])
#   gdal_translate(sds[1], dst_dataset = files_names[i])
# }
# #merge tiles of rasters into one global map
# e <- extent(-180, 180, -90, 90)
# template <- raster(e)
# projection(template) <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"
# writeRaster(template, file=file.path(glob_path, paste0("MODIS/MODIS_", year_sel, "/MODIS", year_sel, ".tif")), format="GTiff")
# mosaic_rasters(gdalfile=files_names,dst_dataset=file.path(glob_path, paste0("MODIS/MODIS_", year_sel, "/MODIS", year_sel, ".tif")),of="GTiff")
# gdalinfo(paste0("MODIS", year_sel, ".tif"))


### LOAD DATA
# Grid
grid <- raster(file.path(proc_path, paste0("maps/grid/grid_", grid_sel, "_r_", year_sel, "_", iso3c_sel, ".tif")))
names(grid) <- "gridID"

# Lc
lc <- raster(file.path(glob_path, paste0("MODIS/MODIS_", year_sel, "/MODIS", year_sel, ".tif")))

# adm
adm <- readRDS(file.path(proc_path, paste0("maps/adm/adm_", year_sel, "_", iso3c_sel, ".rds")))
adm_pro <- spTransform(adm, crs(lc))

#crop lc using adm (cannot use grid as the template because grid and lc have different projections)
lc_raw <- crop(lc, adm_pro)
dir.create(file.path(proc_path, "maps/modis"))
writeRaster(lc_raw, file.path(proc_path, paste0("maps/modis/modis_raw_", year_sel, "_", iso3c_sel, ".tif")), overwrite=TRUE)

# legend
lc_class <- read_csv(file.path(glob_path, "MODIS/MODIS-Legend.csv")) %>%
  dplyr::select(lc_code, lc)

### APROACH
#' Due to the very high resolution of the land cover maps, we developed to following approach to calculate the share of 
#' cropland in a grid cell
#' (1) We resample the grid map tot he resolution of the lc map
#' (2) We link and convert the two maps into a dataframe
#' (3) We calculate the share of cropland in a grid cell.

### RESAMPLE GRID TO LC RESOLUTION
# Specify input and output files
lc_raw_file <- file.path(proc_path, paste0("maps/modis/modis_raw_", year_sel, "_", iso3c_sel, ".tif"))
proj_grid_file <- file.path(proc_path,paste0("maps/modis/grid_modis_raw_", year_sel, "_", iso3c_sel, ".tif"))
grid_file <- file.path(proc_path, paste0("maps/grid/grid_", grid_sel, "_r_", year_sel, "_", iso3c_sel, ".tif"))

# Resample
# No need to mask grid (slow) as the grid_30sec is already masked => use align_raster2_f
grid_lc_res <- align_raster2_f(grid_file, lc_raw_file, proj_grid_file, nThreads = "ALL_CPUS", verbose = T, 
                               output_Raster = T, overwrite = TRUE, r = "near")
names(grid_lc_res) <- "gridID"


### COMBINE LC AND GRID AND CALCULATE SHARES
# Combine and calculate shares of cropland for each gridID
# Remove is.na(griID) but keep is.na(lc_code). For the first no data is available. 
# The second are parts of a gridID which are not covered by the lc map and are needed to calculate the correct lc share
lc_stack <- stack(grid_lc_res, lc_raw)

lc_df  <- as.data.frame(rasterToPoints(lc_stack)) %>%
  set_names(c("x", "y", "gridID", "lc_code")) %>%
  filter(!is.na(gridID)) %>%
  unique() %>% #Why using the unique() function? In this case it does not filter out anything?
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
  mutate(source = "modis") %>%
  dplyr::select(gridID, area, source) 


### SAVE
temp_path <- file.path(proc_path, "synergistic_cropmask")
dir.create(temp_path, recursive = T, showWarnings = F)

saveRDS(lc_df, file.path(temp_path, paste0("cropmask_modis_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds")))


### CLEAN UP
rm(grid, grid_size, grid_lc_res, lc_class, lc_df, lc_raw, lc_stack, temp_path, lc, adm, adm_pro)