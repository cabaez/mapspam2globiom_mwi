#'========================================================================================================================================
#' Project:  crop_map
#' Subject:  Script to create synergistic crop map
#' Author:   Michiel van Dijk, Yating
#' Contact:  michiel.vandijk@wur.nl, y.ru@cgiar.org
#'========================================================================================================================================


### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "car", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "mapview")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(viewer = NULL) # to force mapview to open in the browser and not in RStudio


### LOAD DATA
# Adm
adm <- readRDS(file.path(proc_path, paste0("maps/adm/adm_", year_sel, "_", iso3c_sel, ".rds")))

# Cropmask from different sources
gfsad <- readRDS(file.path(proc_path, paste0("synergistic_cropmask/cropmask_gfsad_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds")))
modis <- readRDS(file.path(proc_path, paste0("synergistic_cropmask/cropmask_modis_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds")))
esa <- readRDS(file.path(proc_path, paste0("synergistic_cropmask/cropmask_esa_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds")))

# Grid
grid <- raster(file.path(proc_path, paste0("maps/grid/grid_", grid_sel, "_r_", year_sel, "_", iso3c_sel, ".tif")))
names(grid) <- "gridID"

# Scoring table
st_raw <- read_excel(file.path(mappings_path, paste0("synergistic_cropmask_score_table.xlsx")), sheet = "score3")


### VISUALISE
# grid_df
grid_df <- as.data.frame(rasterToPoints(grid))

# gfsad raster
gfsad_r <- rasterFromXYZ(left_join(grid_df, gfsad) %>% dplyr::select(x, y, area))
crs(gfsad_r) <- crs(adm)

# glc2000 raster
modis_r <- rasterFromXYZ(left_join(grid_df, modis) %>% dplyr::select(x, y, area))
crs(modis_r) <- crs(adm)

# esa raster
esa_r <- rasterFromXYZ(left_join(grid_df, esa) %>% dplyr::select(x, y, area))
crs(esa_r) <- crs(adm)

pal = mapviewPalette("mapviewSpectralColors")
pal2 = mapviewPalette("mapviewTopoColors")

mapview(gfsad_r) + 
  mapview(modis_r, col.regions = pal(100)) + 
  mapview(esa_r, col.regions = pal2(100)) + 
  mapview(adm, alpha.regions = 0)


### ADD SCORE TO DATA
# Create combined codes in scoring table
st <- st_raw %>%
  gather(source, code_digit, -agreement, -lc_rank) %>%
  mutate(code = ifelse(code_digit == 1, source, 0)) %>%
  group_by(agreement, lc_rank) %>%
  summarize(code = paste0(code, collapse = "-"))

# Code combinations. NOTE ORDER FACTOR SHOULD BE THE SAME AS SCORING TABLE!
lc_code <- bind_rows(gfsad, modis, esa) %>%
  mutate(code = source,
         source = factor(source, levels = c("gfsad", "modis", "esa"))) %>%
  dplyr::select(-area) %>%
  spread(source, code, fill = "0") %>%
  gather(source, code, -gridID) %>%
  group_by(gridID) %>%
  summarize(code = paste0(code, collapse = "-")) %>%
  left_join(st)
summary(lc_code)
table(lc_code$lc_rank)


### CALCULATE MEAN AREA PER GRID CELL AND COMBINE
lc_area <-  bind_rows(gfsad, modis, esa) %>%
  group_by(gridID) %>%
  summarize(lc1 = mean(area, na.rm = T),
            lc_max = max(area, na.rm = T))
summary(lc_area)  

# combine
lc_df <- left_join(lc_code, lc_area) %>%
  filter(!is.na(lc1))
summary(lc_df)


### CREATE RASTER FILES
# syneristic cropmask
lc <- rasterFromXYZ(left_join(grid_df, lc_df) %>% dplyr::select(x, y, lc1))
crs(lc) <- crs(adm)

# syneristic cropmask max
lc_max <- rasterFromXYZ(left_join(grid_df, lc_df) %>% dplyr::select(x, y, lc_max))
crs(lc_max) <- crs(adm)

# syneristic cropmask rank
lc_rank <- rasterFromXYZ(left_join(grid_df, lc_df) %>% dplyr::select(x, y, lc_rank))
crs(lc_rank) <- crs(adm)

mapview(lc) +
  mapview(lc_max) +
  mapview(lc_rank)


### SAVE
temp_path <- file.path(proc_path, paste0("maps/cropmask"))
dir.create(temp_path, recursive = T, showWarnings = F)

# lc_df
saveRDS(lc_df, file.path(proc_path, paste0("synergistic_cropmask/lc_df_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds")))

# lc
#writeRaster(lc, file.path(temp_path, paste0("lc_", grid_sel, "_", year_sel, "_", iso3c_sel, ".tif")), overwrite = T)
writeRaster(lc, file.path(temp_path, paste0("cropratio_", grid_sel, "_", year_sel, "_", iso3c_sel, ".tif")), overwrite = T)

# lc_max
#writeRaster(lc_max, file.path(temp_path, paste0("lc_max_", grid_sel, "_", year_sel, "_", iso3c_sel, ".tif")), overwrite = T)
writeRaster(lc_max, file.path(temp_path, paste0("cropmax_", grid_sel, "_", year_sel, "_", iso3c_sel, ".tif")), overwrite = T)

# lc_rank
#writeRaster(lc_rank, file.path(temp_path, paste0("lc_rank_", grid_sel, "_", year_sel, "_", iso3c_sel, ".tif")), overwrite = T)
writeRaster(lc_rank, file.path(temp_path, paste0("cropcon_", grid_sel, "_", year_sel, "_", iso3c_sel, ".tif")), overwrite = T)

### CLEAN Up
rm(lc, lc_area, lc_code, lc_df, lc_max, lc_rank, st, st_raw,
   adm, esa, esa_r, modis, modis_r, gfsad, gfsad_r, grid, grid_df)
