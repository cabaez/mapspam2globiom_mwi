#'========================================================================================================================================
#' Project:  crop_map
#' Subject:  Code to compare raw statistics: stat, system, gmia, gia
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

### PACKAGES
if(!require(pacman)) install.packages("pacman")
# Key packages
p_load("tidyverse", "readxl", "stringr", "scales", "RColorBrewer", "rprojroot")
# Spatial packages
p_load("rgdal", "ggmap", "raster", "rasterVis", "rgeos", "sp", "mapproj", "maptools", "proj4", "gdalUtils", "mapview")
# Additional packages
p_load("WDI", "countrycode", "plotKML", "sf")


### SET ROOT AND WORKING DIRECTORY
root <- find_root(is_rstudio_project)


### R SETTINGS
options(scipen=999) # surpress scientific notation
options("stringsAsFactors"=FALSE) # ensures that characterdata that is loaded (e.g. csv) is not turned into factors
options(digits=4)
options(viewer = NULL) # to force mapview to open in the browser and not in RStudio



### LOAD DATA
# adm
adm <- readRDS(file.path(proc_path, paste0("maps/adm/adm_", year_sel, "_", iso3c_sel, ".rds")))
adm1 <- readRDS(file.path(proc_path, paste0("maps/adm/adm1_", year_sel, "_", iso3c_sel, ".rds")))

# System
lu_sy_raw <- readRDS(file.path(proc_path, paste0("Agricultural_statistics/lu_sy_", year_sel, "_", iso3c_sel, ".rds"))) 

# Agricultural statistics
lu_adm_raw <- readRDS(file.path(proc_path, paste0("agricultural_statistics/lu_adm_", year_sel, "_", iso3c_sel, ".rds"))) 

# Aquastat
lu_aqua_raw <- readRDS(file.path(proc_path, paste0("agricultural_statistics/aquastat_ir_crops_", year_sel, "_", iso3c_sel, ".rds"))) 

# Grid
grid <- raster(file.path(proc_path, paste0("maps/grid/grid_", grid_sel, "_r_", year_sel, "_", iso3c_sel, ".tif")))
names(grid) <- "gridID"

# lc data
lc_raw <- raster(file.path(proc_path, paste0("maps/cropmask/cropratio_", grid_sel, "_", year_sel, "_", iso3c_sel, ".tif"))) 
names(lc_raw) <- "lc_area"
lc_rank_raw <- raster(file.path(proc_path, paste0("maps/cropmask/cropcon_", grid_sel, "_", year_sel, "_", iso3c_sel, ".tif"))) 
names(lc_rank_raw) <- "lc_rank"
lc_max_raw <- raster(file.path(proc_path, paste0("maps/cropmask/cropmax_", grid_sel, "_", year_sel, "_", iso3c_sel, ".tif"))) 
names(lc_max_raw) <- "lc_area_max"

# Adm_r
adm_r <- readRDS(file.path(proc_path, paste0("maps/adm/adm_r_", grid_sel, "_", year_sel, "_", iso3c_sel, ".rds"))) 

# gia
gia_raw <- raster(file.path(proc_path, paste0("maps/gia/gia_", grid_sel, "_", year_sel, "_", iso3c_sel, ".tif"))) 
names(gia_raw) <- "value"


### PREPARE DATA
# gia df
gia_df <-   as.data.frame(rasterToPoints(stack(gia_raw, grid))) %>%
  dplyr::select(-x, -y) %>%
  left_join(adm_r) %>%
  na.omit

# Create grid area 
grid_size <- area(grid)
grid_size <- grid_size * 100 # in ha
names(grid_size) <- "grid_size"

# Create df of lc map
lc <- stack(grid, lc_raw, lc_rank_raw, lc_max_raw, grid_size) %>%
  as.data.frame(rasterToPoints(.)) %>%
  left_join(adm_r) %>%
  mutate(lc = lc_area,
         lc_max = lc_area_max) %>%
  filter(!is.na(lc_area), !is.na(lc_area_max), !is.na(gridID), !is.na(lc_rank))


#--------------------------------------------------------------------------------------------------
### COMPARE SYSTEMS AT ADM0 FOR CROPS
# combine data
comp_adm0_system_crop <- bind_rows(
  lu_sy_raw %>%
    dplyr::select(value, crop, adm, adm_level, system) %>%
    filter(adm_level == 0) %>%
    mutate(source = "stat"),
  lu_aqua_raw %>%
    group_by(crop) %>%
    filter(crop != "total") %>%
    mutate(source = paste("aquastat", year, sep = "_"))) %>%
  filter(value != 0) %>%
  ungroup %>%
  group_by(crop) %>%
  mutate(n = length(unique(source))) %>%
  filter(n >1)

comp_adm0_system_crop %>%
  ggplot() +
  geom_col(aes(x = source, y = value, fill = system)) +
  labs(title = paste0("Land use comparison by crop in ", iso3c_sel), 
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~crop, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#--------------------------------------------------------------------------------------------------
#### COMPARISON OF LC AND LU

### ADM0
comp_adm0_lu_lc <- bind_rows(
  lu_adm_raw %>%
    dplyr::select(value, crop, adm, adm_level) %>%
    filter(adm_level == 0) %>%
    summarize(value = sum(value, na.rm = T)) %>%
    mutate(source = "stat"),
  lc %>%
    rename(adm = adm0) %>%
    group_by(adm) %>%
    summarize(value = sum(lc, na.rm = T)) %>%
    mutate(source = "lc"),
  lc %>%
    rename(adm = adm0) %>%
    group_by(adm) %>%
    summarize(value = sum(lc_max, na.rm = T)) %>%
    mutate(source = "lc_max"),
  lc %>%
    rename(adm = adm0) %>%
    group_by(adm) %>%
    summarize(value = sum(grid_size, na.rm = T)) %>%
    mutate(source = "grid_size"))
  
comp_adm0_lu_lc %>%
  ggplot() +
  geom_col(aes(x = source, y = value, fill = source)) +
  labs(title = paste0("Land use and land cover comparison at adm0 in ", iso3c_sel), 
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) 


### ADM1
comp_adm1_lu_lc <- bind_rows(
  lu_adm_raw %>%
    dplyr::select(value, crop, adm, adm_level) %>%
    filter(adm_level == 1) %>%
    group_by(adm) %>%
    summarize(value = sum(value, na.rm = T)) %>%
    mutate(source = "stat"),
  lc %>%
    rename(adm = adm1) %>%
    group_by(adm) %>%
    summarize(value = sum(lc, na.rm = T)) %>%
    mutate(source = "lc"),
  lc %>%
    rename(adm = adm1) %>%
    group_by(adm) %>%
    summarize(value = sum(lc_max, na.rm = T)) %>%
    mutate(source = "lc_max"),
  lc %>%
    rename(adm = adm1) %>%
    group_by(adm) %>%
    summarize(value = sum(grid_size, na.rm = T)) %>%
    mutate(source = "grid_size"))

comp_adm1_lu_lc %>%
  ggplot() +
  geom_col(aes(x = source, y = value, fill = source)) +
  labs(title = paste0("Land use and land cover comparison at adm1 in ", iso3c_sel), 
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~adm, scales = "free")

# adm1 where lc < lu
adm1_check <- comp_adm1_lu_lc %>%
  spread(source, value) %>%
  mutate(dif = lc-stat) %>%
  filter(dif < 0)
adm1_check

# plot again
comp_adm1_lu_lc %>%
  filter(adm %in% adm1_check$adm) %>%
  ggplot() +
  geom_col(aes(x = source, y = value, fill = source)) +
  labs(title = paste0("Land use and land cover comparison at adm1 in ", iso3c_sel), 
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~adm, scales = "free")

# adm1 where lc_max < lu
adm1_max_check <- comp_adm1_lu_lc %>%
  spread(source, value) %>%
  mutate(dif = lc_max-stat) %>%
  filter(dif < 0)
adm1_max_check

# plot again
comp_adm1_lu_lc %>%
  filter(adm %in% adm1_max_check$adm) %>%
  ggplot() +
  geom_col(aes(x = source, y = value, fill = source)) +
  labs(title = paste0("Land use and land cover comparison at adm1 in ", iso3c_sel), 
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~adm, scales = "free")

# Find out which crops are causing problems
adm1_problem <- lu_adm_raw %>%
  dplyr::select(value, crop, adm, adm_level) %>%
  filter(adm %in% adm1_max_check$adm) 

adm1_problem %>%
  ggplot() +
  geom_col(aes(x = crop, y = value, fill = crop), position = "stack") +
  labs(title = paste0("Land use and land cover comparison at adm1 in ", iso3c_sel), 
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~adm, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


### ADM2
comp_adm2_lu_lc <- bind_rows(
  lu_adm_raw %>%
    dplyr::select(value, crop, adm, adm_level) %>%
    filter(adm_level == 2) %>%
    group_by(adm) %>%
    summarize(value = sum(value, na.rm = T)) %>%
    mutate(source = "stat"),
  lc %>%
    rename(adm = adm2) %>%
    group_by(adm) %>%
    summarize(value = sum(lc, na.rm = T)) %>%
    mutate(source = "lc"),
  lc %>%
    rename(adm = adm2) %>%
    group_by(adm) %>%
    summarize(value = sum(lc_max, na.rm = T)) %>%
    mutate(source = "lc_max"),
  lc %>%
    rename(adm = adm2) %>%
    group_by(adm) %>%
    summarize(value = sum(grid_size, na.rm = T)) %>%
    mutate(source = "grid_size"))

comp_adm2_lu_lc %>%
  ggplot() +
  geom_col(aes(x = source, y = value, fill = source)) +
  labs(title = paste0("Land use and land cover comparison at adm2 in ", iso3c_sel), 
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~adm, scales = "free")

# adm2 where lc < lu
adm2_check <- comp_adm2_lu_lc %>%
  spread(source, value) %>%
  mutate(dif = lc-stat) %>%
  filter(dif < 0)
adm2_check

# plot again
comp_adm2_lu_lc %>%
  filter(adm %in% adm2_check$adm) %>%
  ggplot() +
  geom_col(aes(x = source, y = value, fill = source)) +
  labs(title = paste0("Land use and land cover comparison at adm2 in ", iso3c_sel), 
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~adm, scales = "free")

# adm2 where lc_max < lu
adm2_max_check <- comp_adm2_lu_lc %>%
  spread(source, value) %>%
  mutate(dif = lc_max-stat) %>%
  filter(dif < 0)
adm2_max_check

# plot again
comp_adm2_lu_lc %>%
  filter(adm %in% adm2_max_check$adm) %>%
  ggplot() +
  geom_col(aes(x = source, y = value, fill = source)) +
  labs(title = paste0("Land use and land cover comparison at adm2 in ", iso3c_sel), 
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~adm, scales = "free")

# Find out which crops are causing problems
adm2_problem <- lu_adm_raw %>%
  dplyr::select(value, crop, adm, adm_level) %>%
  filter(adm %in% adm2_max_check$adm) 

adm2_problem %>%
  ggplot() +
  geom_col(aes(x = crop, y = value, fill = crop), position = "stack") +
  labs(title = paste0("Land use and land cover comparison at adm2 in ", iso3c_sel), 
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~adm, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


#--------------------------------------------------------------------------------------------------
#### COMPARISON I SYSTEM

### LIST I CROPS IN STATS
# I
I_crop <- lu_sy_raw %>% 
  filter(system %in% c("I"), value > 0) %>%
  dplyr::select(crop, system) %>%
  as.data.frame() %>%
  unique

# Share of crop irrigated
I_crop_share <- lu_sy_raw %>%
  filter(adm_level == 0) %>%
  group_by(crop) %>%
  mutate(total = sum(value, na.rm = T),
         share = value/total * 100) %>%
  filter(system == "I") %>%
  arrange(desc(share))


### COMPARE TOTAL LU AT NATIONAL LEVEL FOR SYSTEM I
# Assume for now that cl_osm and fl_osm are fully irrigated
# combine data
comp_adm0_I <- bind_rows(
  lu_sy_raw %>%
    dplyr::select(value, crop, adm, adm_level, system) %>%
    filter(adm_level == 0, system %in% c("I")) %>%
    summarize(value = sum(value, na.rm = T)) %>%
    mutate(source = "stat"),
  gia_df %>%
    summarize(value = sum(value, na.rm = T)) %>%
    mutate(source = "gia"),
  lu_aqua_raw %>%
    filter(variable == "Total harvested irrigated crop area (full control irrigation)") %>%
    group_by(crop) %>%
    mutate(source = paste("aquastat", year, sep = "_")))

comp_adm0_I %>%
  ggplot() +
  geom_col(aes(x = source, y = value, fill = source)) +
  labs(title = paste0("Land use comparison for irrigation in ", iso3c_sel),
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) 


### COMPARE LU BY CROP AT NATIONAL LEVEL FOR SYSTEM I
# combine data
comp_adm0_I_crop <- bind_rows(
  lu_sy_raw %>%
    dplyr::select(adm, crop, system, value, adm_level) %>%
    filter(adm_level == 0, system %in% c("I")) %>%
    mutate(source = "stat"),
  lu_aqua_raw %>%
    group_by(crop) %>%
    filter(crop != "total") %>%
    ungroup() %>%
    mutate(source = paste("aquastat", year, sep = "_"))) %>%
  filter(value != 0) %>%
  ungroup 

comp_adm0_I_crop %>%
  ggplot() +
  geom_col(aes(x = source, y = value, fill = source)) +
  labs(title = paste0("Land use comparison by crop for irrigation system in ", iso3c_sel),
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~crop, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


