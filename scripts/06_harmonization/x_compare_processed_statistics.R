#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Compare cropland and statistics to identify possible problems
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### SET UP ###############
# Load pacman for p_load
if(!require(pacman)){
  install.packages("pacman")
  library(pacman) 
} else {
  library(pacman)
}

# Load key packages
p_load("tidyverse", "readxl", "stringr", "here", "scales", "glue", "countrycode")

# Set root
root <- here()

# R options
options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits

### SOURCE FUNCTIONS
# TO_UPDATE
source(file.path(root, "Code/general/mapspam_functions.r"))


############### LOAD DATA ###############
# Adm
adm <- readRDS(file.path(proc_path, glue("maps/adm/adm_{year_sel}_{iso3c_sel}.rds")))

# Adm_r
adm_r <- readRDS(file.path(proc_path, glue("maps/adm/adm_r_{grid_sel}_{year_sel}_{iso3c_sel}.rds"))) 

# gia
gia_raw <- raster(file.path(proc_path, glue("maps/irrigation/gia_{grid_sel}_{year_sel}_{iso3c_sel}.tif"))) 
names(gia_raw) <- "value"

# gmia
gmia_raw <- raster(file.path(proc_path, glue("maps/irrigation/gmia_{grid_sel}_{year_sel}_{iso3c_sel}.tif"))) 
names(gmia_raw) <- "value"

# # gmia 01
# gmia_01_raw <- raster(file.path(proc_path, glue("maps/irrigation/gmia_01_{grid_sel}_{year_sel}_{iso3c_sel}.tif"))) 
# names(gmia_01_raw) <- "value"

# Grid
grid <- raster(file.path(proc_path, glue("maps/grid/grid_{grid_sel}_r_{year_sel}_{iso3c_sel}.tif")))
names(grid) <- "gridID"

# Farming system
pa_fs_raw <- readRDS(file.path(proc_path, glue("Agricultural_statistics/pa_fs_{year_sel}_{iso3c_sel}.rds"))) 

# Physical area
pa_raw <- readRDS(file.path(proc_path, glue("Agricultural_statistics/pa_{year_sel}_{iso3c_sel}.rds"))) 

# lc data
lc_raw <- raster(file.path(proc_path, glue("maps/cropland/cropland_med_{grid_sel}_{year_sel}_{iso3c_sel}.tif"))) 
names(lc_raw) <- "lc_area"
lc_rank_raw <- raster(file.path(proc_path, glue("maps/cropland/cropland_rank_{grid_sel}_{year_sel}_{iso3c_sel}.tif"))) 
names(lc_rank_raw) <- "lc_rank"
lc_max_raw <- raster(file.path(proc_path, glue("maps/cropland/cropland_max_{grid_sel}_{year_sel}_{iso3c_sel}.tif"))) 
names(lc_max_raw) <- "lc_area_max"


########## PROCESS ##########
# Create df of gia
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


# combine data at adm0
comp_adm0_I <- bind_rows(
  pa_fs_raw %>%
    dplyr::select(pa, crop, adm_name, adm_code, adm_level, system) %>%
    filter(adm_level == 0, system %in% c("I")) %>%
    summarize(value = sum(pa, na.rm = T)) %>%
    mutate(category = "stat",
           source = "stat"),
  # lu_det_all_raw %>%
  #   as.data.frame() %>%
  #   filter(system == "I") %>%
  #   summarize(value = sum(value, na.rm = T)) %>%
  #   mutate(source = "det_all"),
  # lu_det_raw %>%
  #   as.data.frame() %>%
  #   filter(system == "I") %>%
  #   group_by(category) %>%
  #   summarize(value = sum(area, na.rm = T)) %>%
  #   mutate(source = "det"),
  gia_df %>%
    summarize(value = sum(gia, na.rm = T)) %>%
    mutate(category = "gia",
           source = "gia")
  # gmia_df %>%
  #   summarize(value = sum(gmia, na.rm = T)) %>%
  #   mutate(category = "gmia",
  #          source = "gmia")
)

comp_adm0_I %>%
  ggplot() +
  geom_col(aes(x = source, y = value, fill = source)) +
  labs(title = paste0("Land use comparison for irrigation in ", iso3c_sel),
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) 



#--------------------------------------------------------------------------------------------------
### LIST I CROPS IN STATS
# Irrigated crops
pa_fs_raw %>%
  filter(adm_level == 0) %>%
  group_by(crop) %>%
  mutate(total = sum(pa, na.rm = T),
         share = pa/total * 100) %>%
  filter(system == "I") %>%
  dplyr::select(crop, irrigated_area = pa, total, share) %>%
  filter(share >0) %>%
  arrange(desc(irrigated_area))


### COMPARE TOTAL LU AT NATIONAL LEVEL FOR H AND I
# Assume for now that det_all is fully irrigated
# combine data
comp_adm0_sy <- bind_rows(
  pa_fs_raw %>%
    dplyr::select(pa, crop, adm, adm_level, system) %>%
    filter(adm_level == 0, system %in% c("I", "H")) %>%
    group_by(system) %>%
    summarize(value = sum(pa, na.rm = T)) %>%
    mutate(source = "stat_pa"),
  # lu_sy_raw %>%
  #   dplyr::select(value_ha, crop, adm, adm_level, system) %>%
  #   filter(adm_level == 0, system %in% c("I", "H")) %>%
  #   group_by(system) %>%
  #   summarize(value = sum(value_ha, na.rm = T)) %>%
  #   mutate(source = "stat_ha"),
  # # lu_det_all_raw %>%
  #   as.data.frame() %>%
  #   group_by(system) %>%
  #   summarize(value = sum(area, na.rm = T)) %>%
  #   mutate(source = "lu_det_all"),
  # lu_det_raw %>%
  #   as.data.frame() %>%
  #   group_by(system) %>%
  #   summarize(value = sum(area, na.rm = T)) %>%
  #   mutate(source = "det"),
  gia_df %>%
    summarize(value = sum(value, na.rm = T)) %>%
    mutate(source = "gia",
           system = "I"),
  # lu_ir_tot_raw %>%
  #   summarize(value = sum(value, na.rm = T)) %>%
  #   mutate(source = "ir_info"),
  lu_aqua_raw %>%
    filter(variable == "Total harvested irrigated crop area (full control irrigation)") %>%
    group_by(crop) %>%
    mutate(source = paste("aquastat", year, sep = "_"),
           system = "I"))

comp_adm0_sy %>%
  ggplot() +
  geom_col(aes(x = source, y = value, fill = system)) +
  labs(title = paste0("Land use comparison for irrigation in ", iso3c_sel),
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) 


### COMPARE TOTAL LU AT NATIONAL LEVEL FOR SYSTEM I
comp_adm0_sy %>%
  filter(system == "I") %>%
  ggplot() +
  geom_col(aes(x = source, y = value, fill = source)) +
  labs(title = paste0("Land use comparison for irrigation in ", iso3c_sel),
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) 


#--------------------------------------------------------------------------------------------------
### COMPARE SYSTEMS AT ADM0 FOR CROPS
# combine data
comp_adm0_system_crop <- bind_rows(
  lu_sy_raw %>%
    dplyr::select(value, crop, adm, adm_level, system) %>%
    filter(adm_level == 0) %>%
    mutate(source = "stat_pa"),
  lu_sy_raw %>%
    dplyr::select(value = value_ha, crop, adm, adm_level, system) %>%
    filter(adm_level == 0) %>%
    mutate(source = "stat_ha"),
  # lu_det_raw %>%
  #   as.data.frame() %>%
  #   group_by(crop, system) %>%
  #   summarize(value = sum(area, na.rm = T)) %>%
  #   mutate(source = "det"),
  lu_aqua_raw %>%
    group_by(crop) %>%
    filter(crop != "total") %>%
    ungroup() %>%
    mutate(source = paste("aquastat", year, sep = "_"))) %>%
  filter(value != 0) %>%
  ungroup %>%
  group_by(crop) %>%
  mutate(n = length(unique(source))) %>%
  filter(n >1)

comp_adm0_system_crop %>%
  ggplot() +
  geom_col(aes(x = source, y = value, fill = system)) +
  labs(title = paste0("Land use comparison by crop across systems in ", iso3c_sel), 
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~crop, scales = "free") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### COMPARE LU BY CROP AT NATIONAL LEVEL FOR SYSTEM I
# combine data
comp_adm0_I_crop <- bind_rows(
  lu_sy_raw %>%
    dplyr::select(adm, crop, system, value, adm_level) %>%
    filter(adm_level == 0, system %in% c("I")) %>%
    mutate(source = "stat_pa"),
  lu_sy_raw %>%
    dplyr::select(adm, crop, system, value = value_ha, adm_level) %>%
    filter(adm_level == 0, system %in% c("I")) %>%
    mutate(source = "stat_ha"),
  # lu_det_raw %>%
  #   as.data.frame() %>%
  #   filter(system == "I") %>%
  #   group_by(crop, system) %>%
  #   summarize(value = sum(area, na.rm = T)) %>%
  #   mutate(source = "det"),
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


# ### COMPARE LU AT ADM1 LEVEL FOR DET AND STAT
# # combine data
# comp_adm1_det_crop <- bind_rows(
#   lu_adm_raw %>%
#     dplyr::select(value, crop, adm, adm_level) %>%
#     filter(adm_level == 1) %>%
#     mutate(source = "stat_pa"),
#   lu_adm_raw %>%
#     dplyr::select(value = value_ha, crop, adm, adm_level) %>%
#     filter(adm_level == 1) %>%
#     mutate(source = "stat_ha"),
#   lu_det_raw %>%
#     as.data.frame() %>%
#     left_join(adm_r) %>%
#     group_by(adm1, crop) %>%
#     summarize(value = sum(area, na.rm = T)) %>%
#     rename(adm = adm1) %>%
#     mutate(source = "det")) %>%
#   ungroup %>%
#   group_by(adm, crop) %>%
#   mutate(det_av = any(source == "det")) %>%
#   filter(det_av) 
# 
# comp_adm1_det_crop %>%
#   ggplot() +
#   geom_col(aes(x = source, y = value, fill = crop), position = "stack") +
#   labs(title = paste0("Land use comparison by adm2 between det and stat in ", iso3c_sel),
#        x = "", y = "ha") +
#   scale_y_continuous(labels = comma) +
#   facet_grid(crop~adm, scale = "free") +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))


### COMPARE LU AT ADM2 LEVEL FOR DET AND STAT
# combine data
comp_adm2_det_crop <- bind_rows(
  lu_adm_raw %>%
    dplyr::select(value, crop, adm, adm_level) %>%
    filter(adm_level == 2) %>%
    mutate(source = "stat_pa"),
  lu_adm_raw %>%
    dplyr::select(value = value_ha, crop, adm, adm_level) %>%
    filter(adm_level == 2) %>%
    mutate(source = "stat_ha"),
  # lu_det_raw %>%
  #   as.data.frame() %>%
  #   left_join(adm_r) %>%
  #   group_by(adm2, crop) %>%
  #   summarize(value = sum(area, na.rm = T)) %>%
  #   rename(adm = adm2) %>%
  #   mutate(source = "det")
) %>%
  ungroup %>%
  group_by(adm, crop) %>%
  mutate(det_av = any(source == "det")) %>%
  filter(det_av) 

comp_adm2_det_crop %>%
  ggplot() +
  geom_col(aes(x = source, y = value, fill = crop), position = "stack") +
  labs(title = paste0("Land use comparison by adm2 between det and stat in ", iso3c_sel),
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) +
  facet_grid(crop~adm, scale = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



##################################################################################################
#' FINDINGS/CONCLUSIONS PER CROP RELATED TO
#' (1) COMPARISON BETWEEN DET AND SYSTEM
#' (2) DET AND ADM
#' (3) SYSTEM AND AQUASTAT
#'
#' Share of irrigation in SPAM is no longer correct because of outdated information and
#' upscaling of adm statistics to FAOSTAT. We use the latest aquastat figures for irrigated area.

#' 
##################################################################################################

#--------------------------------------------------------------------------------------------------
#### COMPARISON OF LC AND LU

### ADM0
comp_adm0_lu_lc <- bind_rows(
  pa_raw %>%
    dplyr::select(pa, crop, adm_name, adm_level) %>%
    filter(adm_level == 0) %>%
    summarize(value = sum(pa, na.rm = T)) %>%
    mutate(source = "stat_pa"),
  lc %>%
    rename(adm = adm0_name) %>%
    group_by(adm) %>%
    summarize(value = sum(lc, na.rm = T)) %>%
    mutate(source = "lc"),
  lc %>%
    rename(adm = adm0_name) %>%
    group_by(adm) %>%
    summarize(value = sum(lc_max, na.rm = T)) %>%
    mutate(source = "lc_max"),
  lc %>%
    rename(adm = adm0_name) %>%
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
  pa_raw %>%
    dplyr::select(pa, crop, adm_name, adm_code, adm_level) %>%
    filter(adm_level == 1) %>%
    group_by(adm_name) %>%
    summarize(value = sum(pa, na.rm = T)) %>%
    mutate(source = "stat_pa"),
  lc %>%
    rename(adm_name = adm1_name) %>%
    group_by(adm_name) %>%
    summarize(value = sum(lc, na.rm = T)) %>%
    mutate(source = "lc"),
  lc %>%
    rename(adm_name = adm1_name) %>%
    group_by(adm_name) %>%
    summarize(value = sum(lc_max, na.rm = T)) %>%
    mutate(source = "lc_max"),
  lc %>%
    rename(adm_name = adm1_name) %>%
    group_by(adm_name) %>%
    summarize(value = sum(grid_size, na.rm = T)) %>%
    mutate(source = "grid_size"))

comp_adm1_lu_lc %>%
  ggplot() +
  geom_col(aes(x = source, y = value, fill = source)) +
  labs(title = paste0("Land use and land cover comparison at adm0 in ", iso3c_sel), 
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~adm_name, scales = "free")

# adm1 where lc < lu
adm1_check <- comp_adm1_lu_lc %>%
  spread(source, value) %>%
  mutate(dif = lc-stat_pa) %>%
  filter(dif < 0)
adm1_check

# plot again
comp_adm1_lu_lc %>%
  filter(adm_name %in% adm1_check$adm_name) %>%
  ggplot() +
  geom_col(aes(x = source, y = value, fill = source)) +
  labs(title = paste0("Land use and land cover comparison at adm0 in ", iso3c_sel), 
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~adm_name, scales = "free")

# adm1 where lc_max < lu
adm1_max_check <- comp_adm1_lu_lc %>%
  spread(source, value) %>%
  mutate(dif = lc_max-stat_pa) %>%
  filter(dif < 0)
adm1_max_check

# plot again
comp_adm1_lu_lc %>%
  filter(adm_name %in% adm1_max_check$adm_name) %>%
  ggplot() +
  geom_col(aes(x = source, y = value, fill = source)) +
  labs(title = paste0("Land use and land cover comparison at adm1 in ", iso3c_sel), 
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~adm_name, scales = "free")

# Find out which crops are causing problems
adm1_problem <- lu_adm_raw %>%
  dplyr::select(value, crop, adm_name, adm_level) %>%
  filter(adm_name %in% adm1_max_check$adm_name) 

adm1_problem %>%
  ggplot() +
  geom_col(aes(x = crop, y = value, fill = crop), position = "stack") +
  labs(title = paste0("Land use and land cover comparison at adm1 in ", iso3c_sel), 
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~adm, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


##################################################################################################
#' FINDINGS/CONCLUSIONS ADM1 LEVEL
#' 
#'
##################################################################################################


### ADM2
comp_adm2_lu_lc <- bind_rows(
  pa_raw %>%
    dplyr::select(pa, crop, adm_name, adm_code, adm_level) %>%
    filter(adm_level == 2) %>%
    group_by(adm_name) %>%
    summarize(value = sum(pa, na.rm = T)) %>%
    mutate(source = "stat_pa"),
  lc %>%
    rename(adm_name = adm2_name) %>%
    group_by(adm_name) %>%
    summarize(value = sum(lc, na.rm = T)) %>%
    mutate(source = "lc"),
  lc %>%
    rename(adm_name = adm2_name) %>%
    group_by(adm_name) %>%
    summarize(value = sum(lc_max, na.rm = T)) %>%
    mutate(source = "lc_max"),
  lc %>%
    rename(adm_name = adm2_name) %>%
    group_by(adm_name) %>%
    summarize(value = sum(grid_size, na.rm = T)) %>%
    mutate(source = "grid_size"))

comp_adm2_lu_lc %>%
  ggplot() +
  geom_col(aes(x = source, y = value, fill = source)) +
  labs(title = paste0("Land use and land cover comparison at adm0 in ", iso3c_sel), 
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~adm_name, scales = "free")

# adm2 where lc < lu
adm2_check <- comp_adm2_lu_lc %>%
  spread(source, value) %>%
  mutate(dif = lc-stat_pa) %>%
  filter(dif < 0)
adm2_check

# plot again
comp_adm2_lu_lc %>%
  filter(adm_name %in% adm2_check$adm_name) %>%
  ggplot() +
  geom_col(aes(x = source, y = value, fill = source)) +
  labs(title = paste0("Land use and land cover comparison at adm0 in ", iso3c_sel), 
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~adm_name, scales = "free")

# adm2 where lc_max < lu
adm2_max_check <- comp_adm2_lu_lc %>%
  spread(source, value) %>%
  mutate(dif = lc_max-stat_pa) %>%
  filter(dif < 0)
adm2_max_check

# plot again
comp_adm2_lu_lc %>%
  filter(adm_name %in% adm2_max_check$adm_name) %>%
  ggplot() +
  geom_col(aes(x = source, y = value, fill = source)) +
  labs(title = paste0("Land use and land cover comparison at adm1 in ", iso3c_sel), 
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~adm_name, scales = "free")

# Find out which crops are causing problems
adm2_problem <- pa_raw %>%
  dplyr::select(pa, crop, adm_name, adm_level) %>%
  filter(adm_name %in% adm2_max_check$adm_name) 

adm2_problem %>%
  ggplot() +
  geom_col(aes(x = crop, y = pa, fill = crop), position = "stack") +
  labs(title = paste0("Land use and land cover comparison at adm1 in ", iso3c_sel), 
       x = "", y = "ha") +
  scale_y_continuous(labels = comma) +
  facet_wrap(~adm_name, scales = "free") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


