#'========================================================================================================================================
#' Project:  crop_map
#' Subject:  Code to process gaez
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
p_load("tidyverse", "readxl", "stringr", "here", "scales", "glue", "sf", "raster")

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

# Grid
grid <- raster(file.path(proc_path, glue("maps/grid/grid_{grid_sel}_r_{year_sel}_{iso3c_sel}.tif")))
names(grid) <- "gridID"

# Synergy cropland
cl_raw <- readRDS(file.path(proc_path, glue("harmonized/cl_{grid_sel}_{year_sel}_{iso3c_sel}.rds")))

# Farming system
pa_fs_raw <- read_csv(file.path(proc_path, glue("harmonized/pa_fs_{year_sel}_{iso3c_sel}.csv"))) 

# GAEZ suitability 
suit_files <- list.files(file.path(proc_path, glue("maps/gaez/{grid_sel}/suitability")), full.names = T, pattern = glob2rx("*.tif"))
suit_names <- basename(suit_files)
suit_names <- unlist(lapply(strsplit(suit_names, "_"), function(x) paste(x[1], x[2], sep="_")))
suit_raw <- stack(suit_files)
names(suit_raw) <- suit_names

# GAEZ potential yield
py_files <- list.files(file.path(glue(proc_path, "/maps/gaez/{grid_sel}/potential_yield/")), full.names=TRUE, pattern = glob2rx("*.tif"))
py_names <- basename(py_files)
py_names <- unlist(lapply(strsplit(py_names, "_"), function(x) paste(x[1], x[2], sep="_")))
py_raw <- stack(py_files)
names(py_raw) <- py_names

# dm2fm comnversion for GAEZ potential yield
dm2fm <- read_excel(file.path(mappings_path, "mappings_spam.xlsx"), sheet = "gaez_dm2fm")

# GAEZ substitute crops when data is missing
gaez_subs <- read_excel(file.path(mappings_path, "mappings_spam.xlsx"), sheet = "gaez_subs")


############### PREPARATIONS ###############
# Put statistics in long format
pa_fs <- pa_fs_raw %>%
  gather(crop, pa, -adm_code, -adm_name, -adm_level, -system)

## Create gridID and system combinations
crop_system <- pa_fs %>%
  filter(adm_level == 0, pa != 0) %>%
  mutate(crop_system = paste(crop, system , sep = "_"))

priors_base <- expand.grid(gridID = unique(cl_raw$gridID), 
                           crop_system = unique(crop_system$crop_system), stringsAsFactors = F) %>%
  separate(crop_system, into = c("crop", "system"), sep = "_", remove = F)

# create gridID list
grid_df <- as.data.frame(rasterToPoints(grid))


############### GAEZ SUITABILITY ###############
# Select only relevant suit layers
suit_r <- subset(suit_raw, crop_system$crop_system)

# Create df
suit <- as.data.frame(rasterToPoints(stack(grid, suit_r))) %>%
  filter(gridID %in% unique(cl_raw$gridID)) 
summary(suit)

# There could be na values on the edges, related with the downscaling procedure, we inspect maiz_S
suit_na <- dplyr::select(suit, x, y, maiz_S) %>%
  mutate(check = ifelse(is.na(maiz_S), 1, NA)) %>%
  filter(!is.na(check))

# There also seem to be negative suit values, possible because of change in resolution. We set them to zero.
suit <- suit %>%
  dplyr::select(-x, -y) %>%
  gather(crop_system, suit, -gridID) %>%
  filter(crop_system %in% priors_base$crop_system, !is.na(gridID)) %>%
  mutate(suit = ifelse(is.na(suit), 0, suit),
         suit = ifelse(suit < 0, 0, suit))
summary(suit)

# For some crops the suitability maps are 0 throughout. 
# We replace this by maps for comparable crops.
suit_zero <- suit %>%
  group_by(crop_system) %>%
  summarize(sum_suit = sum(suit, na.rm = T)) %>%
  filter(sum_suit == 0)
suit_zero <- suit_zero$crop_system

# Replace missing suit
suit_rep <- map_df(suit_zero, replace_gaez, suit, suit_raw) %>%
  rename(suit = value)

suit <- bind_rows(
  suit %>%
    filter(!crop_system %in% suit_zero),
  suit_rep)

# Consistency checks
if(!identical(sort(unique(suit$crop_system)), sort(unique(crop_system$crop_system)))){
  message("List of crop_system in suitability data is not the same as in statistics.")
  crp_sys_dif <- setdiff(unique(crop_system$crop_system), unique(suit$crop_system))
  stop(glue("These are different: {crp_sys_dif}"))
  }

suit_zero <- suit %>%
  group_by(crop_system) %>%
  summarize(sum_suit = sum(suit, na.rm = T)) %>%
  filter(sum_suit == 0)
suit_zero <- suit_zero$crop_system
if(!is_empty(suit_zero)){
  stop(glue("The following crop_system are all zero: {suit_zero}"))
}

  
############### GAEZ POTENTIAL YIELD ###############
# Select only relevant gaez layers
py_r <- subset(py_raw, crop_system$crop_system)

# Create df
py <- as.data.frame(rasterToPoints(stack(grid, py_r))) %>%
  filter(gridID %in% unique(cl_raw$gridID)) 
summary(py)

# There could be na values on the edges, related with the downscaling procedure, we inspect maiz_S
py_na <- dplyr::select(py, gridID, x, y, maiz_S) %>%
  mutate(check = ifelse(is.na(maiz_S), 1, NA)) %>%
  filter(!is.na(check))

# py_na <- rasterFromXYZ(dplyr::select(py_na, x, y, check))
# plot(py_na$check)  
# plot(adm, add = T)

# There also seem to be negative py values, possible because of change in resolution. We set them to zero.
py <- py %>%
  dplyr::select(-x, -y) %>%
  gather(crop_system, py, -gridID) %>%
  filter(crop_system %in% priors_base$crop_system, !is.na(gridID)) %>%
  mutate(py = ifelse(is.na(py), 0, py),
         py = ifelse(py < 0, 0, py))
summary(py)

# For some crops the potential yield maps are 0 throughout. 
# We replace this by maps for comparable crops.
py_zero <- py %>%
  group_by(crop_system) %>%
  summarize(sum_py = sum(py, na.rm = T)) %>%
  filter(sum_py == 0)
py_zero <- py_zero$crop_system

# Replace missing suit
py_rep <- map_df(py_zero, replace_gaez, py, py_raw) %>%
  rename(py = value)

py <- bind_rows(
  py %>%
    filter(!crop_system %in% py_zero),
  py_rep)

# Consistency checks
if(!identical(sort(unique(py$crop_system)), sort(unique(crop_system$crop_system)))){
  message("List of crop_system in potential yield data is not the same as in statistics.")
  crp_sys_dif <- setdiff(unique(crop_system$crop_system), unique(py$crop_system))
  stop(glue("These are different: {crp_sys_dif}"))
}

py_zero <- py %>%
  group_by(crop_system) %>%
  summarize(sum_py = sum(py, na.rm = T)) %>%
  filter(sum_py == 0)
py_zero <- py_zero$crop_system
if(!is_empty(py_zero)){
  stop(glue("The following crop_system are all zero: {py_zero}"))
}


# Convert to fm
py <- py %>%
  separate(crop_system, into = c("crop", "system"), sep = "_", remove = F) %>%
  left_join(dm2fm) %>%
  mutate(py = py/t_factor) %>%
  dplyr::select(-t_factor)


############### SAVE ###############
# save GAEZ suit
saveRDS(suit, file.path(proc_path, glue("harmonized/suit_{grid_sel}_{year_sel}_{iso3c_sel}.rds")))

# save GAEZ py
saveRDS(py, file.path(proc_path, glue("harmonized/py_{grid_sel}_{year_sel}_{iso3c_sel}.rds")))


############### CLEAN UP ###############
rm(adm, cl_raw, crop_system, dm2fm, gaez_subs, grid, grid_df, pa_fs, pa_fs_raw, priors_base,
   py, py_na, py_names, py_raw, py_rep, py_zero, py_files, py_r, suit, suit_na, suit_raw, suit_zero, 
   suit_rep, suit_files, suit_names, suit_r)
