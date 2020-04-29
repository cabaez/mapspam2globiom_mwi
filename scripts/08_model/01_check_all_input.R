#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Code to harmonize physical area, cropland and irrigated area information
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
p_load("tidyverse", "readxl", "stringr", "here", "scales", "glue", "sf", "raster", "mapview")

# Set root
root <- here()

# R options
options(scipen=999) # Supress scientific notation
options(digits=4) # limit display to four digits


### SOURCE FUNCTIONS
# TO_UPDATE
source(file.path(root, "Code/general/mapspam_functions.r"))


############### LOAD DATA ###############
# Adm_r
adm_r <- readRDS(file.path(proc_path, glue("maps/adm/adm_r_{grid_sel}_{year_sel}_{iso3c_sel}.rds"))) 

# Physical area
pa_raw <- read_csv(file.path(proc_path, glue("harmonized/pa_{year_sel}_{iso3c_sel}.csv"))) 

# Farming system
pa_fs_raw <- read_csv(file.path(proc_path, glue("harmonized/pa_fs_{year_sel}_{iso3c_sel}.csv"))) 

# Synergy cropland
cl_raw <- readRDS(file.path(proc_path, glue("harmonized/cl_{grid_sel}_{year_sel}_{iso3c_sel}.rds")))

# Synergy irrigated area
cl_ir_raw <- readRDS(file.path(proc_path, glue("harmonized/cl_ir_{grid_sel}_{year_sel}_{iso3c_sel}.rds")))

# Score
score_raw <- readRDS(file.path(proc_path, glue("harmonized/score_{grid_sel}_{year_sel}_{iso3c_sel}.rds")))

# Adm list
adm_list <- read_csv(file.path(proc_path, glue("lists/adm_map_list_{year_sel}_{iso3c_sel}.csv")))


############### PREPARATIONS ###############
# Put statistics in long format
pa <- pa_raw %>%
  gather(crop, pa, -adm_code, -adm_name, -adm_level)

pa_fs <- pa_fs_raw %>%
  gather(crop, pa, -adm_code, -adm_name, -adm_level, -system)


############### CONSISTENCY CHECKS ###############

### TO_UPDATE
# CHECK if all values are > 0!!

### COMPARE ADM0 AND SYS
pa_adm0 <- pa %>%
  filter(adm_level == 0) %>%
  summarize(pa = sum(pa, na.rm = T))
  
pa_fs0 <- pa_fs %>%
  filter(adm_level == 0) %>%
  summarize(pa = sum(pa, na.rm = T))

cat("Total adm0 area", pa_adm0$pa)
cat("Total adm0 system area", pa_fs0$pa)
cat("Difference:", pa_adm0$pa-pa_fs0$pa)

# Compare adm0 and SYS at crop level
check_adm_sy <- left_join(
  pa %>%
    filter(adm_level == 0) %>% 
    rename(adm0 = pa),
  pa_fs0 <- pa_fs %>%
    filter(adm_level == 0) %>%
    group_by(crop) %>%
    summarize(sy0 = sum(pa, na.rm = T))) %>%
  mutate(diff = abs(adm0-sy0))
all.equal(check_adm_sy$adm0, check_adm_sy$sy0)


### COMPARE ADM0 AND LC
cl_adm0 <- cl_raw %>%
  filter(adm_level == adm_sel) %>%
  summarize(cl = sum(cl, na.rm = T))

cat("Total adm0 area", pa_adm0$pa)
cat("Total lc area", cl_adm0$cl)
cat("Difference:", cl_adm0$cl - pa_adm0$pa)


### COMPARE ADM1 AND LC
cl_adm1 <- cl_raw %>%
  rename(adm1_code = adm_code) %>%
  left_join(adm_list) %>%
  group_by(adm1_code, adm1_name) %>%
  summarize(cl = sum(cl, na.rm = T)) 

pa_adm1 <- pa %>%
  filter(adm_level == 1) %>%
  rename(adm1_code = adm_code, adm1_name = adm_name) %>%
  group_by(adm1_code, adm1_name) %>%
  summarize(pa = sum(pa, na.rm = T))

check_cl_pa_adm1 <- left_join(cl_adm1, pa_adm1) %>%
  mutate(diff = cl-pa)
filter(check_cl_pa_adm1, diff < 0)


### COMPARE ADM2 AND LC
cl_adm2 <- cl_raw %>%
  rename(adm2_code = adm_code) %>%
  left_join(adm_list) %>%
  group_by(adm2_code, adm2_name) %>%
  summarize(cl = sum(cl, na.rm = T)) 

pa_adm2 <- pa %>%
  filter(adm_level == 2) %>%
  rename(adm2_code = adm_code, adm2_name = adm_name) %>%
  group_by(adm2_code, adm2_name) %>%
  summarize(pa = sum(pa, na.rm = T))

check_cl_pa_adm2 <- left_join(cl_adm2, pa_adm2) %>%
  mutate(diff = cl-pa)
filter(check_cl_pa_adm2, diff < 0)


### COMPARE ADM0 AND ADM1
pa_adm1 <- pa %>%
  filter(adm_level == 1) %>%
  summarize(pa = sum(pa, na.rm = T))

cat("Total adm0 area", pa_adm0$pa)
cat("Total sum adm1 area", pa_adm1$pa)
cat("Difference:", pa_adm0$pa-pa_adm1$pa)


### COMPARE ADM1 and ADM2 AT CROP LEVEL
pa_adm1_crop <- pa %>%
  filter(adm_level == 1) %>%
  rename(adm1_code = adm_code) %>%
  mutate(source = "adm1")

pa_adm2_crop <- pa %>%
  filter(adm_level == 2) %>%
  rename(adm2_code = adm_code) %>%
  left_join(adm_list) %>%
  group_by(adm1_code, crop) %>%
  summarize(pa = plus(pa)) %>%
  mutate(source = "adm2")

check_pa_adm1_crop <- bind_rows(pa_adm1_crop, pa_adm2_crop) %>%
  dplyr::select(adm1_code, crop, pa, source) %>%
  spread(source, pa) %>%
  mutate(diff = adm1-adm2)
check <- na.omit(check_pa_adm1_crop[c("adm1", "adm2")])
all.equal(check$adm1, check$adm2)


### COMPARE ADM0 and ADM1 AT CROP LEVEL
pa_adm0_crop <- pa %>%
  filter(adm_level == 0) %>%
  mutate(source = "adm0")

pa_adm1_crop <- pa %>%
  filter(adm_level == 1) %>%
  group_by(crop) %>%
  summarize(pa = plus(pa)) %>%
  mutate(source = "adm1")

check_pa_adm0_crop <- bind_rows(pa_adm0_crop, pa_adm1_crop) %>%
  dplyr::select(crop, pa, source) %>%
  spread(source, pa) %>%
  mutate(diff = adm0-adm1)
check <- na.omit(check_pa_adm0_crop[c("adm0", "adm1")])
all.equal(check$adm0, check$adm1)


### COMPARE IR_AREA and I
cl_ir <- cl_ir_raw %>%
  summarize(cl_ir = sum(cl_ir, na.rm = T))

pa_I <- pa_fs %>%
  filter(system == "I", adm_level == 0) %>%
  summarize(pa = sum(pa, na.rm = T))

cat("Total potentially irrigated area:", cl_ir$cl_ir)
cat("Total irrigated area required", pa_I$pa)
cat("Difference:", cl_ir$cl_ir - pa_I$pa)


### CHECK IF ALL CROP-SYSTEM COMBINATIONS ARE PRESENT IN SCORE
# We exclude S as it does not depend on score
check_score_cs <- bind_rows(
  pa_fs %>%
    filter(pa != 0) %>%
    dplyr::select(crop, system) %>%
    mutate(source = "sy", value = 1) %>%
    unique(),
  score_raw %>%
    filter(score != 0) %>%
    dplyr::select(crop, system) %>%
    mutate(source = "score", value = 1) %>%
    unique()) %>%
  spread(source, value) %>%
  filter(system != "S") %>%
  filter(is.na(system) | is.na(score))
check_score_cs

### CHECK IF ALL GRIDID IN LC ARE ALSO PRESENT IN SCORE



### CHECK TOTAL OF PRIORS, MUST BE 1 FOR EACH CROP_SYSTEM

### CHECK IF THERE ARE NO NAs IN PRIORS
# na_prior <- priors_raw %>%
#   filter(is.na(prior_scaled))
# unique(na_prior$crop_system)


### CHECK IF SUM OF LC FOR WHICH PRIOR > 0 is > LU FOR EACH CROP_SYSTEM

### CLEAN UP
rm(list = ls()[grep("check", ls())])
rm(list = ls()[grep("pa_", ls())])
rm(list = ls()[grep("cl", ls())])
rm(adm_r, score_raw, priors_raw, na_prior, adm_list, pa)

