#'========================================================================================================================================
#' Project:  mapspam
#' Subject:  Script to combine various statistics
#' Author:   Michiel van Dijk
#' Contact:  michiel.vandijk@wur.nl
#'========================================================================================================================================

############### SET UP ###############
# Install and load pacman package that automatically installs R packages if not available
if("pacman" %in% rownames(installed.packages()) == FALSE) install.packages("pacman")
library(pacman)

# Load key packages
p_load("tidyverse", "readxl", "stringr", "here", "scales", "glue")

# Set root
root <- here()

# R options
options(scipen=999) # Surpress scientific notation
options(digits=4)


############### LOAD DATA ###############
# Harvested area
ha <- read_csv(file.path(param$spam_path, 
  glue("processed_data/agricultural_statistics/ha_adm_{param$year}_{param$iso3c}.csv")))

# Adm list
adm_list <- read_csv(file.path(param$spam_path, 
  glue("processed_data/lists/adm_list_{param$year}_{param$iso3c}.csv")))

# Farming system shares
fs <- read_csv(file.path(param$raw_path,
  glue("subnational_statistics/farming_system_shares_{param$year}_{param$iso3c}.csv")))

# Cropping intensity
ci <- read_csv(file.path(param$raw_path,
  glue("subnational_statistics/cropping_intensity_{param$year}_{param$iso3c}.csv")))


############### PROCESS HARVESTED AREA ###############
# wide to long format
ha <- ha %>% 
  gather(crop, ha, -adm_name, -adm_code, -adm_level)

# Set -999 and empty string values
ha <- ha %>%
  mutate(ha = if_else(ha == -999, NA_real_, ha))

# filter out crops which values are all zero or NA
crop_na_0 <- ha %>%
  group_by(crop) %>%
  filter(all(ha %in% c(0, NA))) %>%
  dplyr::select(crop) %>%
  unique

ha <- ha %>%
  filter(!crop %in% crop_na_0$crop)

# Remove lower level adm data if it would somehow not be used 
ha <- ha %>%
  filter(adm_level <= param$adm_level)


########## PROCESS FARMING SYSTEM SHARES ##########
# wide to long format
fs <- fs %>% 
  gather(crop, fs, -adm_name, -adm_code, -adm_level, -system)

# Set -999 and empty string values
fs <- fs %>%
  mutate(fs = if_else(fs == -999, NA_real_, fs))

# Select relevent crops using ha
fs <- fs %>%
  filter(crop %in% unique(ha$crop))

# Remove lower level adm data if solve_sel = 0
if(param$solve_level == 0){
  fs <- fs %>%
    filter(adm_level == 0) %>%
    dplyr::select(-adm_code, -adm_name, -adm_level)
}


########## CROPPING INTENSITY ##########
# wide to long format
ci <- ci %>% 
  gather(crop, ci, -adm_name, -adm_code, -adm_level, -system)

# Set -999 and empty string values
ci <- ci %>%
  mutate(ci = if_else(ci == -999, NA_real_, ci))

# Select relevent crops using ha
ci <- ci %>%
  filter(crop %in% unique(ha$crop))

# Remove lower level adm data if solve_sel = 0
if(param$solve_level == 0){
  ci <- ci %>%
    filter(adm_level == 0) %>%
    dplyr::select(-adm_code, -adm_name, -adm_level)
}


########## CONSISTENCY CHECKS ##########

# Format checks, numerical values and code is character
# consistency within data: do they add up and are lower level totals not higher than higher level ADMs
# Check if artifical ADMs area is near zero.
# Do shares of farming systems add up.
# Is ADM system the same as the map
# Are crops the same in ci, fs and ha data files. Not possible to have ADM0 data and NA for fs and ci.


########## COMBINE DATA ##########

### CONVERT HARVESTED AREA TO PHYSICAL AREA AND CREATE SYS AREA TOTALS
# In case solve = 0, ADM0 farming system shares are multiplied with ADM0, ADM1, ADM2 subnational 
# statistics to ensure ADM totals add up (i.e. sum(ADM2) = ADM1 and sum(ADM1) = ADM0.
# In case solve = 1, ADM1 farming system shares are multiplied with ADM1 and ADM2 subnational 
# statistics to ensure ADM totals add up (i.e. sum(ADM2) = ADM1.

# Calculate physical area using cropping intensity information.
pa <- ha %>%
  left_join(ci, by = "crop")  %>%
  left_join(fs, by = c("crop", "system"))  %>%
  mutate(pa = ha*fs/ci) %>%
  group_by(adm_name, adm_code, crop, adm_level) %>%
  summarize(pa = plus(pa, na.rm = T)) %>%
  ungroup()

# Calculate ADM0 physical area broken down by farming systems
pa_fs <- pa %>%
  filter(adm_level == 0) %>%
  left_join(fs, by = "crop") %>%
  mutate(pa = pa*fs) %>%
  dplyr::select(-fs) %>%
  ungroup()


########## CONSISTENCY CHECKS ##########
# make sure pa and pa_fs totals are the same
stat_sy_check <- left_join(
  pa %>% 
    filter(adm_level == 0) %>%
    group_by(crop) %>%
    summarize(adm0 = plus(pa, na.rm = F)),
  pa_fs %>%
    group_by(crop) %>%
    summarize(sys0 = plus(pa, na.rm = F))) %>%
  mutate(diff = sys0-adm0)

all.equal(stat_sy_check$adm0, stat_sy_check$sys0)


########## SAVE STATISTICS ##########
# Put everything in wide format for easy reviewe and possible updating
pa <- pa %>%
  spread(crop, pa)

pa_fs <- pa_fs %>%
  spread(crop, pa)

write_csv(pa, file.path(param$spam_path,
  glue("processed_data/agricultural_statistics/pa_{param$year}_{param$iso3c}.csv")))
write_csv(pa_fs, file.path(param$spam_path, 
  glue("pa_fs_{param$year}_{param$iso3c}.csv")))


############### CLEAN UP ###############
rm(adm_list, ci, crop_na_0, fs, ha, pa, pa_fs, stat_sy_check)
